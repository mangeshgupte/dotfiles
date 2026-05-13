;;; markdown-mermaid.el --- Inline Mermaid previews in markdown buffers -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Render ```mermaid fenced code blocks to PNG via the
;;              mmdc CLI and display them as inline overlays after
;;              each block.  Designed to layer on top of markdown-mode
;;              without replacing it.
;;
;; Requirements:
;;   - mmdc (npm install -g @mermaid-js/mermaid-cli) on PATH
;;   - Emacs built with image support (PNG)
;;
;; Behavior:
;;   - Each ```mermaid block is hashed; only changed blocks re-render.
;;   - Rendering runs asynchronously via make-process; the buffer is
;;     never blocked.  Overlays update when mmdc finishes.
;;   - Cached PNGs live in `markdown-mermaid-cache-dir' and are keyed
;;     by SHA1 of the block contents + theme, so toggling themes
;;     re-renders correctly without invalidating the other variant.
;;   - Toggling the minor mode off removes all overlays; turning it
;;     back on re-renders from cache (instant for unchanged blocks).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar markdown-mermaid-mode)  ; forward ref for trigger functions

(defgroup markdown-mermaid nil
  "Inline Mermaid previews in markdown buffers."
  :group 'markdown)

(defcustom markdown-mermaid-mmdc-executable "mmdc"
  "Path to the Mermaid CLI executable."
  :type 'string
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-cache-dir
  (expand-file-name "markdown-mermaid" (or (getenv "XDG_CACHE_HOME")
                                            "~/.cache"))
  "Directory for cached rendered PNGs."
  :type 'directory
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-image-scale 0.8
  "Scale factor applied to rendered images."
  :type 'number
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-render-on-save t
  "If non-nil, re-render previews after each save."
  :type 'boolean
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-render-idle-delay nil
  "If a number, re-render after this many seconds of idle time.
Set to nil to disable idle rendering and rely on save/manual refresh."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'markdown-mermaid)

(defconst markdown-mermaid--block-re
  "^```mermaid[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n```$"
  "Regexp matching a fenced ```mermaid block.  Group 1 is the body.")

(defvar-local markdown-mermaid--idle-timer nil
  "Buffer-local idle timer, if any.")

(defvar-local markdown-mermaid--pending nil
  "Hash table mapping cache keys to t for in-flight renders.")

;;; Cache helpers -----------------------------------------------------

(defun markdown-mermaid--theme ()
  "Return `dark' or `default' based on current frame background."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default"))

(defun markdown-mermaid--cache-key (code)
  "Return a stable cache key for CODE under the active theme."
  (concat (sha1 code) "-" (markdown-mermaid--theme)))

(defun markdown-mermaid--cache-path (key)
  "Return the absolute PNG path for KEY."
  (expand-file-name (concat key ".png") markdown-mermaid-cache-dir))

(defun markdown-mermaid--ensure-cache-dir ()
  (unless (file-directory-p markdown-mermaid-cache-dir)
    (make-directory markdown-mermaid-cache-dir t)))

;;; Rendering ---------------------------------------------------------

(defun markdown-mermaid--render-async (code key callback)
  "Render CODE to PNG keyed by KEY, then call CALLBACK with the path.
If a render for KEY is already in flight, do nothing."
  (unless (gethash key markdown-mermaid--pending)
    (puthash key t markdown-mermaid--pending)
    (markdown-mermaid--ensure-cache-dir)
    (let* ((in  (make-temp-file "mmd-in-" nil ".mmd"))
           (out (markdown-mermaid--cache-path key))
           (theme (markdown-mermaid--theme))
           (buf (current-buffer)))
      (with-temp-file in (insert code))
      (markdown-mermaid--spawn in out theme key buf callback))))

(defun markdown-mermaid--spawn (in out theme key buf callback)
  "Spawn mmdc reading IN, writing OUT, with THEME.  Notify BUF/CALLBACK."
  (make-process
     :name "markdown-mermaid"
     :noquery t
     :buffer (generate-new-buffer " *mmdc*")
     :command (list markdown-mermaid-mmdc-executable
                    "-i" in
                    "-o" out
                    "-b" "transparent"
                    "-t" theme)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (ignore-errors (delete-file in))
         (let ((procbuf (process-buffer proc)))
           (when (buffer-live-p procbuf) (kill-buffer procbuf)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (remhash key markdown-mermaid--pending)
             (when (and (= (process-exit-status proc) 0)
                        (file-exists-p out))
               (funcall callback out))))))))

;;; Overlays ----------------------------------------------------------

(defun markdown-mermaid--make-image-overlay (end png)
  "Create or update overlay after END displaying PNG."
  (let ((ov (or (cl-find-if (lambda (o) (overlay-get o 'markdown-mermaid))
                            (overlays-at end))
                (make-overlay end end))))
    (overlay-put ov 'markdown-mermaid t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'after-string
                 (concat "\n"
                         (propertize " "
                                     'display
                                     (create-image png nil nil
                                                   :scale markdown-mermaid-image-scale))
                         "\n"))
    ov))

(defun markdown-mermaid--clear-overlays ()
  (remove-overlays (point-min) (point-max) 'markdown-mermaid t))

;;; Walk + dispatch ---------------------------------------------------

(defun markdown-mermaid--blocks ()
  "Return list of (BEG END BODY) for each mermaid block in buffer."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-mermaid--block-re nil t)
        (push (list (match-beginning 0) (match-end 0) (match-string 1))
              blocks)))
    (nreverse blocks)))

(defun markdown-mermaid-render-buffer ()
  "Render or refresh previews for all mermaid blocks in buffer."
  (interactive)
  (unless markdown-mermaid--pending
    (setq markdown-mermaid--pending (make-hash-table :test 'equal)))
  ;; Drop overlays whose underlying block no longer exists.  We rebuild
  ;; them per-block below — cheap and avoids stale image positions.
  (markdown-mermaid--clear-overlays)
  (let ((buf (current-buffer)))
    (dolist (block (markdown-mermaid--blocks))
      (let* ((end  (nth 1 block))
             (body (nth 2 block))
             (key  (markdown-mermaid--cache-key body))
             (png  (markdown-mermaid--cache-path key)))
        (if (file-exists-p png)
            (markdown-mermaid--make-image-overlay end png)
          (markdown-mermaid--render-async
           body key
           (lambda (path)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Re-locate the block by hash — buffer may have shifted
                 ;; while mmdc was running.
                 (save-excursion
                   (goto-char (point-min))
                   (while (re-search-forward markdown-mermaid--block-re nil t)
                     (when (equal (markdown-mermaid--cache-key (match-string 1))
                                  key)
                       (markdown-mermaid--make-image-overlay
                        (match-end 0) path)))))))))))))

;;; Triggers ----------------------------------------------------------

(defun markdown-mermaid--after-save ()
  (when (and markdown-mermaid-mode markdown-mermaid-render-on-save)
    (markdown-mermaid-render-buffer)))

(defun markdown-mermaid--start-idle-timer ()
  (when (and markdown-mermaid-render-idle-delay
             (null markdown-mermaid--idle-timer))
    (setq markdown-mermaid--idle-timer
          (run-with-idle-timer markdown-mermaid-render-idle-delay t
                               (lambda ()
                                 (when (and (buffer-live-p (current-buffer))
                                            markdown-mermaid-mode)
                                   (markdown-mermaid-render-buffer)))))))

(defun markdown-mermaid--cancel-idle-timer ()
  (when markdown-mermaid--idle-timer
    (cancel-timer markdown-mermaid--idle-timer)
    (setq markdown-mermaid--idle-timer nil)))

;;;###autoload
(define-minor-mode markdown-mermaid-mode
  "Toggle inline Mermaid diagram previews in markdown buffers.
When enabled, ```mermaid fenced blocks are rendered to PNG via the
mmdc CLI and shown as inline overlays.  Renders run asynchronously
and are cached by content hash."
  :lighter " Mmd"
  :group 'markdown-mermaid
  (if markdown-mermaid-mode
      (progn
        (unless (executable-find markdown-mermaid-mmdc-executable)
          (message "markdown-mermaid: %s not found on PATH"
                   markdown-mermaid-mmdc-executable))
        (setq markdown-mermaid--pending (make-hash-table :test 'equal))
        (add-hook 'after-save-hook #'markdown-mermaid--after-save nil t)
        (markdown-mermaid--start-idle-timer)
        (markdown-mermaid-render-buffer))
    (remove-hook 'after-save-hook #'markdown-mermaid--after-save t)
    (markdown-mermaid--cancel-idle-timer)
    (markdown-mermaid--clear-overlays)
    (setq markdown-mermaid--pending nil)))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here
