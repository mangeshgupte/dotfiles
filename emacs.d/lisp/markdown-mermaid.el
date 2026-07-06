;;; markdown-mermaid.el --- Inline Mermaid previews in markdown buffers -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Render ```mermaid fenced code blocks to SVG via the
;;              mmdc CLI and display each image in place of its source.
;;              Designed to layer on top of markdown-mode without
;;              replacing it.
;;
;; Requirements:
;;   - mmdc (npm install -g @mermaid-js/mermaid-cli) on PATH
;;   - Emacs built with SVG support (image-type-available-p 'svg)
;;
;; Behavior:
;;   - Each ```mermaid block is hashed; only changed blocks re-render.
;;     Output is SVG with htmlLabels disabled, so labels are real SVG
;;     text that librsvg renders (HTML/foreignObject labels do NOT render
;;     in Emacs), in the buffer's prose font (`markdown-mermaid-font-family').
;;   - Rendering runs asynchronously via make-process; the buffer is
;;     never blocked.  Overlays update when mmdc finishes.
;;   - The image replaces the block source via an overlay.  Move point
;;     into a block to reveal the source for editing; move out to see the
;;     image.  Previews are scaled to fit the window and re-fit when the
;;     window width changes (`markdown-mermaid-fit-window').
;;   - Cached SVGs live in `markdown-mermaid-cache-dir', keyed by SHA1 of
;;     the block contents + theme + font, so changing any of them
;;     re-renders without invalidating the other variants.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'markdown-emoji)

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
  "Directory for cached rendered SVGs."
  :type 'directory
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-image-scale 0.8
  "Scale factor applied to rendered images (before window fitting)."
  :type 'number
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-font-family nil
  "Font family for diagram labels, or nil to follow the buffer prose font.
When nil, the `variable-pitch' face family is used (falling back to a
generic sans-serif).  The font must be resolvable by mmdc's renderer."
  :type '(choice (const :tag "Follow variable-pitch face" nil) string)
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-fit-window t
  "If non-nil, scale previews down to fit the window width.
Previews re-fit when the window width changes."
  :type 'boolean
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

(defvar-local markdown-mermaid--overlays nil
  "List of preview overlays in the current buffer.")

(defvar-local markdown-mermaid--last-width nil
  "Pixel width the previews were last fitted to, or nil.")

(defvar-local markdown-mermaid--resize-timer nil
  "Pending idle timer for a fit re-render, or nil.")

;;; Cache helpers -----------------------------------------------------

(defun markdown-mermaid--theme ()
  "Return `dark' or `default' based on current frame background."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default"))

(defun markdown-mermaid--font-family ()
  "Return the font family for diagram labels.
`markdown-mermaid-font-family' if set, else the `variable-pitch' face
family (the buffer's prose font), else a generic sans-serif."
  (or markdown-mermaid-font-family
      (let ((f (face-attribute 'variable-pitch :family nil t)))
        (and (stringp f) f))
      "sans-serif"))

(defun markdown-mermaid--cache-key (code)
  "Return a stable cache key for CODE under the active theme and font."
  (concat (sha1 (concat code "\0" (markdown-mermaid--font-family)))
          "-" (markdown-mermaid--theme)))

(defun markdown-mermaid--cache-path (key)
  "Return the absolute SVG path for KEY."
  (expand-file-name (concat key ".svg") markdown-mermaid-cache-dir))

(defun markdown-mermaid--ensure-cache-dir ()
  (unless (file-directory-p markdown-mermaid-cache-dir)
    (make-directory markdown-mermaid-cache-dir t)))

(defun markdown-mermaid--available-width ()
  "Return the usable pixel width for previews, or nil when unconstrained.
The text-area width of the narrowest window showing the buffer, less a
small edge gap.  Nil when fitting is disabled or the buffer is not
displayed in any window."
  (when markdown-mermaid-fit-window
    (let ((windows (get-buffer-window-list nil nil t)))
      (when windows
        (max 1 (- (apply #'min (mapcar (lambda (w) (window-body-width w t))
                                       windows))
                  (* 2 (frame-char-width))))))))

(defun markdown-mermaid--postprocess-svg (file)
  "Give FILE's root <svg> a concrete pixel size.
mmdc emits width=\"100%\" with a max-width style, which librsvg renders
at an unpredictable size and which defeats `:max-width' fitting.  Replace
it with explicit width/height from the viewBox."
  (when (file-exists-p file)
    (let ((svg (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
      (when (string-match
             "viewBox=\"[0-9.eE+-]+ [0-9.eE+-]+ \\([0-9.eE+]+\\) \\([0-9.eE+]+\\)\""
             svg)
        (let ((w (round (string-to-number (match-string 1 svg))))
              (h (round (string-to-number (match-string 2 svg)))))
          (setq svg (replace-regexp-in-string
                     "width=\"100%\""
                     (format "width=\"%d\" height=\"%d\"" w h)
                     svg t t))
          (setq svg (replace-regexp-in-string "max-width:[ ]*[0-9.]+px;?" "" svg))
          (let ((coding-system-for-write 'utf-8))
            (with-temp-file file (insert svg))))))))

;;; Rendering ---------------------------------------------------------

(defun markdown-mermaid--json-escape (s)
  "Escape backslashes and double quotes in S for embedding in JSON."
  (replace-regexp-in-string "[\"\\]" "\\\\\\&" s))

(defun markdown-mermaid--config-json ()
  "Return a mermaid JSON config: theme, htmlLabels off, and the label font.
`htmlLabels' is disabled so labels are SVG text (librsvg cannot render
the default HTML/foreignObject labels)."
  (format (concat "{\"theme\":\"%s\",\"htmlLabels\":false,"
                  "\"flowchart\":{\"htmlLabels\":false},"
                  "\"themeVariables\":{\"fontFamily\":\"%s\"}}")
          (markdown-mermaid--theme)
          (markdown-mermaid--json-escape (markdown-mermaid--font-family))))

(defun markdown-mermaid--render-async (code key callback)
  "Render CODE to SVG keyed by KEY, then call CALLBACK with the path.
If a render for KEY is already in flight, do nothing."
  (unless (gethash key markdown-mermaid--pending)
    (puthash key t markdown-mermaid--pending)
    (markdown-mermaid--ensure-cache-dir)
    (let* ((in  (make-temp-file "mmd-in-" nil ".mmd"))
           (cfg (make-temp-file "mmd-cfg-" nil ".json"))
           (out (markdown-mermaid--cache-path key))
           (buf (current-buffer))
           (coding-system-for-write 'utf-8))
      (with-temp-file in (insert code))
      (with-temp-file cfg (insert (markdown-mermaid--config-json)))
      (markdown-mermaid--spawn in cfg out key buf callback))))

(defun markdown-mermaid--spawn (in cfg out key buf callback)
  "Spawn mmdc reading IN with config CFG, writing SVG OUT.
Notify BUF/CALLBACK on success."
  (make-process
     :name "markdown-mermaid"
     :noquery t
     :buffer (generate-new-buffer " *mmdc*")
     :command (list markdown-mermaid-mmdc-executable
                    "-i" in
                    "-o" out
                    "-e" "svg"
                    "-b" "transparent"
                    "-c" cfg)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (ignore-errors (delete-file in))
         (ignore-errors (delete-file cfg))
         (let ((procbuf (process-buffer proc)))
           (when (buffer-live-p procbuf) (kill-buffer procbuf)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (remhash key markdown-mermaid--pending)
             (when (and (= (process-exit-status proc) 0)
                        (file-exists-p out))
               (markdown-mermaid--postprocess-svg out)
               (funcall callback out))))))))

;;; Overlays ----------------------------------------------------------

(defun markdown-mermaid--make-image-overlay (beg end svg &optional max-px)
  "Overlay BEG..END with the image at SVG, replacing the block source.
When MAX-PX is non-nil the image is scaled down to fit that many pixels.
The image is shown by default; `markdown-mermaid--reveal' swaps it for
the source while point is inside the block.  The overlay spans the whole
block (never an empty region), so the `evaporate' property is safe -- an
empty overlay carrying `evaporate' is deleted by Emacs the instant it is
created, which previously destroyed the preview."
  (let ((ov (make-overlay beg end))
        (image (apply #'create-image svg 'svg nil
                      :scale markdown-mermaid-image-scale
                      :ascent 'center
                      (when max-px (list :max-width max-px)))))
    (overlay-put ov 'markdown-mermaid t)
    (overlay-put ov 'mm-image image)
    (overlay-put ov 'display image)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'help-echo "Mermaid preview -- move point in to edit")
    (push ov markdown-mermaid--overlays)
    ov))

(defun markdown-mermaid--clear-overlays ()
  (mapc (lambda (ov) (when (overlayp ov) (delete-overlay ov)))
        markdown-mermaid--overlays)
  (setq markdown-mermaid--overlays nil)
  (remove-overlays (point-min) (point-max) 'markdown-mermaid t))

(defun markdown-mermaid--reveal ()
  "Show source for the block under point, the image for the others."
  (dolist (ov markdown-mermaid--overlays)
    (when (overlay-buffer ov)
      (if (and (>= (point) (overlay-start ov))
               (<= (point) (overlay-end ov)))
          (overlay-put ov 'display nil)
        (overlay-put ov 'display (overlay-get ov 'mm-image))))))

;;; Walk + dispatch ---------------------------------------------------

(defun markdown-mermaid--blocks ()
  "Return list of (BEG END BODY) for each mermaid block in buffer."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-mermaid--block-re nil t)
        ;; Sanitize here so the body carried downstream (cache key, mmdc
        ;; input, image) is emoji-free -- mmdc would emit them as SVG <text>
        ;; and crash librsvg.  See `markdown-emoji-sanitize'.
        (push (list (match-beginning 0) (match-end 0)
                    (markdown-emoji-sanitize (match-string 1)))
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
  (let ((buf (current-buffer))
        (target (markdown-mermaid--available-width)))
    (setq markdown-mermaid--last-width target)
    (dolist (block (markdown-mermaid--blocks))
      (let* ((beg  (nth 0 block))
             (end  (nth 1 block))
             (body (nth 2 block))
             (key  (markdown-mermaid--cache-key body))
             (svg  (markdown-mermaid--cache-path key)))
        (if (file-exists-p svg)
            (markdown-mermaid--make-image-overlay beg end svg target)
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
                     (when (equal (markdown-mermaid--cache-key
                                   (markdown-emoji-sanitize (match-string 1)))
                                  key)
                       (markdown-mermaid--make-image-overlay
                        (match-beginning 0) (match-end 0) path
                        markdown-mermaid--last-width))))
                 (markdown-mermaid--reveal)))))))))
  ;; A block under point stays revealed after a (re-)render.
  (markdown-mermaid--reveal))

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

(defun markdown-mermaid--schedule-render (window)
  "Schedule a fit re-render for WINDOW's buffer after a resize/rearrange.
Runs on a short idle timer to coalesce resize bursts and to measure the
window after other hooks (e.g. olivetti's margins) have settled."
  (when (windowp window)
    (with-current-buffer (window-buffer window)
      (when (and markdown-mermaid-mode
                 markdown-mermaid-fit-window
                 (not markdown-mermaid--resize-timer))
        (setq markdown-mermaid--resize-timer
              (run-with-idle-timer
               0.15 nil #'markdown-mermaid--render-if-resized
               (current-buffer)))))))

(defun markdown-mermaid--render-if-resized (buf)
  "Re-fit previews in BUF if the available width has changed."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq markdown-mermaid--resize-timer nil)
      (when (and markdown-mermaid-mode
                 (not (eql (markdown-mermaid--available-width)
                           markdown-mermaid--last-width)))
        (markdown-mermaid-render-buffer)))))

;;;###autoload
(define-minor-mode markdown-mermaid-mode
  "Toggle inline Mermaid diagram previews in markdown buffers.
When enabled, ```mermaid fenced blocks are rendered to SVG via the
mmdc CLI and shown in place of their source, scaled to fit the window.
Move point into a block to reveal the source for editing; move out to
see the image again.  Renders run asynchronously and are cached by
content hash."
  :lighter " Mmd"
  :group 'markdown-mermaid
  (if markdown-mermaid-mode
      (progn
        (unless (executable-find markdown-mermaid-mmdc-executable)
          (message "markdown-mermaid: %s not found on PATH"
                   markdown-mermaid-mmdc-executable))
        (setq markdown-mermaid--pending (make-hash-table :test 'equal))
        (add-hook 'after-save-hook #'markdown-mermaid--after-save nil t)
        (add-hook 'post-command-hook #'markdown-mermaid--reveal nil t)
        (add-hook 'window-size-change-functions
                  #'markdown-mermaid--schedule-render nil t)
        (add-hook 'window-buffer-change-functions
                  #'markdown-mermaid--schedule-render nil t)
        (markdown-mermaid--start-idle-timer)
        (markdown-mermaid-render-buffer))
    (remove-hook 'after-save-hook #'markdown-mermaid--after-save t)
    (remove-hook 'post-command-hook #'markdown-mermaid--reveal t)
    (remove-hook 'window-size-change-functions
                 #'markdown-mermaid--schedule-render t)
    (remove-hook 'window-buffer-change-functions
                 #'markdown-mermaid--schedule-render t)
    (markdown-mermaid--cancel-idle-timer)
    (when (timerp markdown-mermaid--resize-timer)
      (cancel-timer markdown-mermaid--resize-timer))
    (setq markdown-mermaid--resize-timer nil)
    (markdown-mermaid--clear-overlays)
    (setq markdown-mermaid--pending nil)))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here
