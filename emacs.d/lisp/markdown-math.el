;;; markdown-math.el --- Inline LaTeX math previews in markdown buffers -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Render LaTeX math -- inline `$..$' and block `$$..$$' -- to
;;              SVG via MathJax (a small companion node script, markdown-math.js)
;;              and display each rendered image over its source, LaTeX-preview
;;              style.  Layers on top of markdown-mode without replacing it.
;;
;; Requirements:
;;   - Emacs built with SVG support (image-type-available-p 'svg)
;;   - node on PATH, plus `mathjax-full' installed where node can resolve it:
;;       npm install -g mathjax-full
;;     The caller sets NODE_PATH to `markdown-math-node-modules-dir', so a
;;     global install is enough.  Without it the mode still enables and simply
;;     leaves the source visible (like markdown-mermaid without mmdc).
;;
;; Behavior:
;;   - Each `$$..$$' (block) and `$..$' (inline) span is rendered to an SVG,
;;     keyed by SHA1 of the TeX + mode + color + size, so only changed spans
;;     re-render.  Cached SVGs live in `markdown-math-cache-dir'.
;;   - Rendering runs asynchronously via make-process; the buffer is never
;;     blocked.  Overlays gain their image when node finishes.
;;   - The rendered image replaces the source via an overlay.  Move point into
;;     a span to reveal the source for editing; move out and the image returns
;;     (same UX as markdown-table-preview).  Save to re-render.
;;   - Fenced code blocks are skipped; inline spans overlapping a block span
;;     and empty/whitespace-only spans are dropped.
;;   - Toggle per-buffer with C-c m.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'color)

(defvar markdown-math-mode)  ; forward ref for helpers below the definition

(defgroup markdown-math nil
  "Inline LaTeX math previews in markdown buffers."
  :group 'markdown)

(defcustom markdown-math-node-executable "node"
  "Path to the node executable used to run `markdown-math-script'."
  :type 'string :group 'markdown-math)

(defcustom markdown-math-script
  (expand-file-name
   "markdown-math.js"
   (file-name-directory (or load-file-name buffer-file-name
                            "~/.emacs.d/lisp/")))
  "Path to the MathJax render script (markdown-math.js)."
  :type 'file :group 'markdown-math)

(defcustom markdown-math-node-modules-dir "/opt/homebrew/lib/node_modules"
  "Directory added to NODE_PATH so the script can resolve `mathjax-full'.
Defaults to the Homebrew global node_modules path; set to the output
of `npm root -g' on other systems."
  :type 'directory :group 'markdown-math)

(defcustom markdown-math-cache-dir
  (expand-file-name "markdown-math" (or (getenv "XDG_CACHE_HOME")
                                        "~/.cache"))
  "Directory for cached rendered SVGs."
  :type 'directory :group 'markdown-math)

(defcustom markdown-math-scale 1.0
  "Scale factor applied to rendered images."
  :type 'number :group 'markdown-math)

(defcustom markdown-math-inline-hpad 3
  "Horizontal padding, in pixels, added on each side of an inline math image.
A rendered inline span has no side bearing of its own, so its glyphs sit
flush against the image's left edge.  Without this padding the image butts
against the surrounding prose -- the source space before `$' collapses to a
hair -- even though the space is still there.  A few pixels give the image
the breathing room a real glyph gets from its side bearings.  Set to 0 to
disable.  Block (`$$..$$') images sit on their own line and are unaffected."
  :type 'integer :group 'markdown-math)

(defcustom markdown-math-ex-size 8
  "Pixels per MathJax `ex' unit; the base render size before scaling.
Raise for larger equations; roughly half the prose font's pixel height."
  :type 'number :group 'markdown-math)

(defcustom markdown-math-color nil
  "Glyph color for rendered math, or nil to follow the `default' face foreground."
  :type '(choice (const :tag "Follow default face" nil) color)
  :group 'markdown-math)

(defcustom markdown-math-render-on-save t
  "If non-nil, re-render previews after each save."
  :type 'boolean :group 'markdown-math)

(defconst markdown-math--version "1"
  "Bumped when the render pipeline changes, to invalidate stale caches.")

;; Group 1 = opening delimiter, group 2 = TeX body, group 3 = closing.
;; Body allows escaped chars (\\.) but no literal `$', so a run stops at the
;; next delimiter.  Leading `[^\\]'/`[^\\$]' guards against escaped `\\$' and
;; (for inline) against starting inside a `$$'.  Mirrors markdown-mode's own
;; itex regexes.
(defconst markdown-math--regex-display
  "\\(?:^\\|[^\\]\\)\\(\\$\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\$\\)"
  "Regexp matching a `$$..$$' block-math span.")

(defconst markdown-math--regex-inline
  "\\(?:^\\|[^\\$]\\)\\(\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\)"
  "Regexp matching a `$..$' inline-math span.")

(defvar-local markdown-math--overlays nil
  "List of preview overlays in the current buffer.")

(defvar-local markdown-math--pending nil
  "Hash table mapping cache keys to t for in-flight renders.")

;;; Color / cache helpers ---------------------------------------------

(defun markdown-math--color ()
  "Return the glyph color as a hex string."
  (let ((c (or markdown-math-color
               (face-attribute 'default :foreground nil t))))
    (if (and (stringp c) (color-defined-p c))
        (apply #'color-rgb-to-hex (append (color-name-to-rgb c) '(2)))
      (if (eq (frame-parameter nil 'background-mode) 'dark)
          "#dcdcdc" "#222222"))))

(defun markdown-math--cache-key (tex mode)
  "Return a stable cache key for TEX in MODE under the active color/size."
  (sha1 (mapconcat #'identity
                   (list markdown-math--version tex (symbol-name mode)
                         (markdown-math--color)
                         (number-to-string markdown-math-ex-size))
                   "\0")))

(defun markdown-math--cache-path (key)
  "Return the absolute SVG path for KEY."
  (expand-file-name (concat key ".svg") markdown-math-cache-dir))

(defun markdown-math--ensure-cache-dir ()
  (unless (file-directory-p markdown-math-cache-dir)
    (make-directory markdown-math-cache-dir t)))

;;; Scanning ----------------------------------------------------------

(defun markdown-math--fence-ranges ()
  "Return a list of (BEG . END) conses for fenced code blocks."
  (let (ranges start)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match-p "\\`[ \t]*\\(```\\|~~~\\)" line)
            (if start
                (progn (push (cons start (line-end-position)) ranges)
                       (setq start nil))
              (setq start (line-beginning-position)))))
        (forward-line 1))
      (when start (push (cons start (point-max)) ranges)))
    ranges))

(defun markdown-math--in-ranges-p (pos ranges)
  "Return non-nil if POS falls within any (BEG . END) in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r)))) ranges))

(defun markdown-math--overlaps-p (b e regions)
  "Return non-nil if B..E overlaps any (BEG END ...) in REGIONS."
  (cl-some (lambda (r) (and (< b (nth 1 r)) (> e (nth 0 r)))) regions))

(defun markdown-math--scan (regex mode fences skip)
  "Collect (BEG END TEX MODE) for REGEX, skipping FENCES and any SKIP overlap."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((b (match-beginning 1))
              (e (match-end 3))
              (tex (string-trim (match-string-no-properties 2))))
          (goto-char e)
          (unless (or (string-empty-p tex)
                      (markdown-math--in-ranges-p b fences)
                      (and skip (markdown-math--overlaps-p b e skip)))
            (push (list b e tex mode) found)))))
    (nreverse found)))

(defun markdown-math--regions ()
  "Return all math regions as (BEG END TEX MODE), block spans first."
  (let* ((fences (markdown-math--fence-ranges))
         (display (markdown-math--scan markdown-math--regex-display
                                       'display fences nil))
         (inline (markdown-math--scan markdown-math--regex-inline
                                      'inline fences display)))
    (append display inline)))

;;; Overlays ----------------------------------------------------------

(defun markdown-math--image (path &optional mode)
  "Return an SVG image object for the file at PATH.
For inline MODE, add `markdown-math-inline-hpad' pixels of horizontal
margin so the image keeps a glyph's breathing room from adjacent prose."
  (apply #'create-image path 'svg nil
         :scale markdown-math-scale :ascent 'center
         (when (and (eq mode 'inline) (> markdown-math-inline-hpad 0))
           (list :margin (cons markdown-math-inline-hpad 0)))))

(defun markdown-math--make-overlay (beg end mode)
  "Create a preview overlay over BEG..END for MODE (image attached later)."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'markdown-math t)
    (overlay-put ov 'mm-mode mode)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'help-echo "Markdown math preview -- move point in to edit")
    (push ov markdown-math--overlays)
    ov))

(defun markdown-math--reveal ()
  "Show source for the span under point, the image for the others."
  (dolist (ov markdown-math--overlays)
    (when (overlay-buffer ov)
      (let ((img (overlay-get ov 'mm-image)))
        (if (and img
                 (or (< (point) (overlay-start ov))
                     (> (point) (overlay-end ov))))
            (overlay-put ov 'display img)
          (overlay-put ov 'display nil))))))

(defun markdown-math--set-image (ov path)
  "Attach the image at PATH to overlay OV and refresh reveal state."
  (when (and (overlayp ov) (overlay-buffer ov) (file-exists-p path))
    (condition-case err
        (progn
          (overlay-put ov 'mm-image
                       (markdown-math--image path (overlay-get ov 'mm-mode)))
          (markdown-math--reveal))
      (error (message "markdown-math: %s" (error-message-string err))))))

(defun markdown-math--clear ()
  "Remove all preview overlays in the buffer."
  (mapc (lambda (ov) (when (overlayp ov) (delete-overlay ov)))
        markdown-math--overlays)
  (setq markdown-math--overlays nil)
  (remove-overlays (point-min) (point-max) 'markdown-math t))

;;; Rendering ---------------------------------------------------------

(defun markdown-math--sentinel (proc key out stdout buf callback)
  "Handle completion of a render PROC: write OUT from STDOUT, notify BUF."
  (when (memq (process-status proc) '(exit signal))
    (let ((ok (and (eq (process-status proc) 'exit)
                   (= (process-exit-status proc) 0)))
          (svg (when (buffer-live-p stdout)
                 (with-current-buffer stdout (buffer-string)))))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (when (buffer-live-p buf)
        (with-current-buffer buf (remhash key markdown-math--pending)))
      (when (and ok svg (string-match-p "<svg" svg))
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file out (insert svg)))
        (when (buffer-live-p buf)
          (funcall callback out))))))

(defun markdown-math--render-async (tex mode key callback)
  "Render TeX TEX in MODE to an SVG keyed by KEY, then call CALLBACK with the path.
Does nothing if a render for KEY is already in flight."
  (unless (gethash key markdown-math--pending)
    (puthash key t markdown-math--pending)
    (markdown-math--ensure-cache-dir)
    (let* ((out (markdown-math--cache-path key))
           (stdout (generate-new-buffer " *markdown-math-out*"))
           (buf (current-buffer))
           (process-environment
            (cons (concat "NODE_PATH="
                          (expand-file-name markdown-math-node-modules-dir))
                  process-environment))
           (proc (make-process
                  :name "markdown-math"
                  :noquery t
                  :buffer stdout
                  :connection-type 'pipe
                  :coding 'utf-8
                  :command (list markdown-math-node-executable
                                 (expand-file-name markdown-math-script)
                                 (if (eq mode 'display) "display" "inline")
                                 (markdown-math--color)
                                 (number-to-string markdown-math-ex-size))
                  :sentinel (lambda (p _e)
                              (markdown-math--sentinel
                               p key out stdout buf callback)))))
      (process-send-string proc tex)
      (process-send-eof proc))))

(defun markdown-math-render-buffer ()
  "Render or refresh SVG previews for every math span in the buffer."
  (interactive)
  (unless markdown-math--pending
    (setq markdown-math--pending (make-hash-table :test 'equal)))
  (markdown-math--clear)
  (let ((buf (current-buffer)))
    (dolist (m (markdown-math--regions))
      (pcase-let ((`(,beg ,end ,tex ,mode) m))
        (let* ((ov (markdown-math--make-overlay beg end mode))
               (key (markdown-math--cache-key tex mode))
               (path (markdown-math--cache-path key)))
          (if (file-exists-p path)
              (markdown-math--set-image ov path)
            (markdown-math--render-async
             tex mode key
             (lambda (p)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (markdown-math--set-image ov p))))))))))
  (markdown-math--reveal))

;;; Triggers ----------------------------------------------------------

(defun markdown-math--after-save ()
  "Re-render previews on save when enabled."
  (when (and markdown-math-mode markdown-math-render-on-save)
    (markdown-math-render-buffer)))

;;;###autoload
(define-minor-mode markdown-math-mode
  "Toggle inline LaTeX math previews in markdown buffers.
When enabled, `$..$' and `$$..$$' spans are rendered to SVG via
MathJax and shown over their source.  Renders run asynchronously and
are cached by content hash.  Move point into a span to edit; move out
to see the image."
  :lighter " Math"
  :group 'markdown-math
  (if markdown-math-mode
      (progn
        (unless (image-type-available-p 'svg)
          (user-error "markdown-math: this Emacs lacks SVG support"))
        (unless (executable-find markdown-math-node-executable)
          (message "markdown-math: %s not found on PATH"
                   markdown-math-node-executable))
        (unless (file-exists-p (expand-file-name markdown-math-script))
          (message "markdown-math: render script not found at %s"
                   markdown-math-script))
        (setq markdown-math--pending (make-hash-table :test 'equal))
        (add-hook 'after-save-hook #'markdown-math--after-save nil t)
        (add-hook 'post-command-hook #'markdown-math--reveal nil t)
        (markdown-math-render-buffer))
    (remove-hook 'after-save-hook #'markdown-math--after-save t)
    (remove-hook 'post-command-hook #'markdown-math--reveal t)
    (markdown-math--clear)
    (setq markdown-math--pending nil)))

(provide 'markdown-math)
;;; markdown-math.el ends here
