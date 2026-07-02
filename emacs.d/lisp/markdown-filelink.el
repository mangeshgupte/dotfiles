;;; markdown-filelink.el --- Clickable file references in code spans -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Make inline code spans that name a real file -- like
;;              `mu.md' or `../report.md' -- clickable links that open
;;              the file.  Prose notes often cross-reference sibling
;;              documents this way instead of with [text](file) links.
;;
;; A span is linkified only when its text resolves to a file that
;; actually exists, so ordinary code spans are untouched.  Resolution
;; tries, in order:
;;   1. the name relative to the buffer's directory
;;   2. the same name in each parent directory (so `report.md'
;;      resolves when referenced from a subdirectory)
;;   3. a date-prefixed sibling: `vst.md' matches `2026-05-01-vst.md',
;;      newest match wins
;;
;; mouse-1 or C-c C-o on a linkified span opens the file.

;;; Code:

(declare-function markdown-code-block-at-pos "markdown-mode" (pos))

(defgroup markdown-filelink nil
  "Clickable file references in markdown inline code spans."
  :group 'markdown)

(defface markdown-filelink-face
  '((t :underline t))
  "Face layered over code spans that link to a file."
  :group 'markdown-filelink)

(defconst markdown-filelink--regexp
  "`\\([A-Za-z0-9._~/-]+\\.[A-Za-z0-9]+\\)`"
  "Matches a code span containing a single path-like token.
Group 1 is the candidate file name.  Requiring an extension keeps
most ordinary code spans from ever touching the filesystem.")

(defun markdown-filelink--resolve (name)
  "Resolve NAME to an existing file; return its absolute path or nil."
  (unless (file-remote-p default-directory)
    (let ((dir default-directory))
      (cond
       ((file-name-absolute-p name)
        (let ((full (expand-file-name name)))
          (and (file-exists-p full) full)))
       ((file-exists-p (expand-file-name name dir))
        (expand-file-name name dir))
       ((locate-dominating-file dir name)
        (expand-file-name name (locate-dominating-file dir name)))
       ((not (string-match-p "/" name))
        (car (sort (file-expand-wildcards
                    (expand-file-name (concat "*-" name) dir))
                   #'string>)))))))

(defvar markdown-filelink--target nil
  "Resolved path for the span just found by `markdown-filelink--match'.
Set by the matcher; read by `markdown-filelink--propspec' when
font-lock evaluates the highlight spec for the same match.")

(defun markdown-filelink--match (limit)
  "Font-lock matcher: find the next linkifiable code span before LIMIT."
  (catch 'found
    (while (re-search-forward markdown-filelink--regexp limit t)
      (let ((target
             (and (not (markdown-code-block-at-pos (match-beginning 0)))
                  (markdown-filelink--resolve (match-string-no-properties 1)))))
        (when target
          (setq markdown-filelink--target target)
          (throw 'found t))))))

(defun markdown-filelink-open ()
  "Open the file referenced by the code span at point."
  (interactive)
  (let ((target (get-text-property (point) 'markdown-filelink-target)))
    (unless target
      (user-error "No file reference at point"))
    (find-file target)))

(defun markdown-filelink-open-at-mouse (event)
  "Open the file referenced by the code span clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (markdown-filelink-open))

(defvar markdown-filelink-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'markdown-filelink-open-at-mouse)
    (define-key map (kbd "C-c C-o") #'markdown-filelink-open)
    map)
  "Keymap active on linkified code spans.
The spans also carry a `follow-link' property, so plain mouse-1
clicks follow the link as well.")

(defun markdown-filelink--propspec ()
  "Text properties for the span just matched."
  (list 'face 'markdown-filelink-face
        'markdown-filelink-target markdown-filelink--target
        'mouse-face 'highlight
        'follow-link t
        'help-echo (concat "mouse-1: open "
                           (abbreviate-file-name markdown-filelink--target))
        'keymap markdown-filelink-map))

(defconst markdown-filelink--keywords
  '((markdown-filelink--match 1 (markdown-filelink--propspec) prepend)))

;;;###autoload
(define-minor-mode markdown-filelink-mode
  "Toggle clickable file references in inline code spans.
When enabled, a code span whose text names an existing file (such
as `mu.md') is underlined and opens that file on mouse-1 or
\\<markdown-filelink-map>\\[markdown-filelink-open].  See
`markdown-filelink--resolve' for how names are located."
  :lighter ""
  :group 'markdown-filelink
  (if markdown-filelink-mode
      (progn
        ;; markdown-mode already manages keymap/help-echo/mouse-face;
        ;; register our extra properties so refontification cleans them.
        (make-local-variable 'font-lock-extra-managed-props)
        (dolist (prop '(markdown-filelink-target follow-link))
          (add-to-list 'font-lock-extra-managed-props prop))
        (font-lock-add-keywords nil markdown-filelink--keywords 'append)
        (font-lock-flush))
    (font-lock-remove-keywords nil markdown-filelink--keywords)
    (font-lock-flush)))

(provide 'markdown-filelink)
;;; markdown-filelink.el ends here
