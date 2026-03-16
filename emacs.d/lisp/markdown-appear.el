;;; markdown-appear.el --- Reveal markdown syntax at point -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Hide markdown formatting characters, reveal them when
;;              the cursor is on the same line (or block) for easy editing.
;;              Inspired by org-appear.
;;
;; Uses markdown-mode's built-in markup hiding via invisibility-spec.
;; Markup delimiters always carry `invisible markdown-markup' — hiding
;; is controlled by whether `markdown-markup' is in the spec.  This
;; mode enables that, then strips the invisible property from the
;; cursor region so raw markdown is visible for editing.
;;
;; For inline markup the reveal region is one line.  For fenced code
;; blocks (``` or ~~~) the entire block including fences is revealed.

;;; Code:

(defgroup markdown-appear nil
  "Reveal markdown syntax at point."
  :group 'markdown)

(defvar-local markdown-appear--current-region nil
  "Cons (BEG . END) of the region currently revealed, or nil.")

(defun markdown-appear--strip-hiding (beg end)
  "Remove markup-hiding text properties between BEG and END.
Removes `invisible' properties equal to `markdown-markup' and
`display' properties equal to \"\" (both set by markdown-mode
for markup hiding).  Other properties are left alone."
  (with-silent-modifications
    (let ((pos beg))
      (while (< pos end)
        (let* ((next-d (or (next-single-property-change pos 'display nil end) end))
               (next-i (or (next-single-property-change pos 'invisible nil end) end))
               (next (min next-d next-i))
               (disp (get-text-property pos 'display))
               (inv (get-text-property pos 'invisible)))
          (when (equal disp "")
            (remove-text-properties pos next '(display nil)))
          (when (eq inv 'markdown-markup)
            (remove-text-properties pos next '(invisible nil)))
          (setq pos next))))))

(defun markdown-appear--fenced-block-bounds ()
  "Return (BEG . END) of the fenced code block at point, or nil.
Handles both ``` and ~~~ fences."
  (save-excursion
    (let ((fence-re "^[[:space:]]*\\(```\\|~~~\\)"))
      (beginning-of-line)
      (cond
       ;; On a fence line — step inside the block to use markdown-mode detection
       ((looking-at-p fence-re)
        (let ((this-beg (line-beginning-position)))
          ;; Try stepping forward (opening fence case)
          (forward-line 1)
          (if (and (not (eobp)) (markdown-code-block-at-pos (point)))
              ;; We were on the opening fence
              (let ((open-beg this-beg))
                (when (re-search-forward fence-re nil t)
                  (cons open-beg (line-end-position))))
            ;; Try stepping backward (closing fence case)
            (goto-char this-beg)
            (forward-line -1)
            (when (and (not (bobp)) (markdown-code-block-at-pos (point)))
              (let ((close-end (save-excursion
                                 (goto-char this-beg)
                                 (line-end-position))))
                (when (re-search-backward fence-re nil t)
                  (cons (line-beginning-position) close-end)))))))
       ;; Inside a block — search outward for fences
       ((markdown-code-block-at-pos (point))
        (let (open-beg close-end)
          (save-excursion
            (when (re-search-backward fence-re nil t)
              (setq open-beg (line-beginning-position))))
          (save-excursion
            (end-of-line)
            (when (re-search-forward fence-re nil t)
              (setq close-end (line-end-position))))
          (when (and open-beg close-end)
            (cons open-beg close-end))))))))

(defun markdown-appear--compute-region ()
  "Return (BEG . END) of the region to reveal at point.
For fenced code blocks, returns the entire block; otherwise the current line."
  (or (markdown-appear--fenced-block-bounds)
      (cons (line-beginning-position) (line-end-position))))

(defun markdown-appear--after-fontify (beg end)
  "Jit-lock function: strip hiding from revealed region after font-lock runs.
If the fontified region overlaps the current revealed region, re-strip
the hiding properties that font-lock just applied."
  (when (and markdown-appear-mode markdown-appear--current-region)
    (let ((r-beg (car markdown-appear--current-region))
          (r-end (cdr markdown-appear--current-region)))
      (when (and (<= beg r-end) (>= end r-beg))
        (markdown-appear--strip-hiding
         (max beg r-beg) (min end r-end))))))

(defun markdown-appear--post-command ()
  "Reveal syntax in current region, re-hide previous region."
  (when markdown-appear-mode
    (let ((new-region (markdown-appear--compute-region)))
      (unless (equal new-region markdown-appear--current-region)
        ;; Re-fontify previous region (re-applies hiding)
        (when (and markdown-appear--current-region
                   (<= (car markdown-appear--current-region) (point-max)))
          (font-lock-flush (car markdown-appear--current-region)
                           (cdr markdown-appear--current-region)))
        ;; Update tracked region
        (setq markdown-appear--current-region new-region)
        ;; Strip hiding from new region
        (markdown-appear--strip-hiding (car new-region) (cdr new-region))))))

(defun markdown-appear--cleanup ()
  "Remove state and re-fontify buffer."
  (setq markdown-appear--current-region nil))

;;;###autoload
(define-minor-mode markdown-appear-mode
  "Toggle revealing markdown syntax at point.
When enabled, leverages markdown-mode's markup hiding to conceal
formatting delimiters.  Moving the cursor to a line reveals the
raw markdown for editing.  Moving away hides it again."
  :lighter " Appear"
  :group 'markdown-appear
  (if markdown-appear-mode
      (progn
        ;; Enable hiding the proper way: toggle sets the variable,
        ;; updates invisibility-spec, and reloads font-lock keywords.
        (setq-local markdown-hide-markup t)
        (add-to-invisibility-spec 'markdown-markup)
        (markdown-reload-extensions)
        (jit-lock-register #'markdown-appear--after-fontify)
        (add-hook 'post-command-hook #'markdown-appear--post-command nil t))
    (remove-hook 'post-command-hook #'markdown-appear--post-command t)
    (jit-lock-unregister #'markdown-appear--after-fontify)
    (markdown-appear--cleanup)
    ;; Disable hiding
    (setq-local markdown-hide-markup nil)
    (remove-from-invisibility-spec 'markdown-markup)
    (markdown-reload-extensions)))

(provide 'markdown-appear)
;;; markdown-appear.el ends here
