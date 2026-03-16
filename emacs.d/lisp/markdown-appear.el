;;; markdown-appear.el --- Reveal markdown syntax at point -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Hide markdown formatting characters, reveal them when
;;              the cursor is on the same line for easy editing.
;;              Inspired by org-appear.
;;
;; Uses markdown-mode's built-in markup hiding via invisibility-spec.
;; Markup delimiters always carry `invisible markdown-markup' — hiding
;; is controlled by whether `markdown-markup' is in the spec.  This
;; mode enables that, then strips the invisible property from the
;; cursor line so raw markdown is visible for editing.

;;; Code:

(defgroup markdown-appear nil
  "Reveal markdown syntax at point."
  :group 'markdown)

(defvar-local markdown-appear--current-line-beg nil
  "Beginning position of the line currently revealed.")

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

(defun markdown-appear--after-fontify (beg end)
  "Jit-lock function: strip hiding from cursor line after font-lock runs.
If the fontified region overlaps the current cursor line, re-strip
the hiding properties that font-lock just applied."
  (when (and markdown-appear-mode markdown-appear--current-line-beg)
    (let ((line-beg markdown-appear--current-line-beg)
          (line-end (save-excursion
                      (goto-char markdown-appear--current-line-beg)
                      (line-end-position))))
      (when (and (<= beg line-end) (>= end line-beg))
        (markdown-appear--strip-hiding
         (max beg line-beg) (min end line-end))))))

(defun markdown-appear--post-command ()
  "Reveal syntax on current line, re-hide previous line."
  (when markdown-appear-mode
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (unless (eq line-beg markdown-appear--current-line-beg)
        ;; Mark previous line for re-fontification (re-applies hiding)
        (when (and markdown-appear--current-line-beg
                   (<= markdown-appear--current-line-beg (point-max)))
          (let ((prev-end (save-excursion
                            (goto-char markdown-appear--current-line-beg)
                            (line-end-position))))
            (font-lock-flush markdown-appear--current-line-beg prev-end)))
        ;; Update tracked line
        (setq markdown-appear--current-line-beg line-beg)
        ;; Strip hiding from new current line
        (markdown-appear--strip-hiding line-beg line-end)))))

(defun markdown-appear--cleanup ()
  "Remove state and re-fontify buffer."
  (setq markdown-appear--current-line-beg nil))

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
