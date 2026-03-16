;; TAB expands even during isearch
(define-key isearch-mode-map [tab] 'isearch-yank-word)

;; Scroll just one line when hitting the bottom of the window
(setq scroll-step 1)

(setq-default fill-column 100)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs

(define-key global-map (kbd "RET") 'newline-and-indent)


(defun indent-buffer()
  "Indent the whole buffer from point-min to point-max using
   the command indent-region"
  (interactive)
  (indent-region 0 (point-max) nil))

;; Auto-pair delimiters using built-in electric-pair-mode
(electric-pair-mode 1)

;; Turn off current-line-highlighting in term-mode.
(defun my-term-char-mode-fixes (&rest _)
  "Disable line highlighting in terminal modes."
  (setq-local hl-line-mode nil)
  (setq-local global-hl-line-mode nil))
(advice-add 'term-char-mode :after #'my-term-char-mode-fixes)

;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(global-set-key "\M-z" 'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced Editing Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar swap-paren-pairs '("()" "[]"))
(defun swap-parens-at-points (b e)
  (let ((open-char (buffer-substring b (+ b 1)))
        (paren-pair-list (append swap-paren-pairs swap-paren-pairs)))
    (while paren-pair-list
      (if (eq (aref open-char 0) (aref (car paren-pair-list) 0))
          (save-excursion
            (setq to-replace (cadr paren-pair-list))
            (goto-char b)
            (delete-char 1)
            (insert (aref to-replace 0))
            (goto-char (- e 1))
            (delete-char 1)
            (insert (aref to-replace 1))
            (setq paren-pair-list nil))
        (setq paren-pair-list (cdr paren-pair-list))))))

(defun swap-parens ()
  (interactive)
  (cond ((looking-at "\\s(")
         (swap-parens-at-points (point) (save-excursion (forward-sexp) (point))))
        ((and (> (point) 1) (save-excursion (forward-char -1) (looking-at "\\s)")))
         (swap-parens-at-points (save-excursion (forward-sexp -1) (point)) (point)))
        ((message "Not at a paren"))))
