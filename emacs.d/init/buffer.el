;; make searches case-INsensitive
(set-default 'case-fold-search t)

;; TAB expands even during isearch
(define-key isearch-mode-map [tab] 'isearch-yank-word)

;; Scroll just one line when hitting the bottom of the window
(setq scroll-step 1)

;; do NOT add newlines if I cursor past last line in file
(setq next-line-add-newlines nil)

(setq-default fill-column 100)

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Shut off annoying beep. Keep it on only for special situations
(setq visible-bell t)

;; Enable copy/paste from emacs to other apps
(setq
 interprogram-cut-function 'x-select-text
 interprogram-paste-function 'x-selection-value
 save-interprogram-paste-before-kill t
 select-active-regions t
 x-select-enable-clipboard t
 x-select-enable-primary t)

;; Shut off message buffer. Note - if you need to debug emacs,
;; comment these out so you can see what's going on.
;; (setq message-log-max nil)
;; (kill-buffer "*Messages*")

(defun indent-buffer()
  "Indent the whole buffer from point-min to point-max using
   the command indent-region"
  (interactive)
  (indent-region 0 (point-max) nil))

;; autopair
(when (require 'autopair nil 'noerror)
  (autopair-global-mode 1)
  ;; tells autopair to automatically wrap the selection region with the
  ;; delimiters you’re trying to insert.
  (setq autopair-autowrap t))

;; Turn off current-line-highlighting and auto-pair.
(defadvice term-char-mode (after term-char-mode-fixes ())
  (autopair-mode 1)
  (set (make-local-variable 'hl-line-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil))
(ad-activate 'term-char-mode)

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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  \(fn arg char)"
  'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced Editing Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
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
