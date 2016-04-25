;; make searches case-INsensitive
(set-default 'case-fold-search t)

;; TAB expands even during isearch
(define-key isearch-mode-map [tab] 'isearch-yank-word)

(setq-default fill-column 100)
;; (setq-default fill-prefix "    ")

;; Scroll just one line when hitting the bottom of the window
(setq scroll-step 1)

;; do NOT add newlines if I cursor past last line in file
(setq next-line-add-newlines nil)

(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)
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
