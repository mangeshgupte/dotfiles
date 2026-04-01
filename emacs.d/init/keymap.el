 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings / Key Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set alt and command to meta(alt)
(when (system-type-is-darwin)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  )

(global-set-key "\C-cy" 'do-smart-yank)

(fset 'do-smart-yank "\C-y\C-c\C-q")

;; go to specific line in current buffer
(global-set-key "\M-g" 'goto-line)

(global-set-key "\M-[" 'swap-parens)

(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(global-set-key "\M-\C-s" 'isearch-forward)
(global-set-key "\M-\C-r" 'isearch-backward)




;; Make the % key jump to the matching {}[]() if on another, like VI
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Rebind C-z to start a shell (use .emacs_shellname for the shells rc file)
(global-set-key "\C-z" 'shell)

;; C-k kills whole line and newline if at beginning of line
(setq kill-whole-line t)

;; Rectangle marking - use built-in rectangle-mark-mode (C-x SPC)
;; Also available: C-x r k (kill), C-x r y (yank),
;; C-x r t (string-rectangle), C-x r o (open)

;; Swap keys
(setq mcg-key-pairs
      '((?! ?1) (?@ ?2) (?# ?3) (?$ ?4) (?% ?5)
        (?^ ?6) (?& ?7) (?* ?8) (?( ?9) (?) ?0)
		(?{ ?[) (?} ?])  ; (?- ?_) ; (?` ?~)          ; (?| ?\\) (?\" ?')
        ))

(defun mcg-key-swap (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard zapped!! Shift-F10 to restore!")
    (progn
      (keyboard-translate (caar key-pairs)  (cadar key-pairs))
      (keyboard-translate (cadar key-pairs) (caar key-pairs))
      (mcg-key-swap (cdr key-pairs))
      )
    ))

(defun mcg-key-restore (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard restored!! F10 to Zap!")
    (progn
      (keyboard-translate (caar key-pairs)  (caar key-pairs))
      (keyboard-translate (cadar key-pairs) (cadar key-pairs))
      (mcg-key-restore (cdr key-pairs))
	  )))
(global-set-key [f10] (lambda () (interactive) (mcg-key-swap mcg-key-pairs)))
(global-set-key [(shift f10)] (lambda () (interactive) (mcg-key-restore mcg-key-pairs)))

;; Kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)

(global-set-key [(control tab)] 'previous-buffer)
(global-set-key [(control shift return)] 'next-buffer)

;; Split windows displays different buffers in the two windows.
(defun split-window-switch-buffer () (interactive)
  "Split current window and display the two last buffers used."
  (split-window)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

(defun hsplit-window-switch-buffer () (interactive)
  "Split current window horizontally and display the two last buffers used."
  (split-window-horizontally)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

;; split windows should display different buffers
(global-set-key "\C-x2" 'split-window-switch-buffer)
(global-set-key "\C-x3" 'hsplit-window-switch-buffer)


;; fix copy/paste
(add-hook
 'term-mode-hook
 (lambda ()
   (define-key term-raw-map (kbd "C-y") 'term-paste)
   (define-key term-raw-map (kbd "C-v") 'term-paste)
   (define-key term-raw-map (kbd "s-v") 'term-paste)))
