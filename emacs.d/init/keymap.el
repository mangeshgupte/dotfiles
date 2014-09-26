;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings / Key Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cy" 'do-smart-yank)

(fset 'do-smart-yank "\C-y\C-c\C-q")

;; go to specific line in current buffer
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)

(global-unset-key "\C-s")
(global-set-key "\C-s" 'isearch-forward-regexp)

(global-unset-key "\C-r")
(global-set-key "\C-r" 'isearch-backward-regexp)

(global-unset-key "\M-\C-s")
(global-set-key "\M-\C-s" 'isearch-forward)

(global-unset-key "\M-\C-r")
(global-set-key "\M-\C-r" 'isearch-backward)

(when (system-type-is-darwin)
  (define-key osx-key-mode-map [home] 'beginning-of-line)
  (define-key osx-key-mode-map [end] 'end-of-line))

;; Remap Home and End keys to move within current line, and C-Home and
;; C-End keys to beginning and end of buffer
(global-unset-key [home])
(global-set-key [home] 'beginning-of-line)

(global-unset-key [end])
(global-set-key [end] 'end-of-line)

(global-unset-key [home])
(global-set-key [\C-home] 'beginning-of-buffer)

(global-unset-key [home])
(global-set-key [\C-end] 'end-of-buffer)
(global-unset-key [home])
(global-set-key [delete] 'delete-char)          ;; Make DEL behave like delete not like backspace

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

;; Support for marking a rectangle of text with highlighting.
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

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

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-previous-buffer ()
  "Cycle to the previous buffer with keyboard."
  (interactive)
  (bury-buffer))

(defun my-next-buffer ()
  "Cycle to the next buffer with keyboard."
  (interactive)
  (let* ((bufs (buffer-list))
	 (entry (1- (length bufs)))
	 val)
    (while (not (setq val (nth entry bufs)
                      val (and (/= (aref (buffer-name val) 0)
                                   ? )
                               val)))
      (setq entry (1- entry)))
    (switch-to-buffer val)))

(global-set-key [(control tab)] 'my-previous-buffer)
(global-set-key [(control shift return)]  'my-next-buffer)

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
(global-unset-key "\C-x2")
(global-unset-key "\C-x3")
(global-set-key "\C-x2" 'split-window-switch-buffer)
(global-set-key "\C-x3" 'hsplit-window-switch-buffer)


;; fix copy/paste
(add-hook
 'term-mode-hook
 (lambda ()
   (define-key term-raw-map (kbd "C-y") 'term-paste)
   (define-key term-raw-map (kbd "C-v") 'term-paste)
   (define-key term-raw-map (kbd "s-v") 'term-paste)))


