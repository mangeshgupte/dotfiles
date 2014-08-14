;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ C Programming Functions

;; These setting are written in the order as described in the Emacs
;; info pages. ( Hit C-hi, then go to Emacs | Programs | Program
;; Indent | Custom C Indent | Syntactic Symbols for a description of
;; each. I found it easier to open one of my own source files, and hit
;; tab on a particular line to find the name of the syntactic
;; symbol. This assumes that the setting for
;; c-echo-syntactic-information-p is not nil. )

(defconst my-c-style
  '(
    (c-echo-syntactic-information-p . t)
    (c-basic-offset                 . 4)
    (c-toggle-auto-state            . t)
    (c-offsets-alist .
             ((string                . +)
              (c                     . 0)
              (defun-open            . 0)
              (defun-close           . 0)
              (defun-block-intro     . +)
              (class-open            . 0)
              (class-close           . 0)
              (inline-open           . 0)
              (inline-close          . 0)
              (extern-lang-open      . 0)
              (extern-lang-close     . 0)
              (func-decl-cont        . +)
              (knr-argdecl-intro     . 0)
              (knr-argdecl           . 0)
              (topmost-intro         . 0)
              (topmost-intro-cont    . 0)
              (member-init-intro     . +)
              (member-init-cont      . +)
              (inher-intro           . +)
              (inher-cont            . +)
              (block-open            . 0)
              (block-close           . 0)
              (brace-list-open       . 0)
              (brace-list-close      . 0)
              (brace-list-intro      . +)
              (brace-list-entry      . 0)
              (statement             . 0)
              (statement-cont        . +)
              (statement-block-intro . +)
              (statement-case-intro  . +)
              (statement-case-open   . 0)
              (substatement          . +)
              (substatement-open     . 0)
              (case-label            . +)
              (access-label          . -)
              (label                 . 0)
              (do-while-closure      . 0)
              (else-clause           . 0)
              (catch-clause          . 0)
              (comment-intro         . 0)
			  (arglist-intro         . c-lineup-arglist-intro-after-paren)
              (arglist-cont          . c-lineup-arglist)
              (arglist-close         . c-lineup-arglist)
              (arglist-cont-nonempty . c-lineup-arglist-intro-after-paren)
; 	      (arglist-intro         . +)
; 	      (arglist-cont          . 0)
;	      (arglist-close         . 0)
;             (arglist-cont-nonempty . +)
              (stream-op             . +)
              (inclass               . +)
              (inextern-lang         . +)
              (cpp-macro             . 0)
              (friend                . 0)
              (objc-method-intro     . +)
              (objc-method-args-cont . +)
              (objc-method-call-cont . +)
              ))

    (c-hanging-braces-alist '((defun-open   before after)
                              (defun-close  before  after)
                              (class-open before  after)
                              (class-close before  after)
                              (block-open  before after)
                              (block-close . c-snug-do-while)
                              (substatement-open  before after)
                              (statement-case-open  before after)
                              (extern-lang-open  before   after)
                              (extern-lang-close  before  after)
                              (brace-list-open)
                              (brace-list-close    after)
                              (brace-list-intro)
                              (brace-list-entry)))

    (c-comment-only-line-offset . (0 . -1000))

    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))

    (c-cleanup-list             . ((scope-operator
				    empty-defun-braces
				    defun-close-semi)))
    )
  "My C++ Programming Style")

(defun my-c-mode-common-hook ()
  (c-add-style "gallucci" my-c-style t)
  (c-set-offset 'member-init-intro '+)
  (setq tab-width 4
	indent-tabs-mode nil)
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq speedbar-frame-parameters (quote
                 ((minibuffer)
                  (width          . 45)
                  (border-width   . 0)
                  (menu-bar-lines . 0)
                  (unsplittable   . t))))

;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)
(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut and Paster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When I yank a piece of code ( known as paste in Windows lingo ) into an
;; existing function, I like to have it indent itself to the proper level
;; automatically. This simple macro runs yank ( C-y ) followed by an indent
;; current function. ( C-c C-q )
(global-set-key "\C-cy" 'do-smart-yank)

(fset 'do-smart-yank "\C-y\C-c\C-q")

;; Provide access to system clipboard
(setq x-select-enable-clipboard t)

;; Change default major mode to text from fundamental
(setq initial-major-mode
      (lambda ()
        (text-mode)
        (turn-on-auto-fill)))

;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; C-k kills whole line and newline if at beginning of line
(setq kill-whole-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings / Key Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go to specific line in current buffer
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)

; (global-unset-key "\C-s")
; (global-set-key "\C-s" 'isearch-forward)

;; Remap Home and End keys to move within current line, and C-Home and
;; C-End keys to beginning and end of buffer
(global-unset-key [home])
(global-set-key [home] 'beginning-of-line)
(define-key osx-key-mode-map [home] 'beginning-of-line)

(global-unset-key [end])
(global-set-key [end] 'end-of-line)
(define-key osx-key-mode-map [end] 'end-of-line)

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

;; Make RET behave as LFD.
(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
	(local-set-key "\C-m" x)))
(add-hook 'octave-mode-hook 'RET-behaves-as-LFD)


