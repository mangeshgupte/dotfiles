;; Go into proper mode according to file extension
(setq auto-mode-alist
      (append '(("\\.C$"    . c++-mode)
                ("\\.cc$"   . c++-mode)
                ("\\.cpp$"  . c++-mode)
                ("\\.cxx$"  . c++-mode)
                ("\\.hxx$"  . c++-mode)
                ("\\.h$"    . c++-mode)
                ("\\.hh$"   . c++-mode)
                ("\\.idl$"  . c++-mode)
                ("\\.ipp$"  . c++-mode)
                ("\\.c$"    . c-mode)
                ("\\.py$"   . python-mode)
                ("\\.less$" . css-mode)
                ("\\.php$"  . php-mode)
                ("\\Response.php$"  . web-mode)
                ("\\.inc$"  . php-mode)
                ("\\.js\\'"       . javascript-mode)
                ("\\.yaml$"  . yaml-mode)
                ("\\.m$"    . octave-mode)
                ("\\.ma?k\\'" . makefile-mode)
                ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
                ("\\.pl$"   . perl-mode)
                ("\\.pro$"  . prolog-mode)
                ("\\.pm$"   . perl-mode)
                ("\\.java$" . java-mode)
                ("\\.txt$"  . text-mode)
                ("\\.zsh$"  . sh-mode)
                ("\\.zsh-theme$"  . sh-mode)
                ("\\.tex$"  . latex-mode)
                ("\\.sty$"  . latex-mode)
                ("\\.bbl$"  . latex-mode)
                ("\\.html$" . html-helper-mode)
                ("\\.el\\'" . emacs-lisp-mode)
                ("emacs$" . emacs-lisp-mode)
                ("\\.texinfo\\'" . texinfo-mode)
                ("\\.texi\\'" . texinfo-mode)
                ("\\.s\\'"  . asm-mode)
                ("\\.S\\'"  . asm-mode)
                ("\\.asm\\'" . asm-mode)
                ("ChangeLog\\'" . change-log-mode)
                ("change\\.log\\'" . change-log-mode)
                ("changelo\\'" . change-log-mode)
                ("ChangeLog\\.[0-9]+\\'" . change-log-mode)
                ("changelog\\'" . change-log-mode)
                ("changelog\\.[0-9]+\\'" . change-log-mode)
                ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
                ("\\.tar\\'" . tar-mode)
                ("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\)\\'" . archive-mode)
                ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|JAR\\)\\'" . archive-mode)
                ("[]>:/\\]\\..*emacs\\'" . emacs-lisp-mode)
                ("\\`\\..*emacs\\'" . emacs-lisp-mode)
                ("[:/]_emacs\\'"  . emacs-lisp-mode)
                ("\\.gp$"         . gnuplot-mode)
                ("\\.htaccess$"   . apache-mode)
                ("httpd\\.conf$"  . apache-mode)
                ("srm\\.conf$"    . apache-mode)
                ("access\\.conf$" . apache-mode)
                ("\\.bhl$"        . bhl-mode)
                ("\\.mel$"        . mel-mode)
                ("\\.ini\\'" . ini-mode)
                ("\\.csv$'" . csv-mode)
                ("\\.thrift$" . thrift-mode)
              auto-mode-alist)))

(autoload 'ini-mode "ini-mode" nil t)

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
        (inlambda . 0)
        (substatement-open     . 0)
        (case-label            . +)
        (access-label          . -)
        (label                 . 0)
        (do-while-closure      . 0)
        (else-clause           . 0)
        (catch-clause          . 0)
        (comment-intro         . 0)
        (arglist-intro         . '(c-lineup-assignments 0))
        (arglist-cont          . c-lineup-arglist)
        (arglist-cont-nonempty . c-lineup-arglist)
        (arglist-close         . c-lineup-arglist)
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

    (c-hanging-braces-alist '((defun-open after)
                              (defun-close before  after)
                              (class-open before after)
                              (class-close before after)
                              (block-open after)
                              (block-close . c-snug-do-while)
                              (substatement-open after)
                              (statement-case-open after)
                              (extern-lang-open  before   after)
                              (extern-lang-close  before  after)
                              (brace-list-open)
                              (brace-list-close   after)
                              (brace-list-intro)
                              (brace-list-entry)))

    (c-comment-only-line-offset . (0 . -1000))

    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))

    (c-cleanup-list . ((scope-operator
                        empty-defun-braces
                        defun-close-semi)))
    )
  "My C++ Programming Style")

(defun my-c-mode-common-hook ()
  (c-add-style "gallucci" my-c-style t)
  (c-set-offset 'member-init-intro '+)
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-common-hook ()
  (c-add-style "gallucci" my-c-style t)
  (c-set-offset 'member-init-intro '+)
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))

(add-hook 'java-mode-common-hook 'my-java-mode-common-hook)

(defun my-php-mode-hook ()
  (c-add-style "gallucci" my-c-style t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-offset my-tab-width)
    (set (make-local-variable 'tab-stop-list)
		 (number-sequence my-tab-width 200 my-tab-width)))
  (setq fill-column 120))

(add-hook 'php-mode-hook 'my-php-mode-hook)

;; Web mode.
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Python mode
(defun my-python-mode-hook ()
  "Hook for python mode."
  (lambda ()
    (setq python-indent 4)
    (setq fill-column 100)))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; YAML mode
(setq yaml-indent-offset 4)

;; Make RET behave as LFD.
(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
	(local-set-key "\C-m" x)))
(add-hook 'octave-mode-hook 'RET-behaves-as-LFD)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TAGS management.
(setq tags-table-list
      '("/houzz/c2/TAGS"))

(package-initialize)

(elpy-enable)
