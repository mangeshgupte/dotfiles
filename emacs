;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename : .emacs
;; User     : Mangesh Gupte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-command-modifier 'meta) ; Sets the command (Apple) key as Meta
(setq mac-option-modifier 'meta)  ; Sets the option key as Meta (this is server)

;; (setq mac-command-modifier 'ctrl mac-option-modifier 'meta)

;; Frame and window management:

(tabbar-mode -1)		     ; no tabbar
(one-buffer-one-frame-mode -1)       ; no one-buffer-per-frame
(setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
;; (smart-frame-positioning-mode -1)  ; do not place frames behind the Dock or outside of screen boundaries
 
;; (scroll-bar-mode -1)  ; no scrollbars
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs auto customize section. Emacs will add to this section when
;; you use the customize tool available on the help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c++-font-lock-extra-types
   (quote ("\\sw+_t" "\\([iof]\\|str\\)+stream\\(buf\\)?" "ios" "string" "rope" "list"
		   "slist" "deque" "vector" "bit_vector" "set" "multiset" "map" "multimap"
		   "hash\\(_\\(m\\(ap\\|ulti\\(map\\|set\\)\\)\\|set\\)\\)?" "stack" "queue"
		   "priority_queue" "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator"
		   "reference" "const_reference" "LPCTSTR" "BYTE" "WORD" "DWORD" "FIXME" "true"
		   "false" "private" "protected" "public" "__forceinline")))
 '(c-font-lock-extra-types (quote ("FILE" "\\sw+_t" "LPCTSTR" "WORD" "DWORD" "BYTE" "FIXME")))
 '(completion-ignored-extensions
   (quote (".bci" ".bin" ".binf" ".com" ".ext" ".free" ".beam" ".vee" ".jam" ".o" "~" ".lbin"
		   ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/"
		   ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem"
		   ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl"
		   ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
		   ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".aux" ".bbl" ".toc")))
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t))

;; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name)))

;; Get current system's type
(defun insert-system-type()
  (interactive)
  "Get current system's type"
  (insert (format "%s" system-type)))

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

;; Home is at different places.
(cond
 ((system-type-is-darwin) (setenv "HOME" "/Users/mangesh"))
 ((system-type-is-gnu) (setenv "HOME" "/home/mangesh"))


;; Set the path in which i have kept my .el/.elc files
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Common list primitives
(require 'cl)

;; Package management
(require 'package)

(setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")
		("melpa"     . "http://melpa.milkbox.net/packages/")))

(package-initialize)


(setq required-packages
      (list
	   'auto-complete ; auto-completion extension for GNU Emacs.
	   'autopair ; automagically pair braces and quotes.
	   'ido ; Easier opening of files.
	   'flx-ido ; Fuzzy matching for Emacs ... a la Sublime Text.
	   'ido-vertical-mode ; makes ido-mode display vertically.
       'js2-mode ; javascript-mode for emacs.
       'magit ; Emacs mode for Git.
       'markdown-mode ; Emacs mode for Markdown-formatted files.
       'multi-term ; managing multiple terminal buffers in Emacs.
	   'recentf ; is a minor mode that builds a list of recently opened files.
	   'session ; Session management for emacs.
	   'uniquify ; Make buffer names unique
	   'web-mode ; handle mixed php and html files.
	   ))

;; Check if all packages are installed.
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
;; (unless (packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p required-packages)
;;     (when (not (package-installed-p p))
;; 	  (package-install p))))

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
                ("\\.php$"  . php-mode)
                ("\\.inc$"  . php-mode)
                ("\\.m$"    . octave-mode)
                ("\\.ma?k\\'" . makefile-mode)
                ("\\(M\\|m\\|GNUm\\)akefile\\(\\.in\\)?" . makefile-mode)
                ("\\.pl$"   . perl-mode)
                ("\\.pro$"  . prolog-mode)
                ("\\.pm$"   . perl-mode)
                ("\\.java$" . java-mode)
                ("\\.txt$"  . text-mode)
                ("\\.tex$"  . latex-mode)
                ("\\.sty$"  . latex-mode)
                ("\\.bbl$"  . latex-mode)
                ("\\.html$" . html-helper-mode)
                ("\\.el\\'" . emacs-lisp-mode)
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
                ("\\.js\\'"       . javascript-mode)

                )
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make cursor into a box.
(setq-default cursor-type 'box)

;; Highlight current line.
(global-hl-line-mode 0)

;; Highlight matching paranthesis.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Common Settings
(setq user-full-name "Mangesh Gupte")

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; Volatile-highlight. Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the
;; next command is run.

(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))

;; Put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

;; Enable copy/paste from emacs to other apps
(setq
 interprogram-cut-function 'x-select-text
 interprogram-paste-function 'x-selection-value
 save-interprogram-paste-before-kill t
 select-active-regions t
 x-select-enable-clipboard t
 x-select-enable-primary t)

;; Set garbage collection to 20M
(setq gc-cons-threshold 20000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 80)
;; (setq-default fill-prefix "    ")

;; Make sure UTF-8 is used everywhere.

(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Scroll just one line when hitting the bottom of the window
(setq scroll-step 1)

;; do NOT add newlines if I cursor past last line in file
(setq next-line-add-newlines nil)

;; (setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)

;; Shut off annoying beep. Keep it on only for special situations
(setq visible-bell t)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
	 			  dired-directory
				  (revert-buffer-function " %b"
				  ("%b - Dir:  " default-directory)))))))

(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))

;; Appearance
 
;; no mode-specific faces, everything in Monaco
(setq aquamacs-autoface-mode nil)

;; Change the font used by emacs
;; (set-default-font "-apple-monaco*-medium-r-normal--14-*-*-*-*-*-fontset-monaco14")
(set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;;(setq default-frame-alist '((font . "-misc-terminus-medium-r-normal--18-0-0-0-p-0-iso8859-1")))
;; -Adobe-Courier-Medium-R-Normal--14-*-100-100-M-*-ISO8859-1
;; "-dec-terminal-medium-r-normal--14-140-75-75-c-80-iso8859-1"
;; "-*-helvetica-medium-r-*--*-120-*-*-*-*-iso8859-1"
;; "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"
;; (set-face-attribute 'default t :font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;;(add-to-list 'default-frame-alist
;;				'(font . "DejaVu Sans Mono-10"))

;; (set-face-attribute 'mode-line nil :inherit 'unspecified) ; show modeline in Monaco
(set-face-attribute 'echo-area nil :family 'unspecified)  ; show echo area in Monaco

;; Stop ^M's from displaying in system shell window
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Turn off the tool bar
(tool-bar-mode -1)

;; To obtain new font string, place this string:
;; (insert(prin1-to-string(w32-select-font))) in the scratch buffer
;; and hit M-x eval-buffer This will give you the font string you
;; need.

;; Shut off message buffer. Note - if you need to debug emacs,
;; comment these out so you can see what's going on.
;; (setq message-log-max nil)
;; (kill-buffer "*Messages*")

(autoload 'apache-mode "apache-mode" "autoloaded" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(line-number-mode t)
(column-number-mode t)

;; Displays which function the cursor is currently in , in certain modes
(which-func-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCHING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make searches case-INsensitive
(set-default 'case-fold-search t)

;; highlight incremental search
(setq search-highlight t)

;; TAB expands even during isearch
(define-key isearch-mode-map [tab] 'isearch-yank-word)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs to save backups in the global backups directory...
;;(defun make-backup-file-name (file)
;;  (concat "~/.emacs.d/autosave/" (file-name-nondirectory file) "~"))
(setq backup-directory-alist `(("." . "~/.emacs.d/autosave/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosave/" t)))

;; Set a dark color theme.
(autoload 'color-theme "color-theme" "Themes !!" t)
(color-theme-initialize)
(color-theme-clarity)

;; Session
(when (require 'session nil 'noerror)
  (add-hook 'after-init-hook 'session-initialize))

(when (require 'recentf nil 'noerror)
  (recentf-mode 1))

(when (require 'uniquify nil 'noerror)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))

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

;; ido
(defun ido-ignore-non-user-except-ielm (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except *ielm*."
  (and (string-match "^\*" name)
       (not (string= name "*ielm*"))))

(when (require 'ido nil 'noerror)
  (ido-mode t)
  (setq ido-enable-flex-matching t)     ; fuzzy matching is a must have
  (setq ido-everywhere t)
  (setq ido-file-extensions-order '(".php" ".py" ".txt" ".zsh" ".sh" ".ini" ".emacs"))
  (setq ido-ignore-extensions t))

(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

(defun ido-summary-buffers-to-end ()
  ;; Move the summaries to the end of the buffer list.
  ;; This is an example function which can be hooked on to
  ;; `ido-make-buffer-list-hook'.  Any buffer matching the regexps
  ;; `Summary' or `output\*$'are put to the end of the list.
  (let ((summaries (delq nil (mapcar
			      (lambda (x)
				 (if (or
				      (string-match "Summary" x)
					  (string-match "\\*\\'" x))
				     x))
			      ido-temp-list))))
	(ido-to-end summaries)))

;; flx-ido
(when (require 'flx-ido nil 'noerror)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(when (require 'ido-vertical-mode nil 'noerror)
  (ido-vertical-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom built functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mcg-source ()
  "Reread ~/.emacs."
  (interactive)
  (load-file "~/.emacs"))

;; let split windows display different buffers
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

;; ignored extentsions
(delete ".log" completion-ignored-extensions)

;; delete unnecessary autosave files
(setq delete-auto-save-files t) 

(defun indent-buffer()
  "Indent the whole buffer from point-min to point-max using
   the command indent-region"
  (interactive)
  (indent-region 0 (point-max) nil))

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

;; multi-term

;; Use Emacs terminfo, not system terminfo
;; tic -o ~/.terminfo /usr/share/emacs/24.3/etc/e/eterm-color.ti
(setq system-uses-terminfo nil)

;; http://www.emacswiki.org/emacs/MultiTerm
(when (require 'multi-term nil t)
  (global-set-key "\C-z" 'multi-term)
  (global-set-key (kbd "C-c n") 'multi-term-next)
  (global-set-key (kbd "C-c p") 'multi-term-prev)
  (setq multi-term--program "/usr/local/bin/zsh"))

;; Turn off current-line-highlighting and auto-pair.
(defadvice term-char-mode (after term-char-mode-fixes ())
  (autopair-mode -1)
  (set (make-local-variable 'hl-line-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil))
(ad-activate 'term-char-mode)

;; fix copy/paste
(add-hook
 'term-mode-hook
 (lambda ()
   (define-key term-raw-map (kbd "C-y") 'term-paste)
   (define-key term-raw-map (kbd "C-v") 'term-paste)
   (define-key term-raw-map (kbd "s-v") 'term-paste)))


;; autopair
(when (require 'autopair nil 'noerror)
  (autopair-global-mode 1)
  ;; tells autopair to automatically wrap the selection region with the
  ;; delimiters you’re trying to insert.
  (setq autopair-autowrap t))

;; ==========================================================
;; Work Specific Stuff
;; ==========================================================

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'ws-trim)
(global-ws-trim-mode t)

(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
		 (number-sequence my-tab-width 200 my-tab-width)))
  (setq fill-column 120)
  (gtags-mode t)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-const-nonempty 'c-lineup-math))

(defun git-grep-prompt ()
  (let* ((default (current-word))
         (prompt (if default
                     (concat "Search for: (default " default ") ")
                   "Search for: "))
         (search (read-from-minibuffer prompt nil nil nil nil default)))
    (if (> (length search) 0)
        search
      (or default ""))))

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive (list (git-grep-prompt)))
  (grep-find (concat "git --no-pager grep -P -n "
                     (shell-quote-argument search)
					 " `git rev-parse --show-toplevel`")))

(global-set-key [f6] 'git-grep)

;; (semantic-mode 1)
;; (global-ede-mode t)
;; (autoload 'gtags-mode "gtags" "" t)
;; (when window-system (speedbar t))
;; =====================================================================
;; USEFUL NOTES AND OTHER STUFF
;; =====================================================================

;; How to record and display a keyboard macro

;; Just open a buffer and type C-x ( Then start typing in your macro.
;; Once you are finished defining your macro type C-x ) Then type M-x
;; name-last-kbd-macro. This will allow you to call your macro
;; whatever you want. Next open up your .emacs file and position your
;; cursor where you want the code for the macro to appear.  Type M-x
;; insert-kbd-macro and type in the name.  The code will automatically
;; be generated.

;; =====================================================================

;; Use shell-command-on-region M-| to send region to external
;; process. If you use a prefix argument , C-u M-| this will replace
;; the region with the output of the external process. Good for
;; sending something to stdin and reading from stdout.

;; =====================================================================

;; To copy to named register: C-x r s a - Where a is the name of the
;; register ( a - z ) to save the text to.

;; To paste from named register: C-x r i a - Where a is the name of
;; the register ( a - z ) to paste the saved text from.

;; To remember current point: C-x r spc a - Where a is the name of the
;; register to save point to.

;; To jump to named point: C-x r j a - Where a is the name of the
;; register holding desired point to jump to

;; =====================================================================

;; Accumulating text
;;
;; M-x append-to-buffer
;;   Append region to contents of specified buffer.
;; M-x prepend-to-buffer
;;   Prepend region to contents of specified buffer.
;; M-x copy-to-buffer
;;   Copy region into specified buffer, deleting that buffer's old contents.
;; M-x insert-buffer
;;   Insert contents of specified buffer into current buffer at point.
;; M-x append-to-file
;;   Append region to contents of specified file, at the end.

;; =====================================================================
;;
;; Printing
;;
;; M-x ps-spool-buffer[-with-faces]
;; Generate postscript for the current buffer with faces. Stores in a
;; special *postscript* buffer

;; ====================================================================
;; Search and Replace
;;
;; Putting a newline as part of the replaced string.
;; M-x replace-string RET <string> RET C-q C-j RET
