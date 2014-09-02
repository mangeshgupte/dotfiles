(setq mac-command-modifier 'meta) ; Sets the command (Apple) key as Meta
(setq mac-option-modifier 'meta)  ; Sets the option key as Meta (this is server)

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
 ((system-type-is-gnu) (setenv "HOME" "/home/mangesh")))

;; Frame and window management:
(when (system-type-is-darwin)
  (tabbar-mode -1)		     ; no tabbar
  (one-buffer-one-frame-mode -1)       ; no one-buffer-per-frame
  (setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
)

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
(cond
 ((>= emacs-major-version 24)  (require 'package))
 ((< emacs-major-version 24)   (load (expand-file-name "~/.emacs.d/elpa/package.el"))))

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))

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
       'powerline ; Change the display line.
       'color-theme ; Different color themes
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
				("\\.js\\'"       . javascript-mode)
				("\\.yaml$"  . yaml-mode)
				)
              auto-mode-alist))

