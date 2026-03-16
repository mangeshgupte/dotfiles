;;; environment --- Startup settings related to the os, variables etc.

;;; Commentary:

;;; Code:
(defun mcg-source ()
  "Reread ~/.emacs."
  (interactive)
  (load-file "~/.emacs"))

;; Get current system's name
(defun insert-system-name()
  "Get current system's name."
  (interactive)
  (insert (format "%s" system-name)))

;; Get current system's type
(defun insert-system-type()
  "Get current system's type."
  (interactive)
  (insert (format "%s" system-type)))

(defun insert-date()
  "Insert string for the current date formatted like '10 April, 2019, Monday'"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%e %B, %Y, %A")))

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  "Return true if system is darwin-based (Mac OS X)."
  (interactive)
  (string-equal system-type "darwin"))

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  "Return true if system is GNU/Linux-based."
  (interactive)
  (string-equal system-type "gnu/linux"))

(menu-bar-mode -1)

;; Set the path in which i have kept my .el/.elc files
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;; Package management
(require 'package)
(package-initialize)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq required-packages
      (list
       ; 'auto-complete ; auto-completion extension for GNU Emacs.
       'js2-mode ; javascript-mode for emacs.
       'magit ; Emacs mode for Git.
       'markdown-mode ; Emacs mode for Markdown-formatted files.
       'multi-term ; managing multiple terminal buffers in Emacs.
       'session ; Session management for emacs.
       'web-mode ; handle mixed php and html files.
       'powerline ; Change the display line.
       ; 'color-theme-modern  ; Different color themes
       ; 'ido-clever-match ; Smart matching between (exact, prefix, substring, flex)
       'flx-ido ; Fuzzy matching for Emacs ... a la Sublime Text.
       ; 'ido-vertical-mode ; makes ido-mode display vertically.
       'ido-grid-mode ; makes ido-mode display vertically.
       'find-file-in-repository ; Auto complete file names for any file in repo.
       'flycheck ;
       'go-mode ; Major mode for the Go language
       'elpy ; Python mode
       'volatile-highlights ; Highlight the latest changes in the buffer (like text inserted from:
                            ; yank, undo, etc.) until the next command is run.
       'php-mode
       'helm
       'org-ref
       'nginx-mode
       'vue-mode
       'yaml-mode
       'valign ; Pixel-align table columns in markdown/org

       ))


;; Check if all packages are installed.
(defun packages-installed-p ()
  (cl-every #'package-installed-p required-packages))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (message "Installing package '%s'" p)
      (package-install p))))

;; Common Settings
(setq user-full-name "Mangesh Gupte")

;; Replace "yes or no" with y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make sure UTF-8 is used everywhere.
(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8-unix)

;; Change default major mode to text from fundamental
(setq initial-major-mode
      (lambda ()
        (text-mode)
        (turn-on-auto-fill)))

;; Tell emacs to save backups in the global backups directory...
;;(defun make-backup-file-name (file)
;;  (concat "~/.emacs.d/autosave/" (file-name-nondirectory file) "~"))
(setq backup-directory-alist `(("." . "~/.emacs.autosave/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.autosave/" t)))
(setq delete-auto-save-files t)  ; delete unnecessary autosave files

(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; Access internal-server by jumping through bastion
;; (set-default 'tramp-default-proxies-alist
;;              (quote (("hdwu01.hz" "hadoop" "data-util" "/ssh:%h:"))))

;; Don't load outdated library files.
(setq load-prefer-newer t)

;; Disable lock files (.#filename)
(setq create-lockfiles nil)

;; Startup behavior
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Auto-revert files when changed on disk
(global-auto-revert-mode t)

;; Completion ignored extensions
(setq completion-ignored-extensions
      '(".bci" ".bin" ".binf" ".com" ".ext" ".free" ".beam" ".vee" ".jam" ".o" "~" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".aux" ".bbl" ".toc"))

;; Set garbage collection to 20M
(setq gc-cons-threshold 20000000)
;; (setq garbage-collection-messages t)
