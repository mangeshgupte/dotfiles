;;; environment --- Startup settings related to the os, variables etc.

;;; Commentary:

;;; Code:
(defun mcg-source ()
  "Reread ~/.emacs."
  (interactive)
  (load-file "~/.emacs"))

(setq mac-command-modifier 'meta) ; Sets the command (Apple) key as Meta
(setq mac-option-modifier 'meta)  ; Sets the option key as Meta (this is server)

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

(defun insert-date()
  "Insert string for the current date formatted like '10 April, 2019, Monday'"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%e %B, %Y, %A")))

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
(cond
 ((>= emacs-major-version 24)  (require 'package))
 ((< emacs-major-version 24)   (load (expand-file-name "~/.emacs.d/elpa/package.el"))))

(package-initialize)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ))

(setq required-packages
      (list
       ; 'auto-complete ; auto-completion extension for GNU Emacs.
       'js2-mode ; javascript-mode for emacs.
       'magit ; Emacs mode for Git.
       'markdown-mode ; Emacs mode for Markdown-formatted files.
       'multi-term ; managing multiple terminal buffers in Emacs.
       'recentf ; is a minor mode that builds a list of recently opened files.
       'session ; Session management for emacs.
       'web-mode ; handle mixed php and html files.
       'powerline ; Change the display line.
       ; 'color-theme-modern  ; Different color themes
       'ido ; Easier opening of files.
       ; 'ido-clever-match ; Smart matching between (exact, prefix, substring, flex)
       'flx-ido ; Fuzzy matching for Emacs ... a la Sublime Text.
       ; 'ido-vertical-mode ; makes ido-mode display vertically.
       'ido-grid-mode ; makes ido-mode display vertically.
       'find-file-in-repository ; Auto complete file names for any file in repo.
       'flycheck ;
       'go-mode ; Major mode for the Go language
       'elpy ; Python mode
       'php-mode
       'helm
       'org-ref
       'nginx-mode
       ))


;; Check if all packages are installed.
(defun packages-installed-p ()
  (let ((value t))
    (dolist (p required-packages value)
      (if (not (package-installed-p p)) (setq value nil) (setq value (and t value))))
    ))

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

;; SQL settings

;; Set location of mysql binary.
(setq sql-mysql-program "/usr/local/mysql/bin/mysql")

(setq sql-mysql-login-params
      '((user :default "root")
        (database :default "c2")
        (server :default "localhost")))

;; Common Settings
(setq user-full-name "Mangesh Gupte")

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; Set garbage collection to 20M
(setq gc-cons-threshold 20000000)

;; Make sure UTF-8 is used everywhere.
(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

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

(set-default 'tramp-default-proxies-alist
             (quote (("hdwu01.hz" "hadoop" "data-util" "/ssh:%h:"))))

;; Don't load outdated library files.
(setq load-prefer-newer t)

;; Set garbage collection to 20M
(setq gc-cons-threshold 20000000)
;; (setq garbage-collection-messages t)
