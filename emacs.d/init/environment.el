;;; environment --- Startup settings related to the os, variables etc.  -*- lexical-binding: t; -*-

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

;; GUI Emacs on macOS is launched by launchd (Dock/Finder), not the shell,
;; so it does NOT inherit the shell's PATH.  Homebrew's bin dirs and any
;; nvm-managed Node bin (where npm-global CLIs such as mmdc land) are then
;; missing from `exec-path'/PATH, and `executable-find' fails for tools like
;; pandoc and mmdc even though they work in a terminal.  Prepend whichever
;; of these exist to both `exec-path' (for `executable-find'/`call-process')
;; and the PATH env var (for subprocesses spawned via a shell).
(when (system-type-is-darwin)
  (dolist (dir (append '("/usr/local/bin" "/opt/homebrew/bin")
                       (file-expand-wildcards
                        (expand-file-name "~/.nvm/versions/node/*/bin"))))
    (setq dir (expand-file-name dir))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))

;; Set the path in which i have kept my .el/.elc files
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;; Package management.  Emacs 27+ runs `package-initialize' automatically
;; before the init file is loaded, so we only set the archives here.
(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; Declared special so the list works under lexical binding.
(defvar required-packages)
(setq required-packages
      (list
       'js2-mode ; javascript-mode for emacs.
       'magit ; Emacs mode for Git.
       'markdown-mode ; Emacs mode for Markdown-formatted files.
       'multi-term ; managing multiple terminal buffers in Emacs.
       'web-mode ; handle mixed php and html files.
       'powerline ; Change the display line.
       'flx-ido ; Fuzzy matching for Emacs ... a la Sublime Text.
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
       'visual-fill-column ; Soft-wrap text at fill-column
       'olivetti ; Centered text with wide margins for distraction-free reading

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
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Make sure UTF-8 is used everywhere.
(set-language-environment 'UTF-8)
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8-unix)

;; Change default major mode to text from fundamental.
;; (auto-fill is enabled via `text-mode-hook' in buffer.el.)
(setq initial-major-mode 'text-mode)

;; Tell emacs to save backups in the global backups directory...
(setq backup-directory-alist `(("." . "~/.emacs.autosave/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.autosave/" t)))
(setq delete-auto-save-files t)  ; delete unnecessary autosave files

(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

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
      '(".bci" ".bin" ".binf" ".com" ".ext" ".free" ".beam" ".vee" ".jam" ".o" "~" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))

;; Set garbage collection to 20M
(setq gc-cons-threshold 20000000)

;; Prevent auth-source from prompting to save credentials
(setq auth-source-save-behavior nil)
