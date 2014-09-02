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
(when (system-type-is-darwin)
  (setq
   interprogram-cut-function 'x-select-text
   interprogram-paste-function 'x-selection-value
   save-interprogram-paste-before-kill t
   select-active-regions t
   x-select-enable-clipboard t
   x-select-enable-primary t))

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

(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)
(define-key global-map (kbd "RET") 'newline-and-indent)

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
(when (system-type-is-darwin)
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
)

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

;; Rainbow parentheses
(defun rainbow-delimiters-colors ()
  (set-face-foreground 'rainbow-delimiters-depth-1-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "light blue")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "light gray")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "dark green")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "gray")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "slate blue")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "dark blue")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "white"))
(add-hook 'rainbow-delimiters-mode-hook 'rainbow-delimiters-colors)
(when (require 'rainbow-delimiters nil 'noerror)
  (global-rainbow-delimiters-mode))

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

