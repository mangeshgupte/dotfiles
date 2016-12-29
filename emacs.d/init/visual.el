;;; visual --- Startup settings related to the look and feel of the buffers.

;;; Commentary:

;;; Code:

(smart-frame-positioning-mode -1)  ; do not place frames behind the Dock or outside of screen boundaries
(scroll-bar-mode -1)  ; no scrollbars

;; Make cursor into a box.
(setq-default cursor-type 'box)

;; Highlight current line.
(global-hl-line-mode 0)

;; Highlight matching paranthesis.
(show-paren-mode t)
(setq show-paren-delay 0)

;; highlight incremental search
(setq search-highlight t)

;; Put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

;; Enable copy/paste from emacs to other apps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a dark color theme.
(autoload 'color-theme "color-theme" "Themes !!" t)
(color-theme-initialize)
(color-theme-clarity)

;; Volatile-highlight. Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the
;; next command is run.
(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))


;; no mode-specific faces, everything in Monaco
(when (system-type-is-darwin)
  (setq
   interprogram-cut-function 'x-select-text
   interprogram-paste-function 'x-selection-value
   save-interprogram-paste-before-kill t
   select-active-regions t
   x-select-enable-clipboard t
   x-select-enable-primary t)
  (setq aquamacs-autoface-mode nil)

  ;; Change the font used by emacs
  (set-default-font "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  ;;   (set-default-font "-apple-monaco*-medium-r-normal--14-*-*-*-*-*-fontset-monaco14")
  ;; (set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
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
  ;; (add-to-list 'comint-output-filter-functions 'shell-strip-ctrl-m)
  (add-hook 'comint-output-filter-functions 'ansi-color-process-output)
)

;; Turn off the tool bar
(tool-bar-mode -1)
;; To obtain new font string, place this string:
;; (insert(prin1-to-string(w32-select-font))) in the scratch buffer
;; and hit M-x eval-buffer This will give you the font string you
;; need.

;; Rainbow parentheses
(defun rainbow-delimiters-colors ()
  (set-face-foreground 'rainbow-delimiters-depth-1-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "light blue")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "light gray")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "dark green")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "gray")
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "slate blue")
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "dark blue")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "white"))
(add-hook 'rainbow-delimiters-mode-hook 'rainbow-delimiters-colors)
(when (require 'rainbow-delimiters nil 'noerror)
  (global-rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(line-number-mode t)
(column-number-mode t)

;; Displays which function the cursor is currently in, in certain modes
(which-func-mode 1)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                  dired-revert
                  (directory-buffer-function " %b"
				  ("%b - Dir:  " default-directory)))))))

(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))

(setq speedbar-frame-parameters
      (quote
       ((minibuffer)
        (width          . 45)
        (border-width   . 0)
        (menu-bar-lines . 0)
        (unsplittable   . t))))

;; Visual Bookmarks
(when (require 'bm nil 'noerror)
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous)

  ;; Click on fringe to toggle bookmarks, and use mouse wheel to move
  ;; between them.
  (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

  ;; If you would like the markers on the right fringe instead of the
  ;; left, add the following to line:
  (setq bm-marker 'bm-marker-right))

(when (require 'powerline nil 'noerror)
  (powerline-default-theme)
  (custom-set-faces
   '(mode-line             ((t (:foreground "#ffffff" :background "#003330" ))))
   '(mode-line-inactive    ((t (:foreground "#999999" :background "#002320" :weight light :box nil :inherit (mode-line )))))))


;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 5)
;; (setq scroll-conservatively 9999
;;       scroll-preserve-screen-position t)

(setq ansi-color-for-comint-mode t)
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i")


(system-name)
;; Show remote hostname if on a different host.
;; (defconst my-mode-line-buffer-identification
;;   (list
;;    '(:eval
;;      (let ((host-name
;;             (or (file-remote-p default-directory 'host)
;;                 (system-name))))
;;        (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
;;            (substring host-name 0 (match-beginning 1))
;;          host-name)))
;;    ": %12b"))

;; (setq-default
;;   mode-line-buffer-identification
;;   my-mode-line-buffer-identification)

;; (add-hook
;;  'dired-mode-hook
;;  '(lambda ()
;;     (setq
;;      mode-line-buffer-identification
;;      my-mode-line-buffer-identification)))
