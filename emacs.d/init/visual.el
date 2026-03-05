;;; visual --- Startup settings related to the look and feel of the buffers.

;;; Commentary:

;;; Code:

;; Put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)
(scroll-bar-mode -1)  ;; no scrollbars
(tool-bar-mode -1)    ;; Turn off the tool bar
(global-hl-line-mode 0)  ;; Turn off Highlight current line.
(setq-default cursor-type 'box)   ;; Make cursor into a box.


;; Highlight matching paranthesis.
(show-paren-mode t)
(setq show-paren-delay 0)

;; highlight incremental search
(setq search-highlight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a dark color theme.
;; (autoload 'color-theme "color-theme" "Themes !!" t)
;; (color-theme-initialize)
;; (color-theme-clarity)
(load-theme 'manoj-dark)

;; Volatile-highlight.
(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))

;; no mode-specific faces, everything in Monaco
(when (system-type-is-darwin)
  (setq
   save-interprogram-paste-before-kill t
   select-active-regions t
   select-enable-clipboard t
   select-enable-primary t)

  (when (boundp 'aquamacs-version)
    '(aquamacs-autoface-muode nil)
    (smart-frame-positioning-mode -1)  ; do not place frames behind the Dock or outside of screen boundaries
    )

  ;; Set font to match terminal (Monaco 12)
  (set-frame-font "Monaco-12" nil t)
  ;; (set-face-attribute 'mode-line nil :inherit "Monaco-12")

  ;; Stop ^M's from displaying in system shell window
  (add-hook 'comint-output-filter-functions 'ansi-color-process-output)
)

(when (system-type-is-gnu)
  (set-frame-font "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)
  (set-face-attribute 'mode-line nil :family "DejaVuSerifMono")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable powerline
(when (require 'powerline nil 'noerror)
  (powerline-default-theme)
  ;; Set mode-line colors directly to override theme
  (set-face-attribute 'mode-line nil
                      :foreground "#ffffff" :background "#005f87" :box nil :height 1.2)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#b0b0b0" :background "#3a3a3a" :weight 'light :box nil :height 1.2)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "#ffffff" :background "#005f5f")
  (set-face-attribute 'powerline-active2 nil
                      :foreground "#ffffff" :background "#004050")
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground "#999999" :background "#2e2e2e")
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground "#999999" :background "#252525")
  ;; Override manoj-dark's orange buffer name
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "#87d7ff" :background "#000000" :weight 'bold)
  (powerline-reset))

(line-number-mode t)
(column-number-mode t)

;; Displays which function the cursor is currently in, in certain modes
(which-function-mode 1)

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

(setq ring-bell-function 'ignore)

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

;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 5)
;; (setq scroll-conservatively 9999
;;       scroll-preserve-screen-position t)

(setq ansi-color-for-comint-mode t)

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


;; Use indentation instead of stars in org- mode
(setq org-startup-indented t)

;; fontify code in code blocks
(setq org-src-fontify-natively t)
