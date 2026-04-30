;;; visual --- Startup settings related to the look and feel of the buffers.

;;; Commentary:

;;; Code:

(scroll-bar-mode -1)  ;; no scrollbars
(tool-bar-mode -1)    ;; Turn off the tool bar
(setq-default cursor-type 'box)   ;; Make cursor into a box.

;; Highlight matching paranthesis.
(show-paren-mode t)
(setq show-paren-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'manoj-dark)

;; Volatile-highlight.
(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))

(when (system-type-is-darwin)
  (setq
   save-interprogram-paste-before-kill t)

  ;; Menlo: same look as Monaco but with real bold/italic variants
  ;; Larger font on external monitor (lower res = smaller text)
  (defun my-adjust-font-for-monitor (&optional frame)
    "Set font size based on the monitor FRAME is on."
    (let* ((attrs (frame-monitor-attributes frame))
           (width (nth 3 (assq 'geometry attrs)))
           (font-size (if (<= (or width (display-pixel-width)) 1920) 14 12)))
      (set-frame-font (format "Menlo-%d" font-size) nil t)))

  (my-adjust-font-for-monitor)
  (add-hook 'move-frame-functions #'my-adjust-font-for-monitor))

(when (system-type-is-gnu)
  (set-frame-font "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)
  (set-face-attribute 'mode-line nil :family "DejaVuSerifMono")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Displays which function the cursor is currently in, in certain modes
(which-function-mode 1)

;; Enable powerline
(when (require 'powerline nil 'noerror)
  (powerline-default-theme)
  ;; Set mode-line colors directly to override theme
  (set-face-attribute 'mode-line nil
                      :foreground "#ffffff" :background "#005f87" :box nil :height 1.1)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#b0b0b0" :background "#3a3a3a" :weight 'light :box nil :height 1.1)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "#ffffff" :background "#005f5f")
  (set-face-attribute 'powerline-active2 nil
                      :foreground "#ffffff" :background "#004050")
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground "#999999" :background "#2e2e2e")
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground "#999999" :background "#252525")
  ;; Make which-function readable on dark powerline background
  (set-face-attribute 'which-func nil :foreground "#87d7ff")

  ;; Override manoj-dark's orange buffer name
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "#87d7ff" :background "#000000" :weight 'bold
                      :height 1.1)
  (powerline-reset))

(column-number-mode t)


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

;; Markdown faces
(custom-set-faces
 '(markdown-inline-code-face ((t (:foreground "#d7875f"))))
 '(markdown-code-face ((t (:foreground "#d7875f"))))
 '(markdown-pre-face ((t (:foreground "#d7875f"))))
 '(markdown-bold-face ((t (:inherit bold :weight extra-bold))))
 '(markdown-italic-face ((t (:inherit italic :slant italic))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8 :family "Avenir Next"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :family "Avenir Next"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3 :family "Avenir Next"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.15 :family "Avenir Next"))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.05 :family "Avenir Next"))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0 :family "Avenir Next")))))

;; Use indentation instead of stars in org-mode
(setq org-startup-indented t)

;; Olivetti: centered text with wide margins for distraction-free reading.
;; Toggle manually with M-x olivetti-mode; auto-enable for prose modes.
;; Use a fractional width (0.7 = 70% of window) instead of a column count
;; because variable-pitch glyphs vary in width, so column-based sizing
;; misbehaves and the centering looks off.
(when (require 'olivetti nil 'noerror)
  (setq-default olivetti-body-width 0.7)
  (add-hook 'markdown-mode-hook #'olivetti-mode)
  (add-hook 'org-mode-hook #'olivetti-mode))

;; Variable-pitch: proportional font for prose, monospace for code blocks.
;; markdown-mode and org-mode inherit from variable-pitch / fixed-pitch faces,
;; so setting these once gives readable body text with code blocks staying mono.
(defun mcg-first-available-font (families)
  "Return the first font family in FAMILIES that is installed, or nil."
  (cl-find-if (lambda (f) (find-font (font-spec :family f))) families))

(when (system-type-is-darwin)
  (let ((prose-font (mcg-first-available-font
                     '("Iowan Old Style" "iA Writer Quattro S" "Charter" "Menlo"))))
    (when prose-font
      (set-face-attribute 'variable-pitch nil :family prose-font :height 160)))
  (set-face-attribute 'fixed-pitch nil :family "Menlo" :height 140))

(when (system-type-is-gnu)
  (set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :height 140)
  (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 120))

(add-hook 'markdown-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Soft word-wrap: break at word boundaries instead of mid-word at window edge.
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
