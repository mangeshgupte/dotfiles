;;; package -- .emacs dotfile  -*- mode: emacs-lisp -*-

;;; Code:

(load "~/.emacs.d/init/environment.el")
(load "~/.emacs.d/init/programming.el")
(load "~/.emacs.d/init/visual.el")
(load "~/.emacs.d/init/keymap.el")
(load "~/.emacs.d/init/switching.el")
(load "~/.emacs.d/init/buffer.el")
(load "~/.emacs.d/init/registers.el")

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

;; ====================================================================
;; Search and Replace
;;
;; Putting a newline as part of the replaced string.
;; M-x replace-string RET <string> RET C-q C-j RET

;; ====================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill-column yaml-mode web-mode vue-mode volatile-highlights valign session powerline php-mode org-ref nginx-mode multi-term markdown-mode magit js2-mode ido-grid-mode helm go-mode flycheck flx-ido find-file-in-repository elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-bold-face ((t (:inherit bold :weight extra-bold))))
 '(markdown-code-face ((t (:foreground "#d7875f"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8 :family "Helvetica"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :family "Helvetica"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3 :family "Helvetica"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.15 :family "Helvetica"))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.05 :family "Helvetica"))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0 :family "Helvetica"))))
 '(markdown-inline-code-face ((t (:foreground "#d7875f"))))
 '(markdown-italic-face ((t (:inherit italic :slant italic))))
 '(markdown-pre-face ((t (:foreground "#d7875f")))))
