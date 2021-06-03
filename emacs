;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename : .emacs
;; User     : Mangesh Gupte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c++-font-lock-extra-types
   (quote
    ("\\sw+_t" "\\([iof]\\|str\\)+stream\\(buf\\)?" "ios" "string" "rope" "list" "slist" "deque" "vector" "bit_vector" "set" "multiset" "map" "multimap" "hash\\(_\\(m\\(ap\\|ulti\\(map\\|set\\)\\)\\|set\\)\\)?" "stack" "queue" "priority_queue" "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator" "reference" "const_reference" "LPCTSTR" "BYTE" "WORD" "DWORD" "FIXME" "true" "false" "private" "protected" "public" "__forceinline")))
 '(c-font-lock-extra-types
   (quote
    ("FILE" "\\sw+_t" "LPCTSTR" "WORD" "DWORD" "BYTE" "FIXME")))
 '(completion-ignored-extensions
   (quote
    (".bci" ".bin" ".binf" ".com" ".ext" ".free" ".beam" ".vee" ".jam" ".o" "~" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".aux" ".bbl" ".toc")))
 '(custom-safe-themes
   (quote
    ("4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (jinja2-mode vue-mode helm org-ref php-mode web-mode session powerline multi-term markdown-mode magit js2-mode ido-grid-mode go-mode flycheck flx-ido find-file-in-repository elpy)))
 '(session-use-package t nil (session)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#ffffff" :background "#003330"))))
 '(mode-line-inactive ((t (:foreground "#999999" :background "#002320" :weight light :box nil :inherit (mode-line))))))
