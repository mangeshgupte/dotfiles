;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename : .emacs
;; User     : Mangesh Gupte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq mac-command-modifier 'ctrl mac-option-modifier 'meta)

(load "~/.emacs.d/init/emacs-init-1.el")
(load "~/.emacs.d/init/emacs-init-2.el")
(load "~/.emacs.d/init/emacs-init-3.el")
(load "~/.emacs.d/init/emacs-init-4.el")
(load "~/.emacs.d/init/emacs-init-5.el")

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
(put 'downcase-region 'disabled nil)
