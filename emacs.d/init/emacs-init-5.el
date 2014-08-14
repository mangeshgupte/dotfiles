;; ==========================================================
;; Work Specific Stuff
;; ==========================================================

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'ws-trim)
(global-ws-trim-mode t)

(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
		 (number-sequence my-tab-width 200 my-tab-width)))
  (setq fill-column 120)
  (gtags-mode t)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-const-nonempty 'c-lineup-math))

(defun git-grep-prompt ()
  (let* ((default (current-word))
         (prompt (if default
                     (concat "Search for: (default " default ") ")
                   "Search for: "))
         (search (read-from-minibuffer prompt nil nil nil nil default)))
    (if (> (length search) 0)
        search
      (or default ""))))

(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive (list (git-grep-prompt)))
  (grep-find (concat "git --no-pager grep -P -n "
                     (shell-quote-argument search)
					 " `git rev-parse --show-toplevel`")))

(global-set-key [f6] 'git-grep)

;; TAGS management.
(setq tags-table-list
	  '("/houzz/c2/TAGS"))

(elpy-enable)

;; (speedbar-add-supported-extension ".php") ; not necessarily required
;; (add-hook 'php-mode-user-hook 'semantic-default-java-setup)
;; (add-hook 'php-mode-user-hook
;; 		  (lambda ()
;; 			(setq imenu-create-index-function
;; 				  'semantic-create-imenu-index)
;; 			))
;; (semantic-mode 1)

;; (global-ede-mode t)
;; (autoload 'gtags-mode "gtags" "" t)
;; (when window-system (speedbar t))
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
