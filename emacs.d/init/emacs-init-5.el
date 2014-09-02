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

(when (require 'elpy nil 'noerror)
  (elpy-enable)
  )

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
