;; Session
(when (require 'session nil 'noerror)
  (add-hook 'after-init-hook 'session-initialize))

(when (require 'recentf nil 'noerror)
  (recentf-mode 1))

(when (require 'uniquify nil 'noerror)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and buffer navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ffap)
(ffap-bindings)
;; (global-unset-key "\C-x\C-f\C-f")
;; (global-set-key "\C-x\C-f\C-f" 'find-file-at-point)
(setq ffap-require-prefix t)

;; ido
(defun ido-ignore-non-user-except-ielm (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except *ielm*."
  (and (string-match "^\*" name)
       (not (string= name "*ielm*"))))

(when (require 'ido nil 'noerror)
  (ido-mode t)
  (setq ido-enable-flex-matching t)     ; fuzzy matching is a must have
  (setq ido-everywhere t)
  (setq ido-file-extensions-order '(".php" ".py" ".txt" ".zsh" ".sh" ".ini" ".emacs"))
  (setq ido-ignore-extensions t))

(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

(defun ido-summary-buffers-to-end ()
  ;; Move the summaries to the end of the buffer list.
  ;; This is an example function which can be hooked on to
  ;; `ido-make-buffer-list-hook'.  Any buffer matching the regexps
  ;; `Summary' or `output\*$'are put to the end of the list.
  (let ((summaries (delq nil (mapcar
			      (lambda (x)
				 (if (or
				      (string-match "Summary" x)
					  (string-match "\\*\\'" x))
				     x))
			      ido-temp-list))))
	(ido-to-end summaries)))

;; flx-ido
(when (require 'flx-ido nil 'noerror)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(when (require 'ido-vertical-mode nil 'noerror)
  (ido-vertical-mode t))

(when (require 'ibuffer nil 'noerror)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(defun ibuffer-ido-find-file (file &optional wildcards)
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory
           (let ((buf (ibuffer-current-buffer)))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   default-directory)
               default-directory))))
     (list (ido-read-file-name "Find file: " default-directory) t)))
  (find-file file wildcards))

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

;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)
(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;; ignored extentsions
(delete ".log" completion-ignored-extensions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom built functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multi-term

;; Use Emacs terminfo, not system terminfo
;; tic -o ~/.terminfo /usr/share/emacs/24.3/etc/e/eterm-color.ti
(setq system-uses-terminfo nil)

;; http://www.emacswiki.org/emacs/MultiTerm
(when (require 'multi-term nil t)
  (global-set-key "\C-z" 'multi-term)
  (global-set-key (kbd "C-c n") 'multi-term-next)
  (global-set-key (kbd "C-c p") 'multi-term-prev)
  (setq multi-term--program "/usr/local/bin/zsh"))

