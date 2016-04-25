;;; switching --- All settings related to changing buffers, opening files etc.

;;; Commentary:

;;; Code:
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


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:"
    buffer-file-name))))

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

(global-set-key (kbd "C-x f") 'find-file-in-repository)

;; (require 'helm-config)

;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-quick-update                     t ; do not display invisible candidates
;;       helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (helm-mode 1)

