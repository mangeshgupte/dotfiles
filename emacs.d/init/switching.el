;;; switching --- All settings related to changing buffers, opening files etc.

;;; Commentary:

;;; Code:
(when (require 'session nil 'noerror)
  (setq session-use-package t)
  (add-hook 'after-init-hook 'session-initialize))

(recentf-mode 1)

(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and buffer navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ffap)
(ffap-bindings)
(setq ffap-require-prefix t)

;; ido
(when (require 'ido nil 'noerror)
  (ido-mode t)
  (setq ido-enable-flex-matching t)     ; fuzzy matching is a must have
  (setq ido-everywhere t)
  (setq ido-file-extensions-order '(".php" ".py" ".txt" ".zsh" ".sh" ".ini" ".emacs"))
  (setq ido-ignore-extensions t))

(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

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

(setq ido-use-virtual-buffers t)

(defun sudo-edit-current-file ()
  ;; Edit current file with sudo and do not lose position within the file.
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))

(defun prepare-tramp-sudo-string (tempfile)
  "Open TEMPFILE as root, proxying through SSH for remote files."
  (if (file-remote-p tempfile)
      (let ((user (file-remote-p tempfile 'user))
            (host (file-remote-p tempfile 'host))
            (localname (file-remote-p tempfile 'localname)))
        (format "/ssh:%s@%s|sudo:root@%s:%s" user host host localname))
    (concat "/sudo:root@localhost:" tempfile)))

(setq tramp-default-method "ssh")
;; Disable backup and auto-save for remote (TRAMP) files
(setq tramp-backup-directory-alist `((".*" . nil)))
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (auto-save-mode -1))))


;; flx-ido
(when (require 'flx-ido nil 'noerror)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(when (require 'ido-grid-mode nil 'noerror)
  (ido-grid-mode t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)
(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;; ignored extensions
(delete ".log" completion-ignored-extensions)

(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key [C-double-mouse-1] 'browse-url)
