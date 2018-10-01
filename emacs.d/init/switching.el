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
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))

        (tramp-make-tramp-file-name
         "sudo"
         (tramp-file-name-user nil)
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))

(setq tramp-default-method "ssh")

;; flx-ido
(when (require 'flx-ido nil 'noerror)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))
;; (when (require 'ido-clever-match nil 'noerror)
;;   (ido-clever-match-enable))

(when (require 'ido-grid-mode nil 'noerror)
  (ido-grid-mode t))
;; (when (require 'ido-vertical-mode nil 'noerror)
;;   (ido-vertical-mode t))

;; (when (require 'smex nil 'noerror)
;;   (global-set-key (kbd "M-x") 'smex)
;;   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   ;; This is your old M-x.
;;   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

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
(global-set-key [C-double-mouse-1] 'browse-url)
