;;
;; config file
;;
(defconst config-file "~/.emacs")

(defun load-config ()
  (interactive)
  (find-file config-file)
  )

(defun is-linux ()
  (or (eq system-type 'gnu/linux) (eq system-type 'gnu))
  )

(defun is-windows ()
  (eq system-type 'windows-nt)
  )

(cond
 ((is-windows) (message "It's windows"))
 ((is-linux) (message "It's linux"))
 (t (message "Something else")))

;; set path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;
(progn
  (tooltip-mode -1)
  (tool-bar-mode -1)
  )

;; colorize
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'spolsky t)

;; line numbering
(require 'nlinum)
(global-nlinum-mode 1)

(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;; backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200)           ; number of keystrokes between auto-saves (default: 300)
      
;; parenthis
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(setq inhibit-startup-screen t)
(setq c-basic-offset 4) ; indents 4 chars                            
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces

;; localization
(set-input-method 'russian-computer)

; keybindings
(global-set-key (kbd "C-'") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)

