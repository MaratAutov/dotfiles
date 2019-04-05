;;
(add-to-list 'load-path "~/.emacs.d/lisp" t)

(defconst my-config "~/.emacs")

(defun load-cfg ()
  (interactive)
  (find-file my-config)
  )

(defun is-linux ()
  (or (eq system-type 'gnu/linux) (eq system-type 'gnu))
  )

(defun is-windows ()
  (eq system-type 'windows-nt)
  )

;; where we are
(cond
 ((is-windows) (message "It's windows"))
 ((is-linux) (message "It's linux"))
 (t (message "Something else")))

;; ui
(tooltip-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq word-wrap t)

;; scrolling
 (setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

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
      
;; parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(setq inhibit-startup-screen t)
(setq c-basic-offset 4) ; indents 4 chars                            
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces

;; line numbering
(require 'nlinum)
(global-linum-mode 1)

(defun my-linum-mode-hook ()
  (when linum-mode
    (setq-local linum-format
                (concat "%" (number-to-string
			     (if (< 0 (buffer-size))
				 (+ 1 (max 1 (ceiling (log (count-lines 1 (buffer-size)) 10)))) 1))
                        "d "))))
(add-hook 'linum-mode-hook #'my-linum-mode-hook)


;; language
(set-language-environment 'Russian)

;; key
(global-set-key (kbd "C-'") 'toggle-truncate-lines)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "S-C-<up>") 'beginning-of-buffer)
(global-set-key (kbd "S-C-<down>") 'end-of-buffer)

;;
;; don't forget install popup.el
(require 'auto-complete)
(global-auto-complete-mode 1)

