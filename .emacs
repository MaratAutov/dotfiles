;; set up directory
(defconst **config** "~/.emacs")
(defconst **plugin-directory** (concat user-emacs-directory "lisp"))

;;
(add-to-list 'load-path **plugin-directory** t)

;; color
(progn
  (add-to-list 'load-path (concat **plugin-directory** "/emacs-doom-themes"))
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    
	doom-themes-enable-italic t) 

  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )


(defun load-config-file ()
  "Load config file .emacs"
  (interactive)
  (find-file **config**)
  )

(defvar **is-linux** (or (eq system-type 'gnu/linux) (eq system-type 'gnu))
  "Check if linux is operation system."
  )
(defvar **is-windows** (eq system-type 'windows-nt)
  "Check if windows is operation system"
  )

;; where we are
(cond
 (**is-windows** (message "It's windows"))
 (**is-linux** (message "It's linux"))
 (t (message "Something else")))

;; ui
(progn
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)
  (setq word-wrap t)
  (setq use-dialog-box nil)
  (setq ring-bell-function 'ignore)
  (line-number-mode 1)
  (column-number-mode 1)
  (toggle-truncate-lines)
  (transient-mark-mode 1)
  (setq resize-minibuffer-mode t)
  (setq initial-major-mode 'text-mode)
  (delete-selection-mode t)
  (setq delete-active-region nil)
)

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
      auto-save-default nil)            ; no auto-save 
            
;; parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(setq inhibit-startup-screen t)
(setq c-basic-offset 4) ; indents 4 chars                            
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces

;; org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$". org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; recent file
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(setq recentf-max-save-items 50)

;; use system clipboard
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)
(setq save-interprogram-paste-before-kill t)

;; language
(set-language-environment 'Russian)
(setq default-input-method 'russian-computer)
(progn
  (let ((code-page 'utf-8-unix))
    (prefer-coding-system code-page)
    (set-buffer-file-coding-system code-page)
    (set-terminal-coding-system code-page)
    (set-default-coding-systems code-page)
))

;; key
(global-set-key (kbd "C-'") 'toggle-truncate-lines)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<delete>")
                '(lambda (n)
                   (interactive "p")
                   (if (use-region-p)
                       (delete-region (region-beginning) (region-end))
                     (delete-char n)))
		)

;; aliases
;; make frequently used commands short
(progn
  (defalias 'qrr 'query-replace-regexp)
  (defalias 'rr 'replace-regexp)
  (defalias 'lml 'list-matching-lines)
  (defalias 'dml 'delete-matching-lines)
  (defalias 'dnml 'delete-non-matching-lines)
  (defalias 'dtw 'delete-trailing-whitespace)
  (defalias 'sl 'sort-lines)
  (defalias 'rs 'replace-string)

  (defalias 'g 'grep)
  (defalias 'gf 'grep-find)
  (defalias 'fd 'find-dired)

  (defalias 'rb 'revert-buffer)

  (defalias 'sh 'shell)
  (defalias 'ps 'powershell)
  (defalias 'fb 'flyspell-buffer)
  (defalias 'sbc 'set-background-color)
  (defalias 'rof 'recentf-open-files)
  (defalias 'lcd 'list-colors-display)
  (defalias 'cc 'calc)

  (defalias 'eb 'eval-buffer)
  (defalias 'er 'eval-region)
  (defalias 'ed 'eval-defun)
  (defalias 'eis 'elisp-index-search)
  (defalias 'lf 'load-file)

  (defalias 'hm 'html-mode)
  (defalias 'tm 'text-mode)
  (defalias 'elm 'emacs-lisp-mode)
  (defalias 'om 'org-mode)
  (defalias 'ssm 'shell-script-mode)

  (defalias 'wsm 'whitespace-mode)
  (defalias 'gwsm 'global-whitespace-mode)
  (defalias 'vlm 'visual-line-mode)
  (defalias 'glm 'global-linum-mode)

  (fset 'yes-or-no-p 'y-or-n-p) ; y or n is enough
  (defalias 'list-buffers 'buffer-menu)
)
;; keys sets
(setq case-fold-search t)

;;
;; don't forget install popup.el
(defun ac-init()
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories (concat **plugin-directory** "/auto-complete//ac-dict"))
    (setq ac-auto-start        t)
    (setq ac-auto-show-menu    t)
    (global-auto-complete-mode t)
    (add-to-list 'ac-modes   'lisp-mode)
    (add-to-list 'ac-sources 'ac-source-semantic) ;;
    (add-to-list 'ac-sources 'ac-source-variables) ;;
    (add-to-list 'ac-sources 'ac-source-functions) ;;
    (add-to-list 'ac-sources 'ac-source-dictionary) ;;
    (add-to-list 'ac-sources 'ac-source-words-in-all-buffer) ;;
    (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
    (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
    (add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
    (setq ac-auto-start 2)
    (setq ac-dwim t)
    (define-key ac-completing-map (kbd "M-n") 'ac-next)
    (define-key ac-completing-map (kbd "M-p") 'ac-previous)
    (setq ac-override-local-map nil)
    (setq ac-ignore-case t)
    (setq ac-delay 0.5)
    (setq ac-use-fuzzy t)
    (setq ac-use-comphist t)
    (setq ac-use-quick-help nil)
)
(add-to-list 'load-path (concat **plugin-directory** "/auto-complete"))
(ac-init)
(global-auto-complete-mode 1)

;; csv
(autoload 'csv-mode "csv-mode"
  "Major mode for editing csv files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; async
(add-to-list 'load-path (concat **plugin-directory** "/emacs-async"))
(require 'async)

;; helm
(add-to-list 'load-path (concat **plugin-directory** "/helm"))
(require 'helm-config)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; line number
(require 'nlinum)
(require 'nlinum-hl)
(global-linum-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
