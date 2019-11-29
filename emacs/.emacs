;;
(defconst **plugin-directory** (concat user-emacs-directory "plugins"))
(add-to-list 'load-path **plugin-directory** t)


(defconst **themes-directory** (concat user-emacs-directory "themes"))
(add-to-list 'custom-theme-load-path **themes-directory**)
(add-to-list 'load-path **themes-directory** t)

(add-to-list 'custom-theme-load-path (concat **themes-directory** "/alect-themes"))
(add-to-list 'load-path (concat **themes-directory** "/alect-themes") t)

(require 'cl)
(load-theme 'alect-light-alt t)

(semantic-mode 1)

;; SavePlace
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "saved-places")
      save-place-forget-unreadable-files    t)

;; Electric-modes settings
(electric-pair-mode     -1)
(electric-indent-mode   -1)
;; Delete selection
(delete-selection-mode t)

;; ui
(when (display-graphic-p)
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
    (setq redisplay-dont-pause t)
    (setq resize-minibuffer-mode t)
    (setq initial-major-mode 'text-mode)
    (delete-selection-mode t)
    (setq delete-active-region t)
    ;; Fringe settings
    (fringe-mode '(8 . 0))
    (setq-default indicate-buffer-boundaries 'left)
  )
)


; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;; Disable backup/autosave files
(setq backup-inhibited          t
      make-backup-files         nil
      auto-save-default         nil
      auto-save-list-file-name  nil)

;; Coding-system settings
(set-language-environment 'Russian)
(setq buffer-file-coding-system         'utf-8
      file-name-coding-system           'utf-8)
(setq-default coding-system-for-read    'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system             'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

(setq-default display-line-numbers t)

;; Display file size/time in mode-line
(setq display-time-24hr-format  t)
(display-time-mode              t)
(size-indication-mode           t)
(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification
              '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; Indent settings
(setq-default indent-tabs-mode      nil
              tab-width             4
              tab-always-indent     nil
              c-basic-offset        2
              sh-basic-offset       2
              sh-indentation        2
              scala-basic-offset    2
              java-basic-offset     4
              standart-indent       4
              lisp-body-indent      2
              js-indent-level       2
              indent-line-function  'insert-tab)

;; Scrolling settings
(setq scroll-step             1
      scroll-margin           10
      scroll-conservatively   10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)
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

  (defalias 'list-buffers 'buffer-menu)
)

;; keys sets
(setq case-fold-search t)

;; Clipboard settings
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

(setq next-line-add-newlines nil)

;; Highlight search resalts
(setq search-highlight            t
      query-replace-highlight     t
      auto-window-vscroll         nil
      bidi-display-reordering     nil)

;;
(setq split-height-threshold  nil
      split-width-threshold   0)

(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; recent file
(recentf-mode 1)
(setq recentf-max-menu-items      150
      recentf-max-saved-items     550)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Show paren
(setq show-paren-delay 0
      show-paren-style 'expression)
(show-paren-mode 2)

(setq ns-pop-up-frames          nil
      ad-redefinition-action    'accept)

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; key
(global-set-key (kbd "C-'") 'toggle-truncate-lines)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'delete-window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; language environment
(setq default-input-method 'russian-computer)

;; duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'duplicate-line)

;; add auto complete
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

(defun my-toggle-ac-mode ()
  (interactive)
  (cond ((fboundp 'auto-complete-mode)
         (if (and (boundp 'auto-complete-mode) (equal auto-complete-mode t))
             (auto-complete-mode 0)
           (auto-complete-mode t)))
        (t nil)))
(global-set-key (kbd "C-x a") 'my-toggle-ac-mode)


;; org-mode
(progn
  (require 'org)
  (add-to-list 'auto-mode-alist '("\\.org$". org-mode))
  (setq org-support-shift-select t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-iswitchb)
  (add-hook 'org-mode-hook (lambda ()
                           (auto-complete-mode 1)
                           (message "Enable auto complete mode in org mode.")))
)

;; helm
(add-to-list 'load-path (concat **plugin-directory** "/helm"))
