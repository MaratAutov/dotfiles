(defun load-config()
  (interactive)
  (find-file "~/.emacs")
  )

(defvar *windows* nil)
(setq *windows* (eq system-type 'windows-nt))
(defvar *linux* nil)
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

(setq use-dark-theme nil)
(progn
  (let ((font-name (if *windows* "Consolas" "Monospace")))
  (set-face-attribute 'default nil :family font-name :height 100)
  (if use-dark-theme
      (load-file "~/.emacs.d/plugins/darcula-theme.el"))
))


(cond (*linux* (message "You are in linux system"))
      (*windows* (message "You are in windows system")))
(cond (*linux* (message "You are in linux system"))
      (*windows* (message "You are in windows system")))

;;;
(progn
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 1)
  (setq use-dialog-box nil)
  (setq ring-bell-function 'ignore)
  (column-number-mode 1)
  (setq resize-minibuffer-mode t)
  (setq initial-major-mode (quote text-mode))
  (toggle-truncate-lines 1)
  (transient-mark-mode 1)
  (setq org-log-done t)
  (setq tramp-default-method "ssh")
  (setq tramp-chunksize 500)
  (setq inhibit-splash-screen t)
  (setq ingibit-startup-message t)
  )

; load path
(setq load-path (cons "~/.emacs.d/plugins" load-path))

(defun flush-blank-lines (start end)
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun collapse-blank-lines (start end)
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))

;;

(when *linux*
  (require 'server)
  (unless (server-running-p)
    (server-start))) ;;

(require 'dired)
(setq dired-recursive-deletes 'top)

(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-use-popup-menu nil)
(global-set-key (kbd "<f6>") 'imenu)

;; display current buffer name in the title bar
(setq frame-title-format "%b")

; delete selection
(delete-selection-mode 1)

					; no backup
(progn
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq auto-save-list-file-name nil)
)
;; auto revert
(setq auto-revert-mode t)

;; paren mode
(require 'paren)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;;
(require 'electric)
(electric-pair-mode t)

;; org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$". org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-todo-keywords
      '((sequence "TODO" "ERROR" "DOING" "TESTING" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces '(("TODO" . (:foreground "DarkCyan" :weight bold))
                                                      ("DOING" . (:foreground "brown" :weight bold))
                                                      ("ERROR" . (:foreground "OrangeRed" :weight bold))
                                                      ("TESTING" . (:foreground "dark magenta" :weight bold))
                                                      ("DONE" . (:foreground "dark green" :weight bold))
                                                      ("CANCELED" . (:foreground "dark orange" :weight bold))
))

;; recent file
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(setq recentf-max-save-items 50)

;; use system clipboard
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)
(setq save-interprogram-paste-before-kill t)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(delete-selection-mode t)
(setq delete-active-region nil)

;;; Language environment
(set-language-environment 'Russian)
(setq default-input-method 'russian-computer)
(define-coding-system-alias 'windows-1251 'cp1251)
(progn
  (let ((code-page (if *linux* 'utf-8-unix 'cp1251-dos)))
    (message (format "Code page is %s" (prin1-to-string code-page)))
       (prefer-coding-system code-page)
                (set-buffer-file-coding-system code-page)
                (set-terminal-coding-system code-page)
                (set-default-coding-systems code-page)
))

(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format "   %d ") ;; задаем формат нумерации строк

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
;; keys
;; sets
(setq case-fold-search t)

;; keys
(progn
  (global-set-key (kbd "C-'") 'toggle-truncate-lines)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "M-1") 'delete-other-windows)
  (global-set-key (kbd "M-2") 'split-window-vertically)
  (global-set-key (kbd "M-3") 'split-window-horizontally)
  (global-set-key (kbd "M-0") 'delete-window)
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-,") 'previous-buffer)
  (global-set-key (kbd "C-.") 'next-buffer)
  (global-set-key (kbd "C-c C-c") 'comment-region)
  (global-set-key (kbd "C-n") 'new-empty-buffer)
  (global-set-key (kbd "C-x C-k") 'kill-this-buffer)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "<delete>")
                '(lambda (n)
                   (interactive "p")
                   (if (use-region-p)
                       (delete-region (region-beginning) (region-end))
                     (delete-char n)))
		)
)

(defun new-empty-buffer ()
  "Open a new empty buffer."  (interactive)
  (let ((nbuf (generate-new-buffer "new")))
    (switch-to-buffer nbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "C-n") 'new-empty-buffer)

;; date & time functions
(global-unset-key (kbd "M-i"))
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%d.%m.%Y")))

(global-set-key (kbd "M-i d") 'my-insert-date)
(defun my-insert-timestamp()
  (interactive)
  (insert (format-time-string "%d.%m.%Y %H:%M:%S")))
(global-set-key (kbd "M-i t") 'my-insert-timestamp)

(defun ac-init()
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete//ac-dict")
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
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(ac-init)

(defun my-toggle-ac-mode ()
  (interactive)
  (cond ((fboundp 'auto-complete-mode)
         (if (and (boundp 'auto-complete-mode) (equal auto-complete-mode t))
             (auto-complete-mode 0)
           (auto-complete-mode t)))
        (t nil)))
(global-set-key (kbd "C-x a") 'my-toggle-ac-mode)

; highlight symbols
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")

(require 'highlight-symbol)
(progn
  (global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
  (global-set-key (kbd "<f3>") 'highlight-symbol-next)
  (global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
  (global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)
)

(cond (*linux* (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
      (*windows* (setq interprogram-paste-function 'x-selection-value))
      )

;; indent
(defun my-indent (start end)
  (interactive "r")
  (save-excursion
    (if (use-region-p)
	(let ((save-mark (mark)) (save-point (point)))
	  (progn
	    (goto-char end)
	    (if (eq (point) (line-beginning-position))
		(previous-line 1))
	    (goto-char (line-beginning-position))
	    (while (>= (point) start)
	      (goto-char (line-beginning-position))
	      (insert "\t")
	      (previous-line 1)
	      )
	    (goto-char save-mark)
	    (set-mark (point))
	    (goto-char save-point)
	    (setq deactivate-mark nil)
	  ))
    ))
)
(global-set-key (kbd "C->") 'my-indent)

(defun my-unindent (start end)
  (interactive "r")
  (save-excursion
    (if (use-region-p)
	(let ((save-mark (mark)) (save-point (point)))
	  (progn
	    (goto-char end)
	    (if (eq (point) (line-beginning-position))
		(previous-line 1))
	    (goto-char (line-beginning-position))
	    (while (>= (point) start)
	      (goto-char (line-beginning-position))
	      (if (= (string-to-char "\t") (char-after (point))) (delete-char 1))
	      (previous-line 1)
	      )
	    (goto-char save-mark)
	    (set-mark (point))
	    (goto-char save-point)
	    (setq deactivate-mark nil)
	  ))
    ))
)
(global-set-key (kbd "C-<") 'my-unindent)

;; hooks
(defun my-get-tab-stop-list (width max)
  (number-sequence width max width)
  )

(add-hook 'sql-mode-hook
	  '(lambda()
	     (progn
	       (setq c-basic-offset 4)
	       (setq-default tab-width 4)
	       (setq tab-stop-list (my-get-tab-stop-list 4 200))
	       (local-set-key [(tab)] 'tab-to-tab-stop)
	       )))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook
	  '(lambda()
	     (setq tab-stop-list (my-get-tab-stop-list 4 200))
	     (local-set-key [(tab)] 'tab-to-tab-stop)
	     (setq indent-tabs-mode 1)
	     (setq c-basic-offset 4)
	     (setq tab-width 4))
	  )


;; mode line
(require 'powerline)
(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts

(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

;;
(add-to-list 'load-path "~/.emacs.d/plugins/helm-master")
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; haskell
(load-file "~/.emacs.d/plugins/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
