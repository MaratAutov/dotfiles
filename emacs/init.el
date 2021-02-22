;;
;;
;;
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(progn
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell t)
  ;; paren mode
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  )

;;
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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish)
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)  ; Respect the theme colors
  (setq sml/mode-width 'right
      sml/name-width 60)

  (setq-default mode-line-format
  `("%e"
      ,
      mode-line-front-space
      evil-mode-line-tag
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      sml/pos-id-separator
      (vc-mode vc-mode)
      " "
      ;mode-line-position
      sml/pre-modes-separator
      mode-line-modes
      " "
      mode-line-misc-info))

  (setq rm-excluded-modes
    (mapconcat
      'identity
      ; These names must start with a space!
      '(" GitGutter" " MRev" " company"
      " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
      " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
      "\\|")))


;; complition
(use-package icomplete-vertical
  :disabled
  :straight t
  :demand t
  :after orderless
  :bind (:map icomplete-minibuffer-map
              ("C-j"   . icomplete-forward-completions)
              ("C-k"   . icomplete-backward-completions)
              ("C-f"   . icomplete-force-complete-and-exit)
              ("C-M-f" . icomplete-force-complete)
              ("TAB"   . icomplete-force-complete)
              ("RET"   . icomplete-force-complete-and-exit)
              ("M-h"   . backward-kill-word))
  :custom
  (completion-styles '(orderless partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycling t)
  (completion-cycle-threshold 5)
  (icomplete-compute-delay 0.1)
  (icomplete-vertical-prospects-height 7)
  :custom-face
  (icomplete-first-match ((t (:foreground "LightGreen" :weight bold))))
  :config
  ;; Deal with a weird issue where the minibuffer disappears
  ;; in some cases when resize-mini-windows isn't nil
  (setq resize-mini-windows nil)

  ;; Enable icomplete and vertical completions
  (icomplete-mode)
  (icomplete-vertical-mode))


;; themes
;(use-package almost-mono-themes
;  :config
;  (load-theme 'almost-mono-black t)
  ;;(load-theme 'almost-mono-white t)
;  )

(use-package doom-themes :defer t)
(progn
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))

;; helm
(use-package helm
  :straight t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x C-r") #'helm-recentf)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
  )

;; useful things
(progn
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-set-key (kbd "C-'") 'toggle-truncate-lines)
  (global-set-key (kbd "C-c l") 'goto-line)
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
 )

(setq case-fold-search t)

;; Clipboard settings
(progn
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t)
  )

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

;;
(setq ns-pop-up-frames          nil
      ad-redefinition-action    'accept)

;;
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; org mode
(setq org-startup-indented t)
 
;;logging stuff
(setq org-log-done (quote time))
; todo keywords

(setq org-todo-keywords
      (quote ((sequence "TODO" "NEXT" "WAITING" "STARTED" "READY" "|" "DONE" "CANCELLED" "EXPIRED" "REJECTED" "APPROVED(!)")
              (sequence "OPEN" "|" "CLOSED(!)"))))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold))
	("OPEN" . (:foreground "coral" :weight bold))
        ("CANCELLED" . (:foreground "LimeGreen" :weight bold))
        ("READY" . (:foreground "Orchid" :weight bold))
	("REJECTED" . (:foreground "red" :weight bold))
	("DONE" . (:foreground "green" :weight bold))
	("APPROVED" . (:foreground "LigthGreen" :weight bold))
        ))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME" . ?h)
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o)
        ("DEV" . ?d)
        ("WWW" . ?w)
        (:endgroup . nil)
        (:startgroup . nil)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        (:endgroup . nil)
        ("UCANCODE" . ?c)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("BONUS" . ?b)
        ("noexport" . ?x)
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("WWW" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("KEY" . (:foreground "Red" :weight bold))
        ("EASY" . (:foreground "OrangeRed" :weight bold))
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
        ("HARD" . (:foreground "OrangeRed" :weight bold))
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("UCANCODE" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))
        )
      )

;;---------------------------------------------------------
;; eshell
;;---------------------------------------------------------
(use-package eshell
  :defer t
  :config
  (progn
    ;; Fix annoying scrolling
    (add-hook 'eshell-output-filter-functions
              'eshell-postoutput-scroll-to-bottom)

    ;; Put a space above eshell prompt
    (setq eshell-prompt-function (lambda nil (concat "\n" (eshell/pwd) " $ ")))
    ;;
    (xterm-mouse-mode 1)
))

(use-package ahk-mode
  :commands ahk-mode
  :mode ("\\.ahk\\'" . ahk-mode))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|md\\)$" . markdown-mode))

(setq display-line-numbers 'relative
      display-line-numbers-current-absolute t)

;;
;; keys
;;
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defmacro bind (key fn)
  "shortcut for my-keys binding"
  `(define-key my-keys-minor-mode-map
     (kbd ,key)
     ;; handle unquoted function names and lambdas
     ,(if (listp fn)
          fn 
        `',fn)))

;;-----------------------------------------------------------------------------
;; F9: Emacs programs
;;-----------------------------------------------------------------------------
(bind "<f9> e" eshell)
(bind "<f9> f" rgrep)
(bind "<f9> h" (lambda () (interactive) (dired "~")))
(bind "<f9> c" calendar)
(bind "<f9> r" org-remember)
;; (bind "<f9> g" gnus)
(bind "<f9> n" notmuch)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (doom-themes inkpod-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
