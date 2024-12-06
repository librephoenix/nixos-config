;;; init.el --- librephoenix's emacs config -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Emmet K <https://gitlab.com/librephoenix>
;; Maintainer: Emmet K <https://gitlab.com/librephoenix>
;; Source: https://github.com/doomemacs/themes
;;
;;; Commentary:
;;
;; LibrePhoenix's Emacs config.
;;
;;; Code:

; Text
(setq inhibit-startup-message t)

; I want declarative config, no custom
(setq custom-file "/dev/null")

; Some sane settings
(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar
(tooltip-mode -1)               ; Disable tooltips
(set-fringe-mode 10)            ; Give some breathing room
(blink-cursor-mode 0)           ; No blinking
(global-hl-line-mode)           ; Where am I?

; (Somewhat) precision scrolling
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
(setq touch-screen-precision-scroll t)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101)
(global-visual-line-mode 1)     ; Visual lines make more sense

;; Garbage collection to speed things up
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 120000)))
(add-hook 'focus-out-hook 'garbage-collect)

;; auto revert buffers
(setq global-auto-revert-mode t)
(setq auto-revert-use-notify t)

;; For camelCase
(global-subword-mode 1)

;; ripgrep as grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;; I prefer visual lines
(setq display-line-numbers-type 'visual
      line-move-visual t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(menu-bar-mode -1)            ; Disable the menu bar

(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq scroll-preserve-screen-position nil)
(setq redisplay-skip-fontification-on-input t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

;; Enable vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Completion
(use-package hotfuzz)
(use-package orderless)
(setq completion-styles '(orderless flex hotfuzz))

;; Magit
(use-package magit)

;; Projectile
(use-package projectile
  :init
  (projectile-mode +1))

;; Enable vim
(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'motion (kbd "SPC"))
  (setq evil-respect-visual-line-mode t)
  (evil-mode 1))

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init))

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(evil-define-key 'motion 'global (kbd "j") 'evil-next-visual-line)
(evil-define-key 'motion 'global (kbd "k") 'evil-previous-visual-line)
(evil-define-key 'motion 'global (kbd "<leader>.") 'find-file)
(evil-define-key 'motion 'global (kbd "<leader>bi") 'ibuffer)
(evil-define-key 'motion 'global (kbd "<leader>bd") 'delete-buffer)
(evil-define-key 'motion 'global (kbd "<leader>bn") 'next-buffer)
(evil-define-key 'motion 'global (kbd "<leader>bp") 'previous-buffer)
(evil-define-key 'motion 'global (kbd "<leader>pp") 'projectile-switch-project)
(evil-define-key 'motion 'global (kbd "<leader>pf") 'projectile-find-file)
(evil-define-key 'motion 'global (kbd "<leader>pa") 'projectile-add-known-project)
(evil-define-key 'motion 'global (kbd "<leader>gg") 'magit-status)
(evil-define-key 'motion 'global (kbd "<leader>hv") 'describe-variable)
(evil-define-key 'motion 'global (kbd "<leader>hf") 'describe-function)
(evil-define-key 'motion 'global (kbd "<leader>hk") 'describe-key)
(evil-define-key 'motion 'global (kbd "<leader>hF") 'describe-face)
(evil-define-key 'motion 'global (kbd "<leader>ws") 'evil-window-split)
(evil-define-key 'motion 'global (kbd "<leader>wv") 'evil-window-vsplit)
(defun evil-window-split-follow ()
  (interactive)
  (let ((evil-split-window-below t))
  (evil-window-split)))
(defun evil-window-vsplit-follow ()
  (interactive)
  (let ((evil-vsplit-window-right t))
  (evil-window-vsplit)))
(evil-define-key 'motion 'global (kbd "<leader>wS") 'evil-window-split-follow)
(evil-define-key 'motion 'global (kbd "<leader>wV") 'evil-window-vsplit-follow)
(evil-define-key 'motion 'global (kbd "<leader>wd") 'evil-window-delete)
(evil-define-key 'motion 'global (kbd "<leader>wj") 'evil-window-down)
(evil-define-key 'motion 'global (kbd "<leader>wk") 'evil-window-up)
(evil-define-key 'motion 'global (kbd "<leader>wh") 'evil-window-left)
(evil-define-key 'motion 'global (kbd "<leader>wl") 'evil-window-right)
(evil-define-key 'motion 'global (kbd "<leader>fd") 'delete-file-and-buffer)
(evil-define-key 'motion 'org-mode-map (kbd "<leader>mll") 'org-insert-link)
(define-key magit-mode-map (kbd "SPC") nil)

(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-l") 'evil-window-right)

(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(setq magit-bury-buffer-function 'magit-restore-window-configuration)
(add-hook 'git-commit-mode-hook 'evil-insert-state)

(use-package nerd-icons
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (setq custom-theme-directory "~/.config/emacs/themes")
  (load-theme 'doom-stylix t)
  )

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Nix Emacs")
  (setq dashboard-startup-banner 2)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '())
  (setq dashboard-center-content t)
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (setq dashboard-footer-messages '("Here to do customizing, or actual work?"
                                  "M-x insert-inspiring-message"
                                  "My software never has bugs. It just develops random features."
                                  "Dad, what are clouds made of? Linux servers, mostly."
                                  "There is no place like ~"
                                  "~ sweet ~"
                                  "sudo chown -R us ./allyourbase"
                                  "I’ll tell you a DNS joke but it could take 24 hours for everyone to get it."
                                  "I'd tell you a UDP joke, but you might not get it."
                                  "I'll tell you a TCP joke. Do you want to hear it?"))
  (setq dashboard-footer-icon
    (nerd-icons-codicon "nf-cod-vm"
      :height 1.0
      :v-adjust 0
      :face 'font-lock-keyword-face))
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook)
  (set-face-attribute 'default nil :height 150) ; Bigger text
  (when (window-system)
    (set-frame-font "FiraCode Nerd Font"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

; Fix stupid backup confirmations
(setq backup-directory-alist '("." "~/.emacs.d/cache/backups"))
(setq tramp-auto-save-directory "/dev/null")

(require 'sudo-edit)
(setq sudo-edit-local-method "doas")
(setq auth-source-save-behavior nil)

(evil-define-key 'normal 'global (kbd "<leader>fU") 'sudo-edit)
(evil-define-key 'normal 'global (kbd "<leader>fu") 'sudo-edit-find-file)

(use-package doom-themes
  :ensure t
  :init )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
