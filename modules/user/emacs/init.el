;;; init.el --- librephoenix's emacs config -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Emmet K <https://gitlab.com/librephoenix>
;; Maintainer: Emmet K <https://gitlab.com/librephoenix>
;; Source: https://github.com/librephoenix/nixos-config
;; Source: https://gitlab.com/librephoenix/nixos-config
;; Source: https://codeberg.org/librephoenix/nixos-config
;;
;;; Commentary:
;;
;; LibrePhoenix's Emacs config.
;;
;;; Code:

;; organize everything with use-package
(require 'use-package)
(load (expand-file-name "~/.config/emacs/sysvars.el"))

;; use-package-ception
(use-package use-package
  :custom
  (use-package-always-ensure nil)
  (usepackage-always-defer nil))

(use-package emacs
  :defer t
  :config
  ;; Org mode scratch buffers
  (setq initial-major-mode 'org-mode)

  ;; No startup screen
  (setq inhibit-startup-message t)

  ;; Truncate lines is annoying
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  
  ;; Transparent background
  (set-frame-parameter nil 'alpha-background systemOpacity)
  (add-to-list 'default-frame-alist `(alpha-background . ,systemOpacity))
  ;;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  
  ;; I want declarative config, no custom
  (setq custom-file "/dev/null")

  ;; Auto save errors are annoying
  (setq auto-save-default nil)
  
  ;; Disable the menu bar
  (menu-bar-mode -1)
  
  ;; Disable visible scrollbar
  (scroll-bar-mode -1)
  
  ;; Disable the toolbar
  (tool-bar-mode -1)
  
  ;; Disable tooltips
  (tooltip-mode -1)
  
  ;; Breathing room
  (set-fringe-mode 10)
  
  ;; No blinking
  (blink-cursor-mode 0)
  
  ;; Highlight current line
  (global-hl-line-mode)
  
  ;; Bigger text
  (set-face-attribute 'default nil :height 150)

  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 20)
    (left-divider-width . 20)
     (internal-border-width . 20)))
  (set-face-background 'fringe (face-attribute 'default :background))
  
  ;; Fira and glyphs
  (set-frame-font "FiraCode Nerd Font")
  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 ;;(42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
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
                 )))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

    (dashboard-setup-startup-hook)

    ;; Garbage collection threshold
    (setq gc-cons-threshold 120000)
    (add-hook 'focus-out-hook 'garbage-collect)
    
    ;; Auto revert
    (global-auto-revert-mode 1)
    (setq auto-revert-use-notify t
         revert-without-query t)
    
    ;; camelCase and PascalCase
    (global-subword-mode 1)
    
    ;; ripgrep as grep
    (setq grep-command "rg -nS --no-heading "
          grep-use-null-device nil)
    
    ;; "y" or "n" instead of "yes" or "no"
    (setq use-short-answers t)
    
    ;; Enable indentation+completion using TAB
    (setq tab-always-indent 'complete)
    
    ;; Make ESC quit prompts
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    
    ;; Line numbers
    (setq display-line-numbers-type t
          line-move-visual t)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    
    ;; Fix stupid backup confirmations
    (setq backup-directory-alist '("." "~/.emacs.d/cache/backups"))
    (setq tramp-auto-save-directory "/dev/null"))

;; Packages

(use-package line-wrapping-and-numbers
  :load-path "lib/"
  :after (org git-timemachine nix-mode treemacs))

(use-package ultra-scroll
  :init
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 101
        scroll-preserve-screen-position nil
        redisplay-skip-fontification-on-input t)
  (pixel-scroll-precision-mode 1)
  :config
  (ultra-scroll-mode 1))

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package svelte-mode)
(use-package typescript-mode)
(use-package sass-mode)

;; Magit
(use-package magit
  :commands (magit magit-status)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (define-key magit-mode-map (kbd "SPC") nil)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :custom (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package git-timemachine)

(use-package magit-todos
  :after (magit)
  :config
  (setq magit-todos-keywords-list '("TODO" "FIXME" "HACK" "REVIEW" "DEPRECATED" "BUG"))
  (setq magit-todos-keyword-suffix "\\(?:[([][^])]+[])]\\)?.")
  (magit-todos-mode 1))

;; Projectile
(use-package projectile
  :custom
  (projectile-switch-project-action 'magit-status)
  :init
  (projectile-mode +1)
  ;; Project keybinds
  (defun projectile-switch-project-commander ()
    (interactive)
    (projectile-switch-project t))
  (def-projectile-commander-method ?g "Open magit status" (magit-status))
  (def-projectile-commander-method ?/ "Grep through project" (projectile-grep)))

;; Being able to undo is nice...
(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config
  (global-undo-fu-session-mode))

(use-package dired
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  :after (dired)
  :config
  (setq dired-omit-files (rx (seq bol ".")))
  (setq dired-show-dotfiles nil)
  (defun apply-dired-omit ()
    (if (not dired-show-dotfiles)
	(dired-omit-mode 1)))
  (add-hook 'dired-mode-hook 'apply-dired-omit))

;; Muahahahahaha..
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-fu)
  :config
  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'motion (kbd "SPC"))
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-redo-function 'undo-fu-only-redo)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (evil-mode 1))

(use-package default-text-scale
  :after (evil)
  :config
  ;; Zoom in/out keybinds
  (evil-define-key nil 'global (kbd "C--") 'default-text-scale-decrease)
  (evil-define-key nil 'global (kbd "C-+") 'default-text-scale-reset)
  (evil-define-key nil 'global (kbd "C-=") 'default-text-scale-increase))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-want-keybinding t)
  :config
  (evil-collection-init)

  ;; Visual mode keybinds
  (evil-define-key 'motion 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'motion 'global (kbd "k") 'evil-previous-visual-line)

  (defun scratch-plus-toggle ()
    (interactive)
    (if (projectile-project-p)
      (progn
        (select-window (scratch-plus-switch-project nil)))
      (progn
        (select-window (scratch-plus-switch nil)))))

  (defun scratch-plus-only ()
    (interactive)
    (if (projectile-project-p)
      (progn
        (select-window (scratch-plus-switch-project nil)))
      (progn
        (select-window (scratch-plus-switch nil))))
    (delete-other-windows))

  (defun scratch-plus-main-toggle ()
    (interactive)
    (select-window (scratch-plus-switch nil)))


  (defun scratch-plus-main-only ()
    (interactive)
    (select-window (scratch-plus-switch nil))
    (delete-other-windows))

  ;; File and buffer  keybinds
  (evil-define-key 'motion 'global (kbd "<leader>x") 'scratch-plus-toggle)
  (evil-define-key 'motion 'global (kbd "<leader>X") 'scratch-plus-main-toggle)
  (evil-define-key 'motion 'global (kbd "<leader>z") 'scratch-plus-only)
  (evil-define-key 'motion 'global (kbd "<leader>Z") 'scratch-plus-main-only)
  (evil-define-key 'motion 'global (kbd "<leader>.") 'find-file)
  (evil-define-key 'motion 'global (kbd "<leader>bi") 'ibuffer)
  (evil-define-key 'motion 'global (kbd "<leader>bd") 'evil-delete-buffer)
  (evil-define-key 'motion 'global (kbd "<leader>bn") 'next-buffer)
  (evil-define-key 'motion 'global (kbd "<leader>bp") 'previous-buffer)

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
  (evil-define-key 'motion 'global (kbd "<leader>fd") 'delete-file-and-buffer)
  (evil-define-key 'motion 'global (kbd "<leader>fr") 'rename-visited-file)
  (evil-define-key 'motion 'global (kbd "<leader>od") 'dired-jump)
  (defun toggle-dired-omit-mode ()
    "Toggle dired-omit-mode."
    (interactive)
    (if dired-omit-mode
      (progn (dired-omit-mode 0) (setq dired-show-dotfiles t))
      (progn (dired-omit-mode 1) (setq dired-show-dotfiles nil))))
  (evil-define-key 'normal dired-mode-map (kbd "H") 'toggle-dired-omit-mode)

  (evil-define-key 'motion 'global (kbd "<leader>pp") 'projectile-switch-project-commander)
  (evil-define-key 'motion 'global (kbd "<leader>pg") 'projectile-switch-project)
  (evil-define-key 'motion 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'motion 'global (kbd "<leader>pa") 'projectile-add-known-project)
  (evil-define-key 'motion 'global (kbd "<leader>pr") 'projectile-remove-known-project)
  (evil-define-key 'motion 'global (kbd "<leader>/") 'projectile-grep)
  (evil-define-key 'motion 'global (kbd "<leader>gg") 'magit-status)
  (evil-define-key 'motion 'global (kbd "<leader>gt") 'git-timemachine-toggle)

  ;; Describe keybinds
  (evil-define-key 'motion 'global (kbd "<leader>hv") 'describe-variable)
  (evil-define-key 'motion 'global (kbd "<leader>hf") 'describe-function)
  (evil-define-key 'motion 'global (kbd "<leader>hk") 'describe-key)
  (evil-define-key 'motion 'global (kbd "<leader>hF") 'describe-face)

  ;; Window keybinds
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

  (evil-define-key 'motion scratch-plus-minor-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal scratch-plus-minor-mode-map (kbd "q") 'quit-window)

  (evil-define-key 'insert org-mode-map (kbd "<C-return>") '+org/insert-item-below)
  (evil-define-key 'insert org-mode-map (kbd "<C-S-return>") '+org/insert-item-above)
  (evil-define-key 'motion org-mode-map (kbd "<C-return>") '+org/insert-item-below)
  (evil-define-key 'motion org-mode-map (kbd "<C-S-return>") '+org/insert-item-above)
  (evil-define-key 'insert org-mode-map (kbd "<tab>") '+org-indent-maybe-h)
  (evil-define-key 'insert org-mode-map (kbd "<backtab>") '+org-reverse-indent-maybe-h)
  (evil-define-key 'motion org-mode-map (kbd "<leader>mll") 'org-insert-link)
  (evil-define-key 'motion org-mode-map (kbd "<leader>mt") 'org-todo)
  
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-l") 'evil-window-right))

(use-package sudo-edit
  :after (evil)
  :custom
  (sudo-edit-local-method "doas")
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-save-behavior "ask")
  :config
  (sudo-edit-indicator-mode)
  (evil-define-key 'normal 'global (kbd "<leader>fU") 'sudo-edit)
  (evil-define-key 'normal 'global (kbd "<leader>fu") 'sudo-edit-find-file))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package treemacs
  :after (evil)
  :config
  (defun treemacs-display-current-project-exclusively-silently ()
    (interactive)
    "Display current project exclusively in treemacs without switching to treemacs buffer."
    (let ((buffer (current-buffer)))
      (treemacs-add-and-display-current-project-exclusively)
      (switch-to-buffer buffer)))
  (evil-define-key 'normal 'global (kbd "<leader>ot") 'treemacs-add-and-display-current-project-exclusively))

(use-package treemacs-evil
  :after (treemacs))

(use-package nix-mode)
(use-package gdscript-mode)

(use-package lsp-mode
  :config
  (setq lsp-completion-enable t)
  (setq lsp-keymap-prefix "SPC l")
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
  (setq lsp-completion-provider :none)
  (setq major-mode-remap-alist
   '((gdscript-mode . gdscript-ts-mode)))
  :hook
  (lsp-mode . evil-normalize-keymaps)
  (nix-mode . lsp-deferred)
  (gdscript-mode . lsp-deferred)
  (gdscript-ts-mode . lsp-deferred))

(use-package lsp-nix
  :after (lsp-mode))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after (evil)
  :custom
  (lsp-treemacs-theme "nerd-icons-ext")
  :config
  (evil-define-key 'normal 'global (kbd "<leader>os") 'lsp-treemacs-symbols))

(use-package treesit
  :config
  (treesit-major-mode-setup))

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;; command-log-mode
(use-package command-log-mode)

;; Enable corfu
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
  (corfu-auto-delay 0.15)  ;; wait half a second though
  (corfu-auto-prefix 2) ;; also only for words 2 or more
  (corfu-min-width 120)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless flex hotfuzz)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

  :init
  (global-corfu-mode 1))

;; Enable vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; I am a nerd
(use-package nerd-icons)

(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package lsp-treemacs-nerd-icons
  :after (lsp-treemacs)
  :init (with-eval-after-load 'lsp-treemacs
          (require 'lsp-treemacs-nerd-icons)))

(use-package nerd-icons-dired
  :after (nerd-icons dired)
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after (nerd-icons)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Theme and modeline
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        custom-theme-directory "~/.config/emacs/themes")
  (load-theme 'doom-stylix t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Dashboard
(use-package dashboard
  :after (nerd-icons)
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
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Window management with shackle
;; https://github.com/wasamasa/shackle
(use-package shackle
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.4) ; default 0.5

    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '((compilation-mode              :select nil                                               )
            ("*undo-tree*"                                                    :size 0.25 :align right)
            ("*eshell*"                    :select t                          :other t               )
            ("*Shell Command Output*"      :select nil                                               )
            ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
            (occur-mode                    :select nil                                   :align t    )
            ("*Help*"                      :select t   :inhibit-window-quit nil :size 0.3  :align below  )
            ("*Completions*"                                                  :size 0.3  :align t    )
            ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
            ("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
            ("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
            ("\\`\\*helm.*?\\*\\'"   :regexp t                                    :size 0.3  :align t    )
            ("*Calendar*"                  :select t                          :size 0.3  :align below)
            ("*info*"                      :select t   :inhibit-window-quit t                         :popup t)
            ("*Org todo*"                      :select t   :inhibit-window-quit t  :same t                        :popup t)
            (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
            (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
            ))
    (shackle-mode 1))
    (add-to-list 'display-buffer-alist '("\\*Org todo\\*"
                                         (display-buffer-at-bottom)
                                         (side . bottom)
                                         (slot . 4)
                                         (window-height . shrink-window-if-larger-than-buffer)
                                         (dedicated . t))))

;; Completion
(use-package hotfuzz)
(use-package orderless)
(setq completion-styles '(orderless flex hotfuzz))

(use-package org
  :config
  ;; Better cycling
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/autoload/org.el
  (defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point, and no deeper.
  `org-cycle's standard behavior is to cycle between three levels: collapsed,
  subtree and whole document. This is slow, especially in larger org buffer. Most
  of the time I just want to peek into the current subtree -- at most, expand
  *only* the current subtree.
  
  All my (performant) foldings needs are met between this and `org-show-subtree'
  (on zO for evil users), and `org-cycle' on shift-TAB if I need it."
    (interactive "P")
    (unless (or (eq this-command 'org-shifttab)
                (and (bound-and-true-p org-cdlatex-mode)
                     (or (org-inside-LaTeX-fragment-p)
                         (org-inside-latex-macro-p))))
      (save-excursion
        (org-beginning-of-line)
        (let (invisible-p)
          (when (and (org-at-heading-p)
                     (or org-cycle-open-archived-trees
                         (not (member org-archive-tag (org-get-tags))))
                     (or (not arg)
                         (setq invisible-p
                               (memq (get-char-property (line-end-position)
                                                        'invisible)
                                     '(outline org-fold-outline)))))
            (unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t)))))
  (defalias #'+org/toggle-fold #'+org-cycle-only-current-subtree-h)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-tab-first-hook
            ;; Only fold the current tree, rather than recursively
            #'+org-cycle-only-current-subtree-h)
  
  (setq org-return-follows-link t)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "NO(n)")))
  (setq org-use-fast-todo-selection 'prefix)
  (setq org-M-RET-may-split-line nil
      ;; insert new headings after current subtree rather than inside it
      org-insert-heading-respect-content t)

  (defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert "�"))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

  ;;; Commands

  ;;;###autoload
  (defun +org/return ()
    "Call `org-return' then indent (if `electric-indent-mode' is on)."
    (interactive)
    (org-return electric-indent-mode))

  ;;;###autoload
  (defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.

  If on a:
  - checkbox list item or todo heading: toggle it.
  - citation: follow it
  - headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
    subtree; update statistics cookies/checkboxes and ToCs.
  - clock: update its time.
  - footnote reference: jump to the footnote's definition
  - footnote definition: jump to the first reference of this footnote
  - timestamp: open an agenda view for the time-stamp date/range at point.
  - table-row or a TBLFM: recalculate the table's formulas
  - table-cell: clear it and go into insert mode. If this is a formula cell,
    recaluclate it instead.
  - babel-call: execute the source block
  - statistics-cookie: update it.
  - src block: execute it
  - latex fragment: toggle it.
  - link: follow it
  - otherwise, refresh all inline images in current tree."
    (interactive "P")
    (if (button-at (point))
        (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
                type (org-element-type context)))
        (pcase type
          ((or `citation `citation-reference)
           (org-cite-follow context arg))

          (`headline
           (cond ((memq (bound-and-true-p org-goto-map)
                        (current-active-maps))
                  (org-goto-ret))
                 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
                         (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
                         (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
                 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
                 (goto-char (org-element-property :contents-begin context))
                 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage)))
             (if (or (equal (org-element-property :type lineage) "img")
                     (and path (image-type-from-file-name path)))
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))
               (org-open-at-point arg))))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (org-toggle-checkbox))

          (`paragraph
           (+org--toggle-inline-images-in-subtree))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil  t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context))))))))

  ;;;###autoload
  (defun +org/shift-return (&optional arg)
    "Insert a literal newline, or dwim in tables.
  Executes `org-table-copy-down' if in table."
    (interactive "p")
    (if (org-at-table-p)
        (org-table-copy-down arg)
      (org-return nil arg)))

  ;;;###autoload
  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  ;;;###autoload
  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))

(defun +org-indent-maybe-h ()
  "Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
  (interactive)
  (cond ((not (and (bound-and-true-p evil-local-mode)
                   (evil-insert-state-p)))
         nil)
        ((and (bound-and-true-p org-cdlatex-mode)
              (or (org-inside-LaTeX-fragment-p)
                  (org-inside-latex-macro-p)))
         nil)
        ((org-at-item-p)
         (if (eq this-command 'org-shifttab)
             (org-outdent-item-tree)
           (org-indent-item-tree))
         t)
        ((org-at-heading-p)
         (ignore-errors
           (if (eq this-command 'org-shifttab)
               (org-promote)
             (org-demote)))
         t)
        ((org-in-src-block-p t)
         (save-window-excursion
           (org-babel-do-in-edit-buffer
            (call-interactively #'indent-for-tab-command)))
         t)
        ((and (save-excursion
                (skip-chars-backward " \t")
                (bolp))
              (org-in-subtree-not-table-p))
         (call-interactively #'tab-to-tab-stop)
         t)))

(defun +org-reverse-indent-maybe-h ()
  "Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
  (interactive)
  (cond ((not (and (bound-and-true-p evil-local-mode)
                   (evil-insert-state-p)))
         nil)
        ((and (bound-and-true-p org-cdlatex-mode)
              (or (org-inside-LaTeX-fragment-p)
                  (org-inside-latex-macro-p)))
         nil)
        ((org-at-item-p)
         (if (eq this-command 'org-shifttab)
             (org-outdent-item-tree)
           (org-outdent-item-tree))
         t)
        ((org-at-heading-p)
         (ignore-errors
           (if (eq this-command 'org-shifttab)
               (org-promote)
             (org-promote)))
         t)
        ((org-in-src-block-p t)
         (save-window-excursion
           (org-babel-do-in-edit-buffer
            (call-interactively #'indent-for-tab-command)))
         t)
        ((and (save-excursion
                (skip-chars-forward " \t")
                (bolp))
              (org-in-subtree-not-table-p))
         (call-interactively #'tab-to-tab-stop)
         t))))

(use-package org-roam
  :after (org)
  :config
  (setq org-roam-directory (file-truename "~/Notes"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode -1)
  (setq org-roam-capture-templates '(("d" "default" plain "%?" :unnarrowed t :target (file+head
 				    "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}"))))
  (evil-define-key 'motion 'global (kbd "<leader>N.") 'org-node-find)
  (evil-define-key 'motion 'global (kbd "<leader>Nr") 'org-node-refile)
  (evil-define-key 'motion 'global (kbd "<leader>Nb") 'org-roam-buffer-toggle)
  (evil-define-key 'motion 'global (kbd "<leader>nrdd") 'org-roam-dailies-goto-date)
  (evil-define-key 'motion 'global (kbd "<leader>nrdt") 'org-roam-dailies-goto-today)
  (evil-define-key 'motion 'global (kbd "<leader>nrdn") 'org-roam-dailies-goto-next-note)
  (evil-define-key 'motion 'global (kbd "<leader>nrdp") 'org-roam-dailies-goto-previous-note))

(use-package org-node
  :after (org org-roam)
  :config
  (setq org-mem-do-sync-with-org-id t)
  (org-mem-updater-mode)
  (setq org-node-extra-id-dirs '("~/Notes/"))
  (setq org-id-locations-file "~/Notes/.org-id-locations")
  (setq org-node-extra-id-dirs-exclude '("~/Notes/daily/"))
  (org-node-cache-mode)
  (org-node-complete-at-point-mode)
  (setq org-node-datestamp-format "")
  (setq org-node-slug-fn 'org-node-slugify-for-web)
  (setq org-roam-completion-everywhere nil)
  (setq org-node-filter-fn
      (lambda (node)
        (not (string-search "/daily/" (org-node-get-file node)))))
  (setq org-node-renames-allowed-dirs '("~/Notes"))
  (add-hook 'after-save-hook 'org-node-rename-file-by-title)
  (evil-define-key 'motion 'global (kbd "<leader>Ni") 'org-node-insert-link)
  (evil-define-key 'motion 'global (kbd "<leader>Nt") 'org-node-add-tags)
  (evil-define-key 'motion 'global (kbd "<leader>NR") 'org-node-rewrite-links-ask))

(use-package org-node-fakeroam
  :after (org org-node org-roam)
  :defer t
  :config
  (setq org-node-creation-fn #'org-node-fakeroam-new-via-roam-capture)
  (setq org-node-slug-fn #'org-node-fakeroam-slugify-via-roam)
  (setq org-node-datestamp-format "%Y%m%d%H%M%S-")
  (setq org-roam-db-update-on-save nil)
  (setq org-roam-link-auto-replace nil)
  (org-node-fakeroam-fast-render-mode)
  (setq org-node-fakeroam-fast-render-persist t)
  (org-node-fakeroam-redisplay-mode)
  (org-node-fakeroam-jit-backlinks-mode)
  (org-node-fakeroam-db-feed-mode))

(use-package wgrep
  :after (org-node))

(use-package org-modern
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "...")
  (org-modern-star 'replace)
  :config
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  :init
  (global-org-modern-mode))

;; Markdown setup for quartz
(use-package markdown-mode
  :custom
  (markdown-enable-wiki-links t)
  (markdown-wiki-link-alias-first nil))

;; Olivetti
(use-package olivetti
  :custom
  (olivetti-style 'fancy)
  (olivetti-margin-width 100)
  :config
  (setq-default olivetti-body-width 100)
  (add-hook 'org-mode-hook 'olivetti-mode)
  (add-hook 'markdown-mode-hook 'olivetti-mode))

(evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    " " 'nil)

(use-package vterm)

(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.4)))
  (defun vterm-toggle-cd-force ()
    (interactive)
    (vterm-toggle-cd-show)
    (vterm-toggle-insert-cd)
  )
  (evil-define-key 'motion 'global (kbd "M-z") 'vterm-toggle-cd-force)
  (evil-define-key 'insert 'global (kbd "M-z") 'vterm-toggle-cd-force)
  (evil-define-key 'motion vterm-mode-map (kbd "M-z") 'vterm-toggle-hide)
  (evil-define-key 'insert vterm-mode-map (kbd "M-z") 'vterm-toggle-hide)
  )

(use-package scratch-plus
  :custom
  (initial-major-mode 'org-mode)
  (scratch-plus-save-directory (file-truename "~/.config/emacs/scratch"))
  (scratch-plus-project-subdir ".scratch")
  (scratch-plus-restore-type 'demand)
  (scratch-plus-force-restore t)
  (scratch-plus-initial-message "Scratchpad")
  :init
  (add-hook 'after-init-hook #'scratch-plus-mode))

(use-package rainbow-mode)

(provide 'init)
;;; init.el ends here
