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

;; use-package-ception
(use-package use-package
  :demand t
  :custom
  (use-package-always-ensure nil)
  (usepackage-always-defer t))

(use-package emacs
  :config
  ;; No startup screen
  (setq inhibit-startup-message t)
  
  ;; Transparent background
  (set-frame-parameter nil 'alpha-background 85)
  (add-to-list 'default-frame-alist '(alpha-background . 85))
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  
  ;; I want declarative config, no custom
  (setq custom-file "/dev/null")
  
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
  :load-path "./lib"
  :after (org markdown git-timemachine))

(use-package ultra-scroll
  :init
  ;; Mouse & Smooth Scroll
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 101
        scroll-preserve-screen-position nil
        redisplay-skip-fontification-on-input t)
  (pixel-scroll-precision-mode 1)
  :config
  (ultra-scroll-mode 1))

;; Magit
(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package git-timemachine)

(use-package magit-file-icons
  :after (magit nerd-icons)
  :init
  (magit-file-icons-mode 1)
  :custom
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))

(use-package magit-todos
  :after (magit)
  :config
  (setq magit-todos-keywords-list '("TODO" "FIXME" "HACK" "REVIEW" "DEPRECATED" "BUG"))
  (setq magit-todos-keyword-suffix "\\(?:[([][^])]+[])]\\)?.")
  (magit-todos-mode 1))

;; Projectile
(use-package projectile
  :init
  (projectile-mode +1))

;; Being able to undo is nice...
(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config
  (global-undo-fu-session-mode))

;; Muahahahahaha..
(use-package evil
  :after (undo-fu undo-fu-session)
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

(use-package dired
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    " " 'nil))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-want-keybinding nil)
  (evil-collection-key-blacklist (append (list (kbd "SPC")) evil-collection-key-blacklist))
  :config
  (evil-collection-init)

  ;; Visual mode keybinds
  (evil-define-key 'motion 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'motion 'global (kbd "k") 'evil-previous-visual-line)

  ;; File and buffer  keybinds
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

  ;; Project keybinds
  (evil-define-key 'motion 'global (kbd "<leader>pp") 'projectile-switch-project)
  (evil-define-key 'motion 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'motion 'global (kbd "<leader>pa") 'projectile-add-known-project)
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

  (evil-define-key 'insert org-mode-map (kbd "<tab>") 'org-demote-subtree)
  (evil-define-key 'insert org-mode-map (kbd "<backtab>") 'org-promote-subtree)
  (evil-define-key 'motion org-mode-map (kbd "<leader>mll") 'org-insert-link)
  (define-key magit-mode-map (kbd "SPC") nil)
  
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
  :config
  (defun treemacs-display-current-project-exclusively-silently ()
    "Display current project exclusively in treemacs without switching to treemacs buffer."
    (let ((buffer (current-buffer)))
      (treemacs-add-and-display-current-project-exclusively)
      (switch-to-buffer buffer)))
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively-silently))

(use-package treemacs-evil
  :after (treemacs))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix (kbd "SPC l"))
  (setq lsp-completion-provider :none)
  :hook ((gdscript-mode . lsp-deferred)
         (gdscript-ts-mode . lsp-deferred))
  :commands lsp-deferred)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
  (corfu-auto t) ;; auto complete
  (corfu-auto-delay 0.5)  ;; wait half a second though
  (corfu-auto-prefix 3) ;; also only for words 3 or more
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
  (load-theme 'doom-stylix t)
  ;; Heading styles
  (set-face-attribute 'outline-1 nil :height 195 :foreground (nth 1 (nth 14 doom-themes--colors)))
  (set-face-attribute 'outline-2 nil :height 188 :foreground (nth 1 (nth 15 doom-themes--colors)))
  (set-face-attribute 'outline-3 nil :height 180 :foreground (nth 1 (nth 19 doom-themes--colors)))
  (set-face-attribute 'outline-4 nil :height 173 :foreground (nth 1 (nth 23 doom-themes--colors)))
  (set-face-attribute 'outline-5 nil :height 173 :foreground (nth 1 (nth 24 doom-themes--colors)))
  (set-face-attribute 'outline-6 nil :height 165 :foreground (nth 1 (nth 16 doom-themes--colors)))
  (set-face-attribute 'outline-7 nil :height 160 :foreground (nth 1 (nth 18 doom-themes--colors)))
  (set-face-attribute 'outline-8 nil :height 155 :foreground (nth 1 (nth 11 doom-themes--colors))))

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
            ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
            (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
            (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
            ))
    (shackle-mode 1)))

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
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)))

(use-package org-roam
  :after (org)
  :config
  (setq org-roam-directory (file-truename "~/Notes"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode -1)
  (setq org-roam-capture-templates '(("d" "default" plain "%?" :unnarrowed t :target (file+head
 				    "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}"))))
  (evil-define-key 'motion 'global (kbd "<leader>N.") 'org-roam-node-find)
  (evil-define-key 'motion 'global (kbd "<leader>Nr") 'org-roam-refile)
  (evil-define-key 'motion 'global (kbd "<leader>Nb") 'org-roam-buffer-toggle)
  (evil-define-key 'motion 'global (kbd "<leader>nrdd") 'org-roam-dailies-goto-date)
  (evil-define-key 'motion 'global (kbd "<leader>nrdt") 'org-roam-dailies-goto-today)
  (evil-define-key 'motion 'global (kbd "<leader>nrdn") 'org-roam-dailies-goto-next-note)
  (evil-define-key 'motion 'global (kbd "<leader>nrdp") 'org-roam-dailies-goto-previous-note)
  )

(use-package org-node
  :after (org org-roam)
  :config
  (setq org-node-extra-id-dirs '("~/Notes/"))
  (setq org-id-locations-file "~/Notes/.org-id-locations")
  (org-node-cache-mode)
  (org-node-complete-at-point-mode)
  (setq org-roam-completion-everywhere nil)
  (evil-define-key 'motion 'global (kbd "<leader>Ni") 'org-node-insert-link)
  (evil-define-key 'motion 'global (kbd "<leader>NR") 'org-node-rewrite-links-ask)
  )

(use-package org-node-fakeroam
  :after (org org-node org-roam)
  :defer
  :config
  (setq org-node-creation-fn #'org-node-fakeroam-new-via-roam-capture)
  (setq org-node-slug-fn #'org-node-fakeroam-slugify-via-roam)
  (setq org-node-datestamp-format "%Y%m%d%H%M%S-")
  (setq org-roam-db-update-on-save nil) ;; don't update DB on save, not needed
  (setq org-roam-link-auto-replace nil) ;; don't look for "roam:" links on save
  (org-node-fakeroam-fast-render-mode) ;; build the Roam buffer faster
  (setq org-node-fakeroam-fast-render-persist t)
  (org-node-fakeroam-redisplay-mode) ;; autorefresh the Roam buffer
  (org-node-fakeroam-jit-backlinks-mode) ;; skip DB for Roam buffer
  (org-node-fakeroam-db-feed-mode) ;; keep Roam DB up to date
  )

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

;; Olivetti
(use-package olivetti
  :commands (org-mode markdown-mode)
  :custom
  (olivetti-style 'fancy)
  (olivetti-margin-width 100)
  :config
  (setq-default olivetti-body-width 100)
  (add-hook 'org-mode-hook 'olivetti-mode))

(provide 'init)
;;; init.el ends here
