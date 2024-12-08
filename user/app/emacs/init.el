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

;; Startup hook for mainly gui related things
(add-hook 'emacs-startup-hook #'(lambda ()
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
                                                            `([,(cdr char-regexp) 0 font-shape-gstring])))

                                    (dashboard-setup-startup-hook))



				  ))


(add-hook 'after-init-hook #'(lambda ()
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

                               ;; Mouse & Smooth Scroll
                               (setq scroll-step 1
                                     scroll-margin 1
                                     scroll-conservatively 101
                                     scroll-preserve-screen-position nil
                                     redisplay-skip-fontification-on-input t)

                               ;; Visual lines make more sense
                               (global-visual-line-mode 1)     

                               ;; Line numbers
                               (setq display-line-numbers-type 'visual
                                     line-move-visual t)
                               (add-hook 'prog-mode-hook 'display-line-numbers-mode)

                               ;; Fix stupid backup confirmations
                               (setq backup-directory-alist '("." "~/.emacs.d/cache/backups"))
                               (setq tramp-auto-save-directory "/dev/null")))

;; Packages

;; Magit
(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

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
  (evil-collection-init)

 ;; Visual mode keybinds
  (evil-define-key 'motion 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'motion 'global (kbd "k") 'evil-previous-visual-line)

 ;; File and buffer  keybinds
  (evil-define-key 'motion 'global (kbd "<leader>.") 'find-file)
  (evil-define-key 'motion 'global (kbd "<leader>bi") 'ibuffer)
  (evil-define-key 'motion 'global (kbd "<leader>bd") 'delete-buffer)
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

 ;; Project keybinds
  (evil-define-key 'motion 'global (kbd "<leader>pp") 'projectile-switch-project)
  (evil-define-key 'motion 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'motion 'global (kbd "<leader>pa") 'projectile-add-known-project)
  (evil-define-key 'motion 'global (kbd "<leader>/") 'projectile-grep)
  (evil-define-key 'motion 'global (kbd "<leader>gg") 'magit-status)

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


  (evil-define-key 'motion 'org-mode-map (kbd "<leader>mll") 'org-insert-link)
  (define-key magit-mode-map (kbd "SPC") nil)
  
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-l") 'evil-window-right))

(require 'sudo-edit)
(setq sudo-edit-local-method "doas")
(setq auth-source-save-behavior nil)
(evil-define-key 'normal 'global (kbd "<leader>fU") 'sudo-edit)
(evil-define-key 'normal 'global (kbd "<leader>fu") 'sudo-edit-find-file)

(require 'lsp-mode)
(add-hook 'gdscript-ts-mode-hook #'lsp-deferred)
(setq lsp-completion-provider :none)

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package treemacs
  :config
  (add-hook 'projectile-after-switch-project-hook 'treemacs-add-and-display-current-project-exclusively))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix (kbd "SPC l"))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (gdscript-mode . lsp)
         (gdscript-ts-mode . lsp))
  :commands lsp-deferred)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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

;; I am a nerd
(use-package nerd-icons
  :ensure t
  :config
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons")
  (require 'nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (require 'nerd-icons-completion)
  (nerd-icons-completion-mode)
  (require 'nerd-icons-corfu)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package magit-file-icons
  :ensure t
  :after magit
  :init
  (magit-file-icons-mode 1)
  :custom
  ;; These are the default values:
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))

;; Theme and modeline
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        custom-theme-directory "~/.config/emacs/themes")
  (load-theme 'doom-stylix t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Dashboard
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
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Setup treesitter
(require 'treesit)
(treesit-major-mode-setup)

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; command-log-mode
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

;; Org mode config
(require 'org)

;; Heading styles
(set-face-attribute 'outline-1 nil :height 195 :foreground (nth 1 (nth 14 doom-themes--colors)))
(set-face-attribute 'outline-2 nil :height 188 :foreground (nth 1 (nth 15 doom-themes--colors)))
(set-face-attribute 'outline-3 nil :height 180 :foreground (nth 1 (nth 19 doom-themes--colors)))
(set-face-attribute 'outline-4 nil :height 173 :foreground (nth 1 (nth 23 doom-themes--colors)))
(set-face-attribute 'outline-5 nil :height 173 :foreground (nth 1 (nth 24 doom-themes--colors)))
(set-face-attribute 'outline-6 nil :height 165 :foreground (nth 1 (nth 16 doom-themes--colors)))
(set-face-attribute 'outline-7 nil :height 160 :foreground (nth 1 (nth 18 doom-themes--colors)))
(set-face-attribute 'outline-8 nil :height 155 :foreground (nth 1 (nth 11 doom-themes--colors)))

(require 'org-modern)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 20)
  (left-divider-width . 20)
   (internal-border-width . 20)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t)

;; Ellipsis styling
(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; Star styling
(setq org-modern-star 'replace)

(global-org-modern-mode)

;; Olivetti
(use-package olivetti
  :config
  (setq olivetti-style 'fancy
      olivetti-margin-width 100)
  (setq-default olivetti-body-width 100)
  (add-hook 'org-mode-hook 'olivetti-mode))
