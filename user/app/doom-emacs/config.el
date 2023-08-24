;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;------ User configuration ------;;;

;; My default user identity as my yt alias
(setq user-full-name "Emmet")

;; I prefer visual lines
(setq display-line-numbers-type 'visual
      line-move-visual t)
(use-package-hook! evil
  :pre-init
  (setq evil-respect-visual-line-mode t) ;; sane j and k behavior
  t)

;; I also like evil mode visual movement
(map! :map evil-normal-state-map
      :desc "Move to next visual line"
      "j" 'evil-next-visual-line
      :desc "Move to previous visual line"
      "k" 'evil-previous-visual-line)

;; Theme and font
(setq custom-theme-directory "~/.emacs.d/themes")
(setq doom-theme 'doom-stylix)
(setq doom-font (font-spec :family "Inconsolata" :size 20))
;; +unicode-init-fonts-h often errors out
(remove-hook 'doom-init-ui-hook '+unicode-init-fonts-h)

;; Transparent background
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Icons in completion buffers
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
(all-the-icons-completion-mode)

;; This makes non-main buffers dimmer, so you can focus on main buffers
(solaire-global-mode +1)

;; Grammar tasing should be voluntary
(setq writegood-mode nil)

;; Beacon shows where the cursor is, even when fast scrolling
(setq beacon-mode t)

;; Quicker window management keybindings
(bind-key* "C-j" #'evil-window-down)
(bind-key* "C-k" #'evil-window-up)
(bind-key* "C-h" #'evil-window-left)
(bind-key* "C-l" #'evil-window-right)
(bind-key* "C-q" #'evil-window-delete)
(bind-key* "M-q" #'kill-current-buffer)
(bind-key* "M-w" #'+workspace/close-window-or-workspace)
(bind-key* "M-n" #'next-buffer)
(bind-key* "M-p" #'previous-buffer)
(bind-key* "M-z" #'+vterm/toggle)
(bind-key* (kbd "M-<return>") #'+vterm/here)

;; Buffer management
(bind-key* "<mouse-9>" #'next-buffer)
(bind-key* "<mouse-8>" #'previous-buffer)

;; Disables custom.el
(setq custom-file null-device)

;; Fancy splash image
(setq fancy-splash-image "~/.emacs.d/dashboard-logo.png")

(setq +doom-dashboard-menu-sections
'(("Open org roam overview" :icon
  (all-the-icons-octicon "globe" :face 'doom-dashboard-menu-title)
  :face
  (:inherit
   (doom-dashboard-menu-title bold))
  :action org-roam-default-overview)
 ("Roam to another db" :icon
  (all-the-icons-fileicon "org" :face 'doom-dashboard-menu-title)
  :action org-roam-switch-db)
 ("Open agenda" :icon
  (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
  :when
  (fboundp 'org-agenda)
  :action org-agenda-list
  :key "SPC o A a")
 ("Open private configuration" :icon
  (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
  :when
  (file-directory-p doom-user-dir)
  :action doom/open-private-config)
 ("Open documentation" :icon
  (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
  :action doom/help)
 ("Quit emacs" :icon
  (all-the-icons-faicon "level-down" :face 'doom-dashboard-menu-title)
  :action save-buffers-kill-terminal)
 )
)

;; Requires for faster loading
(require 'org-agenda)
(require 'dired)

;; Garbage collection to speed things up
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;; Enable autorevert globally so that buffers update when files change on disk.
;; Very useful when used with file syncing (i.e. syncthing)
(setq global-auto-revert-mode nil)
(setq auto-revert-use-notify t)

;;;------ Registers ------;;;

(map! :leader
      :desc "Jump to register"
      "r" 'jump-to-register)

(set-register ?f '(file . "/home/emmet/Org/Family.s/Notes/hledger.org"))
(set-register ?h '(file . "/home/emmet"))
(set-register ?r '(file . "/home/emmet/.dotfiles/README.org"))
(set-register ?x '(file . "/home/emmet/.dotfiles/user/wm/xmonad/xmonad.org"))
(set-register ?d '(file . "/home/emmet/.dotfiles/user/app/doom-emacs/doom.org"))

;;;------ Org mode configuration ------;;;

;; Set default org directory
(setq org-directory "~/.Org")

(remove-hook 'after-save-hook #'+literate|recompile-maybe)
(set-company-backend! 'org-mode nil)

;; Automatically show images but manually control their size
(setq org-startup-with-inline-images t
      org-image-actual-width nil)

(require 'evil-org)
(require 'evil-org-agenda)
(add-hook 'org-mode-hook 'evil-org-mode -100)

;; Top-level headings should be bigger!
(custom-set-faces!
  '(org-level-1 :inherit outline-1 :height 1.3)
  '(org-level-2 :inherit outline-2 :height 1.25)
  '(org-level-3 :inherit outline-3 :height 1.2)
  '(org-level-4 :inherit outline-4 :height 1.1)
  '(org-level-5 :inherit outline-5 :height 1.1)
  '(org-level-6 :inherit outline-6 :height 1.05)
  '(org-level-7 :inherit outline-7 :height 1.05)
  )

(after! org (org-eldoc-load))

(with-eval-after-load 'org (global-org-modern-mode))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
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
 org-pretty-entities t
 org-ellipsis "…")

(setq-default line-spacing 0.1)

; Automatic table of contents is nice
(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode)
      (add-hook 'markdown-mode-hook 'toc-org-mode))
  (warn "toc-org not found"))

;;---- this block from http://fgiasson.com/blog/index.php/2016/06/21/optimal-emacs-settings-for-org-mode-for-literate-programming/ ----;;
;; Tangle Org files when we save them
(defun tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)
;; ---- end block ---- ;;

;; Better org table editing
;; This breaks multiline visual block edits
;;(setq-default evil-insert-state-exit-hook '(org-update-parent-todo-statistics
;; t))
;;(setq org-table-automatic-realign nil)

;; Better for org source blocks
(setq electric-indent-mode nil)
(setq org-src-window-setup 'current-window)
(delete
  '("^\\*Org Src"
  (+popup-buffer)
  (actions)
  (side . bottom)
  (size . 0.42)
  (window-width . 40)
  (window-height . 0.42)
  (slot)
  (vslot)
  (window-parameters
   (ttl)
   (quit)
   (select . t)
   (modeline . t)
   (autosave . t)
   (transient . t)
   (no-other-window . t)))
 display-buffer-alist)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-download-screenshot-method "flameshot gui -p %s")
(after! org-download
   (setq org-download-method 'directory))

(after! org
  (setq-default org-download-image-dir "img/"
        org-download-heading-lvl nil))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command (concat "emacs-wayshot " filename))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-org-paste()
  "Take an image from the clipboard into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "img/"
                  (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command (concat "wl-paste > " filename))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-org-new-file-from-template()
  "Copy a template from ~/Templates into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq template-file (completing-read "Template file:" (directory-files "~/Templates")))
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "files/"
                  (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) (file-name-extension template-file t)))
  (copy-file (concat "/home/emmet/Templates/" template-file) filename)
  (setq prettyname (read-from-minibuffer "Pretty name:"))
  (insert (concat "[[./files/" (file-name-nondirectory filename) "][" prettyname "]]"))
  (org-display-inline-images))

(when (require 'openwith nil 'noerror)
   (setq openwith-associations
         (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
                  "mpv"
                  '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
                  "libreoffice"
                  '(file))
             '("\\.lyx" "lyx" (file))
             '("\\.chm" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
                  "atril"
                  '(file))
         (list (openwith-make-extension-regexp
                '("kdenlive"))
                  "kdenlive"
                  '(file))
         (list (openwith-make-extension-regexp
                '("kra"))
                  "krita"
                  '(file))
         (list (openwith-make-extension-regexp
                '("blend" "blend1"))
                  "blender"
                  '(file))
         (list (openwith-make-extension-regexp
                '("helio"))
                  "helio"
                  '(file))
         (list (openwith-make-extension-regexp
                '("svg"))
                  "inkscape"
                  '(file))
         (list (openwith-make-extension-regexp
                '("flp"))
                  "~/.local/bin/flstudio"
                  '(file))
             ))
   (openwith-mode 1))

(add-to-list 'display-buffer-alist '("^*Async Shell Command*" . (display-buffer-no-window)))

(map! :leader
      :desc "Insert a screenshot"
;;      "i s" 'my-org-screenshot)
      "i s" 'org-download-screenshot)

(defun org-download-clipboard-basename ()
  (interactive)
  (setq org-download-path-last-dir org-download-image-dir)
  (setq org-download-image-dir (completing-read "directory: " (-filter #'f-directory-p (directory-files-recursively "." "" t)) nil t))
  (org-download-clipboard (completing-read "basename: " '() nil nil))
  (setq org-download-image-dir org-download-path-last-dir)
)

(map! :leader
      :desc "Insert image from clipboard"
      "i p" 'org-download-clipboard
      "i P" 'org-download-clipboard-basename)

(map! :leader
      :desc "Create a new file from a template and insert a link at point"
      "i t" 'my-org-new-file-from-template)

(defun org-copy-link-to-clipboard-at-point ()
  "Copy current link at point into clipboard (useful for images and links)"
  ;; Remember to press C-g to kill this foreground process if it hangs!
  (interactive)
  (if (eq major-mode #'org-mode)
      (link-hint-copy-link-at-point)
  )
  (if (eq major-mode #'ranger-mode)
      (ranger-copy-absolute-file-paths)
  )
  (if (eq major-mode #'image-mode)
      (image-mode-copy-file-name-as-kill)
  )
  (shell-command (concat "~/.emacs.d/scripts/copy-link-or-file/copy-link-or-file-to-clipboard.sh " (gui-get-selection 'CLIPBOARD)) nil nil)
)

(map! :leader
      :desc "Copy link/file at point into system clipbord (C-g to escape if copying a file)"
      "y y" 'org-copy-link-to-clipboard-at-point)

;; Online images inside of org mode is pretty cool
;; This snippit is from Tobias on Stack Exchange
;; https://emacs.stackexchange.com/questions/42281/org-mode-is-it-possible-to-display-online-images
(require 'org-yt)

(defun org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (cl-assert (string-match "\\`img" protocol) nil
             "Expected protocol type starting with img")
  (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
    (cl-assert buf nil
               "Download of image \"%s\" failed." link)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (buffer-substring-no-properties (point) (point-max)))))

(org-link-set-parameters
 "imghttp"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "imghttps"
 :image-data-fun #'org-image-link)

;; Mermaid diagrams
(setq ob-mermaid-cli-path "~/.nix-profile/bin/mmdc")

;; Print org mode
(defun org-simple-print-buffer ()
  "Open an htmlized form of current buffer and open in a web browser to print"
  (interactive)
  (htmlize-buffer)
  (browse-url-of-buffer (concat (buffer-name) ".html"))
  (sleep-for 1)
  (kill-buffer (concat (buffer-name) ".html")))

;; Doesn't work yet, bc htmlize-region takes arguments BEG and END
;(defun org-simple-print-region()
;  "Open an htmlized form of current region and open in a web browser to print"
;  (interactive)
;  (htmlize-region )
;  (browse-url-of-buffer (concat (buffer-name) ".html"))
;  (sleep-for 1)
;  (kill-buffer (concat (buffer-name) ".html")))

(map! :leader
      :prefix ("P" . "Print")
      :desc "Simple print buffer in web browser"
      "p" 'org-simple-print-buffer)

(map! :leader
      :prefix ("P" . "Print")
      :desc "Simple print buffer in web browser"
      "b" 'org-simple-print-buffer)

;(map! :leader
;      :prefix ("P" . "Print")
;      :desc "Simple print region in web browser"
;      "r" 'org-simple-print-region)

;;;------ Org roam configuration ------;;;

(require 'org-roam)
(require 'org-roam-dailies)

(setq org-roam-directory "~/Org/Personal/Notes"
      org-roam-db-location "~/Org/Personal/Notes/org-roam.db")

(setq org-roam-node-display-template
      "${title:65}📝${tags:*}")

(org-roam-db-autosync-mode)

(setq full-org-roam-db-list nil)

(setq full-org-roam-db-list (directory-files "~/Org" t "\\.[p,s]$"))
(dolist (item full-org-roam-db-list)
  (setq full-org-roam-db-list
        (append (directory-files item t "\\.[p,s]$") full-org-roam-db-list)))

(setq org-roam-db-choice "Default")
(setq full-org-roam-db-list-pretty (list "Default"))
(dolist (item full-org-roam-db-list)
  (setq full-org-roam-db-list-pretty
       (append (list
             (replace-regexp-in-string "\\/home\\/emmet\\/Org\\/" "" item)) full-org-roam-db-list-pretty)))

(defun org-roam-open-dashboard ()
  "Open ${org-roam-directory}/dashboard.org (I use this naming convention to create dashboards for each of my org roam maps)"
  (interactive)
  (if (file-exists-p (concat org-roam-directory "/dashboard.org"))
      (org-open-file (concat org-roam-directory "/dashboard.org"))
      (dired org-roam-directory))
)

(defun org-roam-switch-db (&optional arg silent)
  "Switch to a different org-roam database, arg"
  (interactive)
  (when (not arg)
  (setq full-org-roam-db-list nil)

  (setq full-org-roam-db-list (directory-files "~/Org" t "\\.[p,s]$"))
  (dolist (item full-org-roam-db-list)
    (setq full-org-roam-db-list
        (append (directory-files item t "\\.[p,s]$") full-org-roam-db-list)))

  (setq full-org-roam-db-list-pretty (list "Default"))
  (dolist (item full-org-roam-db-list)
    (setq full-org-roam-db-list-pretty
        (append (list
                 (replace-regexp-in-string "\\/home\\/emmet\\/Org\\/" "" item)) full-org-roam-db-list-pretty)))

  (setq org-roam-db-choice (completing-read "Select org roam database: "
                          full-org-roam-db-list-pretty nil t)))
  (when arg
    (setq org-roam-db-choice arg))

  (if (string= org-roam-db-choice "Default")
      (setq org-roam-directory (file-truename "~/Org/Personal/Notes")
            org-roam-db-location (file-truename "~/Org/Personal/Notes/org-roam.db")
            org-directory (file-truename"~/Org/Personal/Notes"))
      (setq org-roam-directory (file-truename (concat "~/Org/" org-roam-db-choice "/Notes"))
            org-roam-db-location (file-truename (concat "~/Org/" org-roam-db-choice "/Notes/org-roam.db"))
            org-directory (file-truename (concat "~/Org/" org-roam-db-choice "/Notes"))))
  (when (not silent)
  (org-roam-open-dashboard))

  (org-roam-db-sync)

  (message (concat "Switched to " org-roam-db-choice " org-roam database!")))

(defun org-roam-default-overview ()
  (interactive)
  (org-roam-switch-db "Default"))

(defun org-roam-switch-db-id-open (arg ID &optional switchpersist)
  "Switch to another org-roam db and visit file with id arg"
  "If switchpersist is non-nil, stay in the new org-roam db after visiting file"
  (interactive)
  (setq prev-org-roam-db-choice org-roam-db-choice)
  (org-roam-switch-db arg 1)
  (org-roam-id-open ID)
  (when (not switchpersist)
    (org-roam-switch-db prev-org-roam-db-choice 1)))

;;;------ Org-roam-agenda configuration ------;;;
(defun text-in-buffer-p (TEXT)
(save-excursion (goto-char (point-min)) (search-forward TEXT nil t)))

(defun apply-old-todos-tag-maybe (&optional FILE)
   (interactive)
   (if (stringp FILE)
   (setq the-daily-node-filename FILE)
   (setq the-daily-node-filename buffer-file-name))
   (if (org-roam-dailies--daily-note-p the-daily-node-filename)
    (if (<= (nth 2 (org-roam-dailies-calendar--file-to-date the-daily-node-filename)) (nth 2 org-agenda-current-date))
      (if (<= (nth 1 (org-roam-dailies-calendar--file-to-date the-daily-node-filename)) (nth 1 org-agenda-current-date))
        (if (<= (nth 0 (org-roam-dailies-calendar--file-to-date the-daily-node-filename)) (nth 0 org-agenda-current-date))
          (funcall (lambda ()
            (with-current-buffer (get-file-buffer the-daily-node-filename) (org-roam-tag-add '("old-todos")))
            (with-current-buffer (get-file-buffer the-daily-node-filename) (org-roam-tag-remove '("todos")))
            )
          )
        )
      )
    )
  )
)

(defun apply-old-todos-tag-maybe-and-save (FILE)
  (interactive)
  (find-file-noselect FILE)
  (apply-old-todos-tag-maybe FILE)
  (with-current-buffer (get-file-buffer the-daily-node-filename) (save-buffer))
  (with-current-buffer (get-file-buffer the-daily-node-filename) (kill-buffer))
)

; This has a bug where it won't sync a new agenda file
; if I'm editing an org roam node file while set to another
; org roam db
(defun add-todos-tag-on-save-org-mode-file()
  (interactive)
  (when (string= (message "%s" major-mode) "org-mode")
    (if (org-roam-node-p (org-roam-node-at-point))
    (funcall (lambda()
      (if (or (text-in-buffer-p "SCHEDULED: <") (text-in-buffer-p "DEADLINE: <"))
        (org-roam-tag-add '("todos"))
        (org-roam-tag-remove '("todos"))
      )
      (apply-old-todos-tag-maybe)
     )
    )
  )
 )
)

(add-hook 'before-save-hook 'add-todos-tag-on-save-org-mode-file)

(defun org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun org-roam-dailies-apply-old-todos-tags-to-all ()
;  (dolist (daily-node org-roam-dailies-files)
;           (apply-old-todos-tag-maybe-and-save daily-node)
;  )
  (setq num 0)
  (while (< num (list-length (org-roam-list-notes-by-tag "todos")))
    (apply-old-todos-tag-maybe-and-save (nth num (org-roam-list-notes-by-tag "todos")))
  (setq num (1+ num))
  )
)

(defun org-roam-append-notes-to-agenda (tag-name db)
  (org-roam-switch-db db t)
;  (org-roam-dailies-apply-old-todos-tags-to-all)
  (setq org-agenda-files (append org-agenda-files (org-roam-list-notes-by-tag "todos")))
)

(defun org-roam-refresh-agenda-list ()
  (interactive)
  (setq prev-org-roam-db-choice org-roam-db-choice)
  (setq org-agenda-files '())
  (dolist (DB full-org-roam-db-list-pretty)
    (org-roam-append-notes-to-agenda "todos" DB)
  )
  (setq org-agenda-files (-uniq org-agenda-files))
  (org-roam-switch-db prev-org-roam-db-choice 1)
)

;; Build agenda for first time during this session
(org-roam-refresh-agenda-list)


(map! :leader
      :prefix ("o a")

      :desc "Refresh org agenda from roam dbs"
      "r" 'org-roam-refresh-agenda-list)

(map! :leader
      :prefix ("N" . "org-roam notes")

      :desc "Capture new roam node"
      "c" 'org-roam-capture

      :desc "Insert roam node link at point"
      "i" 'org-roam-node-insert

      :desc "Find roam node"
      "." 'org-roam-node-find

      :desc "Switch org-roam database"
      "s" 'org-roam-switch-db

      :desc "Update current org-roam database"
      "u" 'org-roam-db-sync

      :desc "Re-zoom on current node in org-roam-ui"
      "z" 'org-roam-ui-node-zoom

      :desc "Visualize org-roam database with org-roam-ui"
      "O" 'org-roam-default-overview

      :desc "Visualize org-roam database with org-roam-ui"
      "o" 'org-roam-open-dashboard)

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  :unnarrowed t))))

(defun org-roam-olivetti-mode ()
  (interactive)
  (if (org-roam-file-p)
      (olivetti-mode))
  (if (org-roam-file-p)
      (doom-disable-line-numbers-h)))

(add-hook 'org-mode-hook 'org-roam-olivetti-mode)

(add-load-path! "~/.emacs.d/org-nursery/lisp")
(require 'org-roam-dblocks)
(use-package org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))

(setq org-id-extra-files 'org-agenda-text-search-extra-files)

;(add-to-list 'display-buffer-alist '("^\\ORUI" display-buffer-in-side-window
;                                    '(side . right)
;                                    (window-width . 50)
;))
;(add-to-list 'display-buffer-alist '("^\\localhost:35901" display-buffer-in-side-window
;                                    '(side . right)
;                                    (window-width . 50)
;))

;;(setq org-roam-ui-browser-function 'eaf-open-browser) ; xorg
(setq org-roam-ui-browser-function 'browse-url) ; wayland

(defun open-org-roam-ui ()
  (interactive)
  (+evil/window-vsplit-and-follow)
  (org-roam-ui-open)
  (evil-window-left 1))

(defun kill-org-roam-ui ()
  (interactive)
;;  (delete-window (get-buffer-window "ORUI" t)) ; xorg
;;  (kill-buffer "ORUI") ; xorg
  (kill-buffer "*httpd*")
)

; xorg
;;(map! :leader
;;      :prefix ("N" . "org-roam notes")
;;      :desc "Visualize org-roam database with org-roam-ui"
;;      "v" 'open-org-roam-ui)

; wayland
(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Visualize org-roam database with org-roam-ui"
      "v" 'org-roam-ui-open)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Kill all org roam ui buffers"
      "V" 'kill-org-roam-ui)

;;;------ Org agenda configuration ------;;;

;; Set span for agenda
(setq org-agenda-span 1
      org-agenda-start-day "+0d")

;; Ricing org agenda
(setq org-agenda-current-time-string "")
(setq org-agenda-time-grid '((daily) () "" ""))

(setq org-agenda-prefix-format '(
(agenda . "  %?-2i %t ")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))

(setq org-agenda-hide-tags-regexp ".*")

(setq org-agenda-category-icon-alist
      `(("Teaching" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
        ("Family" ,(list (all-the-icons-faicon "home" :v-adjust 0.005)) nil nil :ascent center)
        ("Producer" ,(list (all-the-icons-faicon "youtube-play" :height 0.9)) nil nil :ascent center)
        ("Bard" ,(list (all-the-icons-faicon "music" :height 0.9)) nil nil :ascent center)
        ("Story" ,(list (all-the-icons-faicon "book" :height 0.9)) nil nil :ascent center)
        ("Author" ,(list (all-the-icons-faicon "pencil" :height 0.9)) nil nil :ascent center)
        ("Gamedev" ,(list (all-the-icons-faicon "gamepad" :height 0.9)) nil nil :ascent center)
        ("Tech" ,(list (all-the-icons-faicon "laptop" :height 0.9)) nil nil :ascent center)
))

;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

;; Adds hook to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

;; Function to list all my available org agenda files and switch to them
(defun list-and-switch-to-agenda-file ()
  "Lists all available agenda files and switches to desired one"
  (interactive)
  (setq full-agenda-file-list nil)
  (setq choice (completing-read "Select agenda file:" org-agenda-files nil t))
  (find-file choice))

(map! :leader
      :desc "Switch to specific org agenda file"
      "o a s" 'list-and-switch-to-agenda-file)

(map! :leader
      :desc "Open org calendar"
      "o c" #'cfw:open-org-calendar)

(defun org-agenda-switch-with-roam ()
  "Switches to org roam node file and database from org agenda view"
  (interactive)
  (org-agenda-switch-to)
  (if (f-exists-p (concat (dir!) "/org-roam.db"))
    (org-roam-switch-db (f-filename (f-parent (dir!))) t))
  (org-roam-olivetti-mode)
)

(map!
  :map evil-org-agenda-mode-map
  :after org-agenda
  :nvmeg "<RET>" #'org-agenda-switch-with-roam
  :nvmeg "<return>" #'org-agenda-switch-with-roam)
(map!
  :map org-agenda-mode-map
  :after org-agenda
  :nvmeg "<RET>" #'org-agenda-switch-with-roam
  :nvmeg "<return>" #'org-agenda-switch-with-roam)

(require 'org-super-agenda)

(setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Home Tech"
                :and(:file-path "emmet/Agenda" :not (:tag "event"))
                :order 3)

         (:name "Family"
                :and(:file-path "Family" :not (:tag "event"))
                :order 3)

         (:name "Teaching Prep"
                :and(:file-path "Teaching.p" :tag "planning" :not (:tag "grading") :not (:tag "event"))
                :order 3)

         (:name "Teaching Secretarial"
                :and(:file-path "Teaching.p" :tag "secretarial" :not (:tag "grading") :not (:tag "event"))
                :order 3)

         (:name "Teaching Grading"
                :and(:file-path "Teaching.p" :tag "grading" :not (:tag "planning") :not (:tag "event"))
                :order 3)

         (:name "School Side Projects"
                :and(:file-path "Teaching.p" :tag "tech" :not (:tag "planning") :not (:tag "event"))
                :order 3)

         (:name "Gamedev Current Projects"
                :and (:file-path "Gamedev" :todo "STRT")
                :order 5)

         (:name "Youtube"
                :tag "youtube"
                :order 6)

         (:name "Learning"
                :tag "learning"
                :order 7)

          (:name "Today"  ; Optionally specify section name
                :time-grid t
                :date today
                :scheduled today
                :order 1)
))

(org-super-agenda-mode t)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "j" 'org-agenda-next-line)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "k" 'org-agenda-previous-line)

;;;------ magit configuration ------;;;

;; Need the following two blocks to make magit work with git bare repos
(defun ~/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
  (let ((default (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= default home)
      (let ((gitdir (expand-file-name "~/.dotfiles.git/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'~/magit-process-environment)

(evil-set-initial-state 'magit-status-mode 'motion)
(evil-set-initial-state 'magit-log-mode 'motion)
(evil-set-initial-state 'magit-diff-mode 'motion)
(evil-set-initial-state 'magit-refs-mode 'motion)
(evil-define-key 'motion magit-status-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "c" 'magit-commit
  "s" 'magit-stage
  "u" 'magit-unstage
  "l" 'magit-log
  "F" 'magit-pull
  "p" 'magit-push
  "q" '+magit/quit
  (kbd "<return>") 'magit-diff-visit-file-worktree)
(evil-define-key 'motion magit-log-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "q" '+magit/quit
  (kbd "<return>") 'magit-visit-ref)
(evil-define-key 'motion magit-diff-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "q" '+magit/quit
  (kbd "<return>") 'magit-visit-ref)
(evil-define-key 'motion magit-refs-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "q" '+magit/quit
  (kbd "<return>") 'magit-visit-ref)

(evil-set-initial-state 'ibuffer-mode 'motion)
(evil-define-key 'motion 'ibuffer-mode
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "d" 'ibuffer-mark-for-delete
  "q" 'kill-buffer
  (kbd "<return>") 'ibuffer-visit-buffer)

;;;------ dired configuration ------;;;

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(map! :desc "Increase font size"
      "C-=" 'text-scale-increase

      :desc "Decrease font size"
      "C--" 'text-scale-decrease)

;;;------ ranger configuration ------;;;

(map! :map ranger-mode-map
      :desc "Mark current file"
      "m" 'ranger-mark

      :desc "Toggle mark on current file"
      "x" 'ranger-toggle-mark

      :desc "Open ranger"
      "o d" 'ranger)

;;;-- hledger-mode configuration ;;;--

;;; Basic configuration
(require 'hledger-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; The default journal location is too opinionated.
(setq hledger-jfile "/home/emmet/Org/Family.s/Notes/hledger.journal")

;;; Auto-completion for account names
;; For company-mode users:
(add-to-list 'company-backends 'hledger-company)

(evil-define-key* 'normal hledger-view-mode-map "q" 'kill-current-buffer)
(evil-define-key* 'normal hledger-view-mode-map "[" 'hledger-prev-report)
(evil-define-key* 'normal hledger-view-mode-map "]" 'hledger-next-report)

(map! :leader
      :prefix ("l" . "hledger")
      :desc "Exec hledger command"
      "c" 'hledger-run-command

      :desc "Generate hledger balancesheet"
      "b" 'hledger-balancesheet*

      :desc "Exec hledger command"
      "d" 'hledger-daily-report*)

(map! :localleader
      :map hledger-mode-map

      :desc "Reschedule transaction at point"
      "d s" 'hledger-reschedule

      :desc "Edit amount at point"
      "t a" 'hledger-edit-amount)

(require 'focus)

(map! :leader
      :prefix ("F" . "Focus mode")
      :desc "Toggle focus mode"
      "t" 'focus-mode

      :desc "Pin focused section"
      "p" 'focus-pin

      :desc "Unpin focused section"
      "u" 'focus-unpin)

(add-to-list 'focus-mode-to-thing '(org-mode . org-element))
(add-to-list 'focus-mode-to-thing '(python-mode . paragraph))
(add-to-list 'focus-mode-to-thing '(lisp-mode . paragraph))

;(add-hook 'org-mode-hook #'focus-mode)

;;;------ helpful configuration ------;;;

(evil-set-initial-state 'helpful-mode 'normal)
(evil-define-key 'normal helpful-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "q" 'helpful-kill-buffers)

;;;-- Load emacs application framework;;;--
(use-package! eaf
  :load-path "~/.emacs.d/eaf/"
  :init
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)

  (require 'eaf-browser)

  (require 'eaf-evil)
  (define-key key-translation-map (kbd "SPC")
    (lambda (prompt)
      (if (derived-mode-p 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("browser" (if  (string= (eaf-call-sync "eval_function" eaf--buffer-id "is_focus") "True")
                           (kbd "SPC")
                         (kbd eaf-evil-leader-key)))
            (_  (kbd "SPC")))
        (kbd "SPC")))))

(setq browse-url-browser-function 'browse-url-default-browser)

(map! :leader
      :desc "Open web browser"
      "o w" #'eaf-open-browser-with-history)

;;;-- Load emacs direnv;;;--
(require 'direnv)
(direnv-mode)

;;;-- projectile wrapper commands ;;;--
(defun projectile-goto-project ()
  (interactive)
  (projectile-switch-project t)
  ;;(neotree-dir (projectile-project-root))
)

(map! :leader
      :desc "Open project"
      "p p" #'projectile-goto-project)
(map! :leader
      :desc "Projectile commander"
      "p @" #'projectile-commander)
(map! :leader
      :desc "Projectile grep"
      "/" #'projectile-grep)

;;;-- LSP stuff ;;;--
(use-package lsp-mode
  :ensure t)

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(setq lsp-java-workspace-dir "/home/emmet/.local/share/doom/java-workspace")

(require 'gdscript-mode)
(use-package gdscript-mode
  :hook (gdscript-mode . lsp-deferred)
  :ensure t)


