;;; line-wrapping-and-numbers.el --- basic line wrapping management library -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Emmet K <https://gitlab.com/librephoenix>
;; Maintainer: Emmet K <https://gitlab.com/librephoenix>
;; Source: https://github.com/librephoenix/nixos-config
;; Source: https://gitlab.com/librephoenix/nixos-config
;; Source: https://codeberg.org/librephoenix/nixos-config
;;
;;; Commentary:
;;
;; A basic line wrapping management library.
;; Turns on line wrapping for programming modes,
;; and turns it off for thinks like markdown and org.
;;
;;; Code:

;; Line wrapping management
(defun truncate-lines-off ()
  "Stop truncating lines in current buffer."
  (interactive)
  (toggle-truncate-lines 0))
(defun truncate-lines-on ()
  "Truncate lines in current buffer."
  (interactive)
  (toggle-truncate-lines 1))
(defun visual-line-mode-off ()
  "Disable `visual-line-mode` in current buffer."
  (interactive)
  (visual-line-mode 0))
(add-hook 'org-mode-hook 'truncate-lines-on)
(add-hook 'markdown-mode-hook 'truncate-lines-on)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'truncate-lines-off)
(add-hook 'prog-mode-hook 'visual-line-mode-off)
(add-hook 'nix-mode-hook 'truncate-lines-off)
(add-hook 'nix-mode-hook 'visual-line-mode-off)
(defun apply-proper-line-wrapping ()
  "Apply proper line wrapping and visual line mode settings according to whether or not the current mode derives from `prog-mode`."
  (if (derived-mode-p 'prog-mode)
    (progn
      (display-line-numbers-mode)
      (truncate-lines-on)
      (visual-line-mode-off)
      (display-line-numbers-mode 1))
    (progn
      (truncate-lines-off)
      (visual-line-mode)
      (display-line-numbers-mode 0))))
(add-hook 'prog-mode-hook 'apply-proper-line-wrapping)
(add-hook 'org-mode-hook 'apply-proper-line-wrapping)
(if (featurep 'markdown-mode)
  (add-hook 'markdown-mode-hook 'apply-proper-line-wrapping))
(if (featurep 'git-timemachine)
  (add-hook 'git-timemachine-mode-hook 'apply-proper-line-wrapping))

;;; line-wrapping.el ends here
