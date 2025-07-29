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
(defun activate-writing-lines ()
  "Stop truncating lines in current buffer."
  (interactive)
  (setq-local truncate-lines nil)
  (setq visual-line-mode t)
  (display-line-numbers-mode 0))
(defun activate-coding-lines ()
  "Truncate lines in current buffer."
  (interactive)
  (setq-local truncate-lines t)
  (setq visual-line-mode nil)
  (display-line-numbers-mode 1))
(defun activate-coding-lines-without-numbers ()
  "Truncate lines in current buffer."
  (interactive)
  (setq-local truncate-lines t)
  (setq visual-line-mode nil)
  (display-line-numbers-mode 0))
(add-hook 'org-mode-hook 'activate-writing-lines)
(add-hook 'markdown-mode-hook 'activate-writing-lines)
(add-hook 'prog-mode-hook 'activate-coding-lines)
(add-hook 'treemacs-mode-hook 'activate-coding-lines-without-numbers)
(defun apply-proper-line-wrapping ()
  "Apply proper line wrapping and visual line mode
  settings according to whether or not the current
  mode derives from `prog-mode`."
  (if (derived-mode-p 'prog-mode)
    (progn
      (truncate-lines-on)
      )
    (progn
      (truncate-lines-off)
      (display-line-numbers-mode 0))))
(if (featurep 'git-timemachine)
    (add-hook 'git-timemachine-mode-hook 'apply-proper-line-wrapping))

(provide 'line-wrapping-and-numbers)

;;; line-wrapping.el ends here
