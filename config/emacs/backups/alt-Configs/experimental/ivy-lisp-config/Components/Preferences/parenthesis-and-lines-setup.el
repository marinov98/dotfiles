;; parenthesis-and-lines-setup.el --- setup parenthesis and line numbers -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential are the auto-closing parenthesis and line numbers to the developers
;; Even more essential are the relative line numbers to the vim users ;)

;;; Code:
(use-package elec-pair ;; auto closing brackets
    :ensure nil
    :hook ((prog-mode . electric-pair-mode)
            (text-mode . electric-pair-mode))) 

(use-package paren ;; highlight matching parenthesis
    :custom
    (show-paren-when-point-inside-paren t)
    :config
    (show-paren-mode 1))

(use-package hl-line ;; highlight the current line
    :config
    (global-hl-line-mode 1))

;; Line numbers
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
      :custom
      (display-line-numbers-type 'relative) ;; relative line numbers help you see how far you need to jump to get where you want to
      (display-line-numbers-current-absolute t)
      (display-line-numbers-width 2)
      (display-line-numbers-widen t)
      :config
      (global-display-line-numbers-mode 1)
      ;; Disable line numbers for some mode
      (dolist (mode '(org-mode-hook term-mode-hook neotree-mode-hook shell-mode-hook treemacs-mode-hook eshell-mode-hook))
             (add-hook mode (lambda () (display-line-numbers-mode 0))))))

(provide 'parenthesis-and-lines-setup)
;;; parenthesis-and-lines-setup.el ends here
