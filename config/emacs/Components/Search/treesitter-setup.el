;; treesitter-setup.el --- setup treesitter -*- lexical-binding: t; -*-

;;; Commentary:
;; treesitter should take syntax highlighting to the next level

;;; Code:
(when (>= emacs-major-version 29)
  (use-package treesit
    :config
    (setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
     ;; I would uncomment below if you like -ts-mode update to the syntax highlighting (Some syntax changes are worse imo)
     ;; (setq major-mode-remap-alist
     ;;   '((yaml-mode . yaml-ts-mode)
     ;;     (bash-mode . bash-ts-mode)
     ;;     (js2-mode . js-ts-mode)
     ;;     (typescript-mode . typescript-ts-mode)
     ;;     (json-mode . json-ts-mode)
     ;;     (css-mode . css-ts-mode)
     ;;     (python-mode . python-ts-mode)))
    )
  (use-package python-ts-mode
    :mode (("\\.py\\'" . python-ts-mode)))
)

(provide 'treesitter-setup)
;;; treesitter-setup.el ends here
