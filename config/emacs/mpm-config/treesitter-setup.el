;;; treesitter-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for treesitter

;;; Code:
(use-package treesit
  :ensure nil
)

(use-package treesit-auto
  :ensure t
  :after treesit
  :custom
  (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
)

(provide 'treesitter-setup)
;;; treesitter-setup.el ends here
