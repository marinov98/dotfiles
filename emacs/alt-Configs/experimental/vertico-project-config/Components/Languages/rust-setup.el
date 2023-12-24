;; rust-setup.el --- configurations related to Rust -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package rust-mode
    :ensure t)

(use-package rustic
    :ensure t
    :hook (rust-mode . lsp-deferred))

(provide 'rust-setup)
;;; rust-setup.el ends here
