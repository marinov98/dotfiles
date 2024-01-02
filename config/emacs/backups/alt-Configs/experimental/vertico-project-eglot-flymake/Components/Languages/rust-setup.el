;; rust-setup.el --- configurations related to Rust -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package rust-mode
    :ensure t
    :hook (rust-mode . eglot-ensure))

(use-package rustic
    :disabled)

(provide 'rust-setup)
;;; rust-setup.el ends here
