;; go-setup.el --- configurations related to Go -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package go-mode
    :ensure t
    :hook (go-mode . lsp-deferred))

(provide 'go-setup)
;;; go-setup.el ends here
