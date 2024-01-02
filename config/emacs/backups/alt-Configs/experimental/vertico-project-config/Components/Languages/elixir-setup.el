;; elixir-setup.el --- configurations related to Elixir -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package elixir-mode
    :ensure t
    :hook (elixir-mode . lsp-deferred))

;; inferior repl mode
(use-package inf-elixir
    :ensure t
    :hook (elixir-mode . inf-elixir-minor-mode))

(provide 'elixir-setup)
;;; elixir-setup.el ends here
