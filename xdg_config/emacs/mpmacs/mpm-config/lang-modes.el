;; lang-modes.el --- configurations related to Programming language modes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
    :custom
    (python-indent-guess-indent-offset t)
    (python-indent-guess-indent-offset-verbose nil)
    :config
    (setq-default python-basic-offset 4)
    (setq-default python-indent-offset 4)
)

;; venv support
(use-package pyvenv
    :ensure t
    :config
    (pyvenv-mode t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/cpp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :init
  (setq-default c-basic-offset 4)
)

(use-package cpp
  :config
  ;; disable other checkers since we only want to utilize clangd language server
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
)

;; enable modern font lock for >=c++11
(use-package modern-cpp-font-lock
    :ensure t
    :diminish modern-c++-font-lock-mode
    :hook (c++-mode-hook . modern-c++-font-lock-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js/ts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package typescript-mode
    :ensure t
    :mode "\\.ts\\'"
    :config
    (setq-default typescript-indent-level 2) ;; indent 2 spaces by default
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
    :ensure t
    :hook (go-mode . lsp-deferred)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t
)

(use-package rustic
    :ensure t
    :hook (rust-mode . lsp-deferred)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elixir-mode
    :ensure t
    :hook (elixir-mode . lsp-deferred)
)

;; inferior repl mode
(use-package inf-elixir
    :ensure t
    :hook (elixir-mode . inf-elixir-minor-mode)
)


(provide 'lang-modes)
;;; lang-modes.el ends here
