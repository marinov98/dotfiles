;; lsp-setup.el --- Language Server Protocol Config -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP stands for Language Server Protocal and makes setting up autocompletion and syntax checking easy.

;;; Code:
(use-package eglot
  :hook (python-mode . eglot-ensure)
  :bind (:map evil-normal-state-map
              ("gd" . xref-find-definitions)
              ("gr" . xref-find-references)))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
