;; lsp-setup.el --- Language Server Protocol Config -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP stands for Language Server Protocal and makes setting up autocompletion and syntax checking easy.

;;; Code:
(use-package eglot
  :hook (python-ts-mode . eglot-ensure)
  :bind (:map evil-normal-state-map  ;; gd and gr seem to automatically map appropriately to xref find definitions and references
              ("gi" . eglot-find-implementation)
              ("gy" . eglot-find-typeDefinition)))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
