;; lsp-setup.el --- Language Server Protocol Config -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP stands for Language Server Protocal and makes setting up autocompletion and syntax checking easy.

;;; Code:
(use-package eglot
  :hook
  (((c++-mode c-mode css-mode yaml-mode json-mode js-mode js2-mode rjsx-mode typescript-mode web-mode) . eglot-ensure)
   ((bash-ts-mode rust-ts-mode go-ts-mode css-ts-mode yaml-ts-mode json-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)) ;; treesitter modes
  :bind (:map evil-normal-state-map  ;; gd and gr seem to automatically map appropriately to xref find definitions and references
              ("gi" . eglot-find-implementation)
              ("gy" . eglot-find-typeDefinition)))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
