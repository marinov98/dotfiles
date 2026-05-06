;; dap-setup.el --- Debug Adapter Protocol Config -*- lexical-binding: t; -*-

;;; Commentary:
;; DAP stands for Debug Adapter Protocal works similiarly to LSP but for debugging

;;; Code:
;; only installing because dap-mode requires it
(use-package posframe :disabled)

(use-package dap-mode
    :disabled
    :after hydra
    :hook
    (lsp-mode . (lambda () (dap-mode t) (dap-ui-mode t) (dap-tooltip-mode 1) (tooltip-mode 1)))
    :config
    (add-hook 'dap-stopped-hook
    (lambda (arg) (call-interactively #'dap-hydra)))) ;; enable hydra on breakpoint stop

(provide 'dap-setup)
;;; dap-setup.el ends here
