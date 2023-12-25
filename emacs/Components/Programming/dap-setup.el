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

;; frontend

;; debugging in chrome
(use-package dap-chrome
    :ensure nil
    :after dap-mode)

;; debugging in firefox
(use-package dap-firefox
    :ensure nil
    :after dap-mode)

;; Node
(use-package dap-node
    :ensure nil
    :after dap-mode)

;; Python
(use-package dap-python
  :ensure nil
  :after dap-mode
  :config
  (dap-register-debug-template "My App"
  (list :type "python"
        :args "-i"
        :cwd nil
        :env '(("DEBUG" . "1"))
        :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
        :request "launch"
        :name "My App")))

(use-package dap-gdb-lldb
  :ensure nil
  :after dap-mode
  :config
  (dap-register-debug-template
  "GDB config"
  (list :type "gdb"
        :request "launch"
        :name "GDB::Run"
        :target "test"
        :program "test"
        :cwd "/home/marin/Projects")))


(provide 'dap-setup)
;;; dap-setup.el ends here

