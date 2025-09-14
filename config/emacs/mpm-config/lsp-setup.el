;;; lsp-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for lsp including syntax checking, lsp-mode, etc...

;;; Code:
(use-package flycheck
     :ensure t
     :custom-face
     (flycheck-info ((t (:underline (:style wave :color "#87cefa")))))
     (flycheck-warning ((t (:underline (:style wave :color "#ffb95c")))))
     (flycheck-error ((t (:underline (:style wave :color "#cc0202")))))
     :custom
     (flycheck-display-errors-delay 0.5)
     :bind
     (:map evil-normal-state-map
           ("]d" . flycheck-next-error)
           ("[d" . flycheck-previous-error))
     :config
     (global-flycheck-mode t)
     (mpm/leader-keys
        "d l" '(flycheck-list-errors :which-key "Diagnostics List Errors")
        "c f" '(:ignore t :wk "Flycheck options")
        "c f e" '(flycheck-explain-error-at-point :wk "Flycheck explain error")
        "c f s" '(flycheck-select-checker :wk "Flycheck select checker")
        "c f d" '(flycheck-disable-checker :wk "Flycheck disable checker")
        "c f h" '(flycheck-describe-checker :wk "Flycheck describe checker")
        "c f m" '(flycheck-mode :wk "Flycheck mode")
        "c f M" '(flycheck-manual :wk "Flycheck manual")
        "c f v" '(flycheck-verify-setup :wk "Flycheck verify setup")
     )
)


(provide 'lsp-setup)
;;; lsp-setup.el ends here
