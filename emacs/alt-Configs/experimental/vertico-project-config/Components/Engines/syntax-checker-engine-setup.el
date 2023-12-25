;;; syntax-checker-engine-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Checking syntax on the fly... basically

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
     (global-flycheck-mode t))


(provide 'syntax-checker-engine-setup)
;;; syntax-checker-engine-setup.el ends here
