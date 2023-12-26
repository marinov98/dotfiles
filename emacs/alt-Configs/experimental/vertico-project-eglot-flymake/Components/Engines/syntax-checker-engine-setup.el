;;; syntax-checker-engine-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Checking syntax on the fly... basically

;;; Code:
(use-package flymake
     :bind
     (:map evil-normal-state-map
         ("]d" . flymake-goto-next-error)
         ("[d" . flymake-goto-prev-error))
     :hook (prog-mode . flymake-mode))


(provide 'syntax-checker-engine-setup)
;;; syntax-checker-engine-setup.el ends here
