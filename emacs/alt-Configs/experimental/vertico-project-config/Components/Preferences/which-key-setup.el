;; which-key-setup.el --- setup which key -*- lexical-binding: t; -*-

;;; Commentary:
;; Which key grants us the ability to check our bindings as we type

;;; Code:
(use-package which-key
  :ensure t 
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'which-key-setup)
;;; which-key-setup.el ends here
