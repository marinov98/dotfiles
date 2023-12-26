;; evil-component.el --- load all config relating to evil-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; entry point into the evil config

;;; Code:
;; Order matters never forget 
(message "Loading Evil Component...")

(require 'evil-mode-setup)
(require 'evil-leader-setup)
(require 'evil-utility-setup)

(message "Evil Component successfully loaded!")

(provide 'evil-component)
;;; evil-component.el ends here
