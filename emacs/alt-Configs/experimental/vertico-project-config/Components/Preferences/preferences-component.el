;; preferences-component.el --- load all preference config -*- lexical-binding: t; -*-

;;; Commentary:
;; load those preferences up

;;; Code:
(message "Loading Preferences Component...")

(require 'which-key-setup)
(require 'preferences-setup)
(require 'parenthesis-and-lines-setup)
(require 'ligature-setup)
(require 'hydra-setup)

(message "Preferences Component Loaded Successfully!")

(provide 'preferences-component)
;;; preferences-component.el ends here
