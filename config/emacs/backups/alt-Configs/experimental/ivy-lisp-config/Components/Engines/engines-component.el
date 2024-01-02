;;; engines-component-setup.el --- syntax checker -*- lexical-binding: t; -*-
;;; Commentary:
;; Load all the engines!

;;; Code:

(message "Loading Engines Component")

(require 'autocomplete-engine-setup)
(require 'syntax-checker-engine-setup)
(require 'snippet-engine-setup)

(message "Engines Component Successfully Loaded!")

(provide 'engines-component)
;;; engines-component.el ends here
