;; markup-component.el --- load markup-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Loading it all for markup

;;; Code:

(message "Loading Markup Component")

(require 'org-setup)
(require 'md-setup)
(require 'writing-setup)
(require 'latex-pdf-setup)

(message "Markup Component Successfully Loaded!")

(provide 'markup-component)
;;; markup-component.el ends here

