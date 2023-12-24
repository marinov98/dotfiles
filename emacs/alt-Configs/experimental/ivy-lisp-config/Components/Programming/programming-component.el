;; programming-component.el --- programming component loader -*- lexical-binding: t; -*-

;;; Commentary:
;; load away.

;;; Code:

(message "Loading Programming Component")

(require 'file-tree-setup)
(require 'icons-setup)
(require 'git-setup)
(require 'lsp-setup)
(require 'dap-setup)
(require 'diminish-setup)

(message "Programming Component Successfully Loaded!")

(provide 'programming-component)
;;; programming-component.el ends here
