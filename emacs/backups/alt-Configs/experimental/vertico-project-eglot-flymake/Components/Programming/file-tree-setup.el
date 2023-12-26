;; file-tree-setup.el --- file tree explorers -*- lexical-binding: t; -*-

;;; Commentary:
;file-tree-setup; I mainly use neotree

;;; Code:

;; Neotree
(use-package neotree
    :ensure t
    :custom
    (neo-smart-open t) ; update every time its toggled
    (neo-show-hidden-files t)
    (neo-theme (if (display-graphic-p) 'icons 'arrow))) ; add icons (utilizes all-the-icon)


(provide 'file-tree-setup)
;;; file-tree-setup.el ends here
