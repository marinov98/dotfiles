;; file-tree-setup.el --- file tree explorers -*- lexical-binding: t; -*-

;;; Commentary:
;file-tree-setup; I mainly use neotree but treemacs comes with lsp so might as well configure it.

;;; Code:

;; Neotree
(use-package neotree
    :ensure t
    :custom
    (neo-smart-open t) ; update every time its toggled
    (neo-show-hidden-files t)
    (neo-theme (if (display-graphic-p) 'icons 'arrow))) ; add icons (utilizes all-the-icon)

;; treemacs
(use-package treemacs
    :ensure t
    :custom
    (treemacs-resize-icons 4)
    (treemacs-show-cursor t))

(use-package treemacs-evil
    :after treemacs
    :ensure t)

(use-package treemacs-magit
    :after treemacs magit
    :ensure t)
    


(provide 'file-tree-setup)
;;; file-tree-setup.el ends here
