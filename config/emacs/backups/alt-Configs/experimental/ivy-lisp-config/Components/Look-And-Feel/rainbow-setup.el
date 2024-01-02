;; rainbow-setup.el --- setup colors while editings -*- lexical-binding: t; -*-

;;; Commentary:
;; having colors for parenthesis and knowing what the rgb hex looks like is quite useful indeed

;;; Code:
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode 
    :ensure t
    :hook prog-mode org-mode)

(provide 'rainbow-setup)
;;; rainbow-setup.el ends here
