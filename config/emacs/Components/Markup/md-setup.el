;; md-setup.el --- md config -*- lexical-binding: t; -*-

;;; Commentary
;; probably should mix this into another chunk but oh well

;;; Code:
(use-package markdown-mode
    :ensure t
    :commands markdown-mode
    :mode
    ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

(provide 'md-setup)
;;; md-setup.el ends here

