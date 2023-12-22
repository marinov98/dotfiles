;; js-ts-setup.el --- configurations related to JavaScript and Typescript -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup defaults and options

;;; Code:

(use-package js2-mode
    :ensure t
    :config
    (setq js2-strict-missing-semi-warning nil)
    (setq-default js2-basic-offset 2)) ;; set indentation to 2

(use-package typescript-mode
    :ensure t
    :mode "\\.ts\\'"
    :config
    (setq-default typescript-indent-level 2)) ;; indent 2 spaces by default


(provide 'js-ts-setup)
;;; js-ts-setup.el ends here
