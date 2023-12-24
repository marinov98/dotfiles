;; diminish-setup.el --- setup-git -*- lexical-binding: t; -*-

;;; Commentary:
;; This hides modes from your modeline, add the specific mode you do not want to see in the modeline

;;; Code:
(use-package diminish
    :ensure t
    :init
    (diminish 'auto-revert-mode)
    (diminish 'page-break-lines-mode)
    (diminish 'abbrev-mode))

(provide 'diminish-setup)
;;; diminish-setup.el ends here
