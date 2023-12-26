;; evil-utility-setup.el --- Configure evil utility packages -*- lexical-binding: t; -*-

;;; Commentary:
;; These evil packages are extras that enhance the evil-mode experience in emacs

;;; Code:
;; like tpope's vim-surround
(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

;; Multiple-Cursors
(use-package evil-mc
    :ensure t
    :config
    (global-evil-mc-mode 1))

(provide 'evil-utility-setup)
;;; evil-utility-setup.el ends here
