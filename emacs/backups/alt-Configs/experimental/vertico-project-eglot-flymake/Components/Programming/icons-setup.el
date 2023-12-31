;; icons-setup.el --- setup all the icons -*- lexical-binding: t; -*-

;;; Commentary:
;; add some icons to your search results and Dired

;;; Code:

;; Pretty Icons
(use-package all-the-icons
    :ensure t
    :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode))

;; icons for dired/ranger mode
(use-package all-the-icons-dired
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'icons-setup)
;;; icons-setup.el ends here
