;; icons-setup.el --- setup all the icons -*- lexical-binding: t; -*-

;;; Commentary:
;; add some icons to your search results and Dired

;;; Code:

;; Pretty Icons
(use-package all-the-icons
    :ensure t
    :if (display-graphic-p))

;; icons for ivy
(use-package all-the-icons-ivy
    :ensure t
    :after (all-the-icons ivy)
    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
    :custom
    (all-the-icons-ivy-file-commands
      '(counsel-find-file
        counsel-file-jump
        counsel-git
        counsel-recentf
        counsel-projectile
        counsel-projectile-switch-to-buffer
        counsel-projectile-switch-project
        counsel-projectile-find-file
        counsel-projectile-find-file-dwin
        counsel-projectile-find-dir)))

;; icons for dired/ranger mode
(use-package all-the-icons-dired
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'icons-setup)
;;; icons-setup.el ends here
