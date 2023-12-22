;; dashboard-setup.el --- setup Emacs Dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Great configs sometimes come with great startup pages

;;; Code:
(use-package dashboard
    :ensure t
    :custom
    (dashboard-banner-logo-title "MarinMacs")
    (dashboard-set-heading-icons t)
    (dashboard-set-init-info t)
    (dashboard-set-file-icons t)
    (dashboard-set-navigator t)
    (dashboard-items '((recents  . 5)
                       (projects . 5)
                       (bookmarks . 5)
                       (agenda . 5)))
    (dashboard-footer-messages '("Maintained by Marin Marinov since 2018"))
    :init
    (if (file-directory-p mpm-img-dir)
          (setq dashboard-startup-banner (concat mpm-img-dir "/" mpm-dashboard-banner-img))
        (setq dashboard-startup-banner 'logo))
    :config
    (when (string-equal system-type "windows-nt" )
            (advice-add #'dashboard-replace-displayable :override #'identity)) ;; icons have issue displaying on windows, this fixes it
    (dashboard-setup-startup-hook)) 

(provide 'dashboard-setup)
;;; dasboard-setup.el ends here
