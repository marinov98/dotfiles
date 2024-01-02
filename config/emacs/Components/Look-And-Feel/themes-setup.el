;; themes-setup.el --- set emacs colorscheme -*- lexical-binding: t; -*-

;;; Commentary: 
;; all the colorschemes that I have liked on emacs

;;; Code:
;; BE AWARE: emacs can have multiple themes on at the same time
;; Multiple themes can mix into a super theme
;; Some themes do not mix well which is why I disable themes

 (use-package spacemacs-common
     :disabled
     :ensure spacemacs-theme
     :config (load-theme 'spacemacs-dark t))

 (use-package doom-themes
     :disabled
     :custom
     (doom-themes-enable-bold t)
     (doom-themes-enable-italic t)
     :config
     (load-theme 'doom-dracula t)
     (doom-themes-visual-bell-config) ;; Enable flashing mode-line on errors
     (doom-themes-org-config)) ;; Corrects (and improves) org-mode's native fontification.

 (use-package catppuccin-theme
    :ensure t
    :custom
    (catppuccin-flavor 'macchiato)
    :config
    (load-theme 'catppuccin :no-confirm))

 (use-package zerodark-theme
     :disabled
     :ensure t)

 (use-package minimal-theme
     :disabled
     :ensure t
     :config
     (load-theme 'minimal t))

 (use-package zenburn-theme
     :disabled
     :ensure t
     :config
     (load-theme 'zenburn t))

 (use-package poet-theme
     :disabled
     :ensure t)

 (use-package modus-vivendi-theme
     :disabled
     :ensure t
     :config
     (setq modus-vivendi-theme-bold-constructs t)
     (load-theme 'modus-vivendi t))

 (use-package modus-operandi-theme
     :disabled
     :ensure t
     :config (load-theme 'modus-operandi t))

 (use-package jbeans-theme
     :disabled
     :ensure t
     :config
     (load-theme 'jbeans t))

 (use-package planet-theme
     :disabled
     :ensure t
     :config 
     (load-theme 'planet t))

(provide 'themes-setup)
;;; themes-setup.el ends here
