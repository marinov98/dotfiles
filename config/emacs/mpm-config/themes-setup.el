;; themes-setup.el --- set emacs colorscheme and modeline -*- lexical-binding: t; -*-
;;; Commentary: 
;; all the colorschemes that I have liked on emacs

;;; Code:
;; BE AWARE: emacs can have multiple themes on at the same time
;; Multiple themes can mix into a super theme
;; Some themes do not mix well which is why I disable themes

(use-package doom-themes
     :disabled
     :ensure t
     :custom
     (doom-themes-enable-bold t)
     (doom-themes-enable-italic t)
     :config
     (load-theme 'doom-dracula t)
     (doom-themes-visual-bell-config) ;; Enable flashing mode-line on errors
     (doom-themes-org-config) ;; Corrects (and improves) org-mode's native fontification.
)

(use-package modus-themes
    :disabled
    :ensure t
    :config
    (setq modus-themes-bold-constructs t)
    (load-theme 'modus-vivendi-tinted t)
)

(use-package catppuccin-theme
    ;; :disabled
    :ensure t
    :custom
    (catppuccin-flavor 'macchiato)
    :config
    (load-theme 'catppuccin :no-confirm)
)

;; Modeline
(use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)
)

(provide 'themes-setup)
;;; themes-setup.el ends here
