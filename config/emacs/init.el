;; init.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load (concat user-emacs-directory "early-init.el")))

;;; Code:
;; load our custom components except startup (used in early-init.el)
(message "Loading rest of config...")
(require 'evil-setup)
(require 'themes-setup)
(require 'mpm-core)
(require 'icons-setup)
(require 'fuzzy-find-setup)
(require 'lang-modes)
(require 'treesitter-setup)
(require 'markup-setup)
(require 'git-setup)
(require 'autocomplete-setup)
(require 'web-dev-setup)
(require 'lsp-setup)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-directory mpm-org-dir)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
