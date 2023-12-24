;; init.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load "~/.emacs.d/early-init.el"))

;;; Code:
;; load our custom components except startup (used in early-init.el)
(require 'evil-component)
(require 'preferences-component)
(require 'look-and-feel-component)
(require 'search-component)
(require 'engines-component)
(require 'markup-component)
(require 'programming-component)
(require 'language-component)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-default-notes-file (concat org-directory mpm-notes-org))
 '(org-directory mpm-org-dir)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
