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
(require 'markup-setup)
(require 'git-setup)
(require 'web-dev-setup)
(require 'lsp-setup)


(provide 'init)
;;; init.el ends here
