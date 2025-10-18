;; mpm-config.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load (concat user-emacs-directory "early-init.el")))

;;; Code:
;; load our custom components except startup (used in early-init.el)
(message "Loading MPM config...")
(require 'evil-setup)
(require 'mpm-preferences)
(require 'themes-setup)
(require 'mpm-core)
(require 'hydra-setup)
(require 'icons-setup)
(require 'fuzzy-find-setup)
(require 'lang-modes)
(require 'treesitter-setup)
(require 'markup-setup)
(require 'vcs-setup)
(require 'autocomplete-setup)
(require 'web-dev-setup)
(require 'lsp-setup)


(provide 'mpm-config)
;;; mpm-config.el ends here
