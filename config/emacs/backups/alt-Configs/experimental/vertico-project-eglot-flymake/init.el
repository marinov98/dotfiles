;; init.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load (concat user-emacs-directory "early-init.el")))

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
