;; mpm-custom.el --- custom configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load (concat user-emacs-directory "early-init.el")))

;;; Code:
;; load our custom components except startup (used in early-init.el)
(message "Loading MPM custom...")
(require 'startup-config)
(require 'mpm-lib)


(provide 'mpm-custom)
;;; mpm-custom.el ends here
