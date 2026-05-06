;; init.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load (concat user-emacs-directory "early-init.el")))

;;; Code:
;; load our config
(require 'mpm-config)


(provide 'init)
;;; init.el ends here
