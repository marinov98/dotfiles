;; mpm-custom.el --- custom configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; load our custom components except startup (used in early-init.el)
(message "Loading MPM custom...")
(require 'startup-config)
(require 'mpm-lib)

(add-hook 'after-init-hook #'mpm/clean-after-buffers) ;; comment this variable when debugging
(add-hook 'after-init-hook #'mpm/reset-file-name-handler-alist)
(add-hook 'after-init-hook #'mpm/set-memory)

(provide 'mpm-custom)
;;; mpm-custom.el ends here
