;; startup-component.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary
;; load all the packages in this component

;;; Code:
(message "Loading Startup Component...")

(require 'gc-setup)
(require 'clean-buffers-setup)
(require 'mpm-def)
(require 'package-manager-setup)

(add-hook 'after-init-hook #'marinov/reset-file-name-handler-alist)
(add-hook 'after-init-hook #'marinov/set-memory)

(message "Startup Component loaded successfully!")

(provide 'startup-component)
;;; startup-component.el ends here
