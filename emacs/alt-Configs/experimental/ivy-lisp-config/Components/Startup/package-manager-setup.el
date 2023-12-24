;; package-manager-setup.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; setup our package manager 

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			             ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-and-compile (setq use-package-expand-minimally t)))

(provide 'package-manager-setup)
;;; package-manager-setup.el ends here
