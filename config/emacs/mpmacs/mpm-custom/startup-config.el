;;; startup-config.el --- configure and optimize garbage collection -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs startup can be optimizied by setting and resetting garbage collection values

;; Code:
;; Increase garbage threshold to boost startup time then reduce it after initialization is complete (done in mpm/set-memory function)
(setq gc-cons-threshold (* 1024 1024 100))

;; Temporarily disable the file name handler as we dont need it on startup
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


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


(provide 'startup-config)
;;; startup-config.el ends here
