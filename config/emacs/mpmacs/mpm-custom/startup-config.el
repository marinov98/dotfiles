;;; startup-config.el --- configure and optimize garbage collection -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs startup can be optimizied by setting and resetting garbage collection values

;;; Code:
(setq load-prefer-newer t) ;; Avoid the pitfall of loading old bytecode instead of newer
(setenv "LSP_USE_PLISTS" "true") ;; supposed to make lsp faster


;; Temporarily disable the file name handler as we dont need it on startup
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(when (string-equal system-type "windows-nt")
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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
