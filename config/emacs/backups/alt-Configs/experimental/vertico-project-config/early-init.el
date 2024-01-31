;; early-init.el -- startup before loading GUI  -*- lexical-binding: t; -*-

(setq load-prefer-newer t) ;; Avoid the pitfall of loading old bytecode instead of newer
;; (setenv "LSP_USE_PLISTS" "true") ;; supposed to make lsp faster

;;; Commentary:
;; MS-Windows can be slow and can give a lot of issues. This setting below at least fixes the issue of it not recognizing unicode characters and not letting your save your file.
(when (string-equal system-type "windows-nt")
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Code:

;; add all our components to the load path
(let ((default-directory  (concat user-emacs-directory "Components/")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'startup-component)

(provide 'early-init)
;;; early-init.el ends here

