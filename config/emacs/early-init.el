;; early-init.el -- startup before loading GUI  -*- lexical-binding: t; -*-
;;; Commentary:
;; Load the up necessities before init.el
;;; Code:
;; Increase garbage threshold to boost startup time then reduce it after initialization is complete (done in mpm/set-memory function)
(setq gc-cons-threshold (* 1024 1024 200))

(message "Adding mpmacs dirs/subdirs to load path...")
(let ((default-directory  (concat user-emacs-directory "mpmacs/")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'mpm-custom)

(provide 'early-init)
;;; early-init.el ends here
