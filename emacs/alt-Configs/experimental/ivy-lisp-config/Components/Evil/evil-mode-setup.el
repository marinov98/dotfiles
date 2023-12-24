;; evil-mode-setup.el --- evil-mode entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable evil mode

;;; Code:
;; emacs' undo-redo can be used as an option after emacs 28 so we don't need any extra undo packages in that case
(when (< emacs-major-version 28)
  (use-package undo-tree
    :disabled
    :init
    (global-undo-tree-mode)
    :config
    (with-eval-after-load 'undo-tree
      (setq undo-tree-auto-save-history nil)))

	(use-package undo-fu
	  :ensure t
	  :config
	  (global-unset-key (kbd "C-z"))
	  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
	  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
	  ;; On modern systems you may wish to use much higher limits.
	  (setq undo-limit (* 1024 1024 32)) ; 32mb.
	  (setq undo-strong-limit (* 1024 1024 48)) ; 48mb.
	  (setq undo-outer-limit (* 1024 1024 480)))) ; 480mb)


(use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
	(if (< emacs-major-version 28)
	      (setq evil-undo-system 'undo-fu)
        (setq evil-undo-system 'undo-redo))
    :bind
    ((:map evil-normal-state-map
       ("C-/" . comment-line)))
    :config
    (evil-mode 1))

(use-package evil-collection
    :after evil
    :ensure t
    :custom
    (evil-collection-company-use-tng nil) ;; I hacked this already (Personal preference)
    (evil-collection-setup-debugger-keys nil) ;; no need for this (Again.. Personal preference)
    :config
    (evil-collection-init))

(provide 'evil-mode-setup)
;;; evil-mode-setup.el ends here
