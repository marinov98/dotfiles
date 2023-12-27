;; avy-ace-setup.el --- avy and ace-window join the config battle -*- lexical-binding: t; -*-

;;; Commentary:
;; I did not think anything of this package until I actively used it in my workflow.  This package proved to be nothing but amazing for jumping around text.  Its inspiration comes from the vim package easymotion.  This package really boosts your ability to fly around text even with the already fast motion vim keybindings.  I utilize the keybindings in a hydra and a personal keybinding just because its so good.  Check the official repository If you want to know more.

;;; Code:
(use-package avy
    :disabled
    :custom
    (avy-background t)
    (avy-timeout-seconds 0.4))

(use-package ace-window
     :ensure t
     :init
     (global-set-key (kbd "C-:") 'ace-window)
     (setq aw-background nil)
     (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(provide 'avy-ace-setup)
;;; avy-ace-setup.el ends here
