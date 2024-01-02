;; word-window-searching.el --- packages for word/window searching -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package ace-window
     :ensure t
     :init
     (global-set-key (kbd "C-:") 'ace-window)
     (setq aw-background nil)
     (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(provide 'word-window-search-setup)
;;; word-window-search-setup.el ends here
