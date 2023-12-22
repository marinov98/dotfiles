;;; snippet-engine-setup.el --- completion engine of the config -*- lexical-binding: t; -*-

;;; Commentary:
;; YASnippet is a template system for Emacs.  It allows you to type an abbreviation and automatically expand it into function templates.

;;; Code:
(use-package yasnippet
    :after lsp-mode
    :ensure t
    :diminish yas-minor-mode
    :bind
    ((:map yas-minor-mode-map
    ("<tab>" . nil))) ;; changed for S-SPC y
    :config
    (yas-reload-all)
    (add-hook 'text-mode-hook #'yas-minor-mode)
    (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
    :after yasnippet
    :ensure t)

;; snippets for React.js
(use-package react-snippets
    :after yasnippet
    :ensure t)

(provide 'snippet-engine-setup)
;;; snippet-engine-setup.el ends here
