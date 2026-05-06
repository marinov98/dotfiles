;;; autocomplete-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for Completion backend that will provide autocomplete

;;; Code:
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto nil) ;; auto can be dangerous in the wrong place
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-popupinfo-delay nil)
  :bind
  (:map corfu-map
    ("RET" . nil)
    ("C-n" . corfu-next)
    ("C-p" . corfu-previous)
    ("<down>" . corfu-next)
    ("<up>" . corfu-previous)
    ("<escape>" . evil-collection-corfu-quit-and-escape)
    ("C-[" . evil-collection-corfu-quit-and-escape)
    ("<tab>" . corfu-insert)
    ("C-/" . corfu-popupinfo-toggle)
  )
  :config
  (corfu-popupinfo-mode)
  (defun mpm/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))
  )
  (add-hook 'lsp-completion-mode-hook #'mpm/corfu-setup-lsp)
  (setq lsp-completion-provider :none) ; use corfu instead of lsp-completions
  (evil-global-set-key 'insert (kbd "C-SPC") 'completion-at-point)
)

(use-package cape
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
)

(use-package yasnippet-capf
  :ensure t
  :after cape
  :init
  (defun mpm/add-yasnippet-capf ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  :hook
  (
   (emacs-lisp-mode . mpm/add-yasnippet-capf)
   (org-mode . mpm/add-yasnippet-capf)
  )
)

(provide 'autocomplete-setup)
;;; autocomplete-setup.el ends here
