;;; treesitter-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for Completion backend that will provide autocomplete

;;; Code:

(defun mpm/corfu-setup-lsp ()
  "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (lsp-completion-provider :none) ; Use corfu instead for lsp completions
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<tab>" #'corfu-insert
 )
  :init (global-corfu-mode)
  :hook (lsp-completion-mode . mpm/corfu-setup-lsp) ; Use corfu for lsp completion
)

(provide 'autocomplete-setup)
;;; autocomplete-setup.el ends here
