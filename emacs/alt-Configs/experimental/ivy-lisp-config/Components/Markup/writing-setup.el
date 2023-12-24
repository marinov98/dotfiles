;; writing-setup.el --- md config -*- lexical-binding: t; -*-

;;; Commentary:
;; All these packages are best enabled locally, should help when writing essays

;;; Code:
(use-package flyspell
    :ensure t
    :commands (ispell-change-dictionary
               ispell-word
               flyspell-buffer
               flyspell-mode
               flyspell-region)
    :bind
    (:map flyspell-mode-map
    ("C-M-i" . nil))) ;; messes with org autocomplete

(use-package wc-mode
    :ensure t
    :commands wc-mode
    :config
    (global-set-key "\C-cw" 'wc-mode))

(use-package writegood-mode
    :ensure t
    :commands writegood-mode
    :bind ("C-x w" . writegood-mode)) ;; messes with org snippets dont enable by default in org

(provide 'writing-setup)
;;; writing-setup.el ends here
