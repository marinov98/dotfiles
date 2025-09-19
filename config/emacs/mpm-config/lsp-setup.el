;;; lsp-setup.el --- syntax checker -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for lsp including syntax checking, lsp-mode, etc...

;;; Code:
(use-package flycheck
     :ensure t
     :hook (after-init . global-flycheck-mode)
     :custom-face
     (flycheck-info ((t (:underline (:style wave :color "#87cefa")))))
     (flycheck-warning ((t (:underline (:style wave :color "#ffb95c")))))
     (flycheck-error ((t (:underline (:style wave :color "#cc0202")))))
     :custom
     (flycheck-display-errors-delay 0.5)
     :bind
     (:map evil-normal-state-map
           ("]d" . flycheck-next-error)
           ("[d" . flycheck-previous-error))
     :general
     (mpm/leader-keys
        "d l" '(flycheck-list-errors :wk "Diagnostics List Errors")
        "d g" '(flycheck-explain-error-at-point :wk "Display Diagnostics at point")
        "c f" '(:ignore t :wk "Flycheck commands")
        "c f e" '(flycheck-display-error-at-point :wk "Flycheck display error")
        "c f s" '(flycheck-select-checker :wk "Flycheck select checker")
        "c f l" '(flycheck-list-errors :wk "Diagnostics List Errors") ;; alternative binding
        "c f d" '(flycheck-disable-checker :wk "Flycheck disable checker")
        "c f h" '(flycheck-describe-checker :wk "Flycheck describe checker")
        "c f m" '(flycheck-mode :wk "Flycheck mode")
        "c f M" '(flycheck-manual :wk "Flycheck manual")
        "c f v" '(flycheck-verify-setup :wk "Flycheck verify setup")
     )
)

(use-package lsp-mode
       :ensure t
       :commands (lsp lsp-deferred)
       :hook
       (
        ((c++-mode c-mode css-mode yaml-mode json-mode js-mode js2-mode rjsx-mode typescript-mode web-mode) . lsp-deferred)
        ((bash-ts-mode rust-ts-mode go-ts-mode css-ts-mode yaml-ts-mode json-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . lsp-deferred) ;; treesitter modes
       )
       :bind
       (
        (:map evil-normal-state-map
         ("gry" . lsp-find-type-definition)
         ("grd" . lsp-find-declaration)
         ("grn" . lsp-rename)
         ("gr?" . lsp-describe-thing-at-point)
         ("grh" . lsp-signature-activate)
         ("gra" . lsp-execute-code-action)
         :map evil-insert-state-map
         ("C-h" . lsp-signature-activate)
        )
       )
       :general-config
       (mpm/leader-keys
        "l" '(:ignore t :wk "LSP")
        "l f" '(lsp-format-buffer :wk "LSP Format")
        "l a" '(lsp-execute-code-action :wk "LSP Code Action")
        "l o" '(lsp-organize-imports :wk "LSP Organize Imports")
       )
       :custom
       (lsp-auto-guess-root nil)
       (lsp-log-io nil)
       (lsp-idle-delay 0.5)
       (lsp-eldoc-enable-hover nil) ;; graphical bloat imo, but can be good
       (lsp-signature-auto-activate nil) ;; (This tends to cause problems and is SUPER slow ESP on MS Windows) you could manually request them via 'lsp-signature-activate'
       (lsp-headerline-breadcrumb-enable nil) ;; graphical bloat, don't need but useful to the right person
       (lsp-enable-folding nil)
       (lsp-enable-symbol-highlighting nil)
       (lsp-enable-text-document-color nil)
       (lsp-file-watch-threshold 5000)
       (lsp-prefer-flymake nil)
       (lsp-io-messages-max nil)
       :config
       (add-hook 'after-init-hook
                   #'(lambda () ;; in case I disable lsp-ui remap g prefix keys to regular lsp
                       (unless (package-installed-p 'lsp-ui)
                         (define-key evil-normal-state-map (kbd "gd") 'lsp-find-definition)
                         (define-key evil-normal-state-map (kbd "gri") 'lsp-goto-implementation)
                         (define-key evil-normal-state-map (kbd "grr") 'lsp-find-references)
                        )
                     ))
)

(use-package lsp-ui
       :ensure t
       :commands lsp-ui-mode
       :hook (lsp-mode . lsp-ui-mode)
       :bind
       (:map evil-normal-state-map
         ("gd" . lsp-ui-peek-find-definitions)
         ("gri" . lsp-ui-peek-find-implementation)
         ("grr" . lsp-ui-peek-find-references))
       (:map lsp-ui-peek-mode-map
         ("C-n" . lsp-ui-peek--select-next)
         ("C-p" . lsp-ui-peek--select-prev))
       :custom
       (lsp-ui-doc-enable t)
       (lsp-ui-doc-position 'at-point)
       (lsp-ui-doc-include-signature t)
       (lsp-ui-doc-use-childframe t)
       (lsp-ui-doc-show-with-cursor nil)
       (lsp-ui-doc-show-with-mouse t)
       (lsp-ui-sideline-enable t)
       (lsp-ui-sideline-ignore-duplicates t)
       (lsp-ui-sideline-show-diagnostics t)
       (lsp-ui-sideline-show-symbol nil)
       (lsp-ui-sideline-show-hover nil)
       (lsp-ui-peek-enable t)
       (lsp-ui-peek-list-width 60)
       (lsp-ui-peek-peek-height 25)
       :general-config
       (mpm/leader-keys
        "l g" '(lsp-ui-doc-glance :wk "Hover")
        "l w" '(:ignore t :wk "LSP Workspace")
        "l w d" '(lsp-ui-find-workspace-symbol :wk "Document Workspace Symbols")
       )
)

(use-package lsp-pyright
    :ensure t
    :hook ((python-mode python-ts-mode) . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
)

(use-package lsp-java
     :disabled
     :hook ((java-mode java-ts-mode) . lsp-deferred)
)

(use-package consult-lsp
  :ensure t
  :after (flycheck lsp-mode consult)
  :general-config
  (mpm/leader-keys
    "d l" '(consult-lsp-diagnostics :wk "List Diagnostics") ;; overwrite flycheck binding
    "l s" '(consult-lsp-file-symbols :wk "File Symbols")
    "l w s" '(consult-lsp-symbols :wk "Workspace Symbols")
  )
)


(provide 'lsp-setup)
;;; lsp-setup.el ends here
