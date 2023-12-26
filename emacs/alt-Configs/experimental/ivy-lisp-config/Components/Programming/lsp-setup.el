;; lsp-setup.el --- Language Server Protocol Config -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP stands for Language Server Protocal and makes setting up autocompletion and syntax checking easy.

;;; Code:
   (use-package lsp-mode
       :ensure t
       :commands (lsp lsp-deferred)
       :hook
       (((c++-mode c-mode css-mode yaml-mode json-mode js-mode js2-mode rjsx-mode typescript-mode web-mode) . lsp-deferred)
        ((bash-ts-mode rust-ts-mode go-ts-mode css-ts-mode yaml-ts-mode json-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . lsp-deferred)) ;; treesitter modes
       :bind
       (:map evil-normal-state-map
         ("gy" . lsp-find-type-definition)
         ("gh" . lsp-describe-thing-at-point))
       :custom
       (lsp-auto-guess-root nil)
       (lsp-log-io nil)
       (lsp-idle-delay 0.3)
       (lsp-eldoc-enable-hover nil) ;; graphical bloat imo, but can be good
       (lsp-signature-auto-activate nil) ;; (This tends to cause problems and is SUPER slow ESP on MS Windows) you could manually request them via 'lsp-signature-activate'
       (lsp-headerline-breadcrumb-enable nil) ;; graphical bloat, don't need but useful to the right person
       (lsp-enable-folding nil)
       (lsp-enable-text-document-color nil)
       (lsp-file-watch-threshold 5000)
       (lsp-prefer-flymake nil)
       (lsp-io-messages-max nil)
       :config
       (add-hook 'after-init-hook
                   #'(lambda () ;; in case I disable lsp-ui remap g prefix keys to regular lsp
                       (unless (package-installed-p 'lsp-ui)
                         (define-key evil-normal-state-map (kbd "gd") 'lsp-find-definition)
                         (define-key evil-normal-state-map (kbd "gi") 'lsp-goto-implementation)
                         (define-key evil-normal-state-map (kbd "gr") 'lsp-find-references)))))

   (use-package lsp-ui
       :ensure t
       :commands lsp-ui-mode
       :hook (lsp-mode . lsp-ui-mode)
       :bind
       (:map evil-normal-state-map
         ("gd" . lsp-ui-peek-find-definitions)
         ("gi" . lsp-ui-peek-find-implementation)
         ("gr" . lsp-ui-peek-find-references))
       (:map lsp-ui-peek-mode-map
         ("M-j" . lsp-ui-peek--select-next)
         ("M-k" . lsp-ui-peek--select-prev))
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
       (lsp-ui-peek-peek-height 25))

(use-package lsp-pyright
    :ensure t
    :hook ((python-mode python-ts-mode) . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

 (use-package lsp-java
     :disabled
     :hook ((java-mode java-ts-mode) . lsp-deferred))
     ;; :config ;; TODO: properly configure this if I ever use Java / Or don't use this at all
     ;; (setq lsp-java-vmargs
     ;;         (list
     ;;            "-noverify"
     ;;            "-Xmx1G"
     ;;            "-XX:+UseG1GC"
     ;;            "-XX:+UseStringDeduplication"
     ;;            "-javaagent:/path/to/lombok-1.18.6.jar"))) ;; lombok support (TODO: make lombok jar a variable)

(provide 'lsp-setup)
;;; lsp-setup.el ends here
