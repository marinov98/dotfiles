;; web-dev-setup.el --- configurations related to Web Dev -*- lexical-binding: t; -*-

;;; Commentary:
;; Prepare Emacs for web dev

;;; Code:
(use-package web-mode
    :ensure t
    :mode
    (("\\.html?\\'"      . web-mode)
    ("\\.phtml\\'"       . web-mode)
    ("\\.tpl\\.php\\'"   . web-mode)
    ("\\.blade\\.php\\'" . web-mode)
    ("\\.[agj]sp\\'"     . web-mode)
    ("\\.as[cp]x\\'"     . web-mode)
    ("\\.erb\\'"         . web-mode)
    ("\\.ejs\\'"         . web-mode)
    ("\\.mustache\\'"    . web-mode)
    ("\\.djhtml\\'"      . web-mode))
    :custom
    ;; Indentation
    (web-mode-attr-indent-offset 2)
    (web-mode-markup-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-css-indent-offset 2)
    ;; Auto-closing
    (web-mode-auto-close-style 2)
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-auto-quoting t)
    ;; Highlighting
    (web-mode-enable-current-column-highlight t)
    (web-mode-enable-current-element-highlight t)
    :config
    (setq web-mode-enable-engine-detection t))

    (setq-default css-indent-offset 2) ;; web mode for some reason cancels css autocomplete so I have to configure css separately

(use-package prettier :ensure t)

(use-package rjsx-mode
    :ensure t
    :mode
    (("\\.jsx\\'"  . rjsx-mode))
    :init
    (setq-default rjsx-basic-offset 2))
    
(use-package json-mode
    :ensure t
    :commands json-mode
    :init
    (setq-default js-indent-level 2))

(use-package yaml-mode
    :ensure t
    :commands yaml-mode
    :mode (("\\.yml\\'" . yaml-mode)
           ("\\.yaml\\'" . yaml-mode)))
       
(use-package graphql-mode
    :ensure t
    :commands graphql-mode
    :mode
    (("\\.\\(gql\\|graphql\\)\\'" . graphql-mode)))
    
(use-package dockerfile-mode
    :ensure t
    :commands dockerfile-mode
    :mode
    (("Dockerfile'"       . dockerfile-mode)
    ("\\.Dockerfile\\'"  . dockerfile-mode)))
    
(use-package csv-mode
    :ensure t
    :commands csv-mode)


(use-package add-node-modules-path
    :ensure t
    :hook
    ((web-mode . add-node-modules-path)
    (rjsx-mode . add-node-modules-path)))

;; the extras that I never use but might one day
(use-package skewer-mode
    :disabled
    :commands skewer-mode run-skewer
    :config
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'js-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    (skewer-setup))

(use-package impatient-mode
    :disabled
    :ensure t)
  
(use-package emmet-mode
    :disabled
    :hook
    ((css-mode  . emmet-mode)
    (php-mode  . emmet-mode)
    (sgml-mode . emmet-mode)
    (rjsx-mode . emmet-mode)
    (web-mode  . emmet-mode)))

(provide 'web-dev-setup)
;;; web-dev-setup.el ends here
