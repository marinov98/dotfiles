;; completion-framework-setup.el --- completion framework for searching -*- lexical-binding: t; -*-

;;; Commentary:
;; Your completion framework is the heart of your config that lets you fly everywhere

;;; Code:
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; don't care for this but might add it to my workflow later on
(use-package marginalia
  :disabled
  :after vertico
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind(:map evil-normal-state-map
         ("gs" . consult-ripgrep)))

(use-package embark
    :ensure t
    :bind(:map minibuffer-local-map
           ("C-q" . embark-export))) ;; inspired by quickfix list exporting in vim
 

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; wgrep combined ripgrep and/or silver searcher makes changing text in multiple places much easier
(use-package wgrep
  :ensure t
  :custom
  (wgrep-change-readonly-file t))


(provide 'completion-framework-setup)
;;; completion-framework-setup.el ends here
