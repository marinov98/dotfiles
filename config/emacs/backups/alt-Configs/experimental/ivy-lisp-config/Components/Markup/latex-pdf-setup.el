;; latex-pdf-setup.el --- latex-pdf config -*- lexical-binding: t; -*-

;;; Commentary:
;; pdf and latex setup
;; I still actually prefer Overleaf for latex editing…Hoping to just use Emacs for it one day

;;; Code:
(use-package pdf-view
	     :disabled
		 :ensure pdf-tools
		 :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
		 :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
		 :magic ("%PDF" . pdf-view-mode)
		 :bind
		 (:map pdf-view-mode-map
		 ("C-s" . isearch-forward))
		 :init
		 (setq pdf-annot-activate-created-annotations t))

(use-package tex
    :disabled
    :ensure auctex
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil))
   
(use-package company-auctex
    :disabled
    :after (auctex company)
    :config
    (company-auctex-init))

(use-package company-math
    :disabled
    :after (auctex company)
    :config
    (add-to-list 'company-backends 'company-math-symbols-unicode))

(provide 'latex-pdf-setup)
;;; latex-pdf-setup.el ends here


