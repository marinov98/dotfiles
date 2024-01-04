;;; auto-complete-engine-setup.el --- completion engine of the config -*- lexical-binding: t; -*-

;;; Commentary:
;; This is one of the major completion engines in Emacs

;;; Code:
(use-package company
    :ensure t
    :after lsp-mode
    :hook (prog-mode . company-mode)
    :bind
    (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("M-j" . company-select-next)
        ("M-k"  . company-select-previous))
    (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-tooltip-limit 8) ; how many candidates to show
    (company-idle-delay 0.3) ; How much to delay to completion
    (company-minimum-prefix-length 2) ;; show completions after 2 chars
    (company-selection-wrap-around t) ;; goes to start of selection if you reached the bottom
    (company-require-match 'never)) ;; dont need to pick a choice

;; Minor Mode which shows you, in the echo area, the argument list of the function call you are currently writing. Very handy
(use-package eldoc
    :diminish eldoc-mode
    :hook (emacs-lisp-mode . eldoc-mode)
    :config
    (global-eldoc-mode -1))

(provide 'autocomplete-engine-setup)
;;; autocomplete-engine-setup.el ends here
