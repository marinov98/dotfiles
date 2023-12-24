;; python-setup.el --- configurations related to Python -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package python
    :custom
    (python-indent-guess-indent-offset t)
    (python-indent-guess-indent-offset-verbose nil)
    :config
    (setq-default python-basic-offset 4)
    (setq-default python-indent-offset 4))

;; venv support
(use-package pyvenv
    :ensure t
    :config
    (pyvenv-mode t))

;; if you need virtualenv support
(use-package virtualenvwrapper
    :disabled
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell))

;; python formatter of choice
(use-package python-black
    :after python
    :ensure t)

(provide 'python-setup)
;;; python-setup.el ends here
