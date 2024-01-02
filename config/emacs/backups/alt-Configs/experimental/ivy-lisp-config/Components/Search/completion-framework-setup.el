;; completion-framework-setup.el --- completion framework for searching -*- lexical-binding: t; -*-

;;; Commentary:
;; Your completion framework is the heart of your config that lets you fly everywhere

;;; Code:

;; Ivy
(use-package ivy
    :ensure t
    :diminish ivy-mode
    :custom
    (ivy-display-style 'fancy)
    (ivy-count-format "(%d/%d) ")
    (ivy-format-function 'ivy-format-function-line)
    :hook (after-init . ivy-mode)
    :config
    (setq enable-recursive-minibuffers t))

;; Swiper
(use-package swiper
    :ensure t
    :custom
    (swiper-action-recenter t)
    (swiper-goto-start-of-match t)
    (swiper-include-line-number-in-search t)
    :bind
    (("C-s" . swiper-isearch)
    ("C-c C-r" . ivy-resume)
    :map evil-normal-state-map
    ("/" . swiper-isearch)
    ("*" . swiper-thing-at-point)))

  ;; Counsel
(use-package counsel
    :ensure t
	:hook (ivy-mode . counsel-mode)
    :bind
    (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-x b" . counsel-switch-buffer)
    ("M-y" . counsel-yank-pop)
    :map evil-normal-state-map
    ("gs" . counsel-rg)
    :map ivy-minibuffer-map
    ("C-q" . ivy-occur) ;; inspired by vim quickfix list binding
    ("M-j" . ivy-next-line)
    ("M-k" . ivy-previous-line))
    :custom
    (counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
    (counsel-rg-base-command "rg -S --no-heading --line-number --color never %s .")
    (counsel-ag-base-command "ag -S --nocolor --nogroup %s")
    (counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")
    (ivy-initial-inputs-alist nil) ;; removes starting ^ regex in M-x
    (counsel-find-file-at-point t))

;; wgrep combined counsel-rg and/or counsel-ag makes changing text in multiple places much easier
(use-package wgrep
  :ensure t
  :custom
  (wgrep-change-readonly-file t))


(provide 'completion-framework-setup)
;;; completion-framework-setup.el ends here
