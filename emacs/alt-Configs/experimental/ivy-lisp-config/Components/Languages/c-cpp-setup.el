;; c-cpp-setup.el --- configurations related to C/C++ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package cc-mode
  :init
  (setq-default c-basic-offset 4))

(use-package cpp
  :config
  ;; disable other checkers since we only want to utilize clangd language server
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;; enable modern font lock for >=c++11
(use-package modern-cpp-font-lock
    :ensure t
    :diminish modern-c++-font-lock-mode
    :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package clang-format
   :ensure t)

(provide 'c-cpp-setup)
;;; c-cpp-setup.el ends here
