;; init.el --- configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; in case early-init doesn't load (Emacs 27 introduced early-init)
(when (< emacs-major-version 27)
 (load "~/.emacs.d/early-init.el"))

;;; Code:
;; load our custom components except startup (used in early-init.el)
(require 'evil-component)
(require 'preferences-component)
(require 'look-and-feel-component)
(require 'search-component)
(require 'engines-component)
(require 'markup-component)
(require 'programming-component)
(require 'language-component)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-default-notes-file (concat org-directory mpm-notes-org))
 '(org-directory mpm-org-dir)
 '(package-selected-packages
   '(add-node-modules-path all-the-icons-completion all-the-icons-dired
                           catppuccin-theme clang-format company
                           consult csv-mode dap-mode dashboard
                           diminish dockerfile-mode doom-modeline
                           elixir-mode evil-collection evil-mc
                           evil-surround flycheck general git-modes
                           git-timemachine go-mode graphql-mode
                           inf-elixir json-mode ligature lsp-pyright
                           lsp-ui modern-cpp-font-lock neotree
                           orderless org-bullets prettier pretty-hydra
                           python-black pyvenv rainbow-delimiters
                           rainbow-mode react-snippets rjsx-mode
                           rustic treemacs-evil treemacs-magit
                           typescript-mode vertico wc-mode web-mode
                           wgrep which-key writegood-mode yaml-mode
                           yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
