;; treesitter-setup.el --- setup treesitter -*- lexical-binding: t; -*-

;;; Commentary:
;; treesitter should take syntax highlighting to the next level

;;; Code:
(when (>= emacs-major-version 29)
  (use-package treesit
    :hook
    ((c-ts-mode c++-ts-mode bash-ts-mode cmake-ts-mode toml-ts-mode rust-ts-mode go-ts-mode css-ts-mode yaml-ts-mode json-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
    :config
    (setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))))
     ;; I would uncomment below if you like -ts-mode update to the syntax highlighting (Some syntax changes are worse imo)
     ;; (setq major-mode-remap-alist
     ;;   '((yaml-mode . yaml-ts-mode)
     ;;     (bash-mode . bash-ts-mode)
     ;;     (js2-mode . js-ts-mode)
     ;;     (typescript-mode . typescript-ts-mode)
     ;;     (json-mode . json-ts-mode)
     ;;     (css-mode . css-ts-mode)
     ;;     (python-mode . python-ts-mode)))))

(provide 'treesitter-setup)
;;; treesitter-setup.el ends here
