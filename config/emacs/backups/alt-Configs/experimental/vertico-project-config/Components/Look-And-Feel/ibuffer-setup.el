;; ibuffer-setup.el --- customize ibuffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced buffer management. This is a native Emacs feature
;; Organizaing your buffers can be made easier with a well configured ibuffer

;;; Code:
(use-package ibuffer
    :hook (ibuffer-mode . (lambda ()
                            (ibuffer-auto-mode 1)
                            (ibuffer-switch-to-saved-filter-groups "default")))
    :custom
    (ibuffer-show-empty-filter-groups nil)
    :config
    (setq ibuffer-saved-filter-groups
        (quote (("default"
                ("Ranger" (mode . ranger-mode))
                ("Org" (name . "^.*org$"))
                ("Text" (or
                        (mode . markdown-mode)
                        (mode . text-mode)
                        (mode . pdf-view-mode)
                        (mode . LaTeX-mode)))
                ("Git" (or 
                        (mode . gitignore-mode)               
                        (mode . gitconfig-mode)               
                        (mode . magit-mode)))
                ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
                ("Elisp" (mode . emacs-lisp-mode))
                ("Programming" (or ;; I dont have all of these modes but just in case for the future...
                                (mode . python-mode)
                                (mode . ruby-mode)
                                (mode . go-mode)
                                (mode . rust-mode)
                                (mode . swift-mode)
                                (mode . objc-mode)
                                (mode . dart-mode)
                                (mode . haskell-mode)
                                (mode . csharp-mode)
                                (mode . scala-mode)
                                (mode . clojure-mode)
                                (mode . java-mode)
                                (mode . c-mode)
                                (mode . c++-mode)))
                ("Web Dev" (or
                            (mode . web-mode)
                            (mode . rjsx-mode)
                            (mode . css-mode)
                            (mode . js-mode)
                            (mode . typescript-mode)
                            (mode . js2-mode)))
                ("Data" (or
                        (mode . csv-mode)
                        (mode . json-mode)
                        (mode . sql-mode)
                        (mode . yaml-mode)
                        (mode . graphql-mode)
                        (mode . dockerfile-mode)))
                ("Help" (or
                        (name . "\*Help\*")
                        (name . "\*info\*")))
                ("Emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$"))))))))

(provide 'ibuffer-setup)
;;; ibuffer-setup.el ends here
