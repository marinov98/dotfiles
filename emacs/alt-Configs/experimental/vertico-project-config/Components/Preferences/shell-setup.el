;; shell-setup.el --- configuration of shell paths and eshell -*- lexical-binding: t; -*-

;;; Commentary:
;; configure eshell and shell path

;;; Code:
(use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x)) ;; check if its mac
      (exec-path-from-shell-initialize)))

(use-package eshell
     :ensure t
     :bind (("C-`" . eshell))
     :custom
     (eshell-hist-ignoredups t)
     (eshell-scroll-to-bottom-on-input t)
     (eshell-destroy-buffer-when-process-dies t)
     (eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
     :config
     ;; configuration found from this link: https://superuser.com/questions/890937/how-to-show-git-branch-in-emacs-shell
     (defun git-prompt-branch-name ()
       "Get current git branch name"
       (let ((args '("symbolic-ref" "HEAD" "--short")))
           (with-temp-buffer
           (apply #'process-file "git" nil (list t nil) nil args)
           (unless (bobp)
               (goto-char (point-min))
               (buffer-substring-no-properties (point) (line-end-position))))))

       (defun 4lex1v:eshell-prompt ()
       (let ((branch-name (git-prompt-branch-name)))
           (concat
           "\n# " (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) "\n"
           (if branch-name (format "git:(%s) >> " branch-name) ">> ")
           )))         

       (setq eshell-prompt-function #'4lex1v:eshell-prompt
           eshell-prompt-regexp ".*>>+ "))

(provide 'shell-setup)
;;; shell-setup.el ends here
