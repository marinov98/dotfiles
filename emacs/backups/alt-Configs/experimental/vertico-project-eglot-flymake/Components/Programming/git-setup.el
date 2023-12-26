;; git-setup.el --- setup-git -*- lexical-binding: t; -*-

;;; Commentary:
;; setup git related packages.

;;; Code:
;; hydra takes care of my magit bindings
(use-package magit :ensure t)

(use-package git-commit
    :after magit
    :custom
    (git-commit-summary-max-length 50) ;; in accordance with https://chris.beams.io/posts/git-commit/
    :config
    (setq git-commit-style-convention-checks
          '(non-empty-second-line
          overlong-summary-line)))

(use-package git-modes
    :ensure t
    :mode (("\\.gitconfig\\'" . gitconfig-mode)
           ("\\.gitignore\\'" . gitignore-mode)
           ("\\.dockerignore\\'" . gitignore-mode))) ;; syntax from gitignore is more or less identical to that of .dockerignore

(use-package magit-repos
    :after magit
    :commands magit-list-repositories
    :config
    (when (file-directory-p mpm-projects-dir)
      (setq magit-repository-directories `((,mpm-projects-dir . 1)))))

(use-package git-timemachine
    :ensure t
    :commands git-timemachine)

;; Smerge mode deals with merge conflicts in git.
(use-package smerge-mode
    :after hydra
    :hook (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body)))))

(provide 'git-setup)
;;; git-setup.el ends here
