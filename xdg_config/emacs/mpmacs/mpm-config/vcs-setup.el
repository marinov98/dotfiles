;; vcs-setup.el --- setup version control -*- lexical-binding: t; -*-

;;; Commentary:
;; setup git related packages.

;;; Code:
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-list-repositories)
)

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
    :commands magit-list-repositories
    :init
    (when (file-directory-p mpm-projects-dir)
      (setq magit-repository-directories `((,mpm-projects-dir . 1))))
)

(use-package git-timemachine
    :ensure t
    :commands git-timemachine)

;; Smerge mode deals with merge conflicts in git.
(use-package smerge-mode
    :after hydra
    :init
    (pretty-hydra-define hydra-smerge (:color pink :title "⚡ Smerge ⚡" :quit-key "q")
      (
        "Move"
        (("n" smerge-next)
        ("p" smerge-prev))

        "Keep"
        (("b" smerge-keep-base)
        ("u" smerge-keep-upper)
        ("l" smerge-keep-lower)
        ("a" smerge-keep-all)
        ("RET" smerge-keep-current))

        "Diff"
        (("<" smerge-diff-base-upper)
        ("=" smerge-diff-upper-lower)
        (">" smerge-diff-base-lower)
        ("R" smerge-refine)
        ("E" smerge-ediff))


        "Other"
        (("C" smerge-combine-with-next)
        ("r" smerge-resolve)
        ("k" smerge-kill-current)
        ("ZZ" (lambda ()
                (interactive)
                (save-buffer)
                (bury-buffer))
         "Save and bury buffer" :color blue))
      )
    )
    :hook (smerge-mode . hydra-smerge/body)
)

(defhydra hydra-git (:color red)
    "⏳ Git ⏳"
    ("g" magit-status "magit")
    ("d" magit-dispatch "dispatch")
    ("l" magit-list-repositories "list repos")
    ("t" git-timemachine "timemachine")
    ("q" nil "quit" :color blue)
)

(mpm/leader-keys
  "g" '(hydra-git/body :wk "Git Hydra")
)

(provide 'vcs-setup)
;;; vcs-setup.el ends here
