;; fuzzy-find-setup.el --- completion framework for searching -*- lexical-binding: t; -*-

;;; Commentary:
;; Your completion framework is the heart of your config that lets you fly everywhere

;;; Code:
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '(
     (file (styles basic partial-completion))
    )
  )
)

(use-package embark
    :ensure t
    :bind
    (("C-," . embark-act)
     :map minibuffer-local-map
     ("C-q" . embark-export)) ;; inspired by quickfix list exporting in vim
) 

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
)

(use-package consult
  :ensure t
  :config
  (mpm/leader-keys
     "/" '(consult-ripgrep :wk "Multi file text search")
     "*" '(consult-line-multi :wk "Multi buffer text search")
  )
)

;; wgrep combined ripgrep and/or silver searcher makes changing text in multiple places much easier
(use-package wgrep
  :ensure t
  :custom
  (wgrep-change-readonly-file t)
)

;; Managing Projects is great with the right project tools

;; find projects with custom discovery file and not just git
(defun mpm-override-dir (dir)
    (let ((root (locate-dominating-file dir mpm-projects-discovery-file)))
      (and root (cons 'transient root)))
)


;; override vcs for monorepos
;; Returns the parent directory containing a custom override file, if any,
;; to override the standard project.el detection logic when needed.
;; (defun mpm-project-override (dir)
;;   (let ((override (locate-dominating-file dir mpm-projects-override-file)))
;;     (if override
;;       (cons 'vc override)
;;       nil)))

;;; Code:
(use-package project
  :config
  ;; (add-hook 'project-find-functions #'mpm-project-override)
  (add-hook 'project-find-functions #'mpm-override-dir)
  ;; discover projects when none have been created
  (add-hook 'after-init-hook #'(lambda ()
                                 (let ((project-list-target-file (concat user-emacs-directory "projects")))
                                   (when (and (not (file-exists-p project-list-target-file)) (file-directory-p mpm-projects-dir))
                                     (message (format "'%s' file not found, will attempt to discover projects based on '%s'" project-list-target-file mpm-projects-dir))
                                     (project-remember-projects-under mpm-projects-dir)))))
  ;; project hydra
  (pretty-hydra-define hydra-project (:color red :title "🚀 Project 🚀" :quit-key "q")
    (
      "Finding"
      (("f" project-search "search")
      ("d" project-find-dir "find-dir"))

      "Search/Replace" ;; search and replace
      (("g" project-find-regexp "grep project")
      ("c" project-query-replace-regexp "replace"))

      "Switch"
      (("s" project-switch-project "switch project")
      ("b" project-switch-to-buffer "switch buffer"))

      "Finish"
      (("p" project-compile "compile")
      ("k" project-kill-buffers "kill project buffers"))
    )
  )

  (mpm/leader-keys
    "SPC" '(hydra-project/body :wk "Project Hydra")
    "f" '(project-find-file :wk "Project Find File/Buffer")
  )
)



(provide 'fuzzy-find-setup)
;;; fuzzy-find-setup.el ends here
