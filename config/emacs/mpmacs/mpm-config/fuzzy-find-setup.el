;; fuzzy-find-setup.el --- completion framework for searching -*- lexical-binding: t; -*-

;;; Commentary:
;; Your completion framework is the heart of your config that lets you fly everywhere

;;; Code:
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :hook (after-init . vertico-mode)
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
  :general-config
  (mpm/leader-keys
     "/" '(mpm/grep-on-input :wk "Grep on user input")
     "f" '(consult-fd :wk "Find File")
     "l i" '(consult-imenu :wk "Imenu")
     "*" '(mpm/grep-under-cursor :wk "Grep under cursor")
  )
  (mpm/leader-keys
   :states '(visual)
   "*" '(mpm/grep-region :wk "Grep region")
  )
  :custom
  (consult-async-min-input 2)
  :config
  (add-to-list 'consult-fd-args "--hidden --exclude .git" t)

  (define-key evil-normal-state-map (kbd "gl") 'consult-line)
  (define-key evil-normal-state-map (kbd "gL") 'consult-line-multi)

  (defun mpm/grep-on-input ()
         "Grep based on input first, don't live grep"
         (interactive)
         (consult-ripgrep nil (read-string "Grep > "))
  )

  (defun mpm/grep-under-cursor ()
    "Pass the symbol (or word if not a symbol)
     under the cursor to grep"
     (interactive)
     (let ((grep-input (or (thing-at-point 'symbol)
                           (thing-at-point 'word))))
           (if grep-input
               (consult-ripgrep nil grep-input)
               (message "No symbol or word found under cursor!"))
          )
  )

  (defun mpm/grep-region ()
    "Grep the currently selected region."
    (interactive)
    (if (region-active-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (grep-input (buffer-substring start end)))
          (consult-ripgrep nil grep-input))
      (message "No region selected!")))
)

;; wgrep combined ripgrep and/or silver searcher makes changing text in multiple places much easier
(use-package wgrep
  :ensure t
  :custom
  (wgrep-change-readonly-file t)
  :general-config
  (mpm/leader-keys
     "r w" '(:ignore t :wk "Wgrep Actions")
     "r w c" '(wgrep-change-to-wgrep-mode :wk "change to wgrep mode")
     "r w e" '(wgrep-exit :wk "Wgrep Exit")
     "r w a" '(wgrep-abort-changes :wk "Wgrep Abort Changes")
     "r w f" '(wgrep-finish-edit :wk "Wgrep Finish Edit")
  )
)

;; Managing Projects is great with the right project tools
(use-package project
  :general-config
  (mpm/leader-keys
    "SPC" '(hydra-project/body :wk "Project Hydra")
  )
  :config
  ;; find projects with custom discovery file and not just git
  (defun mpm-override-dir (dir)
      (let ((root (locate-dominating-file dir mpm-projects-discovery-file)))
      (and root (cons 'transient root)))
  )
  (add-hook 'project-find-functions #'mpm-override-dir)
  ;; discover projects when none have been created
  (add-hook 'after-init-hook #'(lambda ()
                                 (let ((project-list-target-file (concat user-emacs-directory "projects")))
                                   (when (and (not (file-exists-p project-list-target-file)) (file-directory-p mpm-projects-dir))
                                     (message (format "'%s' file not found, will attempt to discover projects based on '%s'" project-list-target-file mpm-projects-dir))
                                     (project-remember-projects-under mpm-projects-dir)))))
  (pretty-hydra-define hydra-project (:color red :title "🚀 Project 🚀" :quit-key "q")
    (
      "Finding"
      (("f" project-find-file "find file")
      ("d" project-find-dir "find-dir"))

      "Search/Replace" ;; search and replace
      (("/" project-find-regexp "grep project")
      ("s" project-search "search project")
      ("r" project-query-replace-regexp "replace"))

      "Switch"
      (("l" project-switch-project "list/switch project(s)")
      ("b" project-switch-to-buffer "switch buffer"))

      "Finish"
      (("c" project-compile "compile")
      ("k" project-kill-buffers "kill project buffers"))
    )
  )

)


(provide 'fuzzy-find-setup)
;;; fuzzy-find-setup.el ends here
