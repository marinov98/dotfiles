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
    (:map evil-normal-state-map
     ("C-;" . embark-act)
     :map evil-visual-state-map
     ("C-;" . embark-act)
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
     "l s" '(consult-imenu :wk "Imenu")
     "*" '(mpm/word-grep-under-cursor :wk "Grep word")
  )
  (mpm/leader-keys
   :states '(visual)
   "*" '(mpm/grep-region :wk "Grep region")
  )
  :custom
  (consult-async-min-input 2)
  :config
  (add-to-list 'consult-fd-args "--hidden --exclude .git" t)

  (evil-global-set-key 'normal (kbd "gl") 'consult-line)
  (evil-global-set-key 'normal (kbd "gL") 'consult-line-multi)

  (defun mpm/grep-on-input ()
    "Grep based on input first, don't live grep"
    (interactive)
    (consult-ripgrep nil (read-string "Grep > "))
  )

  (defun mpm/word-grep-under-cursor ()
    "Pass the word under the cursor to grep"
    (interactive)
    (let ((grep-input (thing-at-point 'word)))
      (if grep-input
          (consult-ripgrep nil grep-input)
        (message "No word found under cursor!"))
      )
    )

  (defun mpm/grep-region ()
    "Grep the currently selected region."
    (interactive)
    (consult-ripgrep nil (mpm/get-visual-selection))
  )
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

  ;; utilize fuzzy finder for more consistency
  (when (package-installed-p 'consult)
    (advice-add #'project-find-regexp :override #'consult-ripgrep)
  )

  (defun mpm/symbol-grep-under-cursor ()
    "Grep Symbol under cursor"
     (interactive)
     (let ((grep-input (thing-at-point 'symbol)))
           (if grep-input
               (if (package-installed-p 'consult)
                 ;; usually advice-add takes care of it but not when the number of function arguments are different
                 (project-find-regexp nil grep-input)
                 (project-find-regexp grep-input)
               )
               (message "No symbol found under cursor!"))
     )
  )

  (defun mpm/kill-other-project-buffers ()
    "Kill all project buffers except current one"
    (interactive)
    (when (yes-or-no-p "Are you sure you want to kill other project buffers? ")
      (let* ((current-buffer (current-buffer))
             (project-buffers (project-buffers (project-current))))
        (cl-loop for buffer in project-buffers
                 do
                 (unless (eq buffer current-buffer)
                   (kill-buffer buffer)))))
   )



  (evil-global-set-key 'normal (kbd "C-p") 'project-find-file)
  (pretty-hydra-define hydra-project (:color red :title "🚀 Project 🚀" :quit-key "q")
    (
      "Finding"
      (("f" project-find-file "find file")
      ("d" project-find-dir "find-dir"))

      "Search/Replace"
      (("/" project-find-regexp "live grep")
      ("*" mpm/symbol-grep-under-cursor "grep symbol")
      ("s" project-search "search")
      ("r" project-query-replace-regexp "replace"))

      "Switch"
      (("l" project-switch-project "list/switch project")
      ("b" project-switch-to-buffer "switch buffer"))

      "Finish"
      (("c" project-compile "compile")
      ("k" mpm/kill-other-project-buffers "kill other project buffers")
      ("K" project-kill-buffers "kill project buffers"))
    )
  )

)


(provide 'fuzzy-find-setup)
;;; fuzzy-find-setup.el ends here
