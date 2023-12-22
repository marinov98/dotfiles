;; project-management-tool-setup.el --- framework for projects -*- lexical-binding: t; -*-

;;; Commentary:
;; Managing Projects is great with the right project tools


;;; Code:
(use-package projectile
    :ensure t
    :diminish projectile-mode
    :bind 
    (("C-c p" . projectile-command-map))
    :custom 
    (projectile-sort-order 'recently-active)
    (projectile-auto-discover nil) ;; Use 'projectile-discover-projects-in-search-path' manually instead
    (projectile-globally-ignored-file-suffixes '(".pyc" ".o"))
    :init
    (when (file-directory-p mpm-projects-dir)
      (setq projectile-project-search-path `(,mpm-projects-dir)))
    :config
    (dolist (file '(".DS_Store")) ;; add/remove any files
      (add-to-list 'projectile-globally-ignored-files file))
    (dolist (dir '("^\\venv$" "^\\venv_dev$" "^\\node_modules$")) ;; add/remove any directories 
      (add-to-list 'projectile-globally-ignored-directories dir))
    (setq projectile-indexing-method (if (memq system-type '(ms-dos windows-nt cygwin)) ;; ensure alien on anything not windows related
                                           'native
                                         'alien)))

;; Counsel-Projectile (I utilize counsel projectile bindings in my hydra-projectile)
(when (package-installed-p 'ivy)
	(use-package counsel-projectile
	  :ensure t
	  :config (counsel-projectile-mode)))

(provide 'project-management-tool-setup)
;;; project-management-tool-setup.el ends here
