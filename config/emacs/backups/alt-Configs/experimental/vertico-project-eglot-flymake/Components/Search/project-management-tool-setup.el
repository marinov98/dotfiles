;; project-management-tool-setup.el --- framework for projects -*- lexical-binding: t; -*-

;;; Commentary:
;; Managing Projects is great with the right project tools

;; find projects with custom discovery file and not just git
(defun mpm-override-dir (dir)
    (let ((root (locate-dominating-file dir mpm-projects-discovery-file)))
      (and root (cons 'transient root))))


;; override vcs for monorepos
;; Returns the parent directory containing a custom override file, if any,
;; to override the standard project.el detection logic when needed.
(defun mpm-project-override (dir)
  (let ((override (locate-dominating-file dir mpm-projects-override-file)))
    (if override
      (cons 'vc override)
      nil)))

;;; Code:
(use-package project
  :config
  (add-hook 'project-find-functions #'mpm-override-dir)
  (add-hook 'project-find-functions #'mpm-project-override)
  ;; discover projects when none have been created
  (add-hook 'after-init-hook #'(lambda ()
                                 (let ((project-list-target-file (concat user-emacs-directory "projects")))
                                   (when (and (not (file-exists-p project-list-target-file)) (file-directory-p mpm-projects-dir))
                                     (message (format "'%s' file not found, will attempt to discover projects based on '%s'" project-list-target-file mpm-projects-dir))
                                     (project-remember-projects-under mpm-projects-dir))))))

(provide 'project-management-tool-setup)
;;; project-management-tool-setup.el ends here
