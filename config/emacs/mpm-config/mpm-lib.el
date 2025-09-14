;; mpm-vars.el --- Personal definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; defining variables and functions that can be reused is a great way to keep your Emacs config clean and scalable

;;; Code:
(defconst mpm-config-file-location (concat user-emacs-directory "init.el")
  "Configuration file location.")

(defconst mpm-projects-dir "~/Projects"
  "Projects directory.")

(defconst mpm-projects-discovery-file ".project.el"
  "File for project.el to use to discover a project.")

(defconst mpm-projects-override-file ".project-override.el"
  "File to override in monorepos for project.el.")

(defvar mpm-img-dir (concat user-emacs-directory "img")
  "Images directory, mainly for dashboard package.")

(defconst mpm-dashboard-banner-img "targetBanner.png"
  "Dashboard image.")

(defvar mpm-org-dir "~/Projects/org"
  "Org directory.")

(defun goto-MarinMacs ()
  "Jump to configuration file."
  (interactive)
  (find-file mpm-config-file-location))

(defun mpm/set-memory ()
  "Set memory usage settings after start up."
  (setq gc-cons-threshold (* 1024 1024 4)) ;; change this depending on your system
  (setq large-file-warning-threshold (* 1024 1024 80)) ;; (80mb) default threshold is low by modern standards
  (setq read-process-output-max (* 1024 1024)) ;; (1mb) Increase amount of data which Emacs reads from the process (recommended by lsp package)
  (garbage-collect))

(defun mpm/reset-file-name-handler-alist ()
  "Reset file name handlers."
  (setq file-name-handler-alist
	(append default-file-name-handler-alist
		file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))


(defun mpm/clean-after-buffers ()
  "Delete * Buffers after init."
  (setq-default message-log-max nil)
  (kill-buffer "*Messages*")
  (when (package-installed-p 'quelpa-use-package)
    (kill-buffer "*quelpa-build-checkout*"))
  (add-hook 'minibuffer-exit-hook
      #'(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
             (kill-buffer buffer))))))

(provide 'mpm-lib)
;;; custom-vars.el ends here
