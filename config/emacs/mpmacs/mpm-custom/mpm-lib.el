;; mpm-lib.el --- Personal definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; defining variables and functions that can be reused is a great way to keep your Emacs config clean and scalable

;;; Code:
(defconst mpm-config-file-location (concat user-emacs-directory "init.el")
  "Configuration file location.")

(defconst mpm-default-font "Fira Code"
  "Default font when all else fails.")

(defconst mpm-projects-dir "~/projects"
  "Projects directory.")

(defconst mpm-projects-discovery-file ".project.el"
  "File for project.el to use to discover a project.")

(defconst mpm-projects-override-file ".project-override.el"
  "File to override in monorepos for project.el.")

(defconst mpm-dashboard-banner-img "targetBanner.png"
  "Dashboard image.")

(defvar mpm-target-font-size "12"
  "Desired font-size.")

(defvar mpm-target-font "JetBrainsMono Nerd Font"
  "The desired font for the editor.")

(defvar mpm-img-dir (concat user-emacs-directory "img")
  "Images directory, mainly for dashboard package.")

(defvar mpm-org-dir (concat mpm-projects-dir "/org")
  "Org directory.")

(defun goto-MarinMacs ()
  "Jump to configuration file."
  (interactive)
  (find-file mpm-config-file-location))

(defun mpm/font-exists-p (font)
  "Check if the FONT exists." (and (display-graphic-p) (not (null (x-list-fonts font)))))

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

(defun mpm/get-font ()
  "Get the available font."
  (interactive)
  (cond
    ((mpm/font-exists-p mpm-target-font)
        (format "%s %s" mpm-target-font mpm-target-font-size))
    ((mpm/font-exists-p mpm-default-font)
        (format "%s %s" mpm-default-font mpm-target-font-size))
    (t nil)
  )
)

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

(defun mpm/get-visual-selection ()
    "Get visually selected text."
    (interactive)
    (cond
     ((region-active-p) (buffer-substring (region-beginning) (region-end)))
     (t (error "No region selected/found!"))
    )
)

(provide 'mpm-lib)
;;; mpm-lib.el ends here
