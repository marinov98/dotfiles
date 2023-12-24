;; mpm-def.el --- Personal definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; defining variables and functions that can be reused is a great way to keep your Emacs config clean and scalable

;;; Code:
(defconst mpm-config-file-location "~/.emacs.d/init.el"
  "Configuration file location.")

(defconst mpm-projects-dir "~/Projects"
  "Projects directory.")

(defvar mpm-img-dir "~/.emacs.d/img"
  "Images directory, mainly for dashboard package.")

(defconst mpm-dashboard-banner-img "targetBanner.png"
  "Dashboard image.")

(defvar mpm-org-dir "~/Projects/org"
  "Org directory.")

(defvar mpm-notes-org "/Personal/notes.org"
  "Personal notes file.")

(defun goto-MarinMacs ()
  "Jump to configuration file."
  (interactive)
  (find-file mpm-config-file-location))
(global-set-key (kbd "C-c m") 'goto-MarinMacs) ;; secondary binding is SPC u m

(provide 'mpm-def)
;;; mpm-def.el ends here
