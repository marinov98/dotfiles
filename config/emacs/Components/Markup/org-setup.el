;;; org-setup.el --- configuration for org -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup Org mode and its utility packages

;;; Code:
(use-package org
    :ensure t
    :custom
    (org-file-apps
      '(("\\.pdf\\(::[0-9]+\\)?\\'" . "epdfview %s")))
    :pin org)

;; allow easier snippet insertio
(when (version<= "27.0.50" emacs-version)
  (require 'org-tempo))

;; bullets
(use-package org-bullets
    :ensure t
    :hook
    (org-mode . org-bullets-mode))

;; Org custom settings
(custom-set-variables
         '(org-directory mpm-org-dir)
         '(org-default-notes-file (concat org-directory mpm-notes-org)))

(defun marinov/goto-org-directory ()
    "Go to my org directory."
    (interactive)
    (find-file org-directory))

(defun marinov/jump-to-notes ()
    "Go to notes file."
    (interactive)
    (find-file org-default-notes-file))

;; sometimes I edit within org and I forget to enter src but I want to just go to src to evaluate
(defun marinov/enter-eval ()
    "Enter source, and evaluate the buffer."
    (interactive)
    (org-edit-special)
    (eval-buffer))

(provide 'org-setup)
;;; org-setup.el ends here
