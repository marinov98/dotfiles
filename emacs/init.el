;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-


;; maximize garbage threshold to boost startup time
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;;; Function to reduce garbage collection threshold after initialization to avoid crashing
(defun marinov/set-memory ()
  "Set memory usage settings after start up."
  (setq gc-cons-threshold (* 1024 1024 50)) ;; (50mb) default 800kb threshold is low by modern standards
  (setq large-file-warning-threshold (* 1024 1024 80)) ;; (80mb) default 1mb threshold is low by modern standards
  (setq read-process-output-max (* 1024 1024)) ;; (1mb) Increase the amount of data which Emacs reads from the process (recommended by lsp package)
  (setq gc-cons-percentage 0.1))

(add-hook 'after-init-hook #'marinov/set-memory)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; reset file name handler function
(defun marinov/reset-file-name-handler-alist ()
  "Reset file name handlers."
  (setq file-name-handler-alist
	(append default-file-name-handler-alist
		file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(add-hook 'after-init-hook #'marinov/reset-file-name-handler-alist)


(setq load-prefer-newer t) ;; Avoid the pitfall of loading old bytecode instead of newer source

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package)
    (package-install 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git")))



(org-babel-load-file (expand-file-name "~/.emacs.d/MarinMacs.org"))


;;; init.el ends here
