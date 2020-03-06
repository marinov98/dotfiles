;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
;; Link to where I found the code: https://ambrevar.xyz/emacs2/
(defun reset-gc-cons-threshold ()
  "Reset garbage collection to default values after startup."
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
  (setq gc-cons-percentage 0.1))

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(add-hook 'after-init-hook #'reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun reset-file-name-handler-alist ()
  "Reset file name handlers."
  (setq file-name-handler-alist
	(append default-file-name-handler-alist
		file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(add-hook 'after-init-hook #'reset-file-name-handler-alist)


(setq load-prefer-newer t) ;; Avoid the pitfall of “loading old bytecode instead of newer source”

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
