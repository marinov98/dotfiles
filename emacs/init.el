;; -*- lexical-binding: t; -*-
(setq load-prefer-newer t) ;; Avoid the pitfall of loading old bytecode instead of newer 

;; MS-Windows can be slow and can give a lot of issues. This setting below at least fixes the issue of it not recognizing unicode characters and not letting your save your file.
(when (string-equal system-type "windows-nt")
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization Optimization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximize garbage threshold to boost startup time then reduce it after initialization is complete (done in marinov/set-memory function)
(setq gc-cons-threshold most-positive-fixnum)


;; Temporarily disable the file name handler as we dont need it on startup
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


(defun marinov/set-memory ()
  "Set memory usage settings after start up."
  (garbage-collect)
  (setq gc-cons-threshold (* 1024 1024 100)) ;; change this depending on your system 
  (setq large-file-warning-threshold (* 1024 1024 80)) ;; (80mb) default threshold is low by modern standards
  (setq read-process-output-max (* 1024 1024))) ;; (1mb) Increase amount of data which Emacs reads from the process (recommended by lsp package)


(defun marinov/reset-file-name-handler-alist ()
  "Reset file name handlers."
  (setq file-name-handler-alist
	(append default-file-name-handler-alist
		file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(add-hook 'after-init-hook #'marinov/set-memory)
(add-hook 'after-init-hook #'marinov/reset-file-name-handler-alist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
