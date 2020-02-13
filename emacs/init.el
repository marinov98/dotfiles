;;; package --- Summary
;;; Commentary:
;;; Code:

;; uncomment line below if you encounter an error while loading
;; otherwise do not touch it for it can act crazy for the littliest things
;; (setq debug-on-error t) ;; enable debugging in case anything goes wrong

;; Enable the ability to install packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)

;;  install use-package unless it has been previously installed
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; expand marinmacs.org and convert it to marinmacs.el to start-up emacs
(org-babel-load-file (expand-file-name "~/.emacs.d/MarinMacs.org"))
;;; init.el ends here
