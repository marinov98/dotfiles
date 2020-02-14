;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
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
