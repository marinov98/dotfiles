;;; gc-setup.el --- configure and optimize garbage collection -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs startup can be optimizied by setting and resetting garbage collection values

;; Code:
;; Increase garbage threshold to boost startup time then reduce it after initialization is complete (done in marinov/set-memory function)
(setq gc-cons-threshold (* 1024 1024 100))

;; Temporarily disable the file name handler as we dont need it on startup
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


(defun marinov/set-memory ()
  "Set memory usage settings after start up."
  (setq gc-cons-threshold (* 1024 1024 2)) ;; change this depending on your system
  (setq large-file-warning-threshold (* 1024 1024 80)) ;; (80mb) default threshold is low by modern standards
  (setq read-process-output-max (* 1024 1024)) ;; (1mb) Increase amount of data which Emacs reads from the process (recommended by lsp package)
  (garbage-collect)) 

(defun marinov/reset-file-name-handler-alist ()
  "Reset file name handlers."
  (setq file-name-handler-alist
	(append default-file-name-handler-alist
		file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(provide 'gc-setup)
;;; gc-setup.el ends here
