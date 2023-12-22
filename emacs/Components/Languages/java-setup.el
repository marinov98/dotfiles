;; java-setup.el --- configurations related to Java -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun mpm-default-code-style-java()
  (setq c-basic-offset 2
        c-label-offset 0
        tab-width 2))

(add-hook 'java-mode-hook 'mpm-default-code-style-java)

(provide 'java-setup)
;;; java-setup.el ends here
