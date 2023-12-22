;; clean-buffers-setup.el --- Kill Some buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; clean up buffers excessive buffers that are created by emacs after initialization

;;; Code:
(defun marinov/clean-after-buffers ()
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

(add-hook 'after-init-hook #'marinov/clean-after-buffers) ;; comment this variable when debugging

(provide 'clean-buffers-setup)
;; clean-buffers-setup.el ends here
