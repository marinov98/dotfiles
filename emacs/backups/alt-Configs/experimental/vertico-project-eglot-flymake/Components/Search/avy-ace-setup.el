;; avy-ace-setup.el --- packages for word/window searching -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package avy 
    :ensure t
    :custom
    (avy-background t)
    (avy-timeout-seconds 0.4)
    :config
    (pretty-hydra-define hydra-avy (:color blue :title "↵ Avy ↵" :quit-key "q")
      ("Char"
       (("g" avy-goto-char "char 1" :color red)
        ("s" avy-goto-char-2 "char 2" :color red)
        ("t" avy-goto-char-timer "timed char" :color red))

       "Word"
       (("w" avy-goto-word-1 "goto word")
        ("W" avy-goto-word-0 "goto word 0"))

       "Line"
       (("l" avy-goto-line "goto line")
        ("L" avy-goto-end-of-line "goto eoline")
        ("m" avy-move-line "move line")
        ("K" avy-kill-whole-line "kill line")
        ("y" avy-copy-line "yank line"))

       "Resume"
       (("r" avy-resume "resume" :color red)))))

(use-package ace-window
     :ensure t
     :init
     (global-set-key (kbd "C-:") 'ace-window)
     (setq aw-background nil)
     (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(provide 'avy-ace-setup)
;;; avy-ace-setup.el ends here
