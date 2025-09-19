;;; markup-setup.el --- configuration for org -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup Org mode and its utility packages

;;; Code:
(use-package org
    :ensure t
    :custom
    (org-file-apps
      '(("\\.pdf\\(::[0-9]+\\)?\\'" . "epdfview %s")))
    :pin org
    :general-config
    (mpm/leader-keys
      "o" '(hydra-org/body :wk "Org Hydra")
    )
    :config
    ;; allow easier snippet insertion
    (when (version<= "27.0.50" emacs-version)
      (require 'org-tempo)
    )

    (defhydra hydra-org (:color blue :columns 4)
      " ORG "
      ("o" org-open-at-point "open link")
      ("c" org-toggle-comment "comment")
      ("i" org-time-stamp "time stamp")
      ("d" org-export-dispatch "export dispatch")
      ("p" org-priority "priority")
      ("t" org-todo "todo state")
      ("a" org-todo-list "agenda")
      ("l" org-show-todo-tree "show todo tree")
      ("m" marinov/enter-eval "enter and eval")
      ("s" org-edit-special "edit special")
      ("x" org-edit-src-exit "exit special")
      ("g" mpm/goto-org-directory "goto org directory")
      ("q" nil "quit")
    )
)


;; bullets
(use-package org-bullets
    :ensure t
    :hook
    (org-mode . org-bullets-mode)
)

;; Org custom settings
(custom-set-variables
         '(org-directory mpm-org-dir)
)

(defun mpm/goto-org-directory ()
    "Go to my org directory."
    (interactive)
    (find-file org-directory)
)

;; sometimes I edit within org and I forget to enter src but I want to just go to src to evaluate
(defun mpm/enter-eval ()
    "Enter source, and evaluate the buffer."
    (interactive)
    (org-edit-special)
    (eval-buffer)
)

(use-package markdown-mode
    :ensure t
    :commands markdown-mode
    :mode
    ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
)


(use-package flyspell
    :ensure t
    :commands (ispell-change-dictionary
               ispell-word
               flyspell-buffer
               flyspell-mode
               flyspell-region)
    :bind
    (:map flyspell-mode-map
    ("C-M-i" . nil)) ;; messes with org autocomplete
)

(use-package wc-mode
    :ensure t
    :commands wc-mode
)

(use-package writegood-mode
    :ensure t
    :commands writegood-mode
)

(defhydra hydra-writing (:color red :columns 2)
  "✓ Writing and Spelling ✓"
  ("d" ispell-change-dictionary "change dict")
  ("s" ispell-word "spell word")
  ("f" flyspell-buffer "flyspell buffer")
  ("m" flyspell-mode "flyspell mode")
  ("r" flyspell-region "flyspell region")
  ("c" wc-mode "wc mode")
  ("w" writegood-mode "writegood mode")
  ("q" nil "quit")
)

(mpm/leader-keys
  "v" '(hydra-writing/body :wk "Writing Hydra")
)

(use-package pdf-view
	     :disabled
		 :ensure pdf-tools
		 :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
		 :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
		 :magic ("%PDF" . pdf-view-mode)
		 :bind
		 (:map pdf-view-mode-map
		 ("C-s" . isearch-forward))
		 :init
		 (setq pdf-annot-activate-created-annotations t)
)

(use-package tex
    :disabled
    :ensure auctex
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
)

(provide 'markup-setup)
;;; markup-setup.el ends here
