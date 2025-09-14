;; evil-setup.el --- load all config relating to evil-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; entry point into the evil config

;;; Code:
;; Order matters never forget 

(use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
    (setq evil-undo-system 'undo-redo)
    :bind
    ((:map evil-normal-state-map
        ("C-/" . comment-line) ;; vscode inspired binding
        ("gcc" . comment-line) ;; alt-bindings, similar to vim commentary
      :map evil-visual-state-map
        ("gc" . comment-line))) ;; alt-bindings, similar to vim commentary
    :config
    (evil-mode 1)
)

(use-package evil-collection
    :ensure t
    :after evil
    :custom
    (evil-collection-company-use-tng nil) ;; I hacked this already (Personal preference)
    (evil-collection-setup-debugger-keys nil) ;; no need for this (Again.. Personal preference)
    :config
    (evil-collection-init)
)

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer mpm/leader-keys
     :states '(normal insert visual emacs)
     :keymaps 'override
     :prefix "SPC" ;; set leader
     :global-prefix "S-SPC") ;; access leader in insert mode

  (mpm/leader-keys
    "a" '(evil-buffer :wk "Ctrl+^ Buffer")
    "m" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "Find File")
    "s" '(save-buffer :wk "Save Buffer")
  )

  (mpm/leader-keys
       "b" '(:ignore t :wk "Buffers")
       "b i" '(switch-to-buffer :wk "Switch Buffer")
       "b b" '(evil-buffer :wk "Ctrl+^ Buffer")
       "b z" '(bury-buffer :wk "Bury Buffer")
       "b p" '(switch-to-prev-buffer :wk "Previous Buffer")
       "b n" '(switch-to-next-buffer :wk "Next Buffer")
  )

  (mpm/leader-keys
     "d" '(:ignore t :wk "Dired")
     "d d" '(dired :wk "Open dired")
     "d j" '(dired-jump :wk "Dired jump to current")
     "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
     "d w" '(wdired-change-to-wdired-mode :wk "Writable dired")
  )

  (mpm/leader-keys
     "q" '(delete-window :wk "Delete Window")
     "Q" '(save-buffers-kill-terminal :wk "Save Buffers - Kill Terminal")
     "k" '(kill-current-buffer :wk "Kill Current Buffer")
     "K" '(kill-buffer :wk "Kill Buffer")
  )

  (mpm/leader-keys
      "c" '(:ignore t :wk "Coding")
      "c /" '(comment-region :wk "Comment Region")
      "c u" '(uncomment-region :wk "Uncomment Region")
      "c s" '(shell :wk "Shell")
  )

  (mpm/leader-keys
      "e" '(:ignore t :wk "Editing/Evil")
      "e e b" '(eval-buffer :wk "Evaluate elisp in buffer")
      "e e d" '(eval-defun :wk "Evaluate defun containing or after point")
      "e e e" '(eval-expression :wk "Evaluate and elisp expression")
      "e e s" '(eval-last-sexp :wk "Evaluate elisp expression before point")
      "e e r" '(eval-region :wk "Evaluate elisp in region")
      "e s" '(eshell :wk "Emacs Shell")
      "e m" '(hydra-bookmark/body :which-key "Bookmark Hydra")
      "e d" '(evil-goto-definition :wk "Evil goto definition")
      "e *" '(evil-search-word-forward :wk "Evil Search at Point")
      "e /" '(evil-search-forward :wk "Evil search forward")
  )
) 

(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)
)

(use-package evil-mc
    :ensure t
    :config
    (global-evil-mc-mode 1)
)


(provide 'evil-setup)
;;; evil-setup.el ends here
