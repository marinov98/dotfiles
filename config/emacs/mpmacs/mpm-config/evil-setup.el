;; evil-setup.el --- load all config relating to evil-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; entry point into the evil config

;;; Code:

(use-package evil
    :ensure t
    :custom
    (evil-want-C-u-scroll t)
    :init
    (setq evil-want-keybinding nil)
    (setq evil-undo-system 'undo-redo)
    :config
    (evil-mode 1)
    (define-key evil-motion-state-map "gd" 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "gcc") 'comment-line)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)
    (define-key evil-visual-state-map (kbd "gc") 'comment-dwim)
    (define-key evil-visual-state-map (kbd "C-/") 'comment-region)
    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
)

(use-package evil-collection
    :ensure t
    :after evil
    :custom
    ;; statements below disable bindings for specific packages
    (evil-collection-company-use-tng nil)
    (evil-collection-setup-debugger-keys nil)
    (evil-collection-disabled-modules '(corfu))
    (evil-collection-want-find-usages-bindings nil)
    :config
    (evil-collection-init)
    (evil-global-set-key 'normal "grr" 'xref-find-references)
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
    "." '(find-file :wk "(Native) Find File")
    "s" '(save-buffer :wk "Save Buffer")
    ;; quitting
    "q" '(delete-window :wk "Delete Window")
    "Q" '(save-buffers-kill-terminal :wk "Save Buffers - Kill Terminal")
    "k" '(kill-current-buffer :wk "Kill Current Buffer")
    "K" '(kill-buffer :wk "Kill Buffer")
    ;; <leader> b
    "b" '(:ignore t :wk "Buffers")
    "b l" '(switch-to-buffer :wk "List & Switch Buffer")
    "b b" '(evil-buffer :wk "Ctrl+^ Buffer")
    "b s" '(save-some-buffers :wk "Save some buffers")
    "b k" '(kill-matching-buffers :wk "Kill buffers (regex)")
    "b K" '(kill-some-buffers :wk "Kill some buffers")
    "b z" '(bury-buffer :wk "Bury Buffer")
    "b p" '(switch-to-prev-buffer :wk "Previous Buffer")
    "b n" '(switch-to-next-buffer :wk "Next Buffer")
    ;; <leader>d
     "d" '(:ignore t :wk "Dired/Diagnostics")
     "d d" '(:ignore t :wk "Dired")
     "d d o" '(dired :wk "Open dired")
     "d d j" '(dired-jump :wk "Dired jump to current")
     "d d f" '(wdired-finish-edit :wk "Writable dired finish edit")
     "d d w" '(wdired-change-to-wdired-mode :wk "Writable dired")
     "d d c" '(:ignore t :wk "Dired Create")
     "d d c f" '(dired-create-empty-file :wk "Dired Create File")
     "d d c d" '(dired-create-directory :wk "Dired Create Directory")
    ;; <leader>c
     "c" '(:ignore t :wk "Coding")
     "c /" '(comment-region :wk "Comment Region")
     "c u" '(uncomment-region :wk "Uncomment Region")
     "c s" '(shell :wk "Shell")
     ;; <leader>e
     "e" '(:ignore t :wk "Editing/Evil")
     "e e b" '(eval-buffer :wk "Evaluate elisp in buffer")
     "e e d" '(eval-defun :wk "Evaluate defun containing or after point")
     "e e e" '(eval-expression :wk "Evaluate and elisp expression")
     "e e s" '(eval-last-sexp :wk "Evaluate elisp expression before point")
     "e e r" '(eval-region :wk "Evaluate elisp in region")
     "e s" '(eshell :wk "Emacs Shell")
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
    :after (general pretty-hydra)
    :general-config
    (mpm/leader-keys
        "c m" '(hydra-mc/body :wk "Multiple Cursors Hydra")
    )
    :config
    (global-evil-mc-mode 1)
    (pretty-hydra-define hydra-mc (:color pink :title "Multiple Cursors" :quit-key "<escape>")
      (
        "Goto"
        (("n" evil-mc-make-and-goto-next-match "make & next")
        ("N" evil-mc-make-and-goto-prev-match "make & prev")
        ("p" evil-mc-skip-and-goto-next-match "skip & next")
        ("P" evil-mc-skip-and-goto-prev-match "skip & prev")
        ("m" evil-mc-make-all-cursors "Make all"))

        "Line"
        (("J" evil-mc-make-cursor-move-next-line "make & up")
        ("K" evil-mc-make-cursor-move-prev-line "make & down"))

        "Manual"
        (("u" evil-mc-undo-last-added-cursor "undo cursor")
        ("g" evil-mc-make-cursor-here "make cursor")
        ("q" evil-mc-undo-all-cursors "undo all cursors" :color blue)
        ("r" evil-mc-resume-cursors "resume cursors" :color blue))
      )
    )
)


(provide 'evil-setup)
;;; evil-setup.el ends here
