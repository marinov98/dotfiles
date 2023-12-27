;; evil-leader-setup.el --- configure leader key and associated bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; the <leader> key in vim opens doors to many possibilities and key bindings

;;; Code:
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer mpm/leader-keys
     :states '(normal insert visual emacs)
     :keymaps 'override
     :prefix "SPC" ;; set leader
     :global-prefix "S-SPC") ;; access leader in insert mode

  (mpm/leader-keys
     "SPC" '(hydra-project/body :wk "Project Hydra")
     "w" '(hydra-window/body :wk "Window Hydra")
     "S" '(hydra-smerge/body :wk "Smerge Hydra")
     "l" '(hydra-code/body :wk "LSP Hydra")
     "a" '(evil-buffer :wk "Ctrl+^ Buffer")
     "g" '(hydra-git/body :wk "Git Hydra")
     "v" '(hydra-writing/body :wk "Writing Hydra")
     "u" '(hydra-utility/body :wk "Utility Hydra")
     "o" '(hydra-org/body :wk "Org Hydra"))

  (mpm/leader-keys
     "d" '(:ignore t :wk "Dired/Describe")
     "d d" '(dired :wk "Open dired")
     "d h" '(hydra-describe/body :wk "Describe Hydra")
     "d j" '(dired-jump :wk "Dired jump to current")
     "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
     "d w" '(wdired-change-to-wdired-mode :wk "Writable dired")
     "d n" '(neotree-dir :wk "Open directory in neotree"))


  (mpm/leader-keys
     "i" '(:ignore t :wk "Completion framework commands")
     "i f" '(consult-fd :wk "Fd")
     "i r" '(consult-ripgrep :wk "Rg")
     "i s" '(consult-line :wk "Isearch")
     "i i" '(ibuffer :wk "Ibuffer")
     "i y" '(consult-yank-pop :wk "Yank Pop"))

  (mpm/leader-keys
     "b" '(:ignore t :wk "Buffers/BookMark")
     "b i" '(switch-to-buffer :wk "Switch Buffer")
     "b b" '(evil-buffer :wk "Ctrl+^ Buffer")
     "b z" '(bury-buffer :wk "Bury Buffer")
     "b p" '(switch-to-prev-buffer :wk "Previous Buffer")
     "b n" '(switch-to-next-buffer :wk "Next Buffer"))

  (mpm/leader-keys
     "q" '(delete-window :wk "Delete Window")
     "Q" '(save-buffers-kill-terminal :wk "Save Buffers - Kill Terminal")
     "k" '(kill-current-buffer :wk "Kill Current Buffer")
     "K" '(kill-buffer :wk "Kill Buffer"))

  (mpm/leader-keys
     "RET" '(company-complete :wk "Company Complete")
     "y" '(yas-expand :wk "yas-expand")
     "s" '(save-buffer :wk "Save Buffer")
     "f" '(project-find-file :wk "Project Find File/Buffer")
     "." '(find-file :wk "Find File")
     "/" '(consult-line-multi :wk "Multi File Search")
     "m" '(execute-extended-command :wk "M-x"))

  (mpm/leader-keys
     "r" '(:ignore t :wk "Grep Actions")
     "r g" '(consult-git-grep :wk "Git Grep")
     "r /" '(grep :wk "Grep")
     "r r" '(rgrep :wk "Grep Recursive")
     "r w" '(:ignore t :wk "Wgrep Actions")
     "r w c" '(wgrep-change-to-wgrep-mode :wk "change to wgrep mode")
     "r w e" '(wgrep-exit :wk "Wgrep Exit")
     "r w a" '(wgrep-abort-changes :wk "Wgrep Abort Changes")
     "r w f" '(wgrep-finish-edit :wk "Wgrep Finish Edit"))

  (mpm/leader-keys
    "c" '(:ignore t :wk "Coding")
    "c /" '(comment-region :wk "Comment Region")
    "c u" '(uncomment-region :wk "Uncomment Region")
    "c l" '(hydra-launcher/body :wk "Launcher Hydra")
    "c m" '(hydra-mc/body :wk "Multiple Cursors Hydra")
    "c s" '(shell :wk "Shell")
    "c f" '(:ignore t :wk "Flycheck options")
    "c f e" '(flycheck-explain-error-at-point :wk "Flycheck explain error")
    "c f s" '(flycheck-select-checker :wk "Flycheck select checker")
    "c f d" '(flycheck-disable-checker :wk "Flycheck disable checker")
    "c f h" '(flycheck-describe-checker :wk "Flycheck describe checker")
    "c f m" '(flycheck-mode :wk "Flycheck mode")
    "c f M" '(flycheck-manual :wk "Flycheck manual")
    "c f v" '(flycheck-verify-setup :wk "Flycheck verify setup")
    "c f l" '(flycheck-list-errors :which-key "Flycheck List Errors"))

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
    "e /" '(evil-search-forward :wk "Evil search forward"))

  (mpm/leader-keys
    "t" '(:ignore t :wk "Neotree/Treemacs")
    "t n" '(neotree-toggle :wk "Toggle Neotree")))

(provide 'evil-leader-setup)
;;; evil-leader-setup.el ends here
