;; hydra-setup.el --- setup the hydra heads -*- lexical-binding: t; -*-

;;; Commentary:
;; Great hydras make for a great workflow (Let’s hope they are great…)

;;; Code:
(use-package hydra
    :ensure t
    :config
    (setq hydra-is-helpful t)
    (setq hydra-hint-display-type 'lv))
    
(use-package pretty-hydra
    :ensure t)

;; Describe (help describe anything and open up documentation)
(defhydra hydra-describe (:color red :columns 3)
  "Describe 🤓"
  ("d" embark-bindings "bindings")
  ("f" describe-function "func")
  ("F" describe-face "face")
  ("k" describe-key "key")
  ("v" describe-variable "var")
  ("p" describe-package "package")
  ("s" describe-symbol "symbol")
  ("m" which-key-show-major-mode "major mode")
  ("M" describe-mode "modes")
  ("t" describe-theme "theme")
  ("q" nil "quit" :color blue))

;; Project (project management)
(pretty-hydra-define hydra-project (:color red :title "🚀 Project 🚀" :quit-key "q")
  ("Finding"
  (("f" project-search "search")
  ("d" project-find-dir "find-dir"))

  "Search/Replace" ;; search and replace
  (("g" project-find-regexp "grep project")
  ("c" project-query-replace-regexp "replace"))

  "Switch"
  (("s" project-switch-project "switch project")
  ("b" project-switch-to-buffer "switch buffer"))

  "Finish"
  (("p" project-compile "compile")
  ("k" project-kill-buffers "kill project buffers"))))

;; Window (my attempt at window management)
(pretty-hydra-define hydra-window (:color pink :title "⚡⚡ Completion + Windows ⚡⚡" :quit-key "q")
  ("Completion"
  (("f" find-file "find")
  ("x" execute-extended-command "M-x")
  ("b" switch-to-buffer "switch buffer"))

  "Splitting"
  (("o" delete-other-windows "delete other windows")
  ("v" split-window-right "v-split")
  ("2" split-window-below "h-split"))

  "Move"
  (("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("a" ace-window "ace-window"))


  "Resizing"
  (("s" shrink-window "shrink window")
  ("e" enlarge-window "enlarge window")
  ("S" shrink-window-horizontally "shrink horizontally")
  ("E" enlarge-window-horizontally "shrink horizontally")
  ("B" balance-windows "balance windows"))

  "Zoom"
  (("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset"))

  "Quit"
  (("K" kill-current-buffer "kill current buffer")
  ("d" delete-window "delete window")
  ("D" kill-this-buffer "kill buffer"))))

;; Git (magit and timemachine)
(defhydra hydra-git (:color red)
  "⏳ Git ⏳"
  ("g" magit "magit")
  ("d" magit-dispatch "dispatch")
  ("l" magit-list-repositories "list repos")
  ("t" git-timemachine "timemachine")
  ("q" nil "quit" :color blue))

;; Smerge (for handling merge conflicts)
(pretty-hydra-define hydra-smerge (:color pink :title "⚡ Smerge ⚡" :quit-key "q")
  ("Move"
  (("n" smerge-next)
  ("p" smerge-prev))

  "Keep"
  (("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current))

  "Diff"
  (("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff))


  "Other"
  (("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
         (interactive)
         (save-buffer)
         (bury-buffer))
     "Save and bury buffer" :color blue))))


;; Code (jump to definitions and references, list errors, formatting)
(pretty-hydra-define hydra-code (:color red :title "📡 Code 📡" :quit-key "q")
  ("Find"
  (("f" lsp-find-definition "find def")
  ("r" lsp-find-references "find ref")
  ("g" lsp-ui-doc-glance "glance")
  ("i" lsp-ui-imenu "imenu"))

  "Refactor"
  (("c" lsp-rename "rename")
  ("s" hydra-style/body "format style" :color blue))

  "Errors"
  (("l" lsp-ui-flycheck-list "list errors")
  ("b" flycheck-buffer "flycheck buffer")
  ("e" flycheck-next-error "next error")
  ("E" flycheck-previous-error "prev error"))))

(pretty-hydra-define hydra-style (:color blue :title "📡 Style 📡" :quit-key "q")
  ("Buffer"
  (("b" python-black-buffer "python-black")
  ("l" lsp-format-buffer "LSP" :color red)
  ("p" prettier-prettify "prettier")
  ("c" clang-format-buffer "clang"))

   "Imports"
   (("o" lsp-organize-imports "organize imports"))

  "Region"
  (("B" python-black-region "python-black")
  ("C" clang-format-region "clang")
  ("P" prettier-prettify-region "prettier")
  ("L" lsp-format-region "LSP" :color red))))

;; MC (Multiple Cursors)
(pretty-hydra-define hydra-mc (:color pink :title "Multiple Cursors" :quit-key "<escape>")
  ("Goto"
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
   ("r" evil-mc-resume-cursors "resume cursors" :color blue))))

;; Avy (Word finding)
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
  (("r" avy-resume "resume" :color red))))

;; Launcher (launch stuff)
(defhydra hydra-launcher (:color red :columns 2)
  " Launch "
  ("h" man "man")
  ("g" (browse-url "https://www.google.com/") "Google")
  ("G" (browse-url "https://github.com/marinov98") "GitHub")
  ("n" (browse-url "https://www.netflix.com/") "Netflix")
  ("y" (browse-url "https://www.youtube.com/") "YouTube")
  ("m" (browse-url "https://www.messenger.com/") "Messenger")
  ("s" eshell "shell")
  ("a" ansi-term "ansi-term")
  ("q" nil "quit"))

;; Writing (taking notes, and writing)
(defhydra hydra-writing (:color red :columns 2)
  "✓ Writing and Spelling ✓"
  ("d" ispell-change-dictionary "change dict")
  ("s" ispell-word "spell word")
  ("f" flyspell-buffer "flyspell buffer")
  ("m" flyspell-mode "flyspell mode")
  ("r" flyspell-region "flyspell region")
  ("w" writegood-mode "writegood mode")
  ("q" nil "quit"))

;; Utility (useful commands for me)
(pretty-hydra-define hydra-utility (:color red :title "😎 Utility 😎" :quit-key "q")
  ("Search"
  (("r" consult-recent-file "recent files"))

  "Debugging"
  (("b" gdb "gdb")
  ("d" dap-debug "dap debug")
  ("i" dap-debug-edit-template "debug template"))

  "Modes"
  (("u" auto-fill-mode "auto-fill mode")
  ("W" web-mode "web mode")
  ("X" rjsx-mode "rjsx mode")
  ("J" js-mode "js mode"))

  "Personal"
  (("m" goto-MarinMacs "goto config")
  ("s" set-fill-column "set-fill-column")
  ("R" restart-emacs "restart Emacs")
  ("e" eval-buffer "eval buffer")
  ("c" compile "compile"))))

;; Org (for org mode)
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
  ("n" marinov/jump-to-notes "goto notes")
  ("D" marinov/goto-org-directory "goto org directory")
  ("q" nil "quit"))

;; Bookmark (managing bookmarks)
(defhydra hydra-bookmark (:color blue :columns 2)
  "📒 Bookmarks 📒"
  ("j" bookmark-jump "jump")
  ("l" bookmark-bmenu-list "list")
  ("s" bookmark-set "set")
  ("o" bookmark-set-no-overwrite "set no overwrite")
  ("q" nil "quit" :color blue))

(provide 'hydra-setup)
;;; hydra-setup.el ends here
