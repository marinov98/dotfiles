;; mpm-core.el --- personal preferences and core stuff -*- lexical-binding: t; -*-

;;; Commentary 
;; how I prefer to set somethings up

;;; Code:
(use-package hydra
    :ensure t
    :config
    (setq hydra-is-helpful t)
    (setq hydra-hint-display-type 'lv)
)
    
(use-package pretty-hydra
    :after hydra
    :ensure t
)

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
  ("q" nil "quit" :color blue)
)

;; Window (my attempt at window management)
(pretty-hydra-define hydra-window (:color pink :title "⚡⚡ Completion + Windows ⚡⚡" :quit-key "q")
  (
    "Splitting"
    (("o" delete-other-windows "delete other windows")
    ("v" split-window-right "v-split")
    ("H" split-window-below "h-split"))

    "Move"
    (("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right"))

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
    ("D" kill-this-buffer "kill buffer"))
  )
)

;; Bookmark (managing bookmarks)
(defhydra hydra-bookmark (:color blue :columns 2)
  "📒 Bookmarks 📒"
  ("j" bookmark-jump "jump")
  ("l" bookmark-bmenu-list "list")
  ("s" bookmark-set "set")
  ("o" bookmark-set-no-overwrite "set no overwrite")
  ("q" nil "quit" :color blue)
)

;; Utility (useful commands for me)
(pretty-hydra-define hydra-utility (:color red :title "😎 Utility 😎" :quit-key "q")
  (
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
    ("t" neotree-toggle "file tree" :color blue)
    ("e" eval-buffer "eval buffer")
    ("c" compile "compile"))
  )
)

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
  ("q" nil "quit")
)


(mpm/leader-keys
     "w" '(hydra-window/body :wk "Window Hydra")
     "v" '(hydra-writing/body :wk "Writing Hydra")
     "e m" '(hydra-bookmark/body :which-key "Bookmark Hydra")
     "u" '(hydra-utility/body :wk "Utility Hydra")
     "?" '(hydra-describe/body :wk "Describe Hydra")
     "c l" '(hydra-launcher/body :wk "Launcher Hydra")
)

(provide 'hydra-setup)
;;; mpm-core.el ends here
