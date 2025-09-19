;; mpm-core.el --- personal preferences and core stuff -*- lexical-binding: t; -*-

;;; Commentary 
;; how I prefer to set somethings up

;;; Code:
(setq-default indent-tabs-mode nil)                    ;; disable tabs and use spaces
(setq-default tab-width 4)                             ;; set default tab width 4
(setq backward-delete-char-untabify-method 'hungry)    ;; backspaces entire tab instead of one space at a time
(setq compilation-scroll-output 'first-error)          ;; compile scroll location

(setq default-frame-alist '((font . "JetBrainsMono NF Regular 12"))) ;; set font and font size
(setq visible-bell t)                                    ;; disable end of buffer sounds
(setq inhibit-startup-screen t)                          ;; disable startup screen
(when (and (<= 29 emacs-major-version) (not (string-equal system-type "windows-nt")))
  (add-to-list 'default-frame-alist '(alpha-background . 95))) ;; Emacs 29 adds true transparency

(fset 'yes-or-no-p 'y-or-n-p)           ;; change yes or no to y or n
(setq use-dialog-box nil)               ;; Don't pop up UI dialogs when prompting
(menu-bar-mode -1)                      ;; disable menu bar
(scroll-bar-mode -1)                    ;; disable scroll bar
(toggle-scroll-bar -1)                  ;; disable scroll bar toggle
(tool-bar-mode -1)                      ;; disable tool bar
(blink-cursor-mode -1)                  ;; make cursor stop blinking

(setq make-backup-files nil)             ;; stop creating backup~ files
(setq auto-save-default nil)             ;; stop creating autosave# files
(setq create-lockfiles nil)              ;; stop creating any # files

(setq history-length 30)
(put 'minibuffer-history 'history-length 30)
(put 'evil-ex-history 'history-length 30)
(put 'kill-ring 'history-length 25)

(setq gdb-many-windows t)                ;; have multiple windows when debugging
(setq gdb-show-main t)                   ;; Non-nil means display source file containing the main routine at startup


(use-package time
    :custom
    (display-time-24hr-format t) ;; 24hr format because I'm european :)
    :config
    (display-time-mode -1) ;; toggle time mode on and off
) 

(when (string-equal system-type "gnu/linux")
  (setq
    browse-url-browser-function
    '(("https://www.netflix.com/" . browse-url-firefox) ;; firefox deals better with video players on linux
      ("." . browse-url-chromium))
  )
)


(when (>= emacs-major-version 28)
  (use-package ligature
    :ensure t
    :hook (prog-mode . ligature-mode)
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
    )
)


(use-package elec-pair ;; auto closing brackets
    :ensure nil
    :hook ((prog-mode . electric-pair-mode)
           (text-mode . electric-pair-mode))
) 

(use-package paren ;; highlight matching parenthesis
    :custom
    (show-paren-when-point-inside-paren t)
    :config
    (show-paren-mode 1)
)

(use-package hl-line ;; highlight the current line
    :config
    (global-hl-line-mode 1)
)

;; Line numbers
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
      :custom
      (display-line-numbers-type 'relative) ;; relative line numbers help you see how far you need to jump to get where you want to
      (display-line-numbers-current-absolute t)
      (display-line-numbers-width 2)
      (display-line-numbers-widen t)
      :config
      (global-display-line-numbers-mode 1)
      ;; Disable line numbers for some mode
      (dolist (mode '(org-mode-hook term-mode-hook neotree-mode-hook shell-mode-hook eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0)))))
)


(use-package ibuffer
    :hook (ibuffer-mode . (lambda ()
                            (ibuffer-auto-mode 1)
                            (ibuffer-switch-to-saved-filter-groups "default")))
    :custom
    (ibuffer-show-empty-filter-groups nil)
    :config
    (setq ibuffer-saved-filter-groups
        (quote (("default"
                ("Ranger" (mode . ranger-mode))
                ("Org" (name . "^.*org$"))
                ("Text" (or
                        (mode . markdown-mode)
                        (mode . text-mode)
                        (mode . pdf-view-mode)
                        (mode . LaTeX-mode)))
                ("Git" (or 
                        (mode . gitignore-mode)               
                        (mode . gitconfig-mode)               
                        (mode . magit-mode)))
                ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
                ("Elisp" (mode . emacs-lisp-mode))
                ("Programming" (or ;; I dont have all of these modes but just in case for the future...
                                (mode . python-mode)
                                (mode . ruby-mode)
                                (mode . go-mode)
                                (mode . rust-mode)
                                (mode . swift-mode)
                                (mode . objc-mode)
                                (mode . dart-mode)
                                (mode . haskell-mode)
                                (mode . csharp-mode)
                                (mode . scala-mode)
                                (mode . clojure-mode)
                                (mode . java-mode)
                                (mode . c-mode)
                                (mode . c++-mode)))
                ("Web Dev" (or
                            (mode . web-mode)
                            (mode . rjsx-mode)
                            (mode . css-mode)
                            (mode . js-mode)
                            (mode . typescript-mode)
                            (mode . js2-mode)))
                ("Data" (or
                        (mode . csv-mode)
                        (mode . json-mode)
                        (mode . sql-mode)
                        (mode . yaml-mode)
                        (mode . graphql-mode)
                        (mode . dockerfile-mode)))
                ("Help" (or
                        (name . "\*Help\*")
                        (name . "\*info\*")))
                ("Emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$"))))))))

(use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x)) ;; check if its mac
      (exec-path-from-shell-initialize)))

(use-package eshell
     :ensure t
     :bind (("C-`" . eshell))
     :custom
     (eshell-hist-ignoredups t)
     (eshell-scroll-to-bottom-on-input t)
     (eshell-destroy-buffer-when-process-dies t)
     (eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
     :config
     ;; configuration found from this link: https://superuser.com/questions/890937/how-to-show-git-branch-in-emacs-shell
     (defun git-prompt-branch-name ()
       "Get current git branch name"
       (let ((args '("symbolic-ref" "HEAD" "--short")))
           (with-temp-buffer
           (apply #'process-file "git" nil (list t nil) nil args)
           (unless (bobp)
               (goto-char (point-min))
               (buffer-substring-no-properties (point) (line-end-position))))))

       (defun 4lex1v:eshell-prompt ()
       (let ((branch-name (git-prompt-branch-name)))
           (concat
           "\n# " (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) "\n"
           (if branch-name (format "git:(%s) >> " branch-name) ">> ")
           )))         

       (setq eshell-prompt-function #'4lex1v:eshell-prompt
             eshell-prompt-regexp ".*>>+ ")
)

(use-package which-key
  :ensure t 
  :diminish which-key-mode
  :config
  (which-key-mode)
)

(use-package neotree
    :ensure t
    :custom
    (neo-smart-open t) ; update every time its toggled
    (neo-show-hidden-files t)
    (neo-theme (if (display-graphic-p) 'icons 'arrow))
)

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

(use-package dashboard
    :ensure t
    :custom
    (dashboard-banner-logo-title "MarinMacs")
    (dashboard-set-heading-icons t)
    (dashboard-set-init-info t)
    (dashboard-set-file-icons t)
    (dashboard-set-navigator t)
    (dashboard-items '((recents  . 5)
                       (projects . 5)
                       (bookmarks . 5)
                       (agenda . 5)))
    (dashboard-footer-messages '("Quest for the divine workflow continues..."))
    :init
    (if (file-directory-p mpm-img-dir)
          (setq dashboard-startup-banner (concat mpm-img-dir "/" mpm-dashboard-banner-img))
        (setq dashboard-startup-banner 'logo))
    (when (not (package-installed-p 'projectile))
      (setq dashboard-projects-backend 'project-el))
    :config
    (when (string-equal system-type "windows-nt" )
            (advice-add #'dashboard-replace-displayable :override #'identity)) ;; icons have issue displaying on windows, this fixes it
    (dashboard-setup-startup-hook)
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

(provide 'mpm-core)
;;; mpm-core.el ends here
