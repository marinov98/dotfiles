(setq-default indent-tabs-mode nil)                    ;; disable tabs and use spaces
(setq-default tab-width 4)                             ;; set default tab width 4 
(setq backward-delete-char-untabify-method 'hungry)    ;; backspaces entire tab instead of one space at a time

(setq default-frame-alist '((font . "Fira Code-12")
                            (cursor-color . "#ffc600"))) ;; set font, font size, and cursor color
(setq visible-bell t)                                    ;; disable end of buffer sounds
(electric-pair-mode)                                     ;; auto closing brackets
(show-paren-mode 1)                                      ;; highlight matching parenthesis
(global-hl-line-mode 1)                                  ;; highlight current line 
(setq inhibit-startup-screen t)                          ;; disable startup screen

(when (version<= "26.0.50" emacs-version )         
  (setq display-line-numbers-type 'relative)           ;; relative line numbers help you see how far you need to jump to get where you want to 
  (setq display-line-numbers-current-absolute t)
  (setq display-line-numbers-width 2)
  (setq display-line-numbers-widen t)
  (global-display-line-numbers-mode))                  ;; display line numbers in every buffer

(setq display-time-24hr-format t)
(display-time-mode 1)                                  ;; display time in the modeline

(fset 'yes-or-no-p 'y-or-n-p)            ;; change yes or no to y or n
(menu-bar-mode -1)                       ;; disable menu bar
(toggle-scroll-bar -1)                   ;; disable scroll bar
(tool-bar-mode -1)                       ;; disable tool bar
(blink-cursor-mode -1)                   ;; make cursor stop blinking

(setq make-backup-files nil)             ;; stop creating backup~ files
(setq auto-save-default nil)             ;; stop creating autosave# files
(setq create-lockfiles nil)              ;; stop creating any # files

(setq gdb-many-windows t)                ;; have multiple windows when debugging
(setq gdb-show-main t)                   ;; Non-nil means display source file containing the main routine at startup

(defun goto-MarinMacs ()
  "Jump to configuration file"
  (interactive)
  (find-file "~/.emacs.d/MarinMacs.org")) 
(global-set-key (kbd "C-c m") 'goto-MarinMacs) ;; secondary binding is SPC m m

(use-package quelpa-use-package
    :ensure t)

(use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ;; hydras
      "SPC" 'hydra-projectile/body
      "w" 'hydra-window/body
      "z" 'hydra-zoom/body
      "l" 'hydra-lsp/body
      "d" 'hydra-describe/body
      "g" 'hydra-git/body
      "TAB" 'hydra-launcher/body
      "v" 'hydra-writing/body
      "m" 'hydra-utility/body
      "a" 'hydra-avy/body
      "r" 'hydra-org/body
      ;; file finding, searching, and yanking
      "f" 'helm-find-files
      "j" 'helm-browse-project ;; need git installed!
      "i" 'swiper-helm
      "I" 'swiper-multi
      "S" 'swiper-thing-at-point
      "y" 'helm-show-kill-ring
      "c" 'avy-goto-char-timer
      "x" 'helm-M-x
      ;; buffers
      "s" 'save-buffer
      "b" 'helm-mini
      "p" 'switch-to-prev-buffer
      "n" 'switch-to-next-buffer
      "/" 'comment-region
      ;; deletion
      "q" 'delete-window
      "Q" 'save-buffers-kill-terminal
      "k" 'kill-current-buffer
	  "K" 'kill-buffer
      "o" 'delete-other-windows
      ;; package-specific
      "t" 'neotree-toggle
      "h s" 'helm-swoop
      "h a" 'helm-multi-swoop))

(use-package evil
    :ensure t
    :config
    (evil-mode 1)
    ;; make switching windows much easier
    (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
    ; NeoTree override keybindings, package (neotree) is shown further in the config
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;; like tpope's vim-surround
(use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

;; Evil multiple-cursors
(use-package evil-mc
    :ensure t
    :diminish
    :config
    (global-evil-mc-mode 1))


;; Evil magit overrides magit keybindings, package (magit) is shown further in the config
(use-package evil-magit :ensure t)

(use-package org 
     :ensure t
     :pin org)

;; allow easier snippet insertion  
(require 'org-tempo)

 ;; bullets
 (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

 ;; Org custom settings
 (custom-set-variables
          '(org-directory "~/Projects/org")
          '(org-default-notes-file (concat org-directory "/Personal/notes.org")))

(use-package markdown-mode
    :ensure t
    :commands markdown-mode
    :mode
    ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

(use-package flyspell
    :ensure t
    :commands (ispell-change-dictionary
               ispell-word
               flyspell-buffer
               flyspell-mode
               flyspell-region)
    :bind
    (:map flyspell-mode-map
    ("C-M-i" . nil))) ;; messes with org autocomplete

(use-package wc-mode
    :ensure t
    :commands wc-mode
    :config
    (global-set-key "\C-cw" 'wc-mode))

(use-package writegood-mode
    :ensure t
    :commands writegood-mode
    :bind ("C-x w" . writegood-mode)) ;; messes with org snippets dont enable by default in org

(use-package dashboard 
    :ensure t
    :custom
    (dashboard-banner-logo-title "MarinMacs")
    (dashboard-set-heading-icons t)
    (dashboard-set-init-info t)
    (dashboard-set-file-icons t)
    (dashboard-set-navigator t)
    (dashboard-startup-banner 'logo)
    (dashboard-footer-messages '("Maintained by Marin P. Marinov since 2018"))
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents  . 5)
                           (bookmarks . 5)
                           (agenda . 5)
                           (projects . 5))))

;; BE AWARE: emacs can have multiple themes on at the same time
;; Multiple themes can mix into a super theme
;; Some themes do not mix well which is why I disable themes

 (use-package spacemacs-common
     :disabled
     :ensure spacemacs-theme
     :config (load-theme 'spacemacs-dark t))

 (use-package doom-themes
     :ensure t 
     :custom
     (doom-themes-enable-bold t)
     (doom-themes-enable-italic t)
     :config
     (load-theme 'doom-one t)
     (doom-themes-visual-bell-config) ;; Enable flashing mode-line on errors
     (doom-themes-org-config)) ;; Corrects (and improves) org-mode's native fontification.

 (use-package zerodark-theme
     :disabled
     :ensure t)

 (use-package minimal-theme
     :disabled
     :ensure t
     :config
     (load-theme 'minimal t))

 (use-package zenburn-theme
     :disabled
     :ensure t
     :config
     (load-theme 'zenburn t))

 (use-package poet-theme
     :disabled
     :ensure t)

 (use-package modus-vivendi-theme
     :disabled
     :ensure t
     :config
     (setq modus-vivendi-theme-bold-constructs t)
     (load-theme 'modus-vivendi t))

 (use-package modus-operandi-theme
     :disabled
     :ensure t
     :config (load-theme 'modus-operandi t))

 (use-package jbeans-theme
     :disabled
     :ensure t
     :config
     (load-theme 'jbeans t))

 (use-package planet-theme
     :disabled
     :ensure t
     :config 
     (load-theme 'planet t))

;;;;;;;;;;;;;;;;;;;;;;   
;; Spaceline
;;;;;;;;;;;;;;;;;;;;;;   

(use-package spaceline
    :ensure t
    :custom-face
    (spaceline-highlight-face ((t (:background "#ffc600" :foreground "black"))))
    :custom
    (spaceline-toggle-flycheck-info-off)
    :config
    (require 'spaceline-config)
    (setq powerline-default-separator (quote arrow))
    (spaceline-highlight-face-default) 
    (spaceline-spacemacs-theme))

;;;;;;;;;;;;;;;;;;;;;;   
;;  Telephone-line
;;;;;;;;;;;;;;;;;;;;;;   

  (use-package telephone-line
      :disabled  
      :ensure t
      :config
      (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
      (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
        (telephone-line-mode 1))

;;;;;;;;;;;;;;;;;;;;;;   
;; lightweight doom theme
;;;;;;;;;;;;;;;;;;;;;;   

  (use-package doom-modeline
      :disabled
      :ensure t
      :hook (after-init . doom-modeline-mode))

(use-package helm
    :ensure t
    :diminish
    :custom
    (helm-split-window-inside-p t)
    (helm-move-to-line-cycle-in-source t)
    (helm-autoresize-max-height 0)
    (helm-autoresize-min-height 20)
    (helm-M-x-fuzzy-match t)
    (helm-buffers-fuzzy-matching t)
    (helm-recentf-fuzzy-match t)
    (helm-semantic-fuzzy-match t)
    :bind
    (("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    :map helm-map
    ("TAB" . helm-execute-persistent-action)
    ("<tab>" . helm-execute-persistent-action)
    ("C-z" . helm-select-action)
    ("M-j" . helm-next-line)
    ("M-k" . helm-previous-line))
    :config
    (require 'helm-config)
    (helm-mode 1)
    (helm-autoresize-mode 1))

 ;; Swiper 
 (use-package swiper
     :ensure t
     :custom
     (swiper-action-recenter t)
     (swiper-goto-start-of-match t)
     (swiper-include-line-number-in-search t))


 (use-package swiper-helm
     :after (helm swiper)
     :ensure t
     :bind
     ("C-s" . swiper-helm))

 (use-package helm-rg 
     :after helm
     :ensure t
     :config (setq helm-rg-default-directory 'git-root))

 (use-package helm-ag
     :after helm
     :ensure t)

 (use-package helm-ls-git 
     :after helm
     :ensure t)

 (use-package helm-swoop
     :after helm
     :ensure t
     :custom
     (helm-swoop-split-with-multiple-windows nil)
     (helm-swoop-split-direction 'split-window-vertically)
     (helm-swoop-speed-or-color nil)
     (helm-swoop-move-to-line-cycle t)
     (helm-swoop-use-line-number-face t)
     (helm-swoop-use-fuzzy-match t))

(use-package fzf
    :ensure t)
  ;; :init
  ;; (setenv "FZF_DEFAULT_COMMAND" "fd --type f"))

(use-package avy 
    :ensure t
    :custom
    (avy-timeout-seconds 0.3))

(use-package which-key
	:ensure t 
    :diminish
	:config
	(which-key-mode))

(use-package ace-window
     :disabled
     :ensure t
     :init 
     (global-set-key (kbd "M-o") 'ace-window)
     (setq aw-background nil))

;; Ranger
(use-package ranger
   :ensure t
   :commands ranger
   :custom
   (ranger-parent-depth 1)
   (ranger-hide-cursor t)
   (ranger-show-hidden t)
   (ranger-preview-file t)
   (ranger-width-parents 0.2)
   (ranger-width-preview 0.50)
   (ranger-footer-delay 0.2)
   (ranger-preview-delay 0.040)
   :config
   (ranger-override-dired-mode t))

;; Pretty Icons
(use-package all-the-icons
    :ensure t)

;; icons for dired/ranger mode
(use-package all-the-icons-dired
    :ensure t
    :after ranger
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package beacon
    :ensure t
    :diminish
    :custom
    (beacon-color "#ffc600")
    :config
    (beacon-mode 1))

;; Neotree
(use-package neotree
    :ensure t
    :defer t
    :config 
    (setq neo-smart-open t) ; update every time its toggled
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))) ; add icons (utilizes all-the-icons)

;; Projectile-mode 
(use-package projectile
    :ensure t
    :bind 
    (("C-c p" . projectile-command-map))
    :custom 
    (projectile-project-search-path '("~/Projects/"))
    (projectile-sort-order 'recently-active)
    (projectile-completion-system 'helm)
    :config
    (projectile-mode t))

(use-package helm-projectile 
    :ensure t
    :config
    (helm-projectile-on))

(use-package exec-path-from-shell
      :ensure t
      :config
      (when (memq window-system '(mac ns x)) ;; check if its mac
      (exec-path-from-shell-initialize)))

;; Eshell 
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
  (global-set-key (kbd "C-`") 'eshell)

(use-package undo-tree
    :ensure t
    :diminish
    :init
    (global-undo-tree-mode))

(use-package yasnippet
    :ensure t
    :bind 
    ((:map yas-keymap
    ("<tab>" . nil) ;; there are conflicts here with autocomplete
    ("<C-tab>" . yas-next-field-or-maybe-expand))
    (:map yas-minor-mode-map
    ("<tab>" . nil) ;; while this is convenient, it clashes with auto-complete and jump-to-definitions
    ("<C-tab>" . yas-expand)))
    :init
    (yas-global-mode 1)
    :config
    (yas-reload-all))

(use-package yasnippet-snippets :ensure t)

;; snippets for React.js
(use-package react-snippets
    :requires yasnippet
    :ensure t)

(use-package flycheck
     :ensure t
     :bind
     ("C-c f" . 'flycheck-buffer) ;; explicitly run flycheck
     :custom-face
     (flycheck-info ((t (:underline (:style line :color "#9500ff")))))
     (flycheck-warning ((t (:underline (:style line :color "#fbff00")))))
     (flycheck-error ((t (:underline (:style line :color "#ff0000")))))
     :custom
     (flycheck-check-syntax-automatically '(mode-enabled save)); run flycheck only on save
     :config
     (global-flycheck-mode t))

(use-package company
    :ensure t
    :bind
    ("C-c c" . company-complete) ;; for when I need completion at 1 or 2 chars
    (:map company-active-map
    ("<tab>" . nil) ;; I will use this for a different purpose shown below
    ("M-n" . nil) ;; old select next key
    ("M-p" . nil) ;; old select prev key
    ("<tab>" . company-select-next) ;; make tab our new select next key
    ("M-j" . company-select-next)  ;; also make M-j new select next key
    ("M-k"  . company-select-previous))
    :custom
    (company-tooltip-limit 5) ; show 5 candidates at one time
    (company-idle-delay 0.15) ;; delay (in seconds) when candidates are shown, change if you need to, potentially cpu intensive on older machines if set to 0
    (company-minimum-prefix-length 3) ;; show completions after 3 chars
    (company-selection-wrap-around t) ;; goes to start of selection if you reached the bottom 
    (company-require-match 'never) ;; dont need to pick a choice 
    :config
    (global-company-mode t))

;; elisp autocomplete
(defun my-elisp-mode-hook ()
"Hook for `emacs-lisp-mode'"
(set (make-local-variable 'company-backends)
'((company-capf company-elisp company-dabbrev-code company-yasnippet company-files))))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; hydra takes care of my magit bindings
(use-package magit :ensure t)

(use-package git-commit
    :after magit
    :custom
    (git-commit-summary-max-length 50) ;; in accordance with https://chris.beams.io/posts/git-commit/
    :config
    (setq git-commit-style-convention-checks
          '(non-empty-second-line
          overlong-summary-line)))

(use-package magit-repos
    :after magit
    :commands magit-list-repositories
    :custom
    (magit-repository-directories '(("~/Projects" . 1))))

(use-package gitignore-mode
    :ensure t
    :mode (("\\.gitignore\\'" . gitignore-mode)
           ("\\.dockerignore\\'" . gitignore-mode))) ;; syntax from gitignore is more or less identical to that of .dockerignore

(use-package gitconfig-mode
    :ensure t
    :mode "\\.gitconfig\\'")

(use-package git-timemachine
    :ensure t
    :commands git-timemachine)

;; smerge mode deals with merge conflicts in git. Prefix mapping is C-c v
(setq smerge-command-prefix "\C-cv")

(use-package hydra
    :ensure t
    :config
    (setq hydra-is-helpful t)
    (setq hydra-hint-display-type 'lv))

(defhydra hydra-zoom (:color pink)
  "zoom 🞈 🞈"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

;; help
(defhydra hydra-describe (:color red :columns 2)
  "Describe 🤓"
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

(defhydra hydra-projectile (:color red :columns 3)
  "🚀 Projectile 🚀"
  ("h" helm-projectile "launch")
  ("f" helm-projectile-find-file "find")
  ("w" helm-projectile-find-file-dwim "find-dwim")
  ("d" helm-projectile-find-dir "find-dir")
  ("a" helm-projectile-ag "ag") ;; need silversearcher-ag installed!
  ("g" helm-projectile-rg "ripgrep") ;; need ripgrep installed!
  ("s" helm-projectile-switch-project "switch project")
  ("b" helm-projectile-switch-to-buffer "buffer switch")
  ("r" projectile-recentf "recent files")
  ("k" projectile-kill-buffers "kill project buffers")
  ("q" nil "quit" :color blue))

;; My attempt at window management
(defhydra hydra-window (:color pink :columns 5)
 "⚡⚡ Helm + Windows ⚡⚡"
  ("f" helm-find-files "find")
  ("b" helm-mini "switch buffer")
  ;; splitting
  ("o" delete-other-windows "delete other windows")
  ("2" split-window-right "v-split")
  ("3" split-window-below "h-split")
  ;; deletion and quitting
  ("K" kill-current-buffer "kill current buffer")
  ("d" delete-window "delete window")
  ("D" kill-this-buffer "kill buffer")
  ;; resizing
  ("s" shrink-window "shrink window")
  ("e" enlarge-window "enlarge window")
  ("S" shrink-window-horizontally "shrink horizontally")
  ("E" enlarge-window-horizontally "shrink horizontally")
  ("B" balance-windows "balance windows")
  ;; movement
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("x" helm-M-x "M-x")
  ("q" nil "quit" :color blue))

;; git 
(defhydra hydra-git (:color red)
  "⏳ Git ⏳"
  ("g" magit "magit")
  ("d" magit-dispatch "dispatch")
  ("l" magit-list-repositories "list repos")
  ("t" git-timemachine "timemachine")
  ("q" nil "quit" :color blue))

;; lsp
(defhydra hydra-lsp (:color red :columns 3)
  "📡 LSP 📡"
  ("j" lsp-ui-peek-find-definitions "peek-def")
  ("r" lsp-ui-peek-find-references "peek-ref")
  ("c" lsp-rename "rename")
  ("f" lsp-find-definition "find-def")
  ("t" lsp-find-type-definition "find-type-def")
  ("e" flycheck-next-error "next error")
  ("p" flycheck-previous-error "prev error")
  ("l" lsp-ui-flycheck-list "list errors")
  ("b" switch-to-prev-buffer "back") ;; better consistancy than lsp-ui-peek-jump-backward
  ("n" switch-to-next-buffer "next") ;; better than lsp-ui-peek-jump-forward
  ("i" lsp-ui-imenu "imenu")
  ("q" nil "quit" :color blue))

(defhydra hydra-avy (:color red :columns 3)
  "↵ Avy ↵"
  ("c" avy-goto-char "goto char")
  ("C" avy-goto-char-2 "goto char 2")
  ("t" avy-goto-char-timer "timed char")
  ("w" avy-goto-word-1 "goto word")
  ("W" avy-goto-word-0 "goto word 0")
  ("l" avy-goto-line "goto line")
  ("r" avy-resume "resume")
  ("q" nil "quit" :color blue))

(setq
  browse-url-browser-function
  '(("https://www.netflix.com/" . browse-url-firefox) ;; firefox deals better with video players
  ("." . browse-url-chromium)))

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

(defhydra hydra-writing (:color red :columns 2)
 "✓ Writing and Spelling ✓"
 ("d" ispell-change-dictionary "change dict")
 ("s" ispell-word "spell word")
 ("f" flyspell-buffer "flyspell buffer")
 ("m" flyspell-mode "flyspell mode")
 ("r" flyspell-region "flyspell region")
 ("n" flyspell-goto-next-error "next error")
 ("w" writegood-mode "writegood mode")
 ("q" nil "quit"))

(defhydra hydra-utility (:color red :columns 4)
 "😎 Utility 😎"
 ("m" goto-MarinMacs "goto config")
 ("b" gdb "gdb")
 ("e" eval-buffer "eval buffer")
 ("d" dap-debug "dap debug")
 ("i" dap-debug-edit-template "debug template")
 ("r" helm-recentf "recent files")
 ("g" helm-rg "ripgrep")
 ("a" helm-ag "ag")
 ("z" fzf "fzf")
 ("f" flycheck-buffer "flycheck buffer")
 ("R" ranger "ranger")
 ("c" compile "compile")
 ("u" auto-fill-mode "auto-fill-mode")
 ("w" web-mode "web-mode")
 ("j" rjsx-mode "rjsx-mode")
 ("q" nil "quit"))

(defun marinov/goto-org-directory ()
  "goes to my org directory"
  (interactive)
  (find-file org-directory))

(defun marinov/jump-to-notes ()
  "go to notes file"
  (interactive)
  (find-file org-default-notes-file))

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
  ("s" org-edit-special "edit special")
  ("x" org-edit-src-exit "exit special")
  ("n" marinov/jump-to-notes "goto notes")
  ("D" marinov/goto-org-directory "goto org directory")
  ("q" nil "quit"))

(use-package lsp-mode
    :ensure t
    :diminish
    :commands lsp
    :hook
    ((c++-mode . lsp)
    (c-mode . lsp)
    (python-mode . lsp)
    (css-mode . lsp)
    (yaml-mode . lsp)
    (json-mode . lsp)
    (js2-mode . lsp)
    (rjsx-mode . lsp)
    (typescript-mode . lsp)
    (web-mode . lsp))
    :custom
    (lsp-idle-delay 0.15)
    (lsp-flycheck-live-reporting nil) ;; allows our previous flycheck setting to only check syntax on save to work
    :config
    (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

(use-package lsp-ui
    :after lsp-mode flycheck
    :ensure t
    :diminish
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)
    :bind
    (:map lsp-ui-peek-mode-map
    ("M-j" . lsp-ui-peek--select-next)
    ("M-k" . lsp-ui-peek--select-prev))
    :custom
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-delay 0.65) ;; display doc after 6.5/10 of a second
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-sideline-enable nil)
    (lsp-ui-flycheck-list-position 'right)
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-list-width 60)
    (lsp-ui-peek-peek-height 25)
    :config
    (setq eldoc-idle-delay 0.65)) ;; delay eldoc for 6.5/10 second

(use-package company-lsp
    :requires company
    :ensure t
    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil) ;; Disable client-side cache because the LSP server does a better job.
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t)
    :config
    (push 'company-lsp company-backends))

;; enable hydra bindings in dap mode
(use-package dap-hydra
    :ensure nil
    :requires hydra)

;; only installing because dap-mode requires it
(use-package posframe :ensure t)

(use-package dap-mode
    :ensure t
    :requires hydra
    :hook
    (lsp-mode . (lambda () (dap-mode t) (dap-ui-mode t) (dap-tooltip-mode 1) (tooltip-mode 1)))
    :config
    (add-hook 'dap-stopped-hook
    (lambda (arg) (call-interactively #'dap-hydra)))) ;; enable hydra on breakpoint stop

(use-package tex
   :disabled
   :ensure auctex
   :config
   (setq TeX-auto-save t)
   (setq TeX-parse-self t)
   (setq TeX-save-query nil))


;;;;;;;;;;;;;;;;;;
;; PDF
;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
    :disabled
    :ensure t)

(setq-default c-basic-offset 4) ;; indentation for C-based languages

;; disable other checkers since we only want to utilize clangd language server
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)) 

;; enable modern font lock for >=c++11
(use-package modern-cpp-font-lock
    :ensure t
    :config
    (modern-c++-font-lock-global-mode t))

(use-package dap-gdb-lldb
  :ensure nil
  :requires dap-mode
  :config
  (dap-register-debug-template
  "GDB config"
  (list :type "gdb"
        :request "launch"
        :name "GDB::Run"
        :target "test"
        :program "test"
        :cwd "/home/marin/Projects")))

(use-package clang-format 
    :ensure t
    :diminish
    :bind 
    (("C-c R" . clang-format-region) ;; format current line
    ("C-c F" . clang-format-buffer))) ;; format entire file

;; formats file on save
(use-package clang-format+
    :quelpa (clang-format+
             :fetcher github
             :repo "SavchenkoValeriy/emacs-clang-format-plus")
             :config
             (add-hook 'c-mode-common-hook #'clang-format+-mode))

;; version 
(setq py-python-command "python3")
(setq python-shell-interpreter "python3")

;; indentation
(setq-default python-basic-offset 4) 
(setq-default python-indent-offset 4) 
(setq python-indent-guess-indent-offset t) ;; allow emacs to guess offset
(setq python-indent-guess-indent-offset-verbose nil) ;; remove indent warning because we already set indents

;; warnings
(setq lsp-pyls-plugins-pycodestyle-enabled nil) ;; comment if you want code style warnings everywhere

(use-package dap-python
  :ensure nil
  :requires dap-mode
  :config
  (dap-register-debug-template "My App"
  (list :type "python"
        :args "-i"
        :cwd nil
        :env '(("DEBUG" . "1"))
        :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
        :request "launch"
        :name "My App")))

(use-package elpy
   :disabled
   :ensure t
   :config 
   (elpy-enable))

(use-package virtualenvwrapper
   :disabled
   :ensure t
   :config
   (venv-initialize-interactive-shells)
   (venv-initialize-eshell))

(use-package web-mode
    :ensure t
    :mode
    (("\\.html?\\'"      . web-mode)
    ("\\.phtml\\'"       . web-mode)
    ("\\.tpl\\.php\\'"   . web-mode)
    ("\\.blade\\.php\\'" . web-mode)
    ("\\.[agj]sp\\'"     . web-mode)
    ("\\.as[cp]x\\'"     . web-mode)
    ("\\.erb\\'"         . web-mode)
    ("\\.mustache\\'"    . web-mode)
    ("\\.djhtml\\'"      . web-mode)
    ("\\.js\\'"          . web-mode))
    :custom
    ;; Indentation
    (web-mode-attr-indent-offset 2)
    (web-mode-markup-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-css-indent-offset 2)
    ;; Auto-closing
    (web-mode-auto-close-style 2)
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-auto-quoting t)
    ;; Highlighting
    (web-mode-enable-current-column-highlight t)
    (web-mode-enable-current-element-highlight t)
    :config
    (setq web-mode-enable-engine-detection t))

    (setq-default css-indent-offset 2) ;; web mode for some reason cancels css autocomplete so I have to configure css separately

(use-package rainbow-mode 
    :ensure t
    :init 
    (rainbow-mode 1))

(use-package prettier-js
    :ensure t
    :hook
    ((js-mode . prettier-js-mode)
    (js2-mode . prettier-js-mode)
    (web-mode . prettier-js-mode)
    (typescript-mode . prettier-js-mode)
    (rjsx-mode . prettier-js-mode)))

(use-package rjsx-mode
    :ensure t
    :mode
    (("\\.jsx\\'"  . rjsx-mode))
    :init
    (setq-default rjsx-basic-offset 2))

(use-package json-mode
    :ensure t
    :commands json-mode)

(use-package yaml-mode
    :ensure t
    :commands yaml-mode
    :mode (("\\.yml\\'" . yaml-mode)
           ("\\.yaml\\'" . yaml-mode)))

(use-package graphql-mode
    :ensure t
    :commands graphql-mode
    :mode
    (("\\.\\(gql\\|graphql\\)\\'" . graphql-mode)))

(use-package dockerfile-mode 
    :ensure t
    :commands dockerfile-mode
    :mode
    (("Dockerfile'"       . dockerfile-mode)
    ("\\.Dockerfile\\'"  . dockerfile-mode)))

(use-package csv-mode 
    :ensure t
    :commands csv-mode)

(use-package skewer-mode
    :disabled
    :ensure t
    :commands skewer-mode run-skewer
    :config
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    (skewer-setup))

(use-package impatient-mode
    :disabled
    :ensure t)

(use-package emmet-mode
    :ensure t
    :hook
    ((css-mode  . emmet-mode)
    (php-mode  . emmet-mode)
    (sgml-mode . emmet-mode)
    (rjsx-mode . emmet-mode)
    (web-mode  . emmet-mode)))

(use-package add-node-modules-path
    :ensure t
    :hook 
    ((web-mode . add-node-modules-path)
    (rjsx-mode . add-node-modules-path)))

;; debugging in chrome
(use-package dap-chrome
    :ensure nil
    :requires dap-mode)

;; debugging in firefox
(use-package dap-firefox
    :ensure nil
    :requires dap-mode)

(use-package dap-node
    :ensure nil
    :requires dap-mode)

(use-package js2-mode
    :ensure t
    :config 
    (setq js2-strict-missing-semi-warning nil)
    (setq-default js2-basic-offset 2)) ;; set indentation to 2

;; enable typescript in emacs
(use-package typescript-mode
    :ensure t
    :mode (("\\.ts\\'" . typescript-mode)
           ("\\.tsx\\'" . typescript-mode))
    :config
    (setq-default typescript-indent-level 2)) ;; indent 2 spaces by default

;; typescript integrated development environment
(use-package tide
    :ensure t
    :after
    (typescript-mode company flycheck)
    :hook
    ((typscript-mode . tide-setup)
    (typescript-mode . tide-hl-identifier-mode)
    (before-save . tide-format-before-save))
    :config
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))

(use-package diminish
    :ensure t
    :init
    (diminish 'yas-minor-mode)
    (diminish 'modern-c++-font-lock-mode)
    (diminish 'auto-revert-mode)
    (diminish 'page-break-lines-mode)
    (diminish 'eldoc-mode)
    (diminish 'abbrev-mode))
