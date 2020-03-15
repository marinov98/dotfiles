(setq-default indent-tabs-mode nil)                    ;; disable tabs and use spaces
(setq-default tab-width 4)                             ;; set default tab width 4 
(setq backward-delete-char-untabify-method 'hungry)    ;; backspaces entire tab instead of one space at a time

(setq default-frame-alist '((font . "Fira Code-14")
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
      "d" 'hydra-describe/body
      "g" 'hydra-git/body
      "TAB" 'hydra-launcher/body
      "v" 'hydra-writing/body
      "m" 'hydra-marinov/body
      "a" 'hydra-avy/body
      ;; file finding, and searching
      "f" 'find-file
      "i" 'isearch-forward
      "x" 'execute-extended-command
      ;; buffers
      "s" 'save-buffer
      "b" 'switch-to-buffer
      "p" 'switch-to-prev-buffer
      "n" 'switch-to-next-buffer
      ;; deletion
      "q" 'delete-window
      "Q" 'save-buffers-kill-terminal
      "k" 'kill-current-buffer
      "o" 'delete-other-windows
      ;; package-specific
      "t" 'neotree-toggle
      "c" 'avy-goto-char-timer))

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


;; current theme I am running
 (use-package spacemacs-common
     :ensure spacemacs-theme
     :config (load-theme 'spacemacs-dark t))
     
;; others
 (use-package zerodark-theme
     :disabled
     :ensure t)
 
 (use-package minimal-theme
     :disabled
     :ensure t
     :config
     (load-theme 'minimal t))
   
 (use-package nord-theme
     :disabled
     :ensure t
     :config
     (load-theme 'nord t))

 (use-package zenburn-theme
     :disabled
     :ensure t
     :config
     (load-theme 'zenburn t))
    
 (use-package poet-theme
     :disabled
     :ensure t)

 (use-package monokai-theme
     :disabled
     :ensure t
     :config (load-theme 'monokai t))
 

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
 
 (use-package gruvbox-theme
     :disabled
     :ensure t
     :config
     (load-theme 'gruvbox t))

 (use-package base16-theme
     :disabled
     :ensure t
     :config 
     (load-theme 'base16-ocean t))

 (use-package jbeans-theme
     :disabled
     :ensure t
     :config
     (load-theme 'jbeans t))

 (use-package solarized-theme
     :disabled
     :ensure t
     :config
     (load-theme 'solarized-dark t))
 
 (use-package planet-theme
     :disabled
     :ensure t
     :config 
     (load-theme 'planet t))

(use-package minibuffer
    :config
    (setq completion-cycle-threshold 3)
    (setq completion-flex-nospace nil)
    (setq completion-pcm-complete-word-inserts-delimiters t))

(use-package icomplete
    :demand
    :config
    (setq icomplete-delay-completions-threshold 0)
    (setq icomplete-max-delay-chars 0)
    (setq icomplete-compute-delay 0)
    (setq icomplete-show-matches-on-no-input t)
    (setq icomplete-hide-common-prefix nil)
    (setq icomplete-prospects-height 1)
    (setq icomplete-separator " · ") 
    (setq icomplete-with-completion-tables t)
    (setq icomplete-in-buffer t)
    (icomplete-mode 1))

(use-package avy :ensure t)

(use-package which-key
	:ensure t 
	:config
	(which-key-mode))

;; Pretty Icons
(use-package all-the-icons
    :ensure t)

;; icons for dired/ranger mode
(use-package all-the-icons-dired
    :ensure t
    :after ranger
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Neotree
(use-package neotree
    :ensure t
    :defer t
    :config 
    (setq neo-smart-open t) ; update every time its toggled
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))) ; add icons (utilizes all-the-icons)

(use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g b" . dumb-jump-back)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window))
   :ensure)

;; Projectile-mode 
(use-package projectile
    :ensure t
    :bind 
    (("C-c p" . projectile-command-map))
    :custom 
    (projectile-project-search-path '("~/Projects/"))
    (projectile-sort-order 'recently-active)
    (projectile-completion-system 'ivy)
    :config
    (projectile-mode t))

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
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

;; help
(defhydra hydra-describe (:color red :columns 2)
  "Describe"
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

;; projectile, I would change this hydra's global key if I wasn't using vim bindings...
(defhydra hydra-projectile (:color red :columns 3)
  "Projectile"
  ("f" projectile-find-file "find")
  ("w" projectile-find-file-dwim "find-dwim")
  ("d" projectile-find-dir "find-dir")
  ("a" projectile-ag "ag") ;; need silversearcher-ag installed!
  ("s" projectile-switch-project "switch project")
  ("b" projectile-switch-to-buffer "buffer switch")
  ("r" projectile-recentf "recent files")
  ("k" projectile-kill-buffers "kill project buffers")
  ("q" nil "quit" :color blue))

;; My attempt at window management
(defhydra hydra-window (:color pink :columns 4)
 "Windows"
  ("f" find-file "find")
  ("b" switch-buffer "switch buffer")
  ;; splitting
  ("1" delete-other-windows "delete other windows")
  ("2" split-window-right "v-split")
  ("3" split-window-below "h-split")
  ;; deletion and quitting
  ("K" kill-current-buffer "kill current buffer")
  ("d" delete-window "delete window")
  ("D" kill-this-buffer "kill buffer")
  ;; movement
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("x" execute-extended-command "M-x")
  ("q" nil "quit" :color blue))

;; git 
(defhydra hydra-git (:color red)
  "Git"
  ("g" magit "magit")
  ("d" magit-dispatch "dispatch")
  ("t" git-timemachine "timemachine")
  ("q" nil "quit" :color blue))

(defhydra hydra-avy (:color red :columns 3)
  "Avy"
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
  "Launch"
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
 "Writing and Spelling"
 ("d" ispell-change-dictionary "change dict")
 ("s" ispell-word "spell word")
 ("f" flyspell-buffer "flyspell buffer")
 ("m" flyspell-mode "flyspell mode")
 ("r" flyspell-region "flyspell region")
 ("n" flyspell-goto-next-error "next error")
 ("w" writegood-mode "writegood mode")
 ("q" nil "quit"))

(defhydra hydra-marinov (:color red :columns 4)
 "Marinov"
 ("m" goto-MarinMacs "goto config")
 ("b" gdb "gdb")
 ("f" flycheck-buffer "flycheck buffer")
 ("R" ranger "ranger")
 ("c" compile "compile")
 ("w" web-mode "web-mode")
 ("j" rjsx-mode "rjsx-mode")
 ("q" nil "quit"))

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

;; enable modern font lock for >=c++11
(use-package modern-cpp-font-lock
    :ensure t
    :config
    (modern-c++-font-lock-global-mode t))

(use-package clang-format 
    :ensure t
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

(use-package diminish
    :ensure t
    :init
    (diminish 'undo-tree-mode)
    (diminish 'clang-format-mode)
    (diminish 'clang-format+-mode)
    (diminish 'modern-c++-font-lock-mode)
    (diminish 'auto-revert-mode)
    (diminish 'page-break-lines-mode)
    (diminish 'evil-mc-mode)
    (diminish 'eldoc-mode)
    (diminish 'abbrev-mode)
    (diminish 'beacon-mode)
    (diminish 'yas-minor-mode)
    (diminish 'which-key-mode))
