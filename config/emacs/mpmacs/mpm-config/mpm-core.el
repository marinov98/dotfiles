;; mpm-core.el --- personal preferences and core stuff -*- lexical-binding: t; -*-

;;; Commentary 
;; how I prefer to set somethings up

;;; Code:
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
      (display-line-numbers-type 'relative)
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
  :custom
  (exec-path-from-shell-arguments '("-l" "-c"))
  :config
  (when (or (memq window-system '(mac ns x)) (daemonp))
    (exec-path-from-shell-initialize))
)

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

(use-package yasnippet
    :ensure t
    :hook (after-init . yas-global-mode)
    :diminish yas-minor-mode
    :config
    ;; nullify default tab behavior (tab complete ONLY with autocomplete)
    (define-key yas-minor-mode-map [(tab)] nil)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; add new behavior
    (evil-define-key 'insert yas-minor-mode-map (kbd "C-;") 'yas-expand)
    (evil-define-key 'insert yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
)

(use-package yasnippet-snippets
    :after yasnippet
    :ensure t
    :config
    (yas-reload-all)
)

(use-package dashboard
    :ensure t
    :custom
    (dashboard-banner-logo-title "MPMacs")
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

(provide 'mpm-core)
;;; mpm-core.el ends here
