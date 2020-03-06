;;; package --- summary
;;; Commentary:
;;; Code:

;;;;;;;; General

(setq c-basic-offset 4)    ;; indents 4 chars
(setq tab-width 4)         ;; and 4 char wide for TAB
(setq indent-tabs-mode nil);; And force use of spaces
(global-font-lock-mode 1)  ;; syntax highlighting
(require 'linum)
(global-linum-mode 1)      ;; line numbers
(electric-pair-mode)       ;; auto closing brackets
(setq debug-on-error t)    ;; debugging
(fset 'yes-or-no-p 'y-or-n-p) ;; change yes or no to y or n
(menu-bar-mode -1) ;; disable menu bar
(toggle-scroll-bar -1) ;; disable scroll bar
(tool-bar-mode -1) ;; disable tool bar 

;;;; Key-Bindings
(global-set-key (kbd "C-S-p") 'beginning-of-buffer) ;; top of file
(global-set-key (kbd "C-S-n") 'end-of-buffer) ;; end of file
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "<f5>") 'revert-buffer) ;; allow window reload using F5
(global-set-key (kbd "C-`") 'better-shell-shell) ;; open better shell 
(global-set-key (kbd "C-;") 'better-shell-remote-open)
(global-set-key (kbd "s-r") 'compile)
(global-set-key (kbd "C-c C-e") 'eval-buffer) ;; reload


;;;;;;;; General end

;;;;;;;;; Enable the ability to download packages
(require 'package)
;; add as many package-archives as you want below 
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")t)
(package-initialize)

;;;;;;;; Convinience packages

;; Which-key
(which-key-setup-side-window-bottom)

;; Auto-complete 
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Iedit mode
(define-key global-map (kbd "C-c c") 'iedit-mode)

;; Magit (Git control for emacs)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Ivy, Swiper & Counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Company
(require 'company)
(setq company-idle-delay 0) ;; faster auto-completion
(setq company-minimum-prefix-length 3) ;; begin autocompletion after 3 characters have been typed 

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)

;; Neotree
(require 'neotree)
(global-set-key (kbd "C-c C-t")  'neotree-toggle)
;; controls :
;; n (next) p(prev)
;; SPC RET or TAB : open file or fold/unfold dir
;; g : refresh
;; A : maximize or minimize tree
;; H : toggle display hidden files
;; C-c C-n : create file or directory
;; C-c C-d : delete
;; C-c C-r : rename
;; C-c C-c : change root

;; Treemacs
;; (require 'treemacs)

;; Ranger
(require 'ranger)
;; (ranger-mode) actual  ranger
(ranger-override-dired-mode t) ;; minimal ranger mode

;; Projectile-mode NOTE: s stands for command on MacOS, windows button for Windows
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
(define-key projectile-mode-map (kbd "s-g") 'projectile-grep)
(setq projectile-completion-system 'ivy) ;; enable counsel-projectile
(projectile-mode +1)

;; Dump-jump (jump to definition)
(require 'dumb-jump)
(dumb-jump-mode)
;; dumb-jump-go C-M-g core functionality. Attempts to jump to the definition for the thing under point
;; dumb-jump-back C-M-p jumps back to where you were when you jumped.
;; dumb-jump-quick-look C-M-q like dumb-jump-go but only shows tooltip with file, line, and context

;; Ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-background nil) ;; turn off background
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


;;;; Telephone line
(require 'telephone-line)

(setq telephone-line-primary-right-separator 'telephone-line-abs-left
      telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)


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
(telephone-line-mode 1)

;;;; Beacon
(require 'beacon)
(beacon-mode 1)

;;;; Avy

;; go to the selected character
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

;; Go to Line
(global-set-key (kbd "M-g f") 'avy-goto-line)

;; Go to word
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;;;; DashBoard
(require 'dashboard)
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "The Marinov Emacs ;)")
;; Set the banner
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)))

;;;;;;;; Convinience end

;;;;;;;; Org

;; Auto-Complete for org mode
(require 'org-ac)
(org-ac/config-default)
(add-to-list 'ac-modes 'org-mode)

;; Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;;;;;;; Org end

;;;;;;;; C++

;; Set language standard
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++14")))

;; Enable modern C++ font lock
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; Clang-format
(require 'clang-format)
(global-set-key (kbd "C-c u") 'clang-format-region)
(global-set-key (kbd "C-c f") 'clang-format-buffer)
(setq clang-format-style-option ".clang-format") ;; another option is llvm 
;(add-hook 'c++-mode-hook 'clang-format) ;; this gives issues for some reason

;;;; C++ intellisense

;; Company-irony
(require 'company-irony)
(add-to-list 'company-backends 'company-irony)

;; C headers auto completion with irony
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Enable irony mode for C/C++/Objective-C
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; Eldoc indexing
(add-hook 'irony-mode-hook #'irony-eldoc)

;; Initiate company mode when opening/creating C/C++ files
(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

;; Flycheck-irony  
(require 'flycheck-irony)
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  (setq irony--compile-options
      '("-std=c++14"        ;; general 
        "-stdlib=libc++"))) ;; for mac only 

;; ggtags 
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))


;;;;;;;; End of C++

;;;;;;;; Python

;; UNCOMMENT ONLY IF YOU NEED A PYTHON SHELL 
;; virtualenv (interactive python shell) 
;(require 'virtualenvwrapper)
;(venv-initialize-interactive-shells)
;(venv-initialize-eshell)
;(setq venv-location "/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/py2app/recipes/virtualenv.py")

;;;; Elpy powerful python integrated environment
(setq visible-bell t)
(require 'elpy)
(require 'package)
(require 'json)
(package-initialize)
(elpy-enable)
;; fixing bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; Company-Jedi Intellisense
(require 'company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

 (add-hook 'python-mode-hook 'my/python-mode-hook)

;;;;;;;; Python end

;;;;;;;; C#

(require 'csharp-mode)
(defun my-csharp-mode-hook ()
  "Csharp development."
  (electric-pair-local-mode 1))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;;;;;;; C# end

;;;;;;;; Javascript

;; Js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; refactor and xref
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; Company-tern Intellisense for JavaScript
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; Js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
;;;;;;;; JavaScript end

;;;;;;;;  Web-Development (html/css)

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Csswatcher (uncomment only if you've installed csswatcher and really need it) 
;(require 'ac-html-csswatcher)
;(ac-html-csswatcher-setup)

;; Indentation
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-css-indent-offset 4)

;; Auto-closing
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t)

;; Highlighting
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

;; Company-web auto-completion for html/css
(require 'company-web-html)
(defun my-web-mode-hook ()
  "Company auto-complete."
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Es-lint for javascript
(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; Use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

;;;;;;;; End of Web-Development
