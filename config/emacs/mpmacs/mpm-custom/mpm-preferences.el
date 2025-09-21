;; mpm-preferences.el --- personal preferences and core stuff -*- lexical-binding: t; -*-

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

(provide 'mpm-preferences)
;;; mpm-preferences.el ends here
