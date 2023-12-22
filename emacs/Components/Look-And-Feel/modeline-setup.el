;; modeline-setup.el --- set emacs modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; visuals matters so choose the modeline well

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;   
;; Spaceline
;;;;;;;;;;;;;;;;;;;;;;   

  (use-package spaceline
      :disabled
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
      :ensure t
      :hook (after-init . doom-modeline-mode))

(provide 'modeline-setup)
;;; modeline-setup.el ends here
