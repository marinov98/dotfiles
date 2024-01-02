;; language-component.el --- Language Component Loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Load all the desired languages

;;; Code:

(message "Loading Languages Component")

(require 'web-dev-setup)
(require 'python-setup)
(require 'js-ts-setup)
(require 'elixir-setup)
(require 'c-cpp-setup)
(require 'java-setup)
(require 'rust-setup)
(require 'go-setup)

(message "Languages Component Successfully Loaded!")


(provide 'language-component)
;;; language-component.el ends here
