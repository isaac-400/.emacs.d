;;; programming.el -*- lexical-binding: t; -*-
;; Programming related config

;; scheme
(use-package geiser-guile)
(use-package paredit
  :hook (scheme-mode . paredit-mode))
