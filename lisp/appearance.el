;;; appearance.el -*- lexical-binding: t; -*-
;; Appearance related config

;; Minor display changes
(setq display-line-numbers-type "relative")
(global-display-line-numbers-mode 1)            ; Display line numbers in every buffer
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq fill-column 80)
(setq-default line-spacing 1)
(set-face-attribute 'default nil :height 160)
(setq enable-recursive-minibuffers t)

;; Laptop Power monitor
(unless (string-match-p "^Power N/A" (battery))  ; On laptops...
  (display-battery-mode 1))                      ; it's nice to know how much power you have

(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; Flash when the bell rings
(setq ring-bell-function 'ignore); NEVER RING THE BELL

;; Modus Theme
(use-package modus-themes)
(load-theme 'modus-operandi t)

;; transpose-frame
(use-package transpose-frame)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ansi-colors in comint buffers
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
