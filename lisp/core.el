;;; core.el -*- lexical-binding: t; -*

(setq shell-file-name "/bin/bash")

;; because I forget emacs bindings all the time
(use-package which-key
  :config
  (which-key-mode))

(setq mac-option-modifier 'meta)

;; Do not clutter the filesystem with backup files/lockfiles
(setq backup-directory-alist `(("." . "~/.cache/emacs")))

(use-package no-littering)

;; No ugly lockfiles
(setq create-lockfiles nil)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq enable-recursive-minibuffers t)

;; tab = 2 spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;;--- COMPLETION FRAMEWORKS ---

;; ivy?
(use-package ivy
  :init
  (ivy-mode 1))

;; consult
(use-package consult)
;; company
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; avy
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;; Spelling Correction
(use-package flyspell-correct
  :bind ("C-'" . flyspell-correct-wrapper))

;; magit
(use-package magit)

;; fix the exec path on OSX
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; TRAMP config
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

;; ripgrep interface
(use-package deadgrep)

;; change-inner emulates the vim `ci` motions
(use-package change-inner)

;; which pairs nicely with
(delete-selection-mode 1)

;; use the mouse in the terminal
(xterm-mouse-mode 1)

;; set up project.el
(use-package project)
