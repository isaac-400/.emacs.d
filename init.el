;;; init.el -*- lexical-binding: t; -*-
;; Isaac's Emacs Config (from scratch)

;; Who am I?
(setq user-full-name "Isaac Feldman"
      user-mail-address "isaac.c.feldman.23@dartmouth.edu")

;; straight.el Package Manger
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))


;; Garbage Collection Optimization -- https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(use-package gcmh :defer t)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216) ; 16mb
    (setq gc-cons-percentage 0.1)
    (require 'gcmh)
    (gcmh-mode 1)))


;; Loading my other config files -> https://stackoverflow.com/a/2079146
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


(load-user-file "lisp/programming.el")
(load-user-file "lisp/appearance.el")
(load-user-file "lisp/core.el")
(load-user-file "lisp/documents.el")

