;;; init.el -*- lexical-binding: t; -*-
;; Isaac's Emacs Config (from scratch)

;; Who am I?
(setq user-full-name "Isaac Feldman"
      user-mail-address "isaaccfeldman@gmail.com")

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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


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

(defconst if/emacs-user-path (file-name-directory load-file-name)
	"Where the init files live")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file if/emacs-user-path)))

(load-user-file "lisp/documents.el")
(load-user-file "lisp/programming.el")
(load-user-file "lisp/appearance.el")
(load-user-file "lisp/core.el")
(load-user-file "lisp/other.el")
(load-user-file "lisp/amazon.el")


 (setq eww-retrieve-command
           '("wget" "-Sq" "-O" "-"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" "88267200889975d801f6c667128301af0bc183f3450c4b86138bfb23e8a78fb1" "0340489fa0ccbfa05661bc5c8c19ee0ff95ab1d727e4cc28089b282d30df8fc8" default))
 '(org-export-backends '(ascii html icalendar latex md odt org))
 '(org-export-with-toc nil)
 '(org-html-htmlize-output-type 'inline-css)
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
