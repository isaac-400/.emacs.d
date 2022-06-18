;;; documents.el -*- lexical-binding: t; -*-

;; --- ORG MODE ---
;; org-mode is fantastic as well. I keep things close to default.
(setq org-directory "~/Dropbox/")

(setq if/default-org-file "~/Dropbox/todo.org")
;;(setq initial-buffer-choice "~/Dropbox/todo.org")

(setq org-agenda-files (directory-files-recursively "~/Dropbox/" "\.org$"))
(setq org-todo-keywords
      '((sequence  "TODO(t)" "PROJ(p)" "MAYBE(m)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))

;; Skip completed items
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
;; Skip deleted files
(setq org-agenda-skip-unavailable-files t)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline if/default-org-file "INBOX")
         "* NEXT %?\n" :clock-in t :clock-resume t)
        ("c" "caffeine" table-line (file+headline if/default-org-file "Caffeine")
         "|%T|%?|")
        ("m" "mood" table-line (file+headline "~/Dropbox/todo.org" "Mood")
         "|%T|%^{Rating? (1: worst, 5: best|1|2|3|4|5}|%?|")))
(setq org-image-actual-width nil)

(setq org-startup-indented t
      org-src-tab-acts-natively t)
(use-package org-pomodoro)

;; org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (scheme . t)
    (gnuplot . t)))


;; read pdfs in emacs but better

(use-package pdf-tools
  :demand t
  :config
  (pdf-loader-install))
