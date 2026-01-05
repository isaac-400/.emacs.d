;;; documents.el -*- lexical-binding: t; -*-

;; --- ORG MODE ---
;; org-mode is fantastic. I keep things close to default.

(use-package org
  :demand t
  :pin gnu
  :bind (("C-c l" . org-stored-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-directory "~/org/")

  (setq if/default-org-file "~/org/todo.org")

  (setq org-agenda-files
        (directory-files-recursively "~/org/" "\.org$"))
  (setq org-todo-keywords
        '((sequence  "TODO(t)" "IN PROGRESS(p)" "IN REVIEW(r)" "|"
                     "DONE(d)" "CANCELLED(c)" "DEFERRED(f)" "SOMEDAY(s)")))

  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  ;; Skip deleted files
  (setq org-agenda-skip-unavailable-files t)
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline if/default-org-file "INBOX")
           "* TODO %?\n" :clock-in t :clock-resume t)))
  
  (setq org-image-actual-width nil)
  (setq org-startup-indented t
        org-src-tab-acts-natively t))

(add-hook 'org-mode-hook #'visual-line-mode)

;; org export to github flavored markdown
(use-package ox-gfm
  :ensure t)
(eval-after-load "org"
  '(load-library "ox-gfm"))

(use-package ob-async)

;; extra org mode libraries
(use-package org-contrib)
(require 'ol-git-link)

(use-package org-pomodoro)

;; Needs terminal-notifier (brew install terminal-notifier)
(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message))
                
;; org-pomodoro mode hooks
(add-hook 'org-pomodoro-finished-hook
          (lambda ()
          (notify-osx "Pomodoro completed!" "Time for a break.")))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
          (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-killed-hook
          (lambda ()
          (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))

(use-package ob-typescript)

;; org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (scheme . t)
    (typescript . t)
    (java . t)
    (eshell . t)
    (plantuml . t)
    (shell .t)))

;; redisplay images after executing a code block,
;; this is useful for plantuml diagrams.
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+datetree "worklog.org" week))))

(use-package org-transclusion
  :after org)

(use-package org-remark
  :after org
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map. Adjust the keybinds
         ;; as you see fit.
         ("C-c m m" . org-remark-mark)
         ("C-c m l" . org-remark-mark-line)
         :map org-remark-mode-map
         ("C-c m o" . org-remark-open)
         ("C-c m ]" . org-remark-view-next)
         ("C-c m [" . org-remark-view-prev)
         ("C-c m r" . org-remark-remove)
         ("C-c m d" . org-remark-delete)))

;; read pdfs in emacs  better
(use-package pdf-tools
  :demand t
  :config
  (pdf-loader-install))

(use-package olivetti)

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))
