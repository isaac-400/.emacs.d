;;; programming.el -*- lexical-binding: t; -*-
;; Programming related config

;; scheme
(use-package geiser-guile)
(use-package paredit
  :hook (scheme-mode . paredit-mode))

;; typescript
(use-package typescript-mode)

;; json
(use-package json-mode)

;; yaml
(use-package yaml-mode)

;; python
(setq python-python-command "python3") ;; default to python3

;; go
(use-package go-mode)
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist '(go-test "^\s*--- \\(FAIL\\):\s\\([A-Za-z/=]*\\)\s\\(([0-9.]+s)\\)$"))

(use-package eglot)
(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server" "--preview"))
               '(go-mode . ("golsp ")))
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((buildFlags . ["-tags=integration,unit,firecracker_integration"]))))))

(add-hook 'go-mode-hook 'eglot-ensure)

;; tree-sitter syntax highlighting and structural editing
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)

(require 'treesit)

(add-to-list
 'treesit-language-source-alist
 '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"))

;; Flycheck
(use-package flycheck)

(use-package sml-mode)

(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el"))

;; Set up a mode for JSON based templates

(define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")
  
  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))

;; nix
(use-package nix-mode
  :mode "\\.nix\\'")


;; term
;; config from here https://www.reddit.com/r/emacs/comments/wu5rxi/comment/ilagtzv/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(use-package vterm
  :ensure t
  :bind (:map project-prefix-map
              ("t" . project-vterm))
  :preface
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :init
  (add-to-list 'project-switch-commands     '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode))
  :config
  (setq vterm-copy-exclude-prompt t)
  (setq vterm-max-scrollback 100000))
