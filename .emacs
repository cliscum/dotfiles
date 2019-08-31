;;; .emacs --- Casey's Emacs config
;;;
;;; Commentary:
;;;
;;; My GNU Emacs configuration
;;;
;;; Code:

(defvar-local homecookin/t0 (current-time))
(message (format "BEGIN %s" (format-time-string "%c" homecookin/t0)))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 2)
        ("gnu" . 1)
        ("melpa" . 0)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (setq use-package-always-ensure t))

(dolist (combo (list "C-z" "C-x C-z"))
  (global-unset-key (kbd combo)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun homecookin/set-local-whitespace-style-no-tabs ()
  "Strip 'tabs' from whitespace-style for the local buffer."
  (defvar whitespace-style)
  (setq-local whitespace-style
              (seq-remove (lambda (s) (eq s 'tabs)) whitespace-style)))

(use-package ace-window
  :bind (("M-p" . ace-window)))

(use-package ag)

(use-package ansi-color
  :init
  (defun homecookin/colorize-compilation-buffer ()
    (declare-function ansi-color-apply-on-region "ansi-color")
    (defvar compilation-filter-start)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'homecookin/colorize-compilation-buffer))

(use-package auto-complete)

(use-package auto-package-update
  :config (auto-package-update-now)
  :custom (auto-package-update-delete-old-versions t))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package avy
  :bind (("C-c v s" . avy-goto-char-timer)
         ("C-c v l" . avy-goto-line)))

(use-package better-defaults
  :after helm
  :config
  (progn
    ;; TODO: 0.1.3 always sets ido-mode, but subsequent version will not
    ;; if helm-mode is on.
    (ido-mode nil)
    ;; Undo a couple settings that I don't like.
    (setq mouse-yank-at-point nil)
    (setq select-enable-primary nil)))

(use-package browse-url
  :custom
  (browse-url-browser-function (quote browse-url-generic))
  (browse-url-generic-program "/usr/bin/xdg-open"))

(use-package coffee-mode
  :custom (coffee-tab-width 2))

(use-package company
  :config (global-company-mode))

(use-package company-statistics
  :init (add-hook 'company-mode-hook #'company-statistics-mode))

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package csv-mode)

(use-package dashboard
  :config (dashboard-setup-startup-hook)
  :custom (initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package delsel
  :config (delete-selection-mode))

(use-package dockerfile-mode)

(use-package emacs
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)
         ("C-+" . text-scale-increase)))

(use-package expand-region
  :bind (("M-<up>" . er/expand-region)
         ("M-<down>" . er/contract-region)))

(use-package fill-column-indicator
  :config (add-hook 'prog-mode-hook 'fci-mode)
  :custom (fci-rule-column 80))

(use-package fix-word
  :bind (("M-c" . fix-word-capitalize)
         ("M-l" . fix-word-downcase)
         ("M-u" . fix-word-upcase)))

(use-package flatui-theme)

(use-package flycheck
  :config (global-flycheck-mode)
  :custom (flycheck-coffeelintrc "~/coffeelint.json"))

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode))

(use-package go-mode
  :init (add-hook 'go-mode-hook
                  (lambda ()
                    (homecookin/set-local-whitespace-style-no-tabs)
                    (add-hook 'before-save-hook #'gofmt-before-save t t))))

(use-package gnutls
  :custom
  ;; This works around a bug causing Bad Requests responses from
  ;; https://elpa.gnu.org/packages/
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package golden-ratio
  :config (golden-ratio-mode 1)
  :custom (golden-ratio-auto-scale t))

(use-package guide-key
  :config (guide-key-mode t)
  :custom
  (guide-key/guide-key-sequence t)
  (guide-key/idle-delay 0.5))

(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x))
  :init
  (defvar helm-find-files-map)
  :config
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (define-key helm-find-files-map "\t" 'helm-execute-persistent-action))
  :custom
  (helm-M-x-always-save-history t)
  (helm-command-prefix-key "C-c c"))

(use-package helm-ag)

(use-package helm-descbinds)

(use-package helm-flycheck
  :after flycheck
  :init (define-key flycheck-mode-map (kbd "C-c ! h") #'helm-flycheck))

(use-package helm-projectile
  :config (helm-projectile-toggle 1))

(use-package helm-swoop
  :bind (("C-c s" . helm-swoop)
         ("M-i" . helm-swoop)))

(use-package highlight-indentation
  :custom (highlight-indentation-offset 2))

(use-package hl-line
  :custom (global-hl-line-mode t))

(use-package htmlize
  :after fill-column-indicator
  :defer t
  :config
  (progn
    ;; Ripped from
    ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-htmlize.el
    ;;
    ;; It is required to disable `fci-mode' when `htmlize-buffer' is called;
    ;; otherwise the invisible fci characters show up as funky looking
    ;; visible characters in the source code blocks in the html file.
    ;; http://lists.gnu.org/archive/html/emacs-orgmode/2014-09/msg00777.html
    (with-eval-after-load 'fill-column-indicator
      (defvar homecookin/htmlize-initial-fci-state nil
        "Variable to store the state of `fci-mode' when
        `htmlize-buffer' is called.")
      (defun homecookin/htmlize-before-hook-fci-disable ()
        (setq homecookin/htmlize-initial-fci-state fci-mode)
        (when fci-mode
          (fci-mode -1)))
      (defun homecookin/htmlize-after-hook-fci-enable-maybe ()
        (when homecookin/htmlize-initial-fci-state
          (fci-mode 1)))
      (add-hook 'htmlize-before-hook
                'homecookin/htmlize-before-hook-fci-disable)
      (add-hook 'htmlize-after-hook
                'homecookin/htmlize-after-hook-fci-enable-maybe))))

(use-package js
  :custom (js-indent-level 2))

(use-package js2-mode)

(use-package json-mode)

(use-package jsonnet-mode)

(use-package magit
  :bind (("C-c m b" . magit-blame-addition)
         ("C-c m r" . magit-rebase-interactive)
         ("C-c m s" . magit-status)))

(use-package make-mode
  :config
  (add-hook 'makefile-mode-hook 'homecookin/set-local-whitespace-style-no-tabs))

(use-package markdown-mode)

(use-package multiple-cursors
  :bind (("C-M-e" . mc/edit-lines)
         ("C-M-<down>" . mc/mark-next-like-this)
         ("C-M-<up>" . mc/mark-previous-like-this)
         ("C-M-<right>" . mc/mark-all-like-this)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package ob-go)

(use-package ob-ipython)

(use-package ob-typescript)

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-agenda-files (list "~/Dropbox/Documents/org"))
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-startup-truncated nil)
  (org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(use-package org-bullets
  :after org
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package ox-gfm)

(use-package paren
  :custom
  (show-paren-delay 0.25)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t))

(use-package perl-mode
  :custom (perl-indent-level 2))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (declare-function projectile-mode "projectile")
  :config (projectile-mode)
  :custom
  (projectile-completion-system (quote helm))
  (projectile-tags-command "ctags -Re -f \"%s\" --exclude=\"bazel-*\" %s")
  (projectile-tags-file-name ".TAGS"))

(use-package protobuf-mode)

(use-package python
  :after session
  :init (declare-function python-mode "python")
  :config
  (progn
    (define-derived-mode homecookin/python-bazel-build-mode python-mode
      "Bazel BUILD"
      (setq-local python-indent-offset 4)
      (flycheck-mode nil)
      (add-hook 'after-save-hook
                (lambda ()
                  (shell-command
                   (concat "buildifier -mode=fix " buffer-file-name))
                  (revert-buffer nil t t))
                t t))
    (dolist (f (list "BUILD\\'" "WORKSPACE\\'" "\\.bzl\\'"))
      (add-to-list 'auto-mode-alist
                   (cons f 'homecookin/python-bazel-build-mode))))
  :custom (python-indent-offset 2))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :custom (recentf-mode t))

(use-package savehist
  :config (savehist-mode t)
  :custom
  (savehist-additional-variables (quote (regexp-search-ring search-ring))))

(use-package saveplace
  :custom
  (save-place-ignore-files-regexp "COMMIT_EDITMSG\\'"))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package seq)

(use-package session
  :commands session-initialize
  :init (add-hook 'after-init-hook #'session-initialize)
  :custom
  (session-set-file-name-exclude-regexp
   "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|COMMIT_EDITMSG"))

(use-package sh-script
  :custom (sh-basic-offset 2))

(use-package smartparens
  :custom (smartparens-global-mode t))

(use-package spaceline
  :init
  (require 'spaceline-config)
  (declare-function spaceline-emacs-theme "spaceline-config")
  :config (spaceline-emacs-theme))

(use-package sql)

(use-package subword
  :config (global-subword-mode))

(use-package super-save
  :diminish
  :config (super-save-mode)
  :custom (super-save-auto-save-when-idle t))

(use-package tide)

(use-package tramp
  :custom (tramp-connection-timeout 10 nil (tramp)))

(use-package typescript-mode
  :custom (typescript-indent-level 2))

(use-package undo-tree
  :bind (("M-_" . undo-tree-redo))
  :config (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package wakatime-mode
  :config (global-wakatime-mode)
  :custom (wakatime-cli-path "/usr/bin/wakatime"))

(use-package web-mode
  :after (flycheck tide)
  :init
  (progn
    (declare-function flycheck-add-mode "flycheck")
    (declare-function setup-tide-mode "tide")
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "tsx"
                                    (file-name-extension buffer-file-name))
                  (setup-tide-mode)))
              t t)
    (flycheck-add-mode 'typescript-tslint #'web-mode))
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-sql-indent-offset 2))

(use-package whitespace
  :diminish global-whitespace-mode
  :init (add-hook 'before-save-hook #'whitespace-cleanup)
  :config (global-whitespace-mode)
  :custom
  (whitespace-style (quote (face empty tabs lines-tail trailing tab-mark))))

(use-package yaml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-matching-delay 0.5)
 '(blink-matching-paren (quote jump))
 '(c-basic-offset 2)
 '(c-offsets-alist
   (quote
    ((func-decl-cont . ++)
     (annotation-var-cont . 0)
     (inher-intro . ++)
     (arglist-intro . ++))))
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (fix-word menu-bar emacs-lisp-mode emacs-lisp faces use-package-ensure makefile makefile-mode subr org-bullets ob-go ob-ipython ob-typescript ox-gfm htmlize jsonnet-mode use-package helm-swoop dashboard esup flycheck-pos-tip helm-flycheck super-save jiggle-mode web-mode spaceline fill-column-indicator column-marker wakatime-mode undo-tree powerline expand-region golden-ratio yaml-mode use-package tide smartparens rainbow-delimiters markdown-mode magit json-mode js2-mode helm-projectile helm-ag guide-key go-mode ensime eclim dockerfile-mode delight csv-mode coffee-mode better-defaults auto-complete ace-window)))
 '(session-use-package t nil (session))
 '(tab-width 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "IBM Plex Mono")))))

(let ((t1 (current-time)))
  (message (format "DONE %s (%g seconds)"
                   (format-time-string "%c" t1)
                   (float-time (time-subtract t1 homecookin/t0)))))

;;; .emacs ends here
