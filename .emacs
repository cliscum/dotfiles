;;; .emacs --- Casey's Emacs config
;;;
;;; Commentary:
;;;
;;; My GNU Emacs configuration
;;;
;;; Code:

(require 'package)
(require 'seq)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 2)
        ("gnu" . 1)
        ("melpa" . 0)))

(package-initialize)

(let ((to-check (list 'use-package)))
  (let ((to-install (seq-remove 'package-installed-p to-check)))
    (if (> (length to-install) 0)
        (progn
          (package-refresh-contents)
          (dolist (p to-install) (package-install p))))))

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))
(setq use-package-always-ensure t)

(dolist (combo (list "C-z" "C-x C-z"))
  (global-unset-key (kbd combo)))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-subword-mode)

(use-package ace-window
  :bind
  (("M-p" . ace-window))
  )

(use-package ag)

(use-package auto-complete)

(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  )

(use-package avy
  :bind (("C-c a s" . avy-goto-char-timer)
         ("C-c a l" . avy-goto-line))
  )

(use-package better-defaults
  :after (helm)
  :config
  ;; TODO: 0.1.3 always sets ido-mode, but subsequent version will not
  ;; if helm-mode is on.
  (ido-mode nil)
  )

(use-package coffee-mode)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )

(use-package fill-column-indicator
  :init
  (add-hook 'prog-mode-hook 'fci-mode)
  )

(use-package company
  :config
  (global-company-mode)
  )

(use-package company-statistics
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode)
  )

(use-package counsel
  :config
  (counsel-mode)
  )

(use-package csv-mode)

(use-package dockerfile-mode)

(use-package expand-region
  :bind
  (("M-<up>" . er/expand-region)
   ("M-<down>" . er/contract-region))
  )

(use-package flatui-theme
  :config
  (load-theme 'flatui t)
  )

(use-package flycheck
  :init
  (global-flycheck-mode)
  )

(use-package flycheck-pos-tip
  :after flycheck
  :init
  (flycheck-pos-tip-mode)
  )

(use-package go-mode)

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  )

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.5)
  (guide-key-mode t)
  )

(use-package helm
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x))

  :config
  (require 'helm-config)
  (helm-mode 1)
  )

(use-package helm-ag)

(use-package helm-descbinds)

(use-package helm-flycheck
  :after flycheck
  :init
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
  )

(use-package helm-projectile
  :config
  (helm-projectile-toggle 1)
  )

(use-package ivy
  :config
  (ivy-mode)
  )

(use-package js2-mode)

(use-package json-mode)

(use-package magit
  :bind
  (("C-c m s" . magit-status))
  )

(use-package markdown-mode)

(use-package mwim
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  )

(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  )

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  )

(use-package python
  :config
  (define-derived-mode python-bazel-build-mode python-mode
    "Bazel BUILD"
    (setq python-indent-offset 4)
    (flycheck-mode nil))
  (define-derived-mode python-skylark-mode python-mode
    "Skylark"
    (flycheck-mode nil))
  (add-to-list 'auto-mode-alist '("BUILD\\'" . python-bazel-build-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-bazel-build-mode))
  (add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-skylark-mode))
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package smartparens)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sql)

(use-package super-save
  :diminish
  :init
  (super-save-mode)
  )

(use-package swiper-helm
  :bind
  (("C-c s" . swiper-helm))
  )

(use-package tide)

(use-package typescript-mode)

(use-package undo-tree
  :bind (("M-_" . undo-tree-redo))

  :init
  (global-undo-tree-mode)
  )

(use-package wakatime-mode
  :init
  (global-wakatime-mode)
  )

(use-package web-mode
  :after (flycheck tide)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

(use-package whitespace
  :diminish global-whitespace-mode

  :init
  (add-hook 'before-save-hook 'whitespace-cleanup)

  :config
  (global-whitespace-mode)
  )

(use-package yaml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(auto-save-default nil)
 '(blink-matching-delay 0.5)
 '(blink-matching-paren (quote jump))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/xdg-open")
 '(c-basic-offset 2)
 '(c-offsets-alist
   (quote
    ((func-decl-cont . ++)
     (annotation-var-cont . 0)
     (inher-intro . ++)
     (arglist-intro . ++))))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(fci-rule-column 80)
 '(flycheck-coffeelintrc "~/coffeelint.json")
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode t)
 '(helm-command-prefix-key "C-c h")
 '(highlight-indentation-offset 2)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice (quote helm-find))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (dashboard esup flycheck-pos-tip helm-flycheck super-save jiggle-mode web-mode spaceline fill-column-indicator column-marker wakatime-mode undo-tree powerline expand-region golden-ratio counsel yaml-mode use-package tide smartparens rainbow-delimiters markdown-mode magit json-mode js2-mode ivy helm-projectile helm-ag guide-key go-mode ensime eclim dockerfile-mode delight csv-mode coffee-mode better-defaults auto-complete ace-window)))
 '(perl-indent-level 2)
 '(projectile-completion-system (quote helm))
 '(python-indent-offset 2)
 '(recentf-mode t)
 '(sh-basic-offset 2)
 '(show-paren-delay 0.25)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(smartparens-global-mode t)
 '(super-save-auto-save-when-idle t)
 '(swiper-include-line-number-in-search t)
 '(swiper-min-highlight 2)
 '(tab-width 2)
 '(tramp-connection-timeout 10 nil (tramp))
 '(typescript-indent-level 2)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(whitespace-style (quote (face empty tabs lines-tail trailing tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
