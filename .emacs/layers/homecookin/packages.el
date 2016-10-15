;;; packages.el --- homecookin layer packages file for Spacemacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst homecookin-packages
  '(
    auto-highlight-symbol
    cc-mode
    column-marker
    counsel-projectile
    flatui-theme
    highlight-parentheses
    (pmd :location local)
    python
    rainbow-delimiters
    rcirc-notify
    sbt-mode
    smartparens
    wakatime-mode
    ))

(defun homecookin/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :init
    (progn
      (message "+ homecookin/post-init-auto-highlight-symbol :init")
      (setq-default
       auto-highlight-symbol-mode-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "M-<up>") 'ahs-backward)
         (define-key map (kbd "M-<down>") 'ahs-forward)
         (define-key map (kbd "M-S-<up>") 'ahs-backward-definition)
         (define-key map (kbd "M-S-<down>") 'ahs-forward-definition)
         (define-key map (kbd "M--") 'ahs-back-to-start)
         (define-key map (kbd "C-x C-'") 'ahs-change-range)
         (define-key map (kbd "C-x C-a") 'ahs-edit-mode)
         map)))))

(defun homecookin/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    ;; List of specific rules for pmd
    (setq pmd-specific-rules '(("prU" . "unusedcode")
                               ("prE" . "empty")
                               ("prN" . "unnecessary")
                               ("prB" . "basic")
                               ("prI" . "imports")))

    ;; Generates spacemacs binding to call each specific rule
    ;; - first, generates interactive function
    (mapc (lambda (r) (eval `(mk-pmd-rule-function ,(cdr r))))
          pmd-specific-rules)
    ;; - then, creates spacemacs shortcut to this call.
    (mapc (lambda (r)
            (let ((plabel (car r))
                  (pfun (intern (concat "pmd-project-" (cdr r))))
                  (blabel (downcase (car r)))
                  (bfun (intern (concat "pmd-buffer-" (cdr r)))))
              (spacemacs/set-leader-keys-for-major-mode
                'java-mode plabel pfun blabel bfun)))
          pmd-specific-rules)

    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      ;; PMD
      ;; I have to put this here since pmd is lazy load
      "pb" 'pmd-current-buffer
      "pp" 'pmd-current-sbt-project
      )
    ))

(defun homecookin/init-column-marker ()
  (use-package column-marker
    :ensure t
    :init
    (progn
      (message "+ homecookin/init-column-marker :init")
      (add-hook 'prog-mode-hook (lambda () (column-marker-1 80))))))

(defun homecookin/post-init-counsel-projectile ()
  (use-package counsel-projectile
    ;; :defer t
    :config
    (progn
      (message "+ homecookin/post-init-counsel-projectile :config")

      ;; Redefine this to add a preselect param. TODO better way to do this?
      (defun counsel-projectile-switch-to-buffer (&optional virtual)
        "Switch to a project buffer (homecookin).
If optional argument VIRTUAL is non-nil, add project files as
virtual buffers."
        (interactive)
        (ivy-read (projectile-prepend-project-name "Switch to buffer: ")
                  (counsel-projectile--buffer-list virtual)
                  :matcher #'ivy--switch-buffer-matcher
                  :action #'counsel-projectile--switch-buffer-action
                  :preselect (buffer-name (other-buffer (current-buffer)))
                  :require-match t
                  :keymap counsel-projectile-map
                  :caller 'counsel-projectile-switch-to-buffer))
      )))

(defun homecookin/init-flatui-theme ()
  (use-package flatui-theme
    :config
    (progn
      (message "+ homecookin/init-flatui-theme :config")
      (flatui/with-color-variables
        (custom-theme-set-faces
         'flatui
         `(ahs-face ((t (:weight bold :foreground ,sun-flower))))
         `(ahs-plugin-whole-buffer-face ((t (:weight bold :background ,sun-flower :foreground ,wet-asphalt))))
         `(column-marker-1 ((t (:inverse-video t))))
         `(command-log-key ((t (:foreground ,carrot))))
         `(command-log-command ((t (:inherit default))))
         `(company-preview ((t (:background ,silver :foreground ,belize-hole))))
         `(company-preview-common ((t (:weight bold :background ,silver :foreground ,belize-hole))))
         `(company-scrollbar-bg ((t (:background ,concrete))))
         `(company-scrollbar-fg ((t (:background ,sun-flower))))
         `(company-tooltip ((t (:background ,silver))))
         `(company-tooltip-common ((t (:background ,silver))))
         `(company-tooltip-common-selection ((t (:weight bold :background ,silver))))
         `(company-tooltip-selection ((t (:weight bold :background ,silver :foreground ,belize-hole))))
         `(cursor ((t (:background ,peter-river))))
         `(default ((t (:background "#ffffff" :foreground ,midnight-blue))))
         `(fringe ((t (:background ,silver :foreground ,concrete))))
         `(header-line ((t (:background ,clouds :box (:line-width 1 :color ,silver)))))
         `(highlight ((t (:background ,clouds))))
         `(highlight-indentation-current-column-face ((t (:background ,silver))))
         `(highlight-indentation-face ((t (:background ,silver))))
         `(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground ,amethyst))))
         `(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground ,belize-hole))))
         `(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground ,green-sea))))
         `(imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face :foreground ,nephritis))))
         `(isearch ((t (:background ,amethyst :foreground ,clouds))))
         `(isearch-fail ((t (:background ,alizarin :foreground ,clouds))))
         `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,nephritis))))
         `(ivy-current-match ((t (:inherit isearch-lazy-highlight-face))))
         `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,alizarin))))
         `(ivy-minibuffer-match-face-1 ((t (:background ,silver :weight bold))))
         `(ivy-minibuffer-match-face-2 ((t (:background ,green-sea :foreground ,clouds :weight bold))))
         `(ivy-minibuffer-match-face-3 ((t (:background ,peter-river :foreground ,clouds :weight bold))))
         `(ivy-minibuffer-match-face-4 ((t (:background ,amethyst :foreground ,clouds :weight bold))))
         `(ivy-remote ((t (:foreground ,belize-hole))))
         `(match ((t (:background ,nephritis :foreground ,clouds))))
         `(mode-line ((t (:foreground ,clouds :background ,midnight-blue))))
         `(mode-line-buffer-id ((t (:inherit bold :foreground ,sun-flower))))
         `(mode-line-highlight ((t (:underline t))))
         `(mode-line-inactive ((t (:foreground ,concrete :background ,midnight-blue))))
         `(persp-selected-face ((t (:foreground ,belize-hole))))
         `(powerline-active1 ((t (:background ,green-sea :foreground ,clouds))))
         `(powerline-active2 ((t (:background ,wet-asphalt :foreground ,clouds))))
         `(powerline-inactive1 ((t (:background ,midnight-blue :foreground ,concrete))))
         `(powerline-inactive2 ((t (:background ,midnight-blue :foreground ,emerald))))
         `(rainbow-delimiters-depth-1-face ((t (:foreground ,wet-asphalt))))
         `(rainbow-delimiters-depth-10-face ((t (:foreground ,belize-hole))))
         `(rainbow-delimiters-depth-11-face ((t (:foreground ,nephritis))))
         `(rainbow-delimiters-depth-12-face ((t (:foreground ,green-sea))))
         `(rainbow-delimiters-depth-2-face ((t (:foreground ,amethyst))))
         `(rainbow-delimiters-depth-3-face ((t (:foreground ,peter-river))))
         `(rainbow-delimiters-depth-4-face ((t (:foreground ,emerald))))
         `(rainbow-delimiters-depth-5-face ((t (:foreground ,turquoise))))
         `(rainbow-delimiters-depth-6-face ((t (:foreground ,sun-flower))))
         `(rainbow-delimiters-depth-7-face ((t (:foreground ,carrot))))
         `(rainbow-delimiters-depth-8-face ((t (:foreground ,alizarin))))
         `(rainbow-delimiters-depth-9-face ((t (:foreground ,wisteria))))
         `(secondary-selection ((t (:inverse-video t))))
         `(show-paren-match ((t (:inverse-video t :weight bold))))
         `(show-paren-mismatch ((t (:background ,alizarin :foreground ,clouds :weight bold))))
         `(whitespace-tab ((t (:background ,sun-flower))))
         )))))

(defun homecookin/post-init-highlight-parentheses ()
  (use-package highlight-parentheses
    :disabled t
    :init
    (progn
      (message "+ homecookin/post-init-highlight-parentheses :init")
      (with-eval-after-load 'highlight-parentheses
        (setq hl-paren-colors '(,clouds ,clouds ,pomegranate))
        (setq hl-paren-background-colors '(,wisteria ,peter-river nil))
        ))))

(defun homecookin/init-pmd ()
  (use-package pmd
    :commands (pmd-current-buffer pmd-file-or-dir)
    ;; :init
    ;; (push 'pmd company-backends-java-mode)
    :config
    ;; Point to the correct directories for java and pmd-home
    (setq pmd-java-home "/usr/bin/env java")
    (setq pmd-home "/usr/share/java/pmd")
    (setq pmd-ruleset-list
          (list "java-basic" "java-braces" "java-clone" "java-codesize"
                "java-controversial" "java-coupling" "java-design" "java-empty"
                "java-finalizers" "java-imports" "java-junit"
                "java-logging-jakarta-commons" "java-logging-java" "java-naming"
                "java-optimizations" "java-strictexception" "java-strings"
                "java-sunsecure" "java-unnecessary" "java-unusedcode"))
    ))

(defun homecookin/post-init-python ()
  (use-package python
    :init
    (progn
      (message "+ homecookin/post-init-python :init")
      (define-derived-mode python-no-flycheck-mode python-mode
        "Python mode with flycheck disabled."
        (flycheck-mode nil))
      (add-to-list 'auto-mode-alist '("BUILD\\'" . python-no-flycheck-mode))
      (add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-no-flycheck-mode))
      )))

(defun homecookin/post-init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :ensure t
    :init
    (progn
      (message "+ homecookin/post-init-rainbow-delimiters :init")
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))))

(defun homecookin/post-init-rcirc-notify ()
  (use-package rcirc-notify
    :config
    (progn
      (message "+ homecookin/post-init-rcirc-notify :config")

      (setq rcirc-notify-message "%s: %s")
      (setq rcirc-notify-popup-timeout 5000)
      (setq rcirc-notify-timeout 0)

      (defun my-rcirc-notify (proc sender response target text)
        "Notify always."
        (interactive)
        (when (and (not (string= sender (rcirc-nick proc)))
                   (not (string= sender (rcirc-server-name proc)))
                   (rcirc-notify-allowed sender))
          (rcirc-notify sender text)))

      (add-hook 'rcirc-print-hooks 'my-rcirc-notify)
      (remove-hook 'rcirc-notify-page-me-hooks 'spacemacs/rcirc-notify-beep)
      )))

(defun homecookin/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init
    (push 'sbt-mode company-backends-java-mode)
    :config
    ;; compilation-skip-threshold tells the compilation minor-mode
    ;; which type of compiler output can be skipped. 1 = skip info
    ;; 2 = skip info and warnings.
    (setq compilation-skip-threshold 1)

    ;; On 'sbt-run-previous-command skip the question to save buffers and
    ;; have buffers saved automatically instead.
    (setq compilation-ask-about-save nil)

    ;; (spacemacs/declare-prefix-for-mode 'java-mode "ms" "sbt")
    ))

(defun homecookin/post-init-smartparens ()
  (use-package smartparens
    :config
    (progn
      (message "+ homecookin/post-init-smartparens :config")
      (with-eval-after-load 'smartparens
        (sp-pair "(" nil :actions :rem)
        (sp-pair "{" nil :actions :rem)
        (sp-pair "[" nil :actions :rem)
        (sp-pair "'" nil :actions :rem)
        (sp-pair "\"" nil :actions :rem)
        ;; (show-paren-mode nil)
        ;; (show-smartparens-mode nil)
        )
      )))

(defun homecookin/post-init-wakatime-mode ()
  (use-package wakatime-mode
    :config
    (progn
      (message "+ homecookin/post-init-wakatime-mode :config")
      (spacemacs|diminish wakatime-mode " 🕒" " W")
      )))

;;; packages.el ends here
