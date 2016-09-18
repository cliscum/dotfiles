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
    column-marker
    diminish
    flatui-theme
    helm
    highlight-parentheses
    python
    rainbow-delimiters
    smartparens
    web-mode
    ))

(defun homecookin/post-init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init (progn
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

(defun homecookin/init-column-marker ()
  (use-package column-marker
    :defer t
    :ensure t
    :init (progn
            (message "+ homecookin/init-column-marker :init")
            (add-hook 'prog-mode-hook (lambda () (column-marker-1 80))))))

(defun homecookin/post-init-diminish ()
  (use-package diminish
    :defer t
    :config (progn
              (message "+ homecookin/post-init-diminish :config")
              (with-eval-after-load 'wakatime-mode
                (spacemacs|diminish wakatime-mode " ⓦ" " w")
                ))))

(defun homecookin/init-flatui-theme ()
  (use-package flatui-theme
    :defer t
    :config
    (progn
      (message "+ homecookin/init-flatui-theme :config")
      (flatui/with-color-variables
        (custom-theme-set-faces
         'flatui
         `(ahs-face ((t (:weight bold :foreground ,sun-flower))))
         `(ahs-plugin-whole-buffer-face ((t (:weight bold :background ,sun-flower :foreground ,wet-asphalt))))
         `(column-marker-1 ((t (:inverse-video t))))
         `(company-preview ((t (:background ,silver :foreground ,belize-hole))))
         `(company-preview-common ((t (:weight bold :background ,silver :foreground ,belize-hole))))
         `(company-scrollbar-bg ((t (:background ,concrete))))
         `(company-scrollbar-fg ((t (:background ,sun-flower))))
         `(company-tooltip ((t (:background ,silver))))
         `(company-tooltip-common ((t (:background ,silver))))
         `(company-tooltip-common-selection ((t (:weight bold :background ,silver))))
         `(company-tooltip-selection ((t (:weight bold :background ,silver :foreground ,belize-hole))))
         `(cursor ((t (:background ,sun-flower))))
         `(default ((t (:background "#ffffff" :foreground ,midnight-blue))))
         `(fringe ((t (:background ,silver :foreground ,concrete))))
         `(header-line ((t (:background ,clouds :box (:line-width 1 :color ,silver)))))
         `(helm-header-line-left-margin ((t (:background ,nephritis :foreground ,clouds))))
         `(highlight-indentation-current-column-face ((t (:background ,silver))))
         `(highlight-indentation-face ((t (:background ,silver))))
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

(defun homecookin/post-init-helm ()
  (use-package helm
    :defer t
    :bind (("C-x M-f" . helm-find-files)
           ("C-x M-b" . helm-buffers-list)
           ("C-x b" . helm-projectile-switch-to-buffer))))

(defun homecookin/post-init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :disabled t
    :init (progn
            (message "+ homecookin/post-init-highlight-parentheses :init")
            (with-eval-after-load 'highlight-parentheses
              (setq hl-paren-colors '(,clouds ,clouds ,pomegranate))
              (setq hl-paren-background-colors '(,wisteria ,peter-river nil))
              ))))

(defun homecookin/post-init-python ()
  (use-package python
    :defer t
    :init (progn
            (message "+ homecookin/post-init-python :init")
            ;; (with-eval-after-load 'python-mode
            ;;   (flycheck-mode -1)
            ;;   (show-smartparens-mode -1)
            ;;   )
            (define-derived-mode python-no-flycheck-mode python-mode
              "Python mode with flycheck disabled."
              (flycheck-mode nil))
            (add-to-list 'auto-mode-alist
                         '("BUILD\\'" . python-no-flycheck-mode))
            )))

(defun homecookin/post-init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :ensure t
    :init (progn
            (message "+ homecookin/post-init-rainbow-delimiters :init")
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))))

(defun homecookin/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config (progn
              (message "+ homecookin/post-init-smartparens :config")
              ;; (with-eval-after-load 'smartparens
              ;;   (sp-pair "(" nil :actions :rem)
              ;;   (sp-pair "[" nil :actions :rem)
              ;;   (sp-pair "'" nil :actions :rem)
              ;;   (sp-pair "\"" nil :actions :rem)
              ;;   (show-paren-mode nil)
              ;;   (show-smartparens-mode nil)
              ;;   )
              )))

(defun homecookin/post-init-web-mode ()
  (use-package web-mode
    :defer t
    :mode (("\\.jsx\\'" . web-mode)
           ("\\.tsx\\'" . web-mode))
    ))

;;; packages.el ends here
