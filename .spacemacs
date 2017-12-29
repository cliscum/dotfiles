;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.emacs/layers/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      )
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t
            )
     chrome
     command-log
     csv
     docker
     emacs-lisp
     git
     (go :variables go-use-gometalinter t)
     gtags
     homecookin
     html
     imenu-list
     ivy
     javascript
     markdown
     nlinum
     org
     python
     react
     (ruby :variables
           ruby-version-manager 'rbenv
           )
     (scala :variables
            scala-enable-eldoc-mode t
            )
     semantic
     shell-scripts
     sql
     syntax-checking
     typescript
     version-control
     (wakatime :variables
               ;; NOTE: The key is located in ~/.wakatime.cfg.
               wakatime-cli-path "/usr/bin/wakatime"
               wakatime-python-bin "/usr/bin/python3"
               )
     yaml
     )
   dotspacemacs-additional-packages
   '(
     protobuf-mode
     )
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-active-transparency 100
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-check-for-update t
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-command-key ":"
   dotspacemacs-default-font '("Inconsolata"
                               :powerline-scale 1.2
                               :size 16
                               :weight normal
                               :width normal
                               )
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-default-package-repository nil
   dotspacemacs-display-default-layout nil
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-editing-style 'emacs
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-highlight-delimiters nil
   dotspacemacs-inactive-transparency 100
   dotspacemacs-leader-key "m"
   dotspacemacs-line-numbers t
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-persistent-server nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 12
   dotspacemacs-themes '(flatui)
   dotspacemacs-use-ido nil
   dotspacemacs-verbose-loading t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (message "+ dotspacemacs/user-init")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (message "+ dotspacemacs/user-config")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-default)
  (setq-default powerline-default-separator (quote arrow))

  (spacemacs/add-to-hooks 'spacemacs/toggle-golden-ratio-on '(prog-mode-hook))
  ;; (add-hook 'prog-mode-hook 'spacemacs/toggle-highlight-indentation-on)
  ;; (add-hook 'prog-mode-hook (lambda () (smartparens-mode -1)))

  (spacemacs/toggle-aggressive-indent-globally-off)
  (spacemacs/toggle-camel-case-motion-globally-on)
  (spacemacs/toggle-centered-point-globally-off)
  (spacemacs/toggle-highlight-current-line-globally-on)
  (spacemacs/toggle-indent-guide-globally-on)
  ;; (spacemacs/toggle-semantic-stickyfunc-globally-on)
  (spacemacs/toggle-smartparens-globally-off) ; this doesn't work
  (spacemacs/toggle-whitespace-globally-on)

  (message "- dotspacemacs/user-config")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("065efdd71e6d1502877fd5621b984cded01717930639ded0e569e1724d058af8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(evil-want-Y-yank-to-eol t)
 '(flycheck-coffeelintrc "~/coffeelint.json")
 '(highlight-indentation-offset 2)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (company-quickhelp winum unfill org-category-capture nlinum-relative nlinum xresources-theme fontawesome unicode-fonts pcache dockerfile-mode docker tablist docker-tramp minitest insert-shebang hide-comnt go-guru yapfify yaml-mode ws-butler window-numbering which-key wgrep web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tide typescript-mode tagedit stickyfunc-enhance srefactor sql-indent spacemacs-theme spaceline powerline smex smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode protobuf-mode popwin pip-requirements persp-mode pcre2el paradox spinner orgit org-projectile org-present org org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file noflet neotree mwim move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ivy-hydra info+ indent-guide imenu-list ido-vertical-mode hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-make helm helm-core haml-mode google-translate golden-ratio go-eldoc gnuplot gmail-message-mode ham-mode markdown-mode html-to-markdown gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md ggtags flycheck-pos-tip pos-tip flycheck-gometalinter flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight ensime sbt-mode scala-mode emmet-mode elisp-slime-nav edit-server dumb-jump disaster diminish diff-hl define-word cython-mode csv-mode counsel-projectile projectile pkg-info epl counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-go go-mode company-c-headers company-anaconda company command-log-mode column-marker column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed async anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup quelpa package-build flatui-theme)))
 '(perl-indent-level 2)
 '(python-indent-offset 2)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (defun projectile-project-type nil
             (quote sbt)))
     (eval progn
           (require
            (quote projectile))
           (defun projectile-project-type nil
             (quote gulp)))
     (eval progn
           (require
            (quote projectile))
           (defun projectile-project-type nil
             (quote go))))))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(smartparens-global-mode t)
 '(swiper-include-line-number-in-search t)
 '(swiper-min-highlight 2)
 '(tramp-connection-timeout 10)
 '(typescript-indent-level 2)
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(whitespace-style (quote (face empty tabs lines-tail trailing))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#2c3e50" :background "#ecf0f1")))))
