;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Works with Spacemacs version: 0.105.11

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.emacs/layers/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      )
     better-defaults
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t
            )
     command-log
     dockerfile
     emacs-lisp
     git
     go
     gtags
     homecookin
     html
     ivy
     javascript
     markdown
     python
     react
     (ruby :variables
           ruby-version-manager 'rbenv
           )
     (scala :variables
            scala-enable-eldoc-mode t
            )
     semantic
     sql
     syntax-checking
     typescript
     version-control
     (wakatime :variables
               ;; NOTE: The key is located in ~/.wakatime.cfg.
               wakatime-cli-path "/usr/local/bin/wakatime"
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
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-command-key ":"
   dotspacemacs-default-font '("Inconsolata"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   dotspacemacs-leader-key "SPC"
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
  (setq-default powerline-default-separator (quote slant))

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

  ;; TODO remove this in spacemacs >= 0.106.x
  (spaceline-compile)

  (message "- dotspacemacs/user-config")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
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
    ("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(fci-rule-color "#f1c40f" t)
 '(flycheck-coffeelintrc "~/coffeelint.json")
 '(highlight-indentation-offset 2)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")) t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (command-log-mode yapfify yaml-mode ws-butler window-numbering which-key wgrep web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tide tagedit stickyfunc-enhance srefactor sql-indent spacemacs-theme spaceline smex smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort protobuf-mode popwin pip-requirements persp-mode paradox orgit org-plus-contrib org-bullets open-junk-file noflet neotree mwim move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc jade-mode ivy-hydra info+ indent-guide ido-vertical-mode hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-make google-translate golden-ratio go-eldoc gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags flycheck-pos-tip flx-ido flatui-theme fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu ensime emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word cython-mode counsel-projectile company-web company-tern company-statistics company-go company-c-headers company-anaconda column-marker column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
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
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(swiper-include-line-number-in-search t)
 '(tramp-connection-timeout 10)
 '(typescript-indent-level 2)
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9")
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
 '(default ((t (:family "Inconsolata" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))
