;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ex-command-key ";"
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(
                                       git
                                       yaml
                                       auto-completion
                                       syntax-checking
                                       better-defaults
                                       ruby
                                       ruby-on-rails
                                       react
                                       erlang
                                       elixir
                                       javascript
                                       colors
                                       markdown
                                       html
                                       emacs-lisp
                                       org
                                       shell
                                       )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(editorconfig nginx-mode string-inflection)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Specify the startup banner. If the value is an integer then the
   ;; text banner with the corresponding index is used, if the value is
   ;; `random' then the banner is chosen randomly among the available banners,
   ;; if the value is a string then it must be a path to a .PNG file,
   ;; if the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Meslo LG M for Powerline"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; user RVM for ruby version management
   ruby-version-manager 'rvm
   ;; enable rails support
   ruby-enable-ruby-on-rails-support t
   )
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq-default c-basic-offset 2)
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq css-indent-offset 2)
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ;; set shift width to value used by majority of modes I currently use
  (setq-default evil-shift-width 2)
  ;; Search for projectile root top-down so we can detect project within project
  ;; (e.g. ember app within rails app)
  (setq projectile-project-root-files-functions '(projectile-root-top-down
                                                  projectile-root-bottom-up))
  (setq projectile-ignored-directories (append '(projectile-ignored-directories) '("bower_components"
                                                                                   "node_modules")))
  ;; Use Emacs terminfo, not system terminfo
  (setq system-uses-terminfo nil)
  ;; Add basic support for wrapping of ruby code
  (add-to-list 'hs-special-modes-alist
               '(enh-ruby-mode
                 "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                 (lambda (arg) (enh-ruby-end-of-block)) nil))
  ;; ruby indentation settings
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-hanging-brace-indent-level 2)
  ;; scroll compile output
  (setq compilation-scroll-output t)
  ;; make helm buffer wider to display full file names
  (setq helm-buffer-max-length 40)
  ;; until cache invalidation is automatic, disable projectile caching
  (setq projectile-enable-caching nil)
  ;; do not enable full backtrace with rspec
  (setq ruby-test-rspec-options '())
  (setq flycheck-disabled-checkers '(javascript-jshint))
  (setq flycheck-checkers '(javascript-eslint))
  (setq neo-show-hidden-files nil)
  (add-to-list 'hs-special-modes-alist
               '(elixir-mode
                 "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#" nil))
  (setq powerline-default-separator 'utf-8)
  (spaceline-compile)

  (add-to-list 'spacemacs-indent-sensitive-modes 'elixir-mode)
  ;; custom shortcuts
  (evil-leader/set-key "of" 'neotree-find)
  (evil-leader/set-key "or" 'alchemist-mix-rerun-last-test)
  (evil-leader/set-key "oh" `hs-hide-level)
  (evil-leader/set-key "oc" `string-inflection-lower-camelcase)
  )
;; override defaul ruby test command to use spring
(eval-after-load "ruby-test-mode"
  '(defun ruby-test-spec-command (filename &optional line-number)
    (let (command options)
      (setq command "bundle exec spring rspec")
      (setq options ruby-test-rspec-options)
      (if line-number
          (setq filename (format "%s:%s" filename line)))
      (format "%s %s %s" command (mapconcat 'identity options " ") filename)))
  )

(defun sf-extract-regexp (regexp)
  "Like 'occur' but only lists the matches for REGEXP and not the lines."
  (interactive "sRegexp: ")
  (save-excursion
    (goto-char (point-min))
    (let* ((result-buffer-name "*extract-regex-result*")
           (result-buffer (get-buffer result-buffer-name))

           (prefix-face list-matching-lines-prefix-face)
           (marker nil)
           (matchbeg nil)
           (once nil))

      (when result-buffer
        (kill-buffer result-buffer))

      (while (search-forward-regexp regexp nil t)
        ;; only run once (setup the result buffer)
        (when (not once)
          (with-current-buffer (get-buffer-create result-buffer-name)
            (insert (concat "Extract Regex Results for \"" regexp "\":\n\n"))
              (display-buffer "*extract-regex-result*") ;; later
              (setq result-buffer (current-buffer)))
          (setq once t))

        (setq matchbeg (match-beginning 0))
        (setq marker (make-marker))
        (set-marker marker matchbeg)

        ;; more or less copied from replace.el (occur-engine function)
        (let* ((curr-line (count-lines 1 (point)))
               (curstring (match-string 0))
               (match-prefix
                ;; Using 7 digits aligns tabs properly.
                (apply #'propertize (format "%7d:" curr-line)
                       (append
                        (when prefix-face
                          `(font-lock-face ,prefix-face))
                        `(occur-prefix t mouse-face (highlight)
                                       ;; Allow insertion of text at
                                       ;; the end of the prefix (for
                                       ;; Occur Edit mode).
                                       front-sticky t rear-nonsticky t
                                       occur-target ,marker follow-link t
                                       help-echo "mouse-2: go to this occurrence"))))
               (match-str
                ;; We don't put `mouse-face' on the newline,
                ;; because that loses.  And don't put it
                ;; on context lines to reduce flicker.
                (propertize curstring 'mouse-face (list 'highlight)
                            'occur-target marker
                            'follow-link t
                            'help-echo
                            "mouse-2: go to this occurrence"))
               (out-line
                (concat
                 match-prefix
                 ;; Add non-numeric prefix to all non-first lines
                 ;; of multi-line matches.
                 (replace-regexp-in-string
                  "\n"
                  (if prefix-face
                      (propertize "\n       :" 'font-lock-face prefix-face)
                    "\n       :")
                  match-str)
                 ;; Add marker at eol, but no mouse props.
                 (propertize "\n" 'occur-target marker))))
          (with-current-buffer result-buffer
            (insert out-line))))

      (if (not once)
          (message "No match found")
        (with-current-buffer result-buffer
          (occur-mode))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (packed request yasnippet smartparens magit magit-popup git-commit auto-complete helm helm-core simple-httpd tide typescript-mode yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit string-inflection spacemacs-theme spaceline solarized-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pug-mode projectile-rails popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-elixir nginx-mode neotree mwim multi-term move-text mmm-mode minitest markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flycheck-pos-tip flycheck-mix flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erlang emmet-mode elisp-slime-nav editorconfig dumb-jump define-word csv-mode company-web company-tern company-statistics column-enforce-mode color-identifiers-mode coffee-mode clean-aindent-mode chruby bundler auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
