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
   dotspacemacs-configuration-layers '(git
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
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
