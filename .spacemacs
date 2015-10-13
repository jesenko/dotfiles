;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
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
                                       go
                                       perspectives
                                       emacs-lisp
                                       org
                                       shell
                                       )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(editorconfig)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

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
   dotspacemacs-startup-banner 'random
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
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ";"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;; user RVM for ruby version management
   ruby-version-manager 'rvm
   ;; enable rails support
   ruby-enable-ruby-on-rails-support t
   )
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
  )

(defun dotspacemacs/config ()
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
;; override switching of project / tests in phoenix until
;; https://github.com/tonini/alchemist.el/issues/163 is solved upstream.
(eval-after-load "alchemist"
  `(progn
     (defun alchemist--project-open-file-for-current-tests (toggler)
       "Open the appropriate implementation file for the current buffer by calling TOGGLER with filename."
       (let* ((filename (alchemist--project-filename-for-current-tests "web/"))
              (filename
               (if (file-exists-p filename)
                   filename
                 (alchemist--project-filename-for-current-tests "lib/"))))
         (funcall toggler filename))
       )

     (defun alchemist--project-filename-for-current-tests (base-dir)
       "Generates filename for implementation file in project root BASE-DIR."
       (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
              (filename (replace-regexp-in-string "^test/" base-dir filename))
              (filename (replace-regexp-in-string "_test\.exs$" "\.ex" filename))
              (filename (format "%s/%s" (alchemist-project-root) filename)))
         filename))

     (defun alchemist--project-open-tests-for-current-file (toggler)
       "Opens the appropriate test file by calling TOGGLER with filename."
       (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
              (filename (replace-regexp-in-string "^lib/" "test/" filename))
              (filename (replace-regexp-in-string "^web/" "test/" filename))
              (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
              (filename (format "%s/%s" (alchemist-project-root) filename)))
         (if (file-exists-p filename)
             (funcall toggler filename)
           (if (y-or-n-p "No test file found; create one now?")
               (alchemist-project--create-test-for-current-file
                filename (current-buffer))
             (message "No test file found.")))))
     )
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
