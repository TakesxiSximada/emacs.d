;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(setq dotspacemacs-mode-line-theme 'spacemacs)
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(nginx
     clojure
     perl5
     haskell
     rust
     go
     common-lisp
     elixir
     sql
     typescript
     ;; markdown
     csv
     ruby
     html
     plantuml
     yaml
     javascript
     markdown
     python
     wakatime
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     twitter
     ;; git
     ;; markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control
     restclient
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; flycheck-pyflakes
                                      ;; flymake-python-pyflakes
                                      ;; pyflake
                                      async-await
                                      atomic-chrome
                                      clomacs
                                      company
                                      ein
                                      elnode
                                      elpy
                                      elscreen-multi-term
                                      exec-path-from-shell
                                      flymake-cursor
                                      google-translate
                                      helm-elscreen
                                      helm-mt
                                      ini-mode
                                      jedi
                                      jedi-core
                                      kubernetes
                                      magit
                                      multi-eshell
                                      multi-term
                                      python-x
                                      review-mode
                                      s
                                      tss
                                      vue-html-mode
                                      vue-mode
                                      websocket
                                      )
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
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq configuration-layer-elpa-archives '(("melpa" .  "melpa.org/packages/")
                                            ("org" . "orgmode.org/elpa/")
                                            ("gnu" . "elpa.gnu.org/packages/")
                                            ))

  )

(defun dotspacemacs/user-config ()
  "setup"

  ;; private configuration
  (setq my:private-config-file-path "~/.config/emacs/default.el")
  (load-file my:private-config-file-path)

  (add-to-list 'exec-path "/usr/local/bin")  ;; $PATH

  ;; append include path
  (add-to-list 'load-path "~/src/github.com/TakesxiSximada/mastodon-mode.el")
  (add-to-list 'load-path "~/mastodon.el/lisp")
  (add-to-list 'load-path "~/src/github.com/xahlee/xah-replace-pairs/")
  (dolist
      (path (seq-filter
             (lambda (dirname) (string-suffix-p ".el" dirname))
             (directory-files "~/src/bitbucket.org/takesxi_sximada" t)))
    (add-to-list 'load-path path t))

  (setq find-function-C-source-directory "/srv/emacs/src/src")  ;; Emacs C source code dir for tag jump

  ;; https://github.com/syl20bnr/spacemacs/issues/9549
  ;; https://github.com/syl20bnr/spacemacs/issues/9608
  ;; (require 'helm-bookmark)


  ;; ===============
  ;; Global
  ;; ===============
  (add-hook 'find-file-hook '(lambda () (interactive) (view-mode)))  ;; File open with READ ONLY.

  ;; ===============
  ;; Python settings
  ;; ===============

  (defun spacemacs//pyvenv-mode-set-local-virtualenv ())  ;; THIS FUNCTION DO NOT WORK!! orz

  ;; jedi
  ;; ====
  (setq jedi:complete-on-dot t)

  (setq jedi:setup-keys nil)
  (setq jedi:tooltip-method nil)
  (autoload 'jedi:setup "jedi" nil t)

  (defvar jedi:goto-stack '())
  (defun jedi:jump-to-definition ()
    (interactive)
    (add-to-list 'jedi:goto-stack
                 (list (buffer-name) (point)))
    (jedi:goto-definition))

  (defun jedi:jump-back ()
    (interactive)
    (let ((p (pop jedi:goto-stack)))
      (if p (progn
              (switch-to-buffer (nth 0 p))
              (goto-char (nth 1 p))))))

  ;; Miniconda settings
  ;; ==================
  (setq my:conda-envs-path "~/miniconda3/envs/")
  (defun use-python-environment (directory)
    "Activate the virtual environment in DIRECTORY."
    (interactive (list (read-directory-name "Activate venv: " my:conda-envs-path)))
    (message "Use pyvenv: %s" directory)
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")  ;; 自動補完のバックエンドとして Rope か Jedi を選択
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i")
    (pyvenv-activate directory)
    (message "[Require] pip install -U yapf jedi vprof flake8 isort")
    (elpy-rpc-restart))

  ;; Python mode hooks
  ;; =================
  (defun my:python-mode-maps ()
    (message "Update keymap")
    (let ((map elpy-mode-map))
      (define-key map (kbd "M-.") 'jedi:jump-to-definition)
      (define-key map (kbd "M-,") 'jedi:jump-back)
      (define-key map (kbd "C-c d") 'jedi:show-doc)
      (define-key map (kbd "C-<tab>") 'jedi:complete)))


  (defun my:python-mode-hooks ()
    (message "PYTHON MODE")
    ;; Enable isort (before save)
    (make-variable-buffer-local 'python-sort-imports-on-save)
    (setq python-sort-imports-on-save t)

    (add-hook 'before-save-hook 'elpy-format-code nil t)  ;; 保存時に整形処理を実行
    (add-hook 'python-mode-hook 'jedi:setup)  ;; jedi
    (my:python-mode-maps)
    )

  (add-hook 'python-mode-hook 'my:python-mode-hooks)

  ;; Bot
  (defun sximada:start-bot ()
    (interactive)
    (message "Start etomato bot")
    (async-shell-command "cd /srv/snstools/snstools && robo t" (get-buffer-create "*bot*")))

  ;; editorconfig
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

  ;; multi-term
  (setq multi-term-program "/bin/zsh")

  ;; DO NOT TO CLOSE EMMACS !!!!!!!!!!
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; (package-install-file "~/.smacemacs.d/multi-eshell.el")
  (use-package multi-eshell)

  ;; spacemacs上でelscreenを実行するためには以下の修正を入れる必要がある
  ;; https://github.com/knu/elscreen/issues/6#issuecomment-164115783
  (use-package elscreen
    :init
    (setq elscreen-tab-display-control nil)
    (setq elscreen-tab-display-kill-screen nil)
    :config
    (setq elscreen-prefix-key (kbd "C-¥"))
    (elscreen-start)
    )

  ;; wakatime
  (require 'wakatime-mode)
  (global-wakatime-mode t)

  (defun wakatime-client-command (savep)
    "Return client command executable and arguments.
   Set SAVEP to non-nil for write action."
    (format "%s%s--file \'%s\' %s --plugin \"%s/%s\" --time %.2f%s%s"
            (if (s-blank wakatime-python-bin) "" (format "%s " wakatime-python-bin))
            (if (s-blank wakatime-cli-path) "wakatime " (format "%s " wakatime-cli-path))
            (or (buffer-file-name (current-buffer)) (buffer-name))
            (if (buffer-file-name (current-buffer)) "" (format " --entity-type app --project \'%s\' " mode-name))
            wakatime-user-agent
            wakatime-version
            (float-time)
            (if savep " --write" "")
            (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

  (defun wakatime-save ()
    "Send save notice to WakaTime."
    (wakatime-call t))

  (defun wakatime-bind-hooks ()
    "Watch for activity in buffers."
    (add-hook 'after-save-hook 'wakatime-save nil t)
    (add-hook 'auto-save-hook 'wakatime-save nil t)
    (add-hook 'first-change-hook 'wakatime-ping nil t))

  (defun wakatime-unbind-hooks ()
    "Stop watching for activity in buffers."
    (remove-hook 'after-save-hook 'wakatime-save t)
    (remove-hook 'auto-save-hook 'wakatime-save t)
    (remove-hook 'first-change-hook 'wakatime-ping t))


  ;; (defun wakatime-client-command (savep)
  ;;   "Return client command executable and arguments.
  ;;  Set SAVEP to non-nil for write action."
  ;;   (format "%s%s--file \"%s\" --plugin \"%s/%s\" --time %.2f%s%s"
  ;;           (if (s-blank wakatime-python-bin) "" (format "%s " wakatime-python-bin))
  ;;           (if (s-blank wakatime-cli-path) "wakatime " (format "%s " wakatime-cli-path))
  ;;           (or (buffer-file-name (current-buffer)) (buffer-name))
  ;;           wakatime-user-agent
  ;;           wakatime-version
  ;;           (float-time)
  ;;           (if savep " --write" "")
  ;;           (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))


  ;; sql-mode
  ;(use-package sql-mode
  ;  :config
  ;  (custom-set-variables
  ;   '(sql-my
  ;     sql-login-params
  ;     (quote (user password database server port)))))
  ;(setq sql-mysql-login-params '(user password database server port))

  ;; org-mode
  (setq org-todo-keywords
        '((sequence
           "PROBREM(p)" "TODO(t)" "WIP(w)" "PENDING(e)" "REVIEW(r)" "QUESTION(q)" "FEEDBACK(f)" "|"
           "DONE(x)" "CANCEL(c)" "RESOLVED(o)" "KEEP(k)" )))

  ;; Effort estimate
  (setq org-global-properties
        (quote (("Effort_ALL" . "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20")
                ("STYLE_ALL" . "habit"))))

  ;; org-agenda
  (setq org-agenda-files my/org-agenda-files)


  (org-babel-do-load-languages 'org-babel-load-languages
                               '(
                                 (shell . t)
                                 (elixir . t)
                                 (http . t)
                                 (restclient . t)
                                 (python . t)
                                 (plantuml . t)
                                 (ruby . t)
                                 (emacs-lisp . t)
                                 )
                               )
  (setq org-plantuml-jar-path my/org-plantuml-jar-path)

  ;; magithub
  ;; (magithub-feature-autoinject nil)
  ;; (setq magithub-clone-default-directory "~/github")

  (fset 'my-trans
        [?> ?\S-  ?\C-@ ?\C-e C-f6 return ?e ?n return ?j ?a return ?\C-t ?l ?\M-\} ?\M-\} ?\C-n ?\C-@ ?\M-\} ?\M-w ?\C-t ?h ?\C-e ?\C-j ?\C-j ?\C-y ?\C-n])


  ;; google translate
  (when (and (= emacs-major-version 25) (<= emacs-minor-version 2))
    (setq google-translate-base-url
          "https://translate.google.com/translate_a/single"
          google-translate--tkk-url
          "https://translate.google.com/"))


  ;; (defvar google-translate-english-chars "[:ascii:]’“”–"
  ;;   "これらの文字が含まれているときは英語とみなす")
  ;;
  ;; (defun google-translate-enja-or-jaen (&optional string)
  ;;   "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  ;;   (interactive)
  ;;   (setq string
  ;;         (cond ((stringp string) string)
  ;;               (current-prefix-arg
  ;;                (read-string "Google Translate: "))
  ;;               ((use-region-p)
  ;;                (buffer-substring (region-beginning) (region-end)))
  ;;               (t
  ;;                (save-excursion
  ;;                  (let (s)
  ;;                    (forward-char 1)
  ;;                    (backward-sentence)
  ;;                    (setq s (point))
  ;;                    (forward-sentence)
  ;;                    (buffer-substring s (point)))))))
  ;;   (let* ((asciip (string-match
  ;;                   (format "\\`[%s]+\\'" google-translate-english-chars)
  ;;                   string)))
  ;;     (run-at-time 0.1 nil 'deactivate-mark)
  ;;     (google-translate-translate
  ;;      (if asciip "en" "ja")
  ;;      (if asciip "ja" "en")
  ;;      string)))


  (which-key-setup-side-window-bottom)    ;ミニバッファ
  (which-key-setup-side-window-right)     ;右端
  (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1)

  ;; typescript
  (require 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  (require 'tss)
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C->")
  (setq tss-implement-definition-key "C-c i")
  (tss-config-default)
  (setq typescript-indent-level 2)  ;;   インデント幅が4だったので、2に変更。


  ;; hang up emacs
  (setq dotspacemacs-mode-line-unicode-symbols t)

  ;; キーバインド
  (bind-keys :map emacs-lisp-mode-map
             ("C-x C-d" . edebug-defun))

  (require 'term)
  (bind-keys :map term-raw-map
             ("C-@" . set-mark-command)
             ("C-a" . move-beginning-of-line)
             ("C-e" . move-end-of-line)
             ("C-v" . scroll-up-command)
             ("C-f" . forward-char)
             ("M-f" . forward-word)
             ("C-b" . backward-char)
             ("M-b" . backward-word)
             ("M-b" . backward-word)
             ("C-k" . (lambda (&optional arg)
                        (interactive "P") (funcall 'kill-line arg) (term-send-raw)))
             )


  (bind-keys*
   ("C-t h" . (lambda ()
                (interactive)
                (wakatime-save)
                (windmove-left)
                (wakatime-save)
                ))
   ("C-t j" . (lambda ()
                (interactive)
                (wakatime-save)
                (windmove-down)
                (wakatime-save)
                ))
   ("C-t k" . (lambda ()
                (interactive)
                (wakatime-save)
                (windmove-up)
                (wakatime-save)
                ))
   ("C-t l" . (lambda ()
                (interactive)
                (wakatime-save)
                (windmove-right)
                (wakatime-save)
                )))
  (require 'mastodon-mode)

  (bind-keys* ("¥" . "\\")

              ;; 編集
              ("C-h" . backward-delete-char-untabify)
              ("C-x g" . find-grep)
              ("C-x C-g" . goto-line)
              ("C-c C-w" . comment-or-uncomment-region)
              ;; ("C-x a" . exchange-point-and-mark)

              ;; バッファ操作
              ("C-x C-b" . helm-mini)
              ("C-S-x b" . buffer-menu)
              ("C-<backspace>" . kill-buffer)

              ;; paneの移動
              ;; ("C-t h" . windmove-left)
              ;; ("C-t j" . windmove-down)
              ;; ("C-t k" . windmove-up)
              ;; ("C-t l" . windmove-right)

              ;; paneのサイズ変更
              ("s-<left>" . shrink-window-horizontally)
              ("s-<down>" . enlarge-window)
              ("s-<up>" . shrink-window)
              ("s-<right>" . enlarge-window-horizontally)

              ;; キーボードマクロ
              ("<f1>" . start-kbd-macro)
              ("<f2>" . end-kbd-macro)
              ("<f3>" . call-last-kbd-macro)
              ("<f4>" . name-last-kbd-macro)
              ("<f5>" . insert-kbd-macro)

              ;; 翻訳
              ("<f6>" . 'my-trans)
              ("<C-f6>" . google-translate-smooth-translate)

              ;; その他
              ("<f7>" . sximada:pyvenv)
              ;; ("<f7>" . eww-search-words)
              ;; ("<f6>" . google-translate-enja-or-jaen)
              ;; ("<f9>" . browse-wakatime)
              ;; ("<C-f11>" . org-agenda-day-view)
              ("<f10>" . (lambda () (interactive)
                           (switch-to-buffer (find-file-noselect "~/Dropbox/tasks/sximada.org"))))
              ("<f11>" . (lambda ()
                           (interactive)
                           (switch-to-buffer (get-buffer-create "*scratch*"))
                           (emacs-lisp-mode)))
              ("<f12>" . (lambda () (interactive)
                           (switch-to-buffer (find-file-noselect "~/.spacemacs.d/init.el"))))
              ("<C-f12>" . (lambda () (interactive)
                           (switch-to-buffer (find-file-noselect my:private-config-file-path))))
              ("<C-M-f12>" . dotspacemacs/sync-configuration-layers)

              ;; version管理
              ("C-x C-v" . magit-status)

              ;; terminalの起動
              ("C-t b" . helm-mt)
              ("C-t C-t t" . multi-eshell)
              ("C-t C-t C-t" . emt-multi-term)

              ;; elscreen
              ("s-t" . elscreen-create)
              ("s-w" . elscreen-kill)
              ("s-[" . elscreen-previous)
              ("s-]" . elscreen-next)

              ;; read only mode
              ("C-q" . read-only-mode)
              ("C-x C-q" . quoted-insert)


              ;;               ;; ("C-x a" . emoji-cheat-sheet-plus-insert)
              ;; ("C-x C-a" . emoji-cheat-sheet-plus-buffer)
              ;; ("C-x M-a" . emoji-cheat-sheet-plus-display-mode)
              )

  ;; (org-agenda-list)
  ;; (switch-to-buffer "*Org Agenda*")
  ;; (spacemacs/toggle-maximize-buffer)

  ;;
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'web-mode-hook)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-guru go-eldoc company-go go-mode flycheck-pyflakes flymake-cursor flymake-python-pyflakes flymake-easy jedi jedi-core python-environment epc ctable concurrent deferred ob-elixir flycheck-mix flycheck-credo alchemist elixir-mode python-x folding multi-eshell
             tide typescript-mode flycheck csv-mode goto-chg projectile helm helm-core async review-mode twittering-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data helm-gtags ggtags atomic-chrome websocket plantuml-mode yaml-mode switch-buffer-functions elnode db fakir creole web noflet kv restclient-helm ob-restclient ob-http company-restclient know-your-http-well restclient sql-indent wakatime-mode org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot web-beautify livid-mode helm-company helm-c-yasnippet fuzzy company-tern dash-functional tern company-statistics company-anaconda company auto-yasnippet ac-ispell auto-complete skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic helm-elscreen elscreen-multi-term elscreen helm-mt multi-term mmm-mode markdown-toc markdown-mode gh-md magit magit-popup git-commit with-editor ws-butler winum volatile-highlights vi-tilde-fringe uuidgen toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump f dash s define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link which-key use-package macrostep hydra helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag evil elisp-slime-nav bind-map auto-compile ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (common-lisp-snippets go-guru go-eldoc company-go go-mode flycheck-pyflakes flymake-cursor flymake-python-pyflakes flymake-easy jedi jedi-core python-environment epc ctable concurrent deferred ob-elixir flycheck-mix flycheck-credo alchemist elixir-mode python-x folding multi-eshell tide typescript-mode flycheck csv-mode goto-chg projectile helm helm-core async review-mode twittering-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data helm-gtags ggtags atomic-chrome websocket plantuml-mode yaml-mode switch-buffer-functions elnode db fakir creole web noflet kv restclient-helm ob-restclient ob-http company-restclient know-your-http-well restclient sql-indent wakatime-mode org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot web-beautify livid-mode helm-company helm-c-yasnippet fuzzy company-tern dash-functional tern company-statistics company-anaconda company auto-yasnippet ac-ispell auto-complete skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic helm-elscreen elscreen-multi-term elscreen helm-mt multi-term mmm-mode markdown-toc markdown-mode gh-md magit magit-popup git-commit with-editor ws-butler winum volatile-highlights vi-tilde-fringe uuidgen toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump f dash s define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link which-key use-package macrostep hydra helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag evil elisp-slime-nav bind-map auto-compile ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
