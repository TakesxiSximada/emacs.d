;; ---------
;; load-path
;; ---------
(add-to-list 'load-path "/srv/sallies/nvm.el/")
(add-to-list 'load-path "/srv/sallies/our.el/")

;; ------------
;; localeの設定
;; ------------
(setenv "LANG" "ja_JP.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)

;; ----------------
;; toolbarの設定
;; ----------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;------------------------
;; fonts
;;------------------------
(set-face-attribute 'default nil :family "Menlo" :height 120)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))

(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; -----------------
;; backup fileの設定
;; -----------------
(setq make-backup-files nil)
(setq auto-save-default nil)

;; --------------
;; 起動画面の設定
;; --------------
(setq initial-buffer-choice
      (lambda ()
	(switch-to-buffer "*Messages*")))


(load-file "~/.emacs.d/env.el")

;; --------
;; 環境変数
;; --------
(require 'cl)
(require 'subr-x)

(setq exec-path (delete-duplicates
		 (append `(
			   "/Library/TeX/texbin"
			   "/usr/local/opt/gettext/bin"
			   "/usr/local/opt/libxml2/bin"
			   "/usr/local/opt/sqlite/bin"
			   "/usr/local/opt/texinfo/bin"
			   ,(expand-file-name "~/.cargo/bin")
			   ,(expand-file-name "~/.local/bin")
			   ,(expand-file-name "~/google-cloud-sdk/bin")
			   )
			 (split-string (getenv "PATH") ":")
			 exec-path)))

(setenv "PATH" (string-join exec-path ":"))
(setenv "LDFLAGS" (string-join '(
				 "-L/usr/local/opt/gettext/lib"
				 "-L/usr/local/opt/libffi/lib"
				 "-L/usr/local/opt/libxml2/lib"
				 "-L/usr/local/opt/mysql@5.7/lib"
				 "-L/usr/local/opt/openssl/lib"
				 "-L/usr/local/opt/readline/lib"
				 "-L/usr/local/opt/sqlite/lib"
				 "-L/usr/local/opt/texinfo/lib"
				 ) " "))

(setenv "CPPFLAGS" (string-join '(
				  "-I/usr/local/opt/gettext/include"
				  "-I/usr/local/opt/libxml2/include"
				  "-I/usr/local/opt/mysql@5.7/include"
				  "-I/usr/local/opt/openssl/include"
				  "-I/usr/local/opt/readline/include"
				  "-I/usr/local/opt/sqlite/include"
				  ) " "))

(setenv "PKG_CONFIG_PATH" (string-join '(
					 "/usr/local/opt/libffi/lib/pkgconfig"
					 "/usr/local/opt/libxml2/lib/pkgconfig"
					 "/usr/local/opt/mysql@5.7/lib/pkgconfig"
					 "/usr/local/opt/openssl/lib/pkgconfig"
					 "/usr/local/opt/readline/lib/pkgconfig"
					 "/usr/local/opt/sqlite/lib/pkgconfig"
					 ) ":"))
;; ------------
;; Yes/Noの設定
;; ------------
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------
;; our-async-exec
;; --------------
(require 'our-async-exec)

;; -----
;; elenv
;; -----
(setq elenv-root-directory "/srv/")
(add-hook
 'elenv-initialize-package-after-hook
 (lambda ()
   (require 'use-package)

   (use-package powerline :ensure t :defer t)

   (use-package exec-path-from-shell :ensure t :defer t
     :init
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

   (use-package helm :ensure t :defer t
     :init
     (require 'helm-config)
     :config
     (helm-mode t)
     (dired-async-mode t)
     (setq helm-M-x-fuzzy-match t)
     (bind-keys :map helm-map
		("<tab>" . helm-execute-persistent-action)
		("C-i" . helm-execute-persistent-action)
		("C-z" . helm-select-action)))
   (use-package helm-ag :ensure t :defer t
     :init
     (setq helm-ag-use-agignore t))
   (use-package elscreen :ensure t
     :init
     (setq elscreen-display-tab nil)
     (setq elscreen-tab-display-kill-screen nil)
     (setq elscreen-tab-display-control nil)
     (elscreen-start)
     (elscreen-create))
   (use-package magit :ensure t :defer t)

   (use-package async-await :ensure t :defer t)
   (use-package json :ensure t :defer t)
   (use-package request :ensure t :defer t)
   (use-package async-await :ensure t :defer t)
   (use-package gist :ensure t :defer t)
   (use-package helm-themes :ensure t :defer t)
   (use-package http :ensure t :defer t)
   (use-package markdown-mode :ensure t)
   (use-package quickrun :ensure t :defer t)
   (use-package restclient :ensure t :defer t
     :config
     (add-to-list 'restclient-content-type-modes '("text/csv" . http-mode)))
   (use-package websocket :ensure t :defer t)
   (use-package yaml-mode :ensure t :defer t)
   (use-package dockerfile-mode :ensure t :defer t)
   (use-package company :ensure t :defer nil
     :init
     (setq company-idle-delay 0) ; default = 0.5
     (setq company-minimum-prefix-length 2) ; default = 4
     (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
     :bind
     ("C-M-i" . company-complete)
     :config
     (global-company-mode 1)
     (bind-keys :map company-active-map
		("C-n" . company-select-next)
		("C-p" . company-select-previous)
		("C-s" . company-filter-candidates)
		("C-i" . company-complete-selection)
		("C-M-i" . company-complete)))

   (use-package spacemacs-theme :ensure t :defer t
     :no-require t
     :init
     (load-theme 'tsdh-dark t))
   ))

(progn (add-to-list 'load-path "/srv/sallies/elenv/") (require 'elenv) (elenv-activate))  ;; elenv auto inser
(toggle-frame-fullscreen)

;; -----
;; qiita
;; -----
(use-package unicode-escape :ensure t :defer t)

;; -----------
;; our-package
;; -----------
(require 'our)
(require 'our-brew)
(require 'our-cider)
(require 'our-circleci)
(require 'our-discord)
(require 'our-freewifi)
(require 'our-macos)
(require 'our-magit)
(require 'our-mastodon)
(require 'our-need-install)
(require 'our-org)
(require 'our-pyvenv)
(require 'our-qiita)
(require 'our-simeji)
(require 'our-terraform)
(require 'our-wakatime)

(add-to-list 'our-org--target-dir-list "~/Dropbox/tasks")

(load-file "~/.emacs.d/env/discord.el")
(load-file "~/.emacs.d/env/mastodon.el")
(load-file "~/.emacs.d/env/wakatime.el")
(load-file "~/.emacs.d/env/cloudapp.el")

;; -----------------
;; external packages
;; -----------------
(use-package sudden-death :ensure t :defer t)
(use-package dired-filter :ensure t :defer t)
(use-package google-translate :ensure t :defer t)

;; -----
;; redis
;; -----
(unless (executable-find "redis-cli") (our-async-exec "brew install redis"))

;; ------------
;; chromedriver
;; ------------
(unless (executable-find "chromedriver") (our-async-exec "brew cask install chromedriver"))

;; --------
;; lsp-mode
;; --------
(use-package lsp-mode :ensure t :defer t)
(use-package lsp-ui :ensure t :defer t)

;; ------
;; python
;; ------
(use-package pyvenv :ensure t :defer t)
(use-package py-isort :ensure t :defer t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))
(use-package jedi :ensure t :defer t)
(use-package elpy :ensure t :defer t
  :commands elpy-enable
  :init
  (require 'jedi)
  (defvar jedi:goto-stack '())
  (elpy-enable)
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

  :bind (:map elpy-mode-map
	      ("M-." . jedi:jump-to-definition)
	      ("M-," . jedi:jump-back)
	      ("C-c d" . jedi:show-doc)
	      ("C-<tab>" . jedi:complete)))
;; (jedi:install-server)
(jedi:setup)
(elpy-enable)

(require 'our-pyvenv)

;; -------
;; clojure
;; -------
(unless (executable-find "java") (our-async-exec "brew cask install java"))
(unless (executable-find "clj") (our-async-exec "brew install clojure"))

(unless (executable-find "lein") (our-async-exec "brew install leiningen"))
;; ------------------- leiningen install log -----------------------------------------------
;; ==> Downloading https://homebrew.bintray.com/bottles/leiningen-2.8.3.mojave.bottle.tar.gz
;; ######################################################################## 100.0%
;; ==> Pouring leiningen-2.8.3.mojave.bottle.tar.gz
;; ==> Caveats
;; Dependencies will be installed to:
;;   $HOME/.m2/repository
;; To play around with Clojure run `lein repl` or `lein help`.

;; Bash completion has been installed to:
;;   /usr/local/etc/bash_completion.d

;; zsh completions have been installed to:
;;   /usr/local/share/zsh/site-functions
;; ==> Summary
;; 🍺  /usr/local/Cellar/leiningen/2.8.3: 9 files, 13MB

(use-package rainbow-delimiters :ensure t :defer t)
(use-package paredit :ensure t :defer t
  :config
  (bind-keys :map paredit-mode-map
             ("C-h" . paredit-backward-delete)))
(use-package clojure-mode :ensure t :defer t)
(use-package clj-refactor :ensure t :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))
(use-package cider :ensure t :defer t
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

;; --------
;; wakatime
;; --------
(require 'our-wakatime)
(use-package wakatime-mode :ensure t :defer t
  :init
  (our-wakatime-setup)
  (if (and wakatime-api-key
	   (file-exists-p wakatime-cli-path))
      (global-wakatime-mode)))

;; --------
;; org-mode
;; --------
(defun our-org-mode-setup ()
  (org-indent-mode)  ;; org-modeの表示をインデントモードにする
  (org-display-inline-images)  ;; 画像表示
  (setq org-src-fontify-natively t)

  (setq org-todo-keywords
      '((sequence
         "TODO(t)"
	 "WIP(w)"
	 "PENDING(e)"
	 "REVIEW(r)"
	 "PROPOSAL(P)"
	 "PROBREM(p)"
	 "QUESTION(q)"
	 "RESEARCH(R)"
	 "FEEDBACK(f)"
	 "EPIC(g)"
	 "|"
         "WHY(W)"
         "DONE(x)"
	 "CANCEL(c)"
	 "RESOLVED(o)"
	 "KEEP(k)"
	 "DOC(d)"
	 "FAQ(f)"
	 "SPEC(s)"
	 "TIPS(t)")))

  (setq org-global-properties
	(quote (("Effort_ALL" . "1 2 3 5 8 13 21 34 55 89")
		("STYLE_ALL" . "habit")))))

(add-hook 'org-mode-hook 'our-org-mode-setup)

;; ---------
;; org-babel
;; ---------
(use-package ob-restclient :ensure t :defer t)
(use-package org-preview-html :ensure t :defer t)
(our-need-install "plantuml" "plantuml" :darwin "brew install plantuml")
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2019.1/libexec/plantuml.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (dot . t)
   (emacs-lisp . t)
   (plantuml . t)
   (restclient . t)
   (shell . t)
   (python . t)
   ))


;; ----------
;; kubernetes
;; ----------
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; ----------
;; others
;; ----------
(our-need-install "basictex" "basictex" :darwin "brew cask install basictex # M-x our-latex-update")
(defun our-latex-update ()
  (interactive)
  (our-async-exec
   (string-join '(;; パッケージのアップデート
		  "sudo tlmgr update --self --all"
		  ;; デフォルトで A4 用紙を使う
		  "sudo tlmgr paper a4"
		  ;; 日本語用パッケージ群のインストール
		  "sudo tlmgr install collection-langjapanese"
		  ;; 和文フォント ヒラギノのインストールと設定
		  "sudo tlmgr repository add http://contrib.texlive.info/current tlcontrib"
		  "sudo tlmgr pinning add tlcontrib '*'"
		  "sudo tlmgr install japanese-otf-nonfree japanese-otf-uptex-nonfree ptex-fontmaps-macos cjk-gs-integrate-macos"
		  "sudo cjk-gs-integrate --link-texmf --cleanup"
		  "sudo cjk-gs-integrate-macos --link-texmf"
		  "sudo mktexlsr"
		  "sudo kanji-config-updmap-sys --jis2004 hiragino-highsierra-pron"
		  ;; 日本語環境でソースコードの埋め込み
		  ;; FIXME: ここではうまくjlisting.sty.bz2がダウンロード出来ていないのでコメントアウトするしかない
		  ;; "curl https://ja.osdn.net/projects/mytexpert/downloads/26068/jlisting.sty.bz2/ | bzip2 -d "
		  ;; "sudo mv ~/Downloads/jlisting.sty /usr/local/texlive/2018basic/texmf-dist/tex/latex/listings/"
		  ;; "sudo chmod +r /usr/local/texlive/2018basic/texmf-dist/tex/latex/listings/jlisting.sty"
		  ;; "sudo mktexlsr"
		  )
		" && ")))

(our-need-install "ghostscript" "ghostscript" :darwin "brew install ghostscript")
(our-need-install "latexit" "latexit" :darwin "brew cask install latexit")
(our-need-install "pandoc" "pandoc" :darwin "brew install pandoc")

;; ----------
;; keybinding
;; ----------
(require 'simple)
(require 'macros)
(require 'newcomment)
(require 'windmove)
;; (require 'window)

(require 'elscreen)
(require 'helm-ag)
(require 'magit)

(bind-keys* ("¥" . "\\")
	    ("C-h" . backward-delete-char-untabify)
	    ("C-x g" . helm-do-ag)
	    ("C-x C-g" . elenv-dired)
	    ("C-x C-p" . list-processes)
	    ("C-c C-w" . comment-or-uncomment-region)

            ;; keyboard macro
            ("<f1>" . start-kbd-macro)
            ("<f2>" . end-kbd-macro)
            ("<f3>" . call-last-kbd-macro)
            ("<f4>" . name-last-kbd-macro)
            ("<f5>" . insert-kbd-macro)

	    ;; buffers
	    ("C-<backspace>" . kill-buffer)

	    ;; terraform
	    ("C-t C-f" . our-terraform-menu)

	    ;; panes and screen
	    ("C-t C-t" . elscreen-previous)
	    ("C-t h" . windmove-left)
	    ("C-t j" . windmove-down)
	    ("C-t k" . windmove-up)
	    ("C-t l" . windmove-right)

	    ;; translate
	    ("C-t e" . google-translate-at-point-reverse)
	    ("C-t C-e" . google-translate-at-point-reverse)

            ;; panes size
            ("s-<left>" . shrink-window-horizontally)
            ("s-<down>" . enlarge-window)
            ("s-<up>" . shrink-window)
            ("s-<right>" . enlarge-window-horizontally)

	    ;; command
            ("M-_" . our-async-exec-interactive)
            ("C-M-_" . async-shell-command)
	    ;; other
	    ("s-t" . (lambda () (interactive) (message "Oops!")))
            ("<f9>" . google-this)
	    ("<f12>" . elenv-switch-user-init-file)
	    ("<C-f12>" . our-org-open))


;; buffer
(our-bind-key "C-x C-b" 'helm-mini)
(our-bind-key "C-x b" 'helm-buffers-list)
(our-bind-key "C-x C-f" 'helm-find-files)
(our-bind-key "M-x" 'helm-M-x)

;; git
(our-bind-key "C-x C-v" 'magit-status)

;; elscreen
;; (bind-key "C-t C-t" 'elscreen-previousch)
;; (our-bind-key "C-t C-t" 'elscreen-previous)
;; (our-bind-key "C-t C-n" 'elscreen-next)
;; (our-bind-key "C-t C-p" 'elscreen-previous)
;; (our-bind-key "C-t C-l" 'helm-elscreen)
;; (our-bind-key "C-t C-w" 'elscreen-kill)


;; ---------------
;; MONKEY PATCHING
;; ---------------

;; for simple.el
;;
;; async-shell-commandで長い出力を表示する場合にEmacsが固まる問題を回避する
(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
	;; Run preoutput filters
	(let ((functions comint-preoutput-filter-functions))
	  (while (and functions string)
	    (if (eq (car functions) t)
		(let ((functions
                       (default-value 'comint-preoutput-filter-functions)))
		  (while (and functions string)
		    (setq string (funcall (car functions) string))
		    (setq functions (cdr functions))))
	      (setq string (funcall (car functions) string)))
	    (setq functions (cdr functions))))

	;; Insert STRING
	(let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
	      (saved-point (copy-marker (point) t)))

	  ;; We temporarily remove any buffer narrowing, in case the
	  ;; process mark is outside of the restriction
	  (save-restriction
	    (widen)

	    (goto-char (process-mark process))
	    (set-marker comint-last-output-start (point))

            ;; Try to skip repeated prompts, which can occur as a result of
            ;; commands sent without inserting them in the buffer.
            (let ((bol (save-excursion (forward-line 0) (point)))) ;No fields.
              (when (and (not (bolp))
                         (looking-back comint-prompt-regexp bol))
                (let* ((prompt (buffer-substring bol (point)))
                       (prompt-re (concat "\\`" (regexp-quote prompt))))
                  (while (string-match prompt-re string)
                    (setq string (substring string (match-end 0)))))))
            (while (string-match (concat "\\(^" comint-prompt-regexp
                                         "\\)\\1+")
                                 string)
              (setq string (replace-match "\\1" nil nil string)))

	    ;; insert-before-markers is a bad thing. XXX
	    ;; Luckily we don't have to use it any more, we use
	    ;; window-point-insertion-type instead.
	    (insert string)

	    ;; Advance process-mark
	    (set-marker (process-mark process) (point))

	    (unless comint-inhibit-carriage-motion
	      ;; Interpret any carriage motion characters (newline, backspace)
	      (comint-carriage-motion comint-last-output-start (point)))

	    ;; Run these hooks with point where the user had it.
	    (goto-char saved-point)
	    (run-hook-with-args 'comint-output-filter-functions string)
	    (set-marker saved-point (point))

	    (goto-char (process-mark process)) ; In case a filter moved it.

	    (unless comint-use-prompt-regexp
              (with-silent-modifications
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
				       (field inhibit-line-move-field-capture)
				       rear-nonsticky t
				       field output
				       inhibit-line-move-field-capture t))))

	    ;; Highlight the prompt, where we define `prompt' to mean
	    ;; the most recent output that doesn't end with a newline.
	    (let ((prompt-start (save-excursion (forward-line 0) (point)))
		  (inhibit-read-only t))
	      (when comint-prompt-read-only
		(with-silent-modifications
		  (or (= (point-min) prompt-start)
		      (get-text-property (1- prompt-start) 'read-only)
		      (put-text-property (1- prompt-start)
					 prompt-start 'read-only 'fence))
		  (add-text-properties prompt-start (point)
				       '(read-only t front-sticky (read-only)))))
	      (when comint-last-prompt
		;; There might be some keywords here waiting for
		;; fontification, so no `with-silent-modifications'.
		(font-lock--remove-face-from-text-property
		 (car comint-last-prompt)
		 (cdr comint-last-prompt)
		 'font-lock-face
		 'comint-highlight-prompt))
	      (setq comint-last-prompt
		    (cons (copy-marker prompt-start) (point-marker)))
	      ;; ここのプロパティ設定によりとてつもなく処理がおもくなるためコメントアウト
	      ;; (font-lock-prepend-text-property prompt-start (point)
	      ;; 				       'font-lock-face
	      ;; 				       'comint-highlight-prompt)
	      (add-text-properties prompt-start (point) '(rear-nonsticky t)))
	    (goto-char saved-point)))))))
