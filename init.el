(setq find-function-C-source-directory "/opt/ng/emacs/src")
(setq debug-on-error t)

(setq custom-theme-directory (expand-file-name "~/.emacs.d/themes"))
(load-theme 'sximada-dark t)
(toggle-frame-fullscreen)

;; -----------------------------
;; package.el
;; -----------------------------
(setq package-user-dir (expand-file-name "~/.elpa")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  			 ("melpa" . "https://melpa.org/packages/")
  			 ("org" . "https://orgmode.org/elpa/")
  			 ("melpa-stable" . "http://stable.melpa.org/packages/")
  			 ;; marmalade is already not mainted
  			 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
  			 ))
(package-initialize)

;; -----------------------------
;; Other packaging library
;; -----------------------------
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package quelpa :ensure t :defer t)
(use-package quelpa-use-package :ensure t :defer t)
(use-package el-get :ensure t :defer t
  :init
  (setq el-get-dir (expand-file-name "/opt/ng")))

;; -----------------------------
;; Base Key binding
;; ----------------------------
(bind-key* "C-t" nil)

(bind-key* "<f1>" #'start-kbd-macro)
(bind-key* "<f2>" #'end-kbd-macro)
(bind-key* "<f3>" #'call-last-kbd-macro)
(bind-key* "<f4>" #'insert-kbd-macro)
(bind-key* "C-c C-w" 'comment-or-uncomment-region)
(bind-key* "C-h" #'backward-delete-char-untabify)
(bind-key* "C-t C-h" 'windmove-left)
(bind-key* "C-t C-j" 'windmove-down)
(bind-key* "C-t C-k" 'windmove-up)
(bind-key* "C-t C-l" 'windmove-right)
(bind-key* "C-t h" 'windmove-left)
(bind-key* "C-t j" 'windmove-down)
(bind-key* "C-t k" 'windmove-up)
(bind-key* "C-t l" 'windmove-right)
(bind-key* "C-x C-w" 'kill-buffer)

;; -----------------------------
;; Emacs UI
;; -----------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------
;; Misc
;; -----------------------------
(setq make-backup-files nil
      auto-save-default nil
      custom-file (locate-user-emacs-file "custom.el")
      )

;; ------------------------------
;; environment variable utilities
;; ------------------------------
(defun getenv+ (name)
  "環境変数から値を取得し、さもなければシンボルから値を取得する"
  (or (getenv name)
      (symbol-value (intern-soft name))))
;; -----------------------------
;; IDO
;; -----------------------------
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(use-package ido-vertical-mode :ensure t :defer
  :init
  (ido-vertical-mode)
  (add-hook 'ido-setup-hook #'ido-vertical-define-keys-custom))


(defun ido-vertical-define-keys-custom ()
  (define-key ido-completion-map (kbd "M-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)
  )

;; WHY DID I USE ido-completing-read+ PACKAGE?:
;;   I changed value t to ido-everywhere, but ido-vertical-mode did
;;   not work. Should be enabled ido-ubiquitous-mode to work it.

(use-package ido-completing-read+ :ensure t :defer t
  :init
  (ido-ubiquitous-mode 1))

;; -----------------------------
;; org-mode
;; -----------------------------
(use-package org :ensure t :defer t
  :config
  (org-indent-mode)
  (setq org-startup-indented t
        org-archive-location (format-time-string "ARCHIVE_%Y.org::" (current-time))
        ))

(require 'org-clock)

:; automatic timeout timer
(setq org-clock-automatic-timeout (* 60 10))
(setq org-clock-automatic-timeout-timer
      (run-with-idle-timer org-clock-automatic-timeout
			   t 'org-clock-out))

;; -----------------------------
;; Vterm
;; -----------------------------
(use-package vterm :ensure t :defer t
  :bind (:map vterm-mode-map
  	      ("C-c C-v" . vterm-copy-mode)
	      ("C-t" . nil)))


(defun vterm-command (line &optional cwd)
  (interactive (list
		(read-string "Command: " "" nil "")
		(read-directory-name "Directory: " default-directory nil default-directory)))
  (let ((default-directory cwd)
	(vterm-shell line)
	(vterm-buffer-name (format "%s %s: In %s"
				   (car (split-string line))
				   (or (car (cdr (split-string line))) "")
				   (expand-file-name cwd)))
	(vterm-kill-buffer-on-exit nil))
    (vterm)))

;; -----------------------------
;; Others
;; -----------------------------
(use-package sgml-mode :ensure t :defer t
  :config
  (setq sgml-quick-keys 'close))
(use-package add-node-modules-path :ensure t :defer t)
(use-package ag :ensure t :defer t :no-require t)
(use-package avy-menu :ensure t :defer t)
(use-package db :ensure t :defer t)
(use-package dired-filter :ensure t :defer t)
(use-package fakir :ensure t :defer t)
(use-package flycheck :ensure t :defer t)
(use-package github-review  :ensure t :defer t)
(use-package google-translate :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package mew :ensure t :defer t)
(use-package monky :ensure t :defer t)
(use-package nginx-mode :ensure t :defer t)
(use-package ob-async :ensure t :defer t )
(use-package ob-restclient :ensure t :defer t)
(use-package pcre2el :ensure t :defer t)
(use-package request :ensure t :defer t)
(use-package restclient :ensure t :defer t)
(use-package s :ensure t :defer t)
(use-package slime :ensure t :defer t)
(use-package smex :ensure t :defer t)
(use-package terraform-mode :ensure t :defer t)
(use-package transient :ensure t)
(use-package unicode-escape :ensure t :defer t)
(use-package vagrant-tramp :ensure t :defer t)
(use-package web :ensure t :defer t)
(use-package wgrep :ensure t :defer t)
(use-package wgrep-ag :ensure t :defer t)

(use-package company :ensure t :pin melpa
  :config
  (global-company-mode)
  (setq
   company-idle-delay 0 ; default = 0.5
   company-minimum-prefix-length 2 ; default = 4
   company-selection-wrap-around t ; 候補の一番下でさらに下に行こうとすると一番上に戻る
   company-tooltip-idle-delay nil)
  )

;; -----------------------------
;; eglot
;; -----------------------------
(use-package eglot :defer t :ensure t
  :init
  (defun eglot-install-language-server-python ()
    (interactive)
    (make-process :name "*EGLOT INSTALL*"
  		  :buffer (get-buffer-create "*EGLOT INSTALL*")
  		  :command `("pip" "install" "python-language-server")))

  :config
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls")))

  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark)

  ;; :if (eq system-type 'darwin)
  ;; :ensure-system-package
  ;; ("vls" . "npm install -g vls")
  )

;; -----------------------------
;; edit-indirect
;; -----------------------------
(use-package edit-indirect :ensure t :defer t
  :config
  (setq edit-indirect-guess-mode-function #'edit-indirect-custom-apply-major-mode))

(defun edit-indirect-custom-guess-major-mode (_parent-buffer _beg _end)
  "Guess major-mode to parent-buffer major-mode.

Returns symbol of major-mode.
"
  (with-current-buffer _parent-buffer
    (goto-char _beg)

    (if (eq major-mode 'org-mode)
	(if-let ((lang (nth 0 (org-babel-get-src-block-info))))
	    (intern (format "%s-mode" lang))
	  'org-mode)
      major-mode)))

(defun edit-indirect-custom-apply-major-mode  (_parent-buffer _beg _end)
  "Apply major-mode to parent-buffer major-mode."
  (funcall (edit-indirect-custom-guess-major-mode _parent-buffer _beg _end)))

;; -----------------------------
;; Javascript and Typescript
;; -----------------------------
(use-package typescript-mode :defer t :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package js-mode :defer t
  :config
  (setq js-indent-level 2))
(use-package js2-mode :defer t :ensure t
  :config
  (setq js-indent-level 2))

;; -----------------------------
;; Vue.js
;; -----------------------------
(use-package vue-mode :ensure t :defer t
  :requires (vue-mode
	     vue-html-mode
	     css-mode
	     js-mode
	     typescript-mode)
  :config
  (define-key css-mode-map (kbd "C-c i") #'vue-mode-edit-all-indirect)
  (define-key css-mode-map (kbd "M-i") #'vue-mode-edit-indirect-at-point)
  (define-key js-mode-map (kbd "C-c i") #'vue-mode-edit-all-indirect)
  (define-key js-mode-map (kbd "M-i") #'vue-mode-edit-indirect-at-point)
  (define-key typescript-mode-map (kbd "C-c i") #'vue-mode-edit-all-indirect)
  (define-key typescript-mode-map (kbd "M-i") #'vue-mode-edit-indirect-at-point)
  (define-key vue-html-mode-map (kbd "C-c i") #'vue-mode-edit-all-indirect)
  (define-key vue-html-mode-map (kbd "M-i") #'vue-mode-edit-indirect-at-point)
  (define-key vue-mode-map (kbd "C-c i") #'vue-mode-edit-all-indirect)
  (define-key vue-mode-map (kbd "M-i") #'vue-mode-edit-indirect-at-point)

  (defun vue-mode-edit-all-indirect (&optional keep-windows)
    "Open all subsections with `edit-indirect-mode' in seperate windows.
  If KEEP-WINDOWS is set, do not delete other windows and keep the root window
  open."
    (interactive "P")
    (when (not keep-windows)
      (delete-other-windows))
    (save-selected-window
      (split-window-horizontally)
      (dolist (ol (mmm-overlays-contained-in (point-min) (point-max)))
        (let* ((window (split-window-below))
               (mode (or (plist-get vue-dedicated-modes (overlay-get ol 'mmm-mode))
                         (overlay-get ol 'mmm-mode)))
               (buffer (edit-indirect-region (overlay-start ol) (overlay-end ol))))
          (maximize-window)
          (with-current-buffer buffer
            (funcall mode))
          (set-window-buffer window buffer)))
      (balance-windows))
    (when (not keep-windows)
      (delete-window)
      (balance-windows)))
  )

;; -----------------------------
;; Docker
;; -----------------------------
(use-package docker :ensure t :defer t)
(use-package docker-compose-mode :ensure t :defer t)
(use-package docker-tramp :ensure t :defer t)

;; -----------------------------
;; Python
;; -----------------------------
(use-package python :ensure t :defer t
  :requires (eglot)
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  )
(use-package py-isort :ensure t :defer t)
(use-package blacken :ensure t :defer t)
(use-package pyvenv :ensure t :defer t
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/.venv")))

;; -----------------------------
;; My Packages
;; -----------------------------
(el-get-bundle gist:0a849059d1fb61de397f57477ed38c92:trans :type "git")
(el-get-bundle gist:e8a10244aac6308de1323d1f6685658b:change-case :type "git")
(el-get-bundle sudden-death :url "git@github.com:TakesxiSximada/sudden-death.el.git" :type "git")
(el-get-bundle dockerfile-mode :url "git@github.com:TakesxiSximada/dockerfile-mode.git" :type "git")
(el-get-bundle dotenv-mode :url "git@github.com:collective-el/emacs-dotenv-mode.git" :type "git")
(el-get-bundle elnode :type "git" :url "git@github.com:collective-el/elnode.git")
(el-get-bundle foreman-mode :url "git@github.com:collective-el/foreman-mode.git" :type "git")
(el-get-bundle gist:05de904cd0c320733cae:org-file-table :type "git")
(el-get-bundle gist:10985431:go-template-mode :type "git")
(el-get-bundle gist:6961ff8bfb228a7601ed470598fad513:django.el :type "git")
(el-get-bundle gist:73383aaf81656737fa533dd39dcb27a8:docker-compose-up-services :type "git")
(el-get-bundle gist:beb8e1944af406c3fb4f74b6e0e3b5fe:require-to-install-executable :type "git")
(el-get-bundle gist:c4c6ee198a1c576220a144ab825fa2f0:mastodon :type "git")
(el-get-bundle gist:d451221dc2a280b7e35d:kpt.el :type "git")

;; -----------------------------
;; Editor
;; -----------------------------
(defvar editor-buffer-name "*EDITOR*")

(defvar editor-map (make-sparse-keymap))


(defun editor-refresh-export-option-date ()
  "DATEエクスポートオプションの更新"
  (interactive)
  (let* ((timestamp (format-time-string "%+FT%T%z"))
  	 (pattern (format "s/^\#+DATE:.*$/#+DATE: %s/g" timestamp)))
    (call-process-region (point-min) (point-max) "sed" t t t "-e" pattern)))


(defun editor-create-buffer ()
  (interactive)
  (let ((buf-name editor-buffer-name))
    (with-current-buffer (get-buffer-create buf-name)
      (if (= 0 (buffer-size))
  	  (progn
  	    ;; エクスポートオプションの追加
  	    (save-excursion
              (goto-char 0)
              (insert "#+DATE:\n#+TAGS[]: comment\n\n"))

  	    (editor-refresh-export-option-date)))
      (kill-all-local-variables)
      (use-local-map editor-map)
      (editor-mode))
    (switch-to-buffer buf-name)))

(define-derived-mode editor-mode org-mode
  "Editor mode"
  nil)

(defcustom editor-base-directory "/opt/ng/symdon/pages/posts"
  "Editor mode")
(defcustom editor-file-path-directory-style nil
  "Editor mode")

(defun editor-make-new-file-path ()
  "エディターモードの保存先ファイルのパス返す。

  通常ではファイルスタイルorgファイル (XXXX.org) のパスを返す。
  `editor-file-path-directory-style` をNONE NILにするとディレクトリスタ
  イルのパス(XXXX/index.org)を返す。
  "
  (let ((file-style-path (concat (directory-file-name editor-base-directory)
  				 (format "/%s.org" (truncate (float-time))))))
    (if editor-file-path-directory-style
  	(concat (directory-file-name (file-name-sans-extension file-style-path)) "/index.org")
      file-style-path)))

(defcustom editor-new-file-path #'editor-make-new-file-path
  "Editor mode")

(defun editor-save-as-kill ()
  "エディターバッファの内容をファイルに保存してgit commitする"
  (interactive)
  (let ((new-file-path (funcall editor-new-file-path)))

    ;; Create parent directory.
    (make-directory (file-name-directory new-file-path) t)

    ;; Copy buffer content
    (switch-to-buffer
     (with-current-buffer (find-file-noselect new-file-path)
       (insert-buffer-substring (get-buffer editor-buffer-name))
       (save-buffer)
       (current-buffer)))

    ;; Git commit
    (let ((default-directory (file-name-directory new-file-path)))
      (shell-command (format "git add %s" new-file-path))
      (shell-command (format "git commit -m 'Add comment.' %s" new-file-path))))

  (kill-buffer editor-buffer-name))

(defun editor-save-as-kill-file-style ()
  "ファイルスタイルでエディターバッファの内容を保存する"
  (interactive)
  (let ((editor-file-path-directory-style nil))
    (editor-save-as-kill)))

(defun editor-save-as-kill-directory-style ()
  "ディレクトリスタイルでエディターバッファの内容を保存する"
  (interactive)
  (let ((editor-file-path-directory-style t))
    (editor-save-as-kill)))

(transient-define-prefix editor-save-as ()
			 "Editor mode save as..."
			 ["Save as"
			  ("f" "Save as file style" editor-save-as-kill-file-style)
			  ("d" "Save as directory style" editor-save-as-kill-directory-style)
			  ("s" "Save as default" editor-save-as-kill)
			  ])

(bind-keys :map editor-mode-map
  	   ("C-x C-s" . editor-save-as))

;; -----------------------------
;; macOS
;; -----------------------------
(defun macos-app (&optional app buf)
  "Start macOS application from Emacs"
  (interactive
   (list (completing-read
	  "Application: "
  	  (apply #'append
  		 (mapcar (lambda (application-path)
  			   (mapcar (lambda (name) (concat (directory-file-name application-path) "/" name))
  				   (directory-files application-path nil ".app")))
  			 '("/Applications"
  			   "/Applications/Utilities"
  			   "/System/Applications"
  			   "/System/Applications/Utilities"))))
  	 (get-buffer-create "*Application*")))
  (make-process :name "*App*"
  		:buffer (get-buffer-create "*App*")
  		:command `("open" "-g" ,app)
  		))

;; -----------------------------
;; asciidoc-view
;; -----------------------------
(defun asciidoc-view ()
  (interactive)
  (shell-command (format "asciidoc -o /tmp/foo.html %s" (buffer-file-name)))
  (eww-open-file "/tmp/foo.html"))

;; -----------------------------
;; urllib-encode
;; -----------------------------
(require 'url-util)

(defun our-url-encode ()
  (interactive)
  (kill-new
   (url-hexify-string
    (buffer-substring-no-properties
     (region-beginning) (region-end)))))

;; -----------------------------
;; sql-fmt
;; - https://github.com/maxcountryman/forma
;; - https://www.emacswiki.org/emacs/SqlBeautify
;; -----------------------------
(defvar sql-fmt-command "forma --max-width 60")

(defun sql-fmt-region (beg end)
  (interactive "r")
  (save-restriction
    (shell-command-on-region beg end
			     sql-fmt-command
			     nil t)))

(defun sql-fmt-buffer ()
  (interactive)
  (sql-fmt-region (point-min) (point-max)))

;; -----------------------------
;; wakatime
;; -----------------------------
;; (setq wakatime-response-buffer nil)

;; (defun restclient-http-handle-response (status method url bufname raw stay-in-window)
;;   "Switch to the buffer returned by `url-retreive'.
;; The buffer contains the raw HTTP response sent by the server."
;;   (setq restclient-within-call nil)
;;   (setq restclient-request-time-end (current-time))
;;   (if (= (point-min) (point-max))
;;       (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
;;     (when (buffer-live-p (current-buffer))
;;       (with-current-buffer (restclient-decode-response
;;                             (current-buffer)
;;                             bufname
;;                             restclient-same-buffer-response)
;;         (run-hooks 'restclient-response-received-hook)
;;         (unless raw
;;           (restclient-prettify-response method url))
;;         (buffer-enable-undo)
;; 	(restclient-response-mode)
;;         (run-hooks 'restclient-response-loaded-hook))))
;;   (current-buffer))

;; (defun wakatime-update-response-buffer ()
;;   (setq wakatime-response-buffer (current-buffer)))

;; (setq waka-work-type-list
;;       '("browsing"
;;         "building"
;;         "code reviewing"
;;         "coding"
;;         "debugging"
;;         "designing"
;;         "indexing"
;;         "learning"
;;         "manual testing"
;;         "meeting"
;;         "planning"
;;         "researching"
;;         "running tests"
;;         "writing docs"
;;         "writing tests"
;;         ))

;; (setq org-waka-work-type-property-name "WAKATIME_WORK_TYPE")

;; (defun org-waka-set-work-type (work-type)
;;   (interactive (list (completing-read "WORK TYPE: "
;; 				      waka-work-type-list)))
;;   (org-set-property org-waka-work-type-property-name work-type))


;; (defun waka-get-category ()
;;   (interactive)
;;   (if-let ((current-task-buffer (org-clock-is-active)))
;;       (with-current-buffer current-task-buffer
;; 	(save-excursion
;; 	  (goto-char (marker-position org-clock-marker))
;; 	  (cdr (assoc org-waka-work-type-property-name (org-entry-properties)))))
;;     "planning"))

;; (defun waka-get-entity ()
;;   (interactive)
;;   (buffer-name))

;; (defun waka-get-language ()
;;   (interactive)
;;   major-mode)


;; (defun wakatime-send-heatbeat ()
;;   (interactive)
;;   (with-current-buffer (find-file-noselect
;; 			(expand-file-name "~/.emacs.d/wakatime.http"))
;;     (if (buffer-live-p wakatime-response-buffer)
;; 	(let ((kill-buffer-query-functions nil))
;; 	  (kill-buffer wakatime-response-buffer))
;;       (setq wakatime-response-buffer (restclient-http-send-current-stay-in-window)))))

;; (add-hook 'restclient-response-loaded-hook 'wakatime-update-response-buffer)
;; (setq wakatime-timer (run-with-idle-timer 20 t 'wakatime-send-heatbeat))
;; (define-key org-mode-map (kbd "C-c C-x C-w") #'org-waka-set-work-type)


;; -------------------------
;; Load README configuration
;; -------------------------
(require 'org)

(save-window-excursion
  ;; Almost the same as org-babel-load-file, But the tangled filenames
  ;; are added dot to prefix for make it easier to choose in dired.
  (let ((tangled-file (org-babel-tangle-file
		       (expand-file-name "README.org" user-emacs-directory)
		       ".README.el"
		       "emacs-lisp\\|elisp")))
    (load-file (car tangled-file))))

;; -------------------------
;; Load README configuration
;; -------------------------
(use-package dockerfile-mode :ensure t :defer t)

(require 'dockerfile-mode)

(defun dockerfile-get-docker-image-from-inbuffer ()
  "# iamge: DockerImageName"
  (interactive)
  (let ((image-name-line (save-excursion
		      (goto-char (point-min))
		      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (s-trim (car (cdr (s-split ":" image-name-line))))))


(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (ido-completing-read "Image name: "
		       dockerfile-image-name-history
		       nil nil nil nil
		       (dockerfile-get-docker-image-from-inbuffer)))


(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If prefix arg NO-CACHE is set, don't cache the image.
The build string will be of the format:
`sudo docker build --no-cache --tag IMAGE-NAME --build-args arg1.. -f filename directory`"
  (interactive (list (dockerfile-read-image-name)
		     (not (y-or-n-p "Using cache?"))))
  (save-buffer)
  (vterm-command
   (format
    "%s%s build --ssh=default %s %s %s -f %s %s"  ;; FIX
    (if dockerfile-use-sudo "sudo " "")
    dockerfile-mode-command
    (if no-cache "--no-cache" "")
    (dockerfile-tag-string image-name)
    (dockerfile-build-arg-string)
    (shell-quote-argument (dockerfile-standard-filename (buffer-file-name)))
    (shell-quote-argument (dockerfile-standard-filename default-directory)))
   default-directory))
;; nil
   ;; (lambda (_) (format "*docker-build-output: %s *" image-name))))


(define-key dockerfile-mode-map (kbd "C-c C-c") #'dockerfile-build-buffer)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; load custom file.
(load-file custom-file)

;; Setup PATH environment variable
(setenv "PATH" (string-join exec-path ":"))
