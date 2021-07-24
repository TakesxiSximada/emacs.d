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
  (setq el-get-dir (expand-file-name "~/.el-get")))

;; -----------------------------
;; Base Key binding
;; -----------------------------
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-t")  nil)
(global-set-key (kbd "C-t h") 'windmove-left)
(global-set-key (kbd "C-t C-h") 'windmove-left)
(global-set-key (kbd "C-t j") 'windmove-down)
(global-set-key (kbd "C-t C-j") 'windmove-down)
(global-set-key (kbd "C-t k") 'windmove-up)
(global-set-key (kbd "C-t C-k") 'windmove-up)
(global-set-key (kbd "C-t l") 'windmove-right)
(global-set-key (kbd "C-t C-l") 'windmove-right)
(global-set-key (kbd "C-c C-w") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-w") 'kill-buffer)
(global-set-key (kbd "C-x C-w") 'kill-buffer)

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

;; -----------------------------
;; IDO
;; -----------------------------
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(use-package ido-vertical-mode :ensure t :defer
  :init
  (ido-vertical-mode))

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
;; Org
;; -----------------------------
(use-package org :ensure t :defer t
  :config
  (org-indent-mode)
  (setq org-startup-indented t
        org-archive-location (format-time-string "ARCHIVE_%Y.org::" (current-time))
        ))
(require 'org-clock)

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
	(vterm-buffer-name (format "%s: In %s"
				   (car (split-string line))
				   (expand-file-name cwd)))
	(vterm-kill-buffer-on-exit nil))
    (vterm)))

;; -----------------------------
;; Others
;; -----------------------------
(use-package add-node-modules-path :ensure t :defer t)
(use-package ag :ensure t :defer t :no-require t)
(use-package avy-menu :ensure t :defer t)
(use-package blacken :ensure t :defer t)
(use-package db :ensure t :defer t)
(use-package dired-filter :ensure t :defer t)
(use-package docker :ensure t :defer t)
(use-package docker-compose-mode :ensure t :defer t)
(use-package docker-tramp :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package fakir :ensure t :defer t)
(use-package flycheck :ensure t :defer t)
(use-package github-review  :ensure t :defer t)
(use-package google-translate :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package mew :ensure t :defer t)
(use-package monky :ensure t :defer t)
(use-package nginx-mode :ensure t :defer t)
(use-package ob-async :ensure t :defer t )
(use-package ob-restclient :ensure t :defer t)
(use-package pcre2el :ensure t :defer t)
(use-package py-isort :ensure t :defer t)
(use-package pyvenv :ensure t :defer t)
(use-package request :ensure t :defer t)
(use-package restclient :ensure t :defer t)
(use-package s :ensure t :defer t)
(use-package slime :ensure t :defer t)
(use-package smex :ensure t :defer t)
(use-package sudden-death :ensure t :defer t)
(use-package terraform-mode :ensure t :defer t)
(use-package transient :ensure t)
(use-package unicode-escape :ensure t :defer t)
(use-package vagrant-tramp :ensure t :defer t)
(use-package web :ensure t :defer t)
(use-package wgrep :ensure t :defer t)
(use-package wgrep-ag :ensure t :defer t)

(use-package eglot :defer t :ensure t
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package company :ensure t :pin melpa
  :config
  (global-company-mode)
  (setq
   company-idle-delay 0 ; default = 0.5
   company-minimum-prefix-length 2 ; default = 4
   company-selection-wrap-around t ; 候補の一番下でさらに下に行こうとすると一番上に戻る
   company-tooltip-idle-delay nil)
  )

(use-package typescript-mode :defer t :ensure t
  :config
  (setq typescript-indent-level 2))

;; -----------------------------
;; My Packages
;; -----------------------------
(el-get-bundle gist:0a849059d1fb61de397f57477ed38c92:trans :type "git")
(el-get-bundle gist:e8a10244aac6308de1323d1f6685658b:change-case :type "git")

(el-get-bundle dotenv-mode :url "git@github.com:collective-el/emacs-dotenv-mod.el.git" :type "git")
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
;; Extend Key binding
;; -----------------------------
(global-set-key (kbd "C-M-i") 'company-complete)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-v") 'magit-status)
(global-set-key (kbd "C-t C-c") 'vterm-command)
(global-set-key (kbd "C-t C-w") 'editor-create-buffer)

(global-set-key (kbd "s-t") 'make-frame)
(global-set-key (kbd "C-t C-t") 'other-frame)

;; -----------------------------
;; Path
;; -----------------------------
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/opt/openjdk/bin")
(add-to-list 'exec-path (expand-file-name "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/whalebrew"))
(add-to-list 'exec-path (expand-file-name "~/.goenv/bin"))
(add-to-list 'exec-path (expand-file-name "~/.goenv/shims"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'exec-path (expand-file-name "~/.nvm/versions/node/v8.15.0/bin"))
(add-to-list 'exec-path (expand-file-name "~/.poetry/bin"))
(add-to-list 'exec-path (expand-file-name "~/.whalebrew-bin/bin"))
(add-to-list 'exec-path (expand-file-name "~/.whalebrew-bin/bin"))
(add-to-list 'exec-path (expand-file-name "~/Library/Python/.bin"))
(add-to-list 'exec-path (expand-file-name "~/development/flutter/bin"))
(add-to-list 'exec-path (expand-file-name "~/google-cloud-sdk/bin"))
(add-to-list 'exec-path "/opt/ng/symdon/whalebrew")
(setenv "PATH" (string-join exec-path ":"))

(setenv "CPPFLAGS" (string-join '("-I/usr/local/opt/openjdk/include")))
