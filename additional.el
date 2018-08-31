;; -*- lexical-binding: t -*-
(require 'use-package)
(use-package async-await :ensure t :defer t)
(use-package json :ensure t :defer t)
(use-package request :ensure t :defer t)
(use-package async-await :ensure t :defer t)
(use-package gist :ensure t :defer t)
(use-package helm-themes :ensure t :defer t)
(use-package http :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package markdown-mode :ensure t)
(use-package quickrun :ensure t :defer t)
(use-package restclient :ensure t :defer t)
(use-package websocket :ensure t :defer t)
(use-package windmove :ensure t :defer t)

(use-package helm :ensure t :defer t
  :config
  (helm-mode t)
  (dired-async-mode t)
  (setq helm-M-x-fuzzy-match t)
  (bind-keys :map helm-map
	     ("<tab>" . helm-execute-persistent-action)
	     ("C-i" . helm-execute-persistent-action)
	     ("C-z" . helm-select-action)))

(use-package elscreen :ensure t :defer t
  :config
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start)
  (elscreen-create))


(use-package editorconfig :ensure t :defer t
	     :init
	     (add-hook 'after-change-major-mode-hook 'editorconfig-apply)
	     :config
	     (editorconfig-mode 1))

(use-package spacemacs-theme :ensure t :defer t
  :no-require t
  :init
  (load-theme 'tsdh-dark t))


(use-package google-this :ensure t :defer t
  :config
  (google-this-mode 1)
  (setq google-this-location-suffix "co.jp")
  (defun google-this-url () "URL for google searches."
	 (concat google-this-base-url google-this-location-suffix
		 "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja")))


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


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


(use-package slack
  :ensure t :defer t
  :commands (slack-start)
  :init
  ;; (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (mapc (lambda (args) (apply 'slack-register-team args)) our-secrets-slack-team-alist))

(setq org-clock-out-remove-zero-time-clocks t)

;; custom
(load-file "/srv/mastodon/mast.el")
(add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

(defun our-create-buffer-name (cmd)
  (format "*%s*" (let ((elms (split-string cmd)))
		   (mapconcat 'identity
			      (append (last (split-string (car elms) "/"))
				      (cdr elms))
			      " "))))

(defun our-async-exec (cmd cwd &optional buffer)
  (let ((default-directory cwd))
    (async-shell-command cmd (or buffer (our-create-buffer-name cmd)))))
