;; -*- coding: utf-8 -*-
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	;; ("melpa-stable" . "http://stable.melpa.org/packages/")
	))

(use-package magit :defer t :ensure t :no-require t)

(bind-keys* ("C-x C-v" . magit-status))

(defun rust-lang-install ()
  (interactive)
  (async-shell-command "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh)"))
