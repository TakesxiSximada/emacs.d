;; -*- coding: utf-8 -*-
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	;; ("melpa-stable" . "http://stable.melpa.org/packages/")
	)

(use-package magit :defer t :ensure t :no-require t)

