;; -*- lexical-binding: t -*-
(setq user-common-before-file (buffer-file-name))
(when user-init-file
  (setq user-emacs-directory (file-name-directory user-init-file)))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;------------------------
;; use-package
;;------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(use-package powerline :ensure t :defer t)

