;; -*- coding: utf-8 -*-
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	))

(require 'windmove)
(use-package magit :defer t :ensure t :no-require t)
(use-package smex :defer t :ensure t :no-require t)

(bind-keys*
 ("¥" . "\\")
 ("C-h" . backward-delete-char-untabify)
 ("C-c C-w" . comment-or-uncomment-region)

 ;; keyboard macro
 ("<f1>" . start-kbd-macro)
 ("<f2>" . end-kbd-macro)
 ("<f3>" . call-last-kbd-macro)

 ;; move buffer
 ("C-t h" . windmove-left)
 ("C-t j" . windmove-down)
 ("C-t k" . windmove-up)
 ("C-t l" . windmove-right)

 ;; Git
 ("C-x C-v" . magit-status)
 )

;; Input I/F
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defun rust-lang-install ()
  (interactive)
  (async-shell-command "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh)"))
