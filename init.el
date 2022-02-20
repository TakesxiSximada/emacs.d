;; -----------------------------
;; Basic Behavior
;; ----------------------------
(setq-default find-function-C-source-directory "/opt/ng/emacs/src"
	      debug-on-error t
	      make-backup-files nil
	      auto-save-default nil
	      custom-file (locate-user-emacs-file "custom.el")
	      custom-theme-directory (expand-file-name "~/.emacs.d/themes")
	      custom-readme-file (expand-file-name "README.org" user-emacs-directory)
	      custom-env-file (expand-file-name ".env" user-emacs-directory)
	      custom-additional-load-file-list nil
	      )

;; -----------------------------
;; Basic Key bindings
;; ----------------------------
(define-key global-map (kbd "C-t") nil)
(define-key global-map (kbd "<f1>") #'start-kbd-macro)
(define-key global-map (kbd "<f2>") #'end-kbd-macro)
(define-key global-map (kbd "<f3>") #'call-last-kbd-macro)
(define-key global-map (kbd "<f4>") #'insert-kbd-macro)
(define-key global-map (kbd "C-c C-w") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-h") #'backward-delete-char-untabify)
(define-key global-map (kbd "C-t C-h") 'windmove-left)
(define-key global-map (kbd "C-t C-j") 'windmove-down)
(define-key global-map (kbd "C-t C-k") 'windmove-up)
(define-key global-map (kbd "C-t C-l") 'windmove-right)
(define-key global-map (kbd "C-t h") 'windmove-left)
(define-key global-map (kbd "C-t j") 'windmove-down)
(define-key global-map (kbd "C-t k") 'windmove-up)
(define-key global-map (kbd "C-t l") 'windmove-right)
(define-key global-map (kbd "C-x C-w") 'kill-buffer)

;; -----------------------------
;; Basic U/I
;; ----------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------
;; Load emacs customize file
;; ----------------------------
(load-file custom-file)

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
;; Color Theme
;; -----------------------------
(load-theme 'simple-darkness t)

;; -------------------------
;; Load README configuration
;; -------------------------
(when (package-installed-p 'org)
  (require 'org)
  (save-window-excursion
    ;; Almost the same as org-babel-load-file, But the tangled filenames
    ;; are added dot to prefix for make it easier to choose in dired.
    (let ((tangled-file (org-babel-tangle-file custom-readme-file
					       ".README.el"
					       "emacs-lisp\\|elisp")))
      (load-file (car tangled-file)))))

;; -----------------------------
;; Misc settings
;; -----------------------------

;; Switch Full screen
(toggle-frame-fullscreen)

;; Setup PATH environment variable
(setenv "PATH" (string-join exec-path ":"))

;; -----------------------------
;; Distributions
;; -----------------------------
(defun start-spacemacs ()
  (interactive)
  (setq package-user-dir (expand-file-name "~/.elpa.spacemacs")
	spacemacs-start-directory (expand-file-name "~/.emacs.d/distributions/spacemacs/")
	spacemacs-bootstrap-file (file-name-concat spacemacs-start-directory "init.el")
	custom-file (locate-user-emacs-file "custom-spacemacs.el")
	)
  (setenv "SPACEMACSDIR" (expand-file-name "~/.emacs.d/spacemacs.d/"))
  (package-initialize t)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (load spacemacs-bootstrap-file nil nil))


(defun start-doom-emacs ()
  (interactive)
  (setenv "EMACSDIR" (expand-file-name "~/.emacs.d/distributions/doom-emacs/"))
  (setenv "DOOMLOCALDIR" (expand-file-name "~/.emacs.d/doom.d/"))
  (setenv "DOOMDIR" (expand-file-name "~/.emacs.d/doom.d/"))

  (setq package-user-dir (expand-file-name "~/.elpa.doom-emacs")
	custom-file (locate-user-emacs-file "custom-doom-emacs.el")
	)

  (setq user-emacs-directory "/Users/sximada/.emacs.d/distributions/doom-emacs/")
  (load (concat user-emacs-directory "core/core") nil 'nomessage)
  (load (expand-file-name "~/.emacs.d/distributions/doom-emacs/init.el") nil 'nomessage)
  (switch-to-buffer (get-buffer "*doom*")))
