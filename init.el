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
(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------
;; Load emacs customize file
;; ----------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(load-file custom-file)

;; -----------------------------
;; package.el
;; -----------------------------
(setq package-user-dir (expand-file-name (format "~/.elpa.%d" emacs-major-version))
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
;; Setup PATH environment variable
(setenv "PATH" (string-join exec-path ":"))

;; AWS CLI for local
(defun awsl-switch-origin (origin)
  (interactive "s:AWSL_ORIGIN: ")
  (setenv "AWSL_ORIGIN" origin)
  (message (format "Change `AWSL_ORIGIN` environment variable: %s"
		   origin)))

;; AWS CLI for local
(defun aws-switch-profile (profile-name)
  (interactive "s:AWS_PROFILE: ")
  (setenv "AWS_PROFILE" profile-name)
  (message (format "Change `AWS_PROFILE` environment variable: %s"
		   profile-name)))
