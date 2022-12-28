;; -----------------------------
;; Basic Behavior
;; ----------------------------
(setq-default find-function-C-source-directory "/opt/ng/emacs/src"
	      debug-on-error nil
	      eval-expression-debug-on-error nil
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
  			 ("org" . "https://orgmode.org/elpa/")
  			 ("melpa" . "https://melpa.org/packages/")
			 ;; ("cubelpa" . "https://sximada.github.io/cubelpa-repo/packages/")
  			 ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
  			 ;; marmalade is already not mainted
  			 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
  			 ))
(package-initialize)

;; -----------------------------
;; Color Theme
;; -----------------------------
(load-theme 'symdon-kids t)


;; -----------------------------
;; Django configuration
;; -----------------------------
(defcustom django-run-shell-default-code-list
  '("from django.contrib.auth import get_user_model"
    "UserModel = get_user_model()"
    "from django.http.request import HttpRequest"
    "from rest_framework.request import Request")
  "")
;; -------------------------
;; Load README configuration
;; -------------------------
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (string-join exec-path ":"))
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

;; flymake
(require 'flymake)
(require 'flymake-diagnostic-at-point)

(add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
(set-face-attribute 'flymake-error nil :foreground "black" :background "red2" :box '(color "black"))
(set-face-attribute 'flymake-warning nil :foreground "black" :background "yellow" :box '(color "black"))
(set-face-attribute 'flymake-note nil :foreground "black" :background "DeepSkyBlue" :box '(color "black"))

;; For python configuration
(require 'eglot)
(require 'flymake-collection)
(require 'traverse-directory)
(require 'change-case)


(defcustom python-autoflake-executable "autoflake"
  "Autoflake command")

(defun python-autoflake-reformat ()
  (interactive)

  (let ((before-save-hook nil))
    (save-buffer))
  (call-process python-autoflake-executable nil nil nil "-i" "--remove-all-unused-imports" buffer-file-name)
  (revert-buffer t t t))

(defcustom django-run-test-dotenv nil
  "Dot env file path for django")

(defcustom django-run-test-python-executable "python"
  "Python command")


(defun django-run-shell ()
  (interactive)
  (if-let ((default-directory (traverse-directory-to-up
			       (file-name-directory buffer-file-name)
    			       '(traverse-directory-django-manage-py-p))))
      (progn
	(let ((process-environment (if (not django-run-test-dotenv)
				       process-environment
				     (with-current-buffer (find-file-noselect django-run-test-dotenv)
				       (string-split (buffer-substring-no-properties (point-min) (point-max))
						     "\n" t "#.*$")))))
	  (run-python (format "%s manage.py shell" django-run-test-python-executable)))

	(if-let ((current-dotted-name (change-case-dotted-case-render
				       (change-case-path-case-parse
					(string-remove-suffix
					 ".py"
					 (string-replace default-directory
							 "" buffer-file-name))))))
	    (mapcan (lambda (code)
		      (comint-send-string (get-buffer "*Python*")
					  (concat code "\n")))
		    (append django-run-shell-default-code-list
			    `(,(format "from %s import *" current-dotted-name))))))))

(defun django-run-test ()
  (interactive)
  (let ((before-save-hook nil))
    (save-buffer))

  (let ((test-buffer (get-buffer-create "*DJANGO*")))
    (if-let ((old-process (get-buffer-process test-buffer)))
	(signal-process old-process 1))

    (with-current-buffer test-buffer
      (erase-buffer))

    (display-buffer test-buffer)

    (if-let* ((default-directory (traverse-directory-to-up (file-name-directory buffer-file-name)
							   '(traverse-directory-django-manage-py-p)))
	      (test-dotted-name
	       (change-case-dotted-case-render
		(let ((parsed-dir-name-list
		       (change-case-path-case-parse
			(string-remove-suffix
			 ".py"
			 (string-replace default-directory
                           		 "" buffer-file-name)))))
		  (if (string-prefix-p "test" (car (last parsed-dir-name-list)))
		      parsed-dir-name-list
		    (butlast parsed-dir-name-list))))))
	(let ((process-environment (if (not django-run-test-dotenv)
				       process-environment
				     (with-current-buffer (find-file-noselect django-run-test-dotenv)
				       (string-split (buffer-substring-no-properties (point-min) (point-max))
						     "\n" t "#.*$")))))
	  (make-process :name "*DJANGO*"
			:buffer test-buffer
			:command `(,django-run-test-python-executable "manage.py" "test" "--no-input" "--keepdb" ,test-dotted-name))))))


(require 'flymake-collection-define)
(require 'flymake-collection-mypy)
(require 'flymake-mypy-custom)

(defun flymake-python-setup ()
  (flycheck-mode 0)
  ;; タグジャンプはeglotを使用した方が楽
  (setq eglot-server-programs (append `((python-mode . (,python-lsp-server-executable))) eglot-server-programs))
  (eglot-ensure)

  ;; バッファ保存時にフォーマットする
  (add-hook 'before-save-hook 'python-isort-buffer nil t)
  (add-hook 'before-save-hook 'python-autoflake-reformat nil t)
  (add-hook 'before-save-hook 'blacken-buffer nil t)
  (add-hook 'after-save-hook 'django-run-test nil t)

  ;; flymake関連はeglotが邪魔をするため、除去する。
  ;; 直接起動したほうが柔軟な対応が可能。
  (setq-local flymake-diagnostic-functions nil)
  (add-hook 'flymake-diagnostic-functions 'flymake-collection-flake8 nil t)
  (add-hook 'flymake-diagnostic-functions 'flymake-mypy-custom nil t)

  (flymake-mode-on)
  )

(setq python-mode-hook nil)
(add-hook 'python-mode-hook #'flymake-python-setup)

(define-key python-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key python-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key python-mode-map (kbd "M-<return>") 'django-run-shell)
