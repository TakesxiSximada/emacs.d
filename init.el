;; -*- coding: utf-8 -*-
(toggle-frame-fullscreen)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'sximada-dark t)

(when window-system
  (global-hl-line-mode t))
(unless window-system
  (setq hl-line-face 'underline)
  (global-hl-line-mode))

;; locale
(setenv "LANG" "ja_JP.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)

;; toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; fonts
(set-face-attribute 'default nil :family "Menlo" :height 120)
(let ((typ (frame-parameter nil 'font)))
  (unless (string-equal "tty" typ)
    (set-fontset-font typ 'japanese-jisx0208
                      (font-spec :family "Hiragino Kaku Gothic ProN"))))

(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; backup file
(setq make-backup-files nil)
(setq auto-save-default nil)


;; Splash
(setq initial-buffer-choice
      (lambda ()
	(switch-to-buffer "*Messages*")))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	))

;;; Environment Variable
(require 'cl)
(require 'subr-x)

(setq exec-path (delete-duplicates
		 (append `(
			   "/Library/TeX/texbin"
			   ;; "/Users/sximada/.nvm/versions/node/v8.10.0/bin"
			   "/Users/sximada/.nvm/versions/node/v8.15.0/bin"
			   ;; "/Users/sximada/.nvm/versions/node/v12.6.0/bin"
			   "/Users/sximada/development/flutter/bin"
			   "/usr/local/bin"
			   "/usr/local/opt/gettext/bin"
			   "/usr/local/opt/libxml2/bin"
			   "/usr/local/opt/openssl/bin"
			   "/usr/local/opt/sqlite/bin"
			   "/usr/local/opt/texinfo/bin"
			   ,(expand-file-name "~/.cargo/bin")
			   ,(expand-file-name "~/.local/bin")
			   ,(expand-file-name "~/google-cloud-sdk/bin")
			   )
			 (split-string (getenv "PATH") ":")
			 exec-path)))

(setenv "PATH" (string-join exec-path ":"))

(setenv "LDFLAGS" (string-join '(
				 "-L/usr/local/opt/gettext/lib"
				 "-L/usr/local/opt/libffi/lib"
				 "-L/usr/local/opt/libxml2/lib"
				 "-L/usr/local/opt/openssl/lib"
				 "-L/usr/local/opt/readline/lib"
				 "-L/usr/local/opt/sqlite/lib"
				 "-L/usr/local/opt/texinfo/lib"
				 ) " "))

(setenv "CPPFLAGS" (string-join '(
				  "-I/usr/local/opt/gettext/include"
				  "-I/usr/local/opt/libxml2/include"
				  "-I/usr/local/opt/openssl/include"
				  "-I/usr/local/opt/readline/include"
				  "-I/usr/local/opt/sqlite/include"
				  ) " "))

(setenv "PKG_CONFIG_PATH" (string-join '(
					 "/usr/local/opt/libffi/lib/pkgconfig"
					 "/usr/local/opt/libxml2/lib/pkgconfig"
					 "/usr/local/opt/openssl/lib/pkgconfig"
					 "/usr/local/opt/readline/lib/pkgconfig"
					 "/usr/local/opt/sqlite/lib/pkgconfig"
					 ) ":"))

;;; Environment Variable Ends here

(require 'windmove)

(use-package magit :defer t :ensure t :no-require t)
(use-package monky :defer t :ensure t :no-require t)
(use-package transient :defer t :ensure t :no-require t)

;; Input I/F
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(use-package smex :ensure t :no-require t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )

(use-package ido-vertical-mode :ensure t :no-require t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
  )

(use-package ido-completing-read+ :ensure t :defer t :no-require t
  :config
  (ido-ubiquitous-mode 1))

;;; For Silver Searcher (ag)
(use-package ag :ensure t :defer t :no-require t)

;;; For Nginx
(use-package nginx-mode :ensure t)


;;; For Docker
(use-package docker :defer t :ensure t :no-require t)
(use-package docker-compose-mode :defer t :ensure t :no-require t)
(use-package docker-tramp :defer t :ensure t :no-require t)
(use-package dockerfile-mode :defer t :ensure t :no-require t)

;;; For LSP
(use-package eglot :defer t :ensure t :no-require t
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark))

;;; For restclient
(use-package restclient :defer t :ensure t :no-require t)

;;; For babel
(use-package ob-restclient :defer t :ensure t :no-require t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (restclient . t)
   ;;(rustic . t)
   (shell . t)
   (sql . t)))

;;; For Rust
(use-package rustic :defer t :ensure t :no-require t
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-rls-pkg 'eglot))


;;; For Python
(use-package pyvenv :defer t :ensure t :no-require t
  :config
  ;; https://github.com/jorgenschaefer/pyvenv/blob/fa6a028349733b0ecb407c4cfb3a715b71931eec/pyvenv.el#L168-L184
  (defun pyvenv-create (venv-name python-executable)
    "Create virtualenv.  VENV-NAME  PYTHON-EXECUTABLE."
    (interactive (list
                  (read-from-minibuffer "Name of virtual environment: ")
                  (read-file-name "Python interpreter to use: "
                                  (file-name-directory (executable-find "python"))
                                  nil nil "python")))
    (let ((venv-dir (concat (file-name-as-directory (pyvenv-workon-home))
                            venv-name)))
      (unless (file-exists-p venv-dir)
	(run-hooks 'pyvenv-pre-create-hooks)
	(with-current-buffer (generate-new-buffer "*virtualenv*")
          (call-process python-executable nil t t
			"-m" "venv" venv-dir)
          (display-buffer (current-buffer)))
	(run-hooks 'pyvenv-post-create-hooks))
      (pyvenv-activate venv-dir)))
  )

;;; For TypeScript
(use-package typescript-mode :defer t :ensure t :no-require t
  :config
  (custom-set-variables '(typescript-indent-level 2)))

;;; For React
(use-package rjsx-mode :defer t :ensure t :no-require t
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil))

;;; Our Async Exec
(defvar our-async-exec-cmd-history nil)
(defvar our-async-exec-cwd-history nil)
(defvar our-async-exec-cmd nil)

(defun our-create-buffer-name (cmd &optional cwd)
  (format "`%s`%s"
	  (let ((elms (split-string cmd)))
	    (mapconcat 'identity
		       (append (last (split-string (car elms) "/"))
			       (cdr elms))
		       " "))
	  (if (and cwd (> (length cwd) 0)) (format ": %s" cwd) "")))


(defun our-async-exec (cmd &optional cwd buffer)
  (let ((buf (or buffer (get-buffer-create (our-create-buffer-name cmd cwd)))))
    (with-current-buffer buf
      (setq-local default-directory (or cwd default-directory))
      (async-shell-command cmd buf)
      ;; (our-async-exec-mode)
      (setq-local default-directory (or cwd default-directory))
      (setq-local our-async-exec-cmd cmd))))


(defun our-async-exec-retry ()
  (interactive)
  (if-let* ((cmd our-async-exec-cmd))
      (progn
	(async-shell-command our-async-exec-cmd (current-buffer))
	(our-async-exec-mode)
	(setq-local our-async-exec-cmd cmd))))


(defun our-async-exec-interactive (cmd &optional cwd buffer)
  (interactive
   (list (read-string "Command: "
		      ""
		      'our-async-exec-cmd-history
		      "")
	 (read-string "Directory: "
		      default-directory
		      'our-async-exec-cwd-history
		      default-directory)))
  (our-async-exec cmd cwd buffer))

(defun our-async-exec-close ()
  (interactive)
  (kill-buffer (current-buffer)))


(defun our-get-buffer-create (&optional name)
  (interactive "sBuffer Name: ")
  (let ((buf-name (format "*%s*" name)))
    (get-buffer-create buf-name)
    (message (format "Created a buffer: %s" buf-name))))


;; MONKEY PATCHING
(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
	;; Run preoutput filters
	(let ((functions comint-preoutput-filter-functions))
	  (while (and functions string)
	    (if (eq (car functions) t)
		(let ((functions
                       (default-value 'comint-preoutput-filter-functions)))
		  (while (and functions string)
		    (setq string (funcall (car functions) string))
		    (setq functions (cdr functions))))
	      (setq string (funcall (car functions) string)))
	    (setq functions (cdr functions))))

	;; Insert STRING
	(let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
	      (saved-point (copy-marker (point) t)))

	  ;; We temporarily remove any buffer narrowing, in case the
	  ;; process mark is outside of the restriction
	  (save-restriction
	    (widen)

	    (goto-char (process-mark process))
	    (set-marker comint-last-output-start (point))

            ;; Try to skip repeated prompts, which can occur as a result of
            ;; commands sent without inserting them in the buffer.
            (let ((bol (save-excursion (forward-line 0) (point)))) ;No fields.
              (when (and (not (bolp))
                         (looking-back comint-prompt-regexp bol))
                (let* ((prompt (buffer-substring bol (point)))
                       (prompt-re (concat "\\`" (regexp-quote prompt))))
                  (while (string-match prompt-re string)
                    (setq string (substring string (match-end 0)))))))
            (while (string-match (concat "\\(^" comint-prompt-regexp
                                         "\\)\\1+")
                                 string)
              (setq string (replace-match "\\1" nil nil string)))

	    ;; insert-before-markers is a bad thing. XXX
	    ;; Luckily we don't have to use it any more, we use
	    ;; window-point-insertion-type instead.
	    (insert string)

	    ;; Advance process-mark
	    (set-marker (process-mark process) (point))

	    (unless comint-inhibit-carriage-motion
	      ;; Interpret any carriage motion characters (newline, backspace)
	      (comint-carriage-motion comint-last-output-start (point)))

	    ;; Run these hooks with point where the user had it.
	    (goto-char saved-point)
	    (run-hook-with-args 'comint-output-filter-functions string)
	    (set-marker saved-point (point))

	    (goto-char (process-mark process)) ; In case a filter moved it.

	    (unless comint-use-prompt-regexp
              (with-silent-modifications
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
				       (field inhibit-line-move-field-capture)
				       rear-nonsticky t
				       field output
				       inhibit-line-move-field-capture t))))

	    ;; Highlight the prompt, where we define `prompt' to mean
	    ;; the most recent output that doesn't end with a newline.
	    (let ((prompt-start (save-excursion (forward-line 0) (point)))
		  (inhibit-read-only t))
	      (when comint-prompt-read-only
		(with-silent-modifications
		  (or (= (point-min) prompt-start)
		      (get-text-property (1- prompt-start) 'read-only)
		      (put-text-property (1- prompt-start)
					 prompt-start 'read-only 'fence))
		  (add-text-properties prompt-start (point)
				       '(read-only t front-sticky (read-only)))))
	      (when comint-last-prompt
		;; There might be some keywords here waiting for
		;; fontification, so no `with-silent-modifications'.
		(font-lock--remove-face-from-text-property
		 (car comint-last-prompt)
		 (cdr comint-last-prompt)
		 'font-lock-face
		 'comint-highlight-prompt))
	      (setq comint-last-prompt
		    (cons (copy-marker prompt-start) (point-marker)))
	      ;; ここのプロパティ設定によりとてつもなく処理がおもくなるためコメントアウト
	      ;; (font-lock-prepend-text-property prompt-start (point)
	      ;; 				       'font-lock-face
	      ;; 				       'comint-highlight-prompt)
	      (add-text-properties prompt-start (point) '(rear-nonsticky t)))
	    (goto-char saved-point)))))))


(easy-mmode-define-minor-mode our-async-exec-mode
			      "This is our-async-exec-mode"
			      nil
			      "OurAsyncExec"
			      '(("C-c C-g" . our-async-exec-retry)
				("C-c C-q" . our-async-exec-close)))

;;; Our Async Exec Ends here.

;;; Our Git Clone
(defun our-git-config-entry (name email ssh-private-key-path)
  `((name . ,name)
    (email . ,email)
    (key . ,ssh-private-key-path)))

(defvar our-git-config nil)

(defun our-git-clone (repo label cwd name)
  (interactive
   (list
    (completing-read "Repository: " nil)
    (ido-completing-read
     "Git configuration: "
     (mapcar (lambda (n) (car n)) our-git-config)
     nil nil nil nil nil)
    (ido-read-directory-name "Directory: ")
    (completing-read "Name: " nil)))

  (unless (file-exists-p cwd)
    (make-directory cwd))

  (let ((entry (cdr (assoc label our-git-config))))
    (our-async-exec
     (format "git -c core.sshCommand='ssh -i %s -F /dev/null' clone %s %s"
	     (cdr (assoc 'key entry))
	     repo name)
     cwd)))


(defun our-git-config-apply (label cwd)
  (interactive
   (list
    (ido-completing-read
	  "Git configuration: "
	  (mapcar (lambda (n) (car n)) our-git-config)
	  nil nil nil nil nil)
    (ido-read-directory-name "Directory: ")))

  (let ((entry (cdr (assoc label our-git-config))))
    (our-async-exec
     (format "git config user.name '%s' && git config user.email '%s' && git config core.sshCommand 'ssh -i %s -F /dev/null'"
	     (cdr (assoc 'name entry))
	     (cdr (assoc 'email entry))
	     (cdr (assoc 'key entry)))

     cwd)))
;;; Our Git Clone Ends here.

;;; Our open user init file
(defun our-open-user-init-file ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    user-init-file)))
;;; Our open init file Ends here.


(defun rust-lang-install ()
  (interactive)
  (async-shell-command "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh)"))

;;; For elscreen
(use-package elscreen :ensure t :no-require t
  :init
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (bind-key* "C-t C-t" 'elscreen-previous)
  :config
  (elscreen-start)
  (elscreen-create))

;;; For flycheck
(use-package flycheck :defer :ensure t :no-require t
  :init
  (add-hook 'python-mode-hook 'flycheck-mode))

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

 ;; My Customize
 ("M-_" . our-async-exec-interactive)

 ;; File open utility
 ("<f12>" . our-open-user-init-file)
 )

(load-file "~/.emacs.d/settings.el")
