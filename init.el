;; -*- coding: utf-8 -*-
;; Waiting initialize process
(read-key "Press any key...")

(toggle-frame-fullscreen)

;; theme
(message "Setup theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'sximada-dark t)

(when window-system
  (global-hl-line-mode t))
(unless window-system
  (setq hl-line-face 'underline)
  (global-hl-line-mode))

;; locale
(message "Setup locale")
(setenv "LANG" "ja_JP.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)

;; toolbar
(message "Setup toolbar")
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; fonts
(message "Setup fonts")
(set-face-attribute 'default nil :family "Menlo" :height 120)


(let ((typ (frame-parameter nil 'font)))
  (unless (string-equal "tty" typ)
    (set-fontset-font typ 'japanese-jisx0208
                      (font-spec :family "Hiragino Kaku Gothic ProN"))))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; backup file
(message "Setup backup file")
(setq make-backup-files nil)
(setq auto-save-default nil)


(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; package
(message "Setup backup file")
(require 'package nil 'noerror)

;; elpa/gnutls workaround
(if (string< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq
 package-enable-at-startup t
 package-user-dir (expand-file-name "~/.elpa")
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		    ("melpa" . "https://melpa.org/packages/")
		    ("org" . "https://orgmode.org/elpa/")
		    ;; ("marmalade" . "http://marmalade-repo.org/packages/")  ;; marmalade is already not mainted
		    ("melpa-stable" . "http://stable.melpa.org/packages/")
		    ))
(eval-when-compile
  (unless (file-exists-p (locate-user-emacs-file "tmp/bootstrap-stamp"))
    ;; (package-refresh-contents)
    (with-temp-buffer (write-file (locate-user-emacs-file "tmp/bootstrap-stamp")))
    ))

(package-initialize)
;; (package-refresh-contents)

(progn ;; Setup packaging tools
  (package-install 'use-package)
  (use-package quelpa :ensure t)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))
  ;; (use-package el-get :ensure t))

(use-package ox-qmd :ensure t
  :quelpa (ox-qmd :fetcher github :repo "0x60df/ox-qmd"))

(use-package shogi-mode :ensure t
  :quelpa (shogi-mode :fetcher github :repo "akicho8/shogi-mode"))

(use-package gist
  :ensure t :defer t
  :custom
  (gist-ask-for-filename t)
  (gist-ask-for-description t))


;;; Environment Variable
(message "Setup Environment Variable")
(require 'cl)
(require 'subr-x)

;;; Whalebrew configuration.
(setq whalebrew-config-dir (expand-file-name "~/.whalebrew"))
(setq whalebrew-install-path (concat whalebrew-config-dir "/bin"))
(setenv "WHALEBREW_INSTALL_PATH" whalebrew-install-path)
(mkdir whalebrew-install-path t)


(setq exec-path (delete-duplicates
		 (append `(
			   ,whalebrew-install-path
			   ,(expand-file-name "~/.whalebrew-bin/bin")
			   ,(expand-file-name "~/.cargo/bin")
			   ,(expand-file-name "~/.goenv/bin")
			   ,(expand-file-name "~/.goenv/shims")
			   ,(expand-file-name "~/.local/bin")
			   ,(expand-file-name "~/.nvm/versions/node/v8.15.0/bin")
			   ,(expand-file-name "~/.poetry/bin")
			   ,(expand-file-name "~/Library/Python/.bin")
			   ,(expand-file-name "~/development/flutter/bin")
			   ,(expand-file-name "~/google-cloud-sdk/bin")
			   "/Library/TeX/texbin"
			   "/usr/local/opt/mysql-client/bin"
			   "/usr/local/bin"
			   "/usr/local/opt/openjdk/bin"
			   "/usr/local/opt/apr-util/bin"
			   "/usr/local/opt/binutils/bin"
			   "/usr/local/opt/curl-openssl/bin"
			   "/usr/local/opt/gettext/bin"
			   "/usr/local/opt/icu4c/bin"
			   "/usr/local/opt/icu4c/sbin"
			   "/usr/local/opt/libpq/bin"
			   "/usr/local/opt/libxml2/bin"
			   "/usr/local/opt/llvm/bin"
			   "/usr/local/opt/ncurses/bin"
			   "/usr/local/opt/openldap/bin"
			   "/usr/local/opt/openldap/sbin"
			   "/usr/local/opt/openssl/bin"
			   "/usr/local/opt/php@7.2/bin"
			   "/usr/local/opt/php@7.2/sbin"
			   "/usr/local/opt/sqlite/bin"
			   "/usr/local/opt/texinfo/bin"
			   "/usr/local/opt/openssl@1.1/bin"
			   )
			 (split-string (getenv "PATH") ":")
			 exec-path)))

(setenv "PATH" (string-join exec-path ":"))

(setenv "LDFLAGS" (string-join '(
				 "-L/usr/local/Cellar/python@3.7/3.7.10_2/Frameworks/Python.framework/Versions/Current/lib"
				 "-L/usr/local/opt/mysql-client/lib"
				 "-L/usr/local/opt/binutils/lib"
				 "-L/usr/local/opt/curl-openssl/lib"
				 "-L/usr/local/opt/gettext/lib"
				 "-L/usr/local/opt/icu4c/lib"
				 "-L/usr/local/opt/libffi/lib"
				 "-L/usr/local/opt/libpq/lib"
				 "-L/usr/local/opt/libxml2/lib"
				 "-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
				 "-L/usr/local/opt/llvm/lib"
				 "-L/usr/local/opt/openldap/lib"
				 "-L/usr/local/opt/openssl/lib"
				 "-L/usr/local/opt/openssl@1.1/lib"
				 "-L/usr/local/opt/php@7.2/lib"
				 "-L/usr/local/opt/readline/lib"
				 "-L/usr/local/opt/readline/lib"
				 "-L/usr/local/opt/sqlite/lib"
				 "-L/usr/local/opt/texinfo/lib"
				 "-L/usr/local/opt/ncurses/lib"
				 ;; "-L/usr/local/opt/openssl@1.1/lib"
				 ) " "))
(setenv "CPPFLAGS" (string-join '(
				 "-L/usr/local/Cellar/python@3.7/3.7.10_2/Frameworks/Python.framework/Versions/Current/include"
				  "-I/usr/local/opt/openjdk/include"
				  "-I/usr/local/opt/mysql-client/include"
				  "-I/usr/local/opt/binutils/include"
				  "-I/usr/local/opt/curl-openssl/include"
				  "-I/usr/local/opt/gettext/include"
				  "-I/usr/local/opt/icu4c/include"
				  "-I/usr/local/opt/libpq/include"
				  "-I/usr/local/opt/libxml2/include"
				  "-I/usr/local/opt/llvm/include"
				  "-I/usr/local/opt/openldap/include"
				  "-I/usr/local/opt/openssl/include"
				  "-I/usr/local/opt/openssl@1.1/include"
				  "-I/usr/local/opt/php@7.2/include"
				  "-I/usr/local/opt/readline/include"
				  "-I/usr/local/opt/readline/include"
				  "-I/usr/local/opt/sqlite/include"
				  "-I/usr/local/opt/ncurses/include"
				  ;; "-I/usr/local/opt/openssl@1.1/include"
				  ) " "))

(setenv "PKG_CONFIG_PATH" (string-join '(
					 "/usr/local/Cellar/python@3.7/3.7.10_2/lib/pkgconfig"
					 "/usr/local/opt/libffi/lib/pkgconfig"
					 "/usr/local/opt/libxml2/lib/pkgconfig"
					 "/usr/local/opt/openssl/lib/pkgconfig"
					 "/usr/local/opt/openssl@1.1/lib/pkgconfig"
					 "/usr/local/opt/readline/lib/pkgconfig"
					 "/usr/local/opt/sqlite/lib/pkgconfig"
					 "/usr/local/opt/curl-openssl/lib/pkgconfig"
					 "/usr/local/opt/icu4c/lib/pkgconfig"
					 "/usr/local/opt/readline/lib/pkgconfig"
					 "/usr/local/opt/ncurses/lib/pkgconfig"
					 "/usr/local/opt/mysql-client/lib/pkgconfig"
					 ) ":"))

;;; Environment Variable Ends here
(message "Environment Variable Ends here")
(require 'windmove)

(setenv "GIT_PAGER" "cat")  ;; Do not use the git command pager
(use-package magit :defer t :ensure t :no-require t)
(use-package monky :defer t :ensure t :no-require t)
(use-package transient :defer t :ensure t :no-require t)

(use-package s :ensure t)
(require 's)

;; Input I/F
(message "Input I/F")
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(use-package smex :ensure t :no-require t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )

(use-package ido-completing-read+ :ensure t :defer t :no-require t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode :ensure t :no-require t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
  )

;;; For Silver Searcher (ag)
(message "For Silver Searcher (ag)")

(use-package ag :ensure t :defer t :no-require t)
(use-package wgrep :ensure t :defer t)
(use-package wgrep-ag :ensure t :defer t)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)


;;; For Nginx
(message "For Nginx")
(use-package nginx-mode :ensure t)


;;; For Docker
(message "For Docker")
(use-package dockerfile-mode :ensure t)
(require 'dockerfile-mode)
(use-package docker :defer t :ensure t :no-require t)
(use-package docker-compose-mode :defer t :ensure t :no-require t)
(use-package docker-tramp :defer t :ensure t :no-require t)

(use-package dockerfile-mode :ensure t :no-require t)
(require 'dockerfile-mode)

(defun dockerfile-get-docker-image-from-inbuffer ()
  "# iamge: DockerImageName"
  (interactive)
  (let ((image-name-line (save-excursion
		      (goto-char (point-min))
		      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (s-trim (car (cdr (s-split ":" image-name-line))))))


(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (ido-completing-read "Image name: "
		       dockerfile-image-name-history
		       nil nil nil nil
		       (dockerfile-get-docker-image-from-inbuffer)))


(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If prefix arg NO-CACHE is set, don't cache the image.
The build string will be of the format:
`sudo docker build --no-cache --tag IMAGE-NAME --build-args arg1.. -f filename directory`"

  (interactive (list (dockerfile-read-image-name) prefix-arg))
  (save-buffer)
    (compilation-start
        (format
            "%s%s build --progress plain %s %s %s -f %s %s"  ;; FIX
            (if dockerfile-use-sudo "sudo " "")
            dockerfile-mode-command
            (if no-cache "--no-cache" "")
            (dockerfile-tag-string image-name)
            (dockerfile-build-arg-string)
            (shell-quote-argument (dockerfile-standard-filename (buffer-file-name)))
            (shell-quote-argument (dockerfile-standard-filename default-directory)))
    nil
    (lambda (_) (format "*docker-build-output: %s *" image-name))))


(bind-key "C-c C-c" #'dockerfile-build-buffer 'dockerfile-mode-map)


;;; For LSP
(message "For LSP")
(use-package eglot :defer t :ensure t :no-require t
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark))

;;; For restclient
(message "For restclient")
(use-package restclient :defer t :ensure t :no-require t)

;;; For babel
(message "For babel")
(use-package ob-async :defer t :ensure t)
(require 'ob-async)
(require 'ob-plantuml)
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2019.1/libexec/plantuml.jar")

(use-package ob-restclient :defer t :ensure t :no-require t)
(use-package ob-async :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (scheme . t)
   (emacs-lisp . t)
   (python . t)
   (restclient . t)
   ;;(rustic . t)
   (shell . t)
   (sql . t)))

;;; Editorconfig
(message "Editorconfig")
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; For Rust
(message "For Rust")
(use-package rustic :defer t :ensure t :no-require t
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-rls-pkg 'eglot))

;;; For Python
(message "For Python")
(use-package pyvenv :ensure t :no-require t)

;; https://github.com/jorgenschaefer/pyvenv/blob/fa6a028349733b0ecb407c4cfb3a715b71931eec/pyvenv.el#L168-L184
(require 'pyvenv)
(setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs"))

(defun pyvenv-create (venv-name python-executable)
  "Create virtualenv.  VENV-NAME  PYTHON-EXECUTABLE."
  (interactive (list
                (read-from-minibuffer "Name of virtual environment: ")
                (read-file-name "Python interpreter to use: "
                                (file-name-directory (executable-find "python3.9"))
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

;;; For TypeScript
(use-package typescript-mode :defer t :ensure t :no-require t
  :config
  (custom-set-variables '(typescript-indent-level 2)))

;;; For React
(message "For React")
(use-package rjsx-mode :defer t :ensure t :no-require t
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil))

;;; Our Async Exec
(message "Our Async Exec")
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


;; ---------------
;; MONKEY PATCHING
;; ---------------

;; for simple.el
(message "for simple.el")

;; async-shell-commandで長い出力を表示する場合にEmacsが固まる問題を回避する
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
(message "Our Git Clone")
(defun our-git-config-entry (name email ssh-private-key-path)
  `((name . ,name)
    (email . ,email)
    (key . ,ssh-private-key-path)))

(defvar our-git-config nil)


(defun our-git-fetch-unshallow (repo label cwd name)
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
     (format "git -c core.sshCommand='ssh -i %s -F /dev/null' clone  --depth 1 %s %s"
	     (cdr (assoc 'key entry))
	     repo name)
     cwd)))


(defun our-git-submodule-add (repo label cwd name)
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
     (format "git -c core.sshCommand='ssh -i %s -F /dev/null' submodule add %s %s"
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

;;; Our open user task file
(defun our-open-user-task-file ()
  (interactive)
  (find-file "~/Dropbox/tasks/README.org"))
;;; Our open user task file Ends here.

(defun lang-install-rust ()
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

(use-package prettier-js :ensure t :defer t
  :init
  (add-hook 'yaml-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'html-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  )

;;; For flycheck
(message "For flycheck")
(defun configure-flycheck-yamlint ()
  (interactive)
  (flycheck-select-checker 'yaml-yamllint)
  (flycheck-mode))

(use-package flycheck :defer :ensure t :no-require t
  :init
  (add-hook 'yaml-mode-hook 'configure-flycheck-yamlint))

;;; For vue.js
(use-package add-node-modules-path :ensure t :defer t)
(require 'flycheck)
(use-package vue-mode :ensure t :defer t
  :init
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)
  :config
  (add-hook 'vue-mode-hook #'add-node-modules-path)
  (add-hook 'vue-mode-hook 'flycheck-mode))

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
 ("C-t C-h" . windmove-left)
 ("C-t C-j" . windmove-down)
 ("C-t C-k" . windmove-up)
 ("C-t C-l" . windmove-right)

 ("C-t C-h" . windmove-left)
 ("C-t C-j" . windmove-down)
 ("C-t C-k" . windmove-up)
 ("C-t C-l" . windmove-right)

 ;; Git
 ("C-x C-v" . magit-status)

 ;; My Customize
 ;; ("M-_" . our-async-exec-interactive)
 ("s-`" . our-async-exec-interactive)
 ("<C-ESC>" . our-async-exec-interactive)
 ("C-t C-c" . our-async-exec-interactive)

 ;; File open utility
 ("<f12>" . our-open-user-init-file)
 ("S-<f12>" . our-open-user-task-file)
 )

(use-package projectile :ensure t :defer t)
(load-file "/opt/ng/symdon/settings.el")

(use-package org
  :config
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  :bind (("<f9>" . 'org-set-effort)
	 ("<S-f10>" . 'org-clock-in)
	 ("<S-f11>" . 'org-clock-out)))

;; org-export
(custom-set-variables '(org-export-with-sub-superscripts nil))

;; org-agenda
;; (setq org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY     %100ITEM 100%TAGS")
(use-package company :ensure t :pin melpa
  :config
  (global-company-mode)
  (custom-set-variables
   '(company-idle-delay nil)
   '(company-tooltip-idle-delay nil))
  )

(defun our-buffer-copy-current-file-path ()
  "バッファのファイルパスをクリップボードにコピーする"
  (interactive)
  (let ((path (buffer-file-name)))
    (if path
	(progn
          (kill-new path)
          (message (format "Copied: %s" path)))
      (message (format "Cannot copied")))))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(bind-key* "C-t a" 'org-agenda)
(bind-key* "M-RET" 'find-file-at-point)
;; (bind-key* "C-t C-j" 'org-capture)

;; n https://github.com/tj/n
(setenv "N_PREFIX" (expand-file-name "~/.local"))
(put 'set-goal-column 'disabled nil)
(bind-key "C-[ C-[" 'mark-word)
(bind-key* "M-]" 'mark-word)

(bind-key "s-t" nil)  ;; command + t でfontの設定画面が開いてしまうが使わないので開かないように設定する.
(bind-key* "C-x C-w" 'ido-kill-buffer)

;; カレンダーの表示に星をつける
(setq calendar-month-header
      '(propertize (format " %s %d " (calendar-month-name month) year)
		   'font-lock-face 'calendar-month-header))

;; (use-package emoji-fontset
;;   :if window-system
;;   :init
;;   (emoji-fontset-enable "Symbola"))

;; org-agenda
(custom-set-variables
 '(org-agenda-span 1)
 '(org-todo-keywords '((sequence
			"INBOX" "MAYBE" "ACTION" "WAITING" "TODO" "EPIC"
			"|"
			"DONE" "CANCEL")))
 '(org-global-properties '(("Effort_ALL" . "1 2 3 5 8 13 21 34 55 89 144 233 377 610 987")))
 '(org-columns-default-format "%TODO %PRIORITY %Effort{:} %DEADLINE %ITEM %TAGS")
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-deadline-warning-days 0)  ;; 当日分のeffortを集計するためにdeadlineが今日でないものは除外する
 '(org-agenda-custom-commands
   '(("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
					; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (todo "ISSUE") ; review all projects (assuming you use todo keywords to designate projects)
       (todo "INBOX")
       (todo "MAYBE")
       (todo "ACTION")
       (todo "TODO")
       (todo "WAITING")
       (todo "DONE")
       (todo "CANCEL")))
     )))


(bind-key* "C-t t" #'org-clock-jump-to-current-clock)

(require 'org-agenda)
(bind-keys :map org-agenda-mode-map
	   ("C-c C-c" . org-agenda-todo)
	   ("C-c C-e" . org-agenda-set-effort)
	   ("C-c C-i" . org-agenda-clock-in)
	   ;; ("C-c C-o" . org-agenda-clock-out)
	   )

;; #+PROPERTY: Effort_ALL 1 2 3 5 8 13 21 34 55 89 144 233
;; #+STARTUP: indent hidestars inlineimages
;; #+TODO: TODO(t) ISSUE(i) EPIC(e) IDEA(i) BLOCK(b) SURVEY(s) PENDING(p) WIP(w) | DONE(d!) CANCEL(c!) DOC SPEC
;; #+COLUMNS: %40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM
;; (put 'narrow-to-region 'disabled nil)


;; setup golang
(message "Setup golang")
(use-package go-mode :ensure t :defer t)

(progn
  (use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred))

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; Optional - provides fancier overlays.
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  ;; Company mode is a standard completion package that works well with lsp-mode.
  (use-package company
    :ensure t
    :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))

  )

;; org-agendaの項目を開いたら作業時間を自動で計測する
(defun org-clock-in-interactive (&optional starting-p)
  (when (yes-or-no-p "Clock In?")
    (org-clock-in)))
;; (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree)
;; (add-hook 'org-agenda-after-show-hook #'org-clock-in-interactive)
;; (add-hook 'org-clock-out-hook #'org-agenda-list)
;; (add-hook 'org-clock-out-hook #'widen)


(remove-hook 'org-clock-out-hook #'org-agenda-list)
(remove-hook 'org-clock-out-hook #'widen)

(bind-key "C-c C-x C-t o" #'org-clock-out)
(bind-key "C-c C-x C-t b" #'org-clock-in-last)

(use-package unicode-escape :ensure t :defer t)  ;; for qiita
(use-package avy-menu :ensure t :defer t)  ;; for terraform
(use-package sudden-death :ensure t :defer t)
(use-package dired-filter :ensure t :defer t)
(use-package google-translate :ensure t :defer t)

;; github-review
(use-package github-review :defer t :ensure t)
(use-package request :defer t :ensure t)
(use-package fakir :defer t :ensure t)
(use-package web :defer t :ensure t)
(use-package db :defer t :ensure t)

;; (require 'el-get
;; (package-delete' el-get t)
;; (require 'cl-lib)

;; For el-get
(message "Require el-get start")
(add-to-list 'load-path "/opt/ng/el-get")
(custom-set-variables
 '(el-get-dir (expand-file-name "~/.el-get")))
(require 'el-get nil 'noerror)
(add-to-list 'el-get-recipe-path "/opt/ng/el-get/recipes")
;; (setq el-get-dir (expand-file-name "~/.el-get"))
(message "Require el-get done")

;; (add-to-list 'load-path "/usr/local/ng/el-get")

;; (add-to-list 'el-get-recipe-path "/usr/local/ng/el-get/recipes")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")


;; MyPackage
(message "My Pakcage")
(el-get-bundle dotenv-mode :url "git@github.com:collective-el/emacs-dotenv-mode.git" :type "git")
(require 'dotenv-mode)

(el-get-bundle gist:05de904cd0c320733cae:org-file-table :type "git")
(require 'org-file-table)

(el-get-bundle gist:e8a10244aac6308de1323d1f6685658b:change-case :type "git")
(require 'change-case)

(el-get-bundle gist:6961ff8bfb228a7601ed470598fad513:django.el :type "git")
(require 'django)

(el-get-bundle gist:c4c6ee198a1c576220a144ab825fa2f0:mastodon :type "git")
(require 'mastodon)
(with-current-buffer (find-file-noselect (expand-file-name "~/.config/mastodon/mstdn.jp"))
  (dotenv-mode-apply-all))

(el-get-bundle gist:73383aaf81656737fa533dd39dcb27a8:docker-compose-up-services :type "git")
(require 'docker-compose-up-services)

(el-get-bundle gist:beb8e1944af406c3fb4f74b6e0e3b5fe:require-to-install-executable :type "git")
(require 'require-to-install-executable)

(el-get-bundle elnode :type "git" :url "git@github.com:collective-el/elnode.git")
(require 'elnode)

(el-get-bundle foreman-mode :url "git@github.com:collective-el/foreman-mode.git" :type "git")
(require 'foreman-mode)

(el-get-bundle gist:d451221dc2a280b7e35d:kpt.el :type "git")
(require 'kpt)


(el-get-bundle gist:10985431:go-template-mode :type "git")
(require 'go-template-mode)


;; -------------
;; external tool
;; -------------
(require-to-install-executable "redis" "redis-cli" :darwin "brew install redis")
(require-to-install-executable "Chrome Driver" "chromedriver" :darwin "brew cask install chromedriver")
(require-to-install-executable "basictex" "basictex" :darwin "brew cask install basictex # M-x our-latex-update")
(require-to-install-executable "ghostscript" "ghostscript" :darwin "brew install ghostscript")
(require-to-install-executable "pandoc" "pandoc" :darwin "brew install pandoc")
(require-to-install-executable "plantuml" "plantuml" :darwin "brew install plantuml")


;; -----------
;; our-package
;; -----------
;; (require 'our)
;; (require 'our-brew)
;; (require 'our-cider)
;; (require 'our-circleci)
;; (require 'our-discord)
;; (require 'our-freewifi)
;; (require 'our-macos)
;; (require 'our-magit)
;; (require 'our-mastodon)
;; (require 'our-org)
;; (require 'our-pyvenv)
;; (require 'our-qiita)
;; (require 'our-simeji)
;; (require 'our-terraform)

;; (add-to-list 'our-org--target-dir-list "~/Dropbox/tasks")

;; (load-file "~/.emacs.d/env/discord.el")
;; (load-file "~/.emacs.d/env/mastodon.el")
;; (load-file "~/.emacs.d/env/cloudapp.el")


;; -------
;; clojure
;; -------
;; (unless (executable-find "java") (our-async-exec "brew cask install java"))
;; (unless (executable-find "clj") (our-async-exec "brew install clojure"))
;; (unless (executable-find "lein") (our-async-exec "brew install leiningen"))
(use-package rainbow-delimiters :ensure t :defer t)
(use-package paredit :ensure t :defer t
  :config
  (bind-keys :map paredit-mode-map
             ("C-h" . paredit-backward-delete)))
(use-package clojure-mode :ensure t :defer t)
(use-package clj-refactor :ensure t :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))
(use-package cider :ensure t :defer t
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

;; --------
;; wakatime
;; --------
(use-package wakatime-mode :ensure t :defer t
  :config
  (global-wakatime-mode))

(defun wakatime-open-dashboard ()
  (interactive)
  (browse-url "https://wakatime.com/dashboard"))

(defun wakatime-open-config ()
  (interactive)
  (find-file (expand-file-name "~/.wakatime.cfg")))


;; --------
;; org-mode
;; --------
;; (defun our-org-mode-setup ()
;;   (org-indent-mode)  ;; org-modeの表示をインデントモードにする
;;   (org-display-inline-images)  ;; 画像表示
;;   (setq org-src-fontify-natively t)

;;   (setq org-todo-keywords
;;       '((sequence
;;          "TODO(t)"
;; 	 "WIP(w)"
;; 	 "PENDING(e)"
;; 	 "REVIEW(r)"
;; 	 "PROPOSAL(P)"
;; 	 "PROBREM(p)"
;; 	 "QUESTION(q)"
;; 	 "RESEARCH(R)"
;; 	 "FEEDBACK(f)"
;; 	 "EPIC(g)"
;; 	 "|"
;;          "WHY(W)"
;;          "DONE(x)"
;; 	 "CANCEL(c)"
;; 	 "RESOLVED(o)"
;; 	 "KEEP(k)"
;; 	 "DOC(d)"
;; 	 "FAQ(f)"
;; 	 "SPEC(s)"
;; 	 "TIPS(t)")))

;;   (setq org-global-properties
;; 	(quote (("Effort_ALL" . "1 2 3 5 8 13 21 34 55 89")
;; 		("STYLE_ALL" . "habit")))))

;; (add-hook 'org-mode-hook 'our-org-mode-setup)


;; ---------
;; org-babel
;; ---------
;; (use-package ob-restclient :ensure t :defer t)
;; (use-package org-preview-html :ensure t :defer t)

;; (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2019.1/libexec/plantuml.jar")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (dot . t)
;;    (emacs-lisp . t)
;;    (plantuml . t)
;;    (restclient . t)
;;    (shell . t)
;;    (python . t)
;;    (sql . t)
;;    ))


;; ----------
;; kubernetes
;; ----------
;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview))



;; ----------
;; others
;; ----------

;; (defun our-latex-update ()
;;   (interactive)
;;   (our-async-exec
;;    (string-join '(;; パッケージのアップデート
;; 		  "sudo tlmgr update --self --all"
;; 		  ;; デフォルトで A4 用紙を使う
;; 		  "sudo tlmgr paper a4"
;; 		  ;; 日本語用パッケージ群のインストール
;; 		  "sudo tlmgr install collection-langjapanese"
;; 		  ;; 和文フォント ヒラギノのインストールと設定
;; 		  "sudo tlmgr repository add http://contrib.texlive.info/current tlcontrib"
;; 		  "sudo tlmgr pinning add tlcontrib '*'"
;; 		  "sudo tlmgr install japanese-otf-nonfree japanese-otf-uptex-nonfree ptex-fontmaps-macos cjk-gs-integrate-macos"
;; 		  "sudo cjk-gs-integrate --link-texmf --cleanup"
;; 		  "sudo cjk-gs-integrate-macos --link-texmf"
;; 		  "sudo mktexlsr"
;; 		  "sudo kanji-config-updmap-sys --jis2004 hiragino-highsierra-pron"
;; 		  ;; 日本語環境でソースコードの埋め込み
;; 		  ;; FIXME: ここではうまくjlisting.sty.bz2がダウンロード出来ていないのでコメントアウトするしかない
;; 		  ;; "curl https://ja.osdn.net/projects/mytexpert/downloads/26068/jlisting.sty.bz2/ | bzip2 -d "
;; 		  ;; "sudo mv ~/Downloads/jlisting.sty /usr/local/texlive/2018basic/texmf-dist/tex/latex/listings/"
;; 		  ;; "sudo chmod +r /usr/local/texlive/2018basic/texmf-dist/tex/latex/listings/jlisting.sty"
;; 		  ;; "sudo mktexlsr"
;; 		  )
;; 		" && ")))



;; -----
;; elenv
;; -----
;; (setq elenv-root-directory "/srv/")
;; (add-hook
;;  'elenv-initialize-package-after-hook
;;  (lambda ()
;;    (require 'use-package)

;;    (use-package powerline :ensure t :defer t)

;;    (use-package exec-path-from-shell :ensure t :defer t
;;      :init
;;      (when (memq window-system '(mac ns x))
;;        (exec-path-from-shell-initialize)))

;;    (use-package helm :ensure t :defer t
;;      :init
;;      (require 'helm-config)
;;      :config
;;      (helm-mode t)
;;      (dired-async-mode t)
;;      (setq helm-M-x-fuzzy-match t)
;;      (bind-keys :map helm-map
;; 		("<tab>" . helm-execute-persistent-action)
;; 		("C-i" . helm-execute-persistent-action)
;; 		("C-z" . helm-select-action)))
;;    (use-package helm-ag :ensure t :defer t
;;      :init
;;      (setq helm-ag-use-agignore t))
;;    (use-package elscreen :ensure t
;;      :init
;;      (setq elscreen-display-tab nil)
;;      (setq elscreen-tab-display-kill-screen nil)
;;      (setq elscreen-tab-display-control nil)
;;      (elscreen-start)
;;      (elscreen-create))
;;    (use-package magit :ensure t :defer t)

;;    (use-package async-await :ensure t :defer t)
;;    ;; (use-package json :ensure t :defer t)
;;    (use-package request :ensure t :defer t)
;;    (use-package async-await :ensure t :defer t)
;;    (use-package gist :ensure t :defer t)
;;    (use-package helm-themes :ensure t :defer t)
;;    (use-package http :ensure t :defer t)
;;    (use-package markdown-mode :ensure t)
;;    (use-package quickrun :ensure t :defer t)
;;    (use-package restclient :ensure t :defer t
;;      :config
;;      (add-to-list 'restclient-content-type-modes '("text/csv" . http-mode)))
;;    (use-package websocket :ensure t :defer t)
;;    (use-package yaml-mode :ensure t :defer t)
;;    (use-package dockerfile-mode :ensure t :defer t)
;;    (use-package company :ensure t :defer nil
;;      :init
;;      (setq company-idle-delay 0) ; default = 0.5
;;      (setq company-minimum-prefix-length 2) ; default = 4
;;      (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;;      :bind
;;      ("C-M-i" . company-complete)
;;      :config
;;      (global-company-mode 1)
;;      (bind-keys :map company-active-map
;; 		("C-n" . company-select-next)
;; 		("C-p" . company-select-previous)
;; 		("C-s" . company-filter-candidates)
;; 		("C-i" . company-complete-selection)
;; 		("C-M-i" . company-complete)))

;;    (use-package spacemacs-theme :ensure t :defer t
;;      :no-require t
;;      :init
;;      (load-theme 'tsdh-dark t))
;;    (use-package foreman-mode :ensure t :defer t)
;;    ))

(defun pip-requirements-user-install ()
  (interactive)
  (our-async-exec-interactive
   (format "pip3.9 install -U --user -r %s" (buffer-file-name))))

(use-package pip-requirements :ensure t :defer t
  :bind (:map pip-requirements-mode-map
	      ("C-c C-c" . pip-requirements-user-install)))

(setq-default indicate-empty-lines t)

;; ------
;; Editor
;; ------
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

(bind-key* "C-t C-w" 'editor-create-buffer)

;; -----------
;; macOS
;; -----------
(defun macos-app (&optional app buf)
  "Start macOS application from Emacs"
  (interactive (list (completing-read "Application: "
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

(use-package py-isort :ensure t)
(use-package blacken :ensure t)
;; (use-package promql-mode :ensure t :defer t)


;; FIXME: python-doctest flycheck checker is not fully completed.
;; for Python Mode
;; (flycheck-define-checker python-doctest
;;   "Python doctest flycheck checker"
;;   :command ("python" "-m" "doctest" source-inplace)
;;   :modes (python-mode)
;;   :enabled (lambda () t)
;;   :error-patterns ((error
;; 		    line-start (repeat 70 "\*") "\n"
;; 		    line-start "File " "\"" (file-name) "\", line " line ", in " (+ printing) "\n"
;; 		    (message (+ (not "*"))))))
;; (require 'python)
;; (defun python-mode-configure ()
;;   (bind-key "s-n" 'flycheck-next-error)
;;   (bind-key "s-p" 'flycheck-previous-error)
;;   (flymake-mode-off)
;;   (flycheck-mode)
;;   (flycheck-add-next-checker 'python-pycompile 'python-doctest)
;;   (flycheck-disable-checker 'python-pylint)
;;   (flycheck-disable-checker 'python-flake8)
;;   (flycheck-disable-checker 'python-mypy)
;;   (flycheck-select-checker 'python-pycompile)
;;   (flycheck-select-checker 'python-doctest)
;;   (flycheck-mode)
;;   )
;; (add-hook 'python-mode-hook 'python-mode-configure)

(setq debug-on-error t)
(use-package pcre2el :ensure t)
(defun scratch-buffer-create (buf-name)
  "Create new scratch buffer"
  (interactive "sBuffer Name: ")
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (lisp-interaction-mode))
    (switch-to-buffer buf)))


(require 'sql)
(require 'python)
(bind-keys :map python-mode-map
	   ("s-n" . flymake-goto-next-error)
	   ("s-p" . flymake-goto-prev-error)
           ("C-c C-c" . python-shell-send-buffer))


(bind-keys :map org-mode-map
	   ("<s-return>" . org-shiftright))

(bind-keys*
 ("C-t C-p" . projectile-switch-project)
 ("C-t C-o" . macos-app)
 )

(projectile-mode)

(custom-set-variables
 '(projectile-switch-project-action #'projectile-dired))


;; Symdon Shell Command
(defvar symdon-shell-command-line nil)

(define-derived-mode symdon-shell-mode fundamental-mode "Symdon SHELL"
  "Major mode for Symdon shell."
  )

(require 'term)

(defun symdon-shell-command (line &optional cwd)
  (interactive (list
		(read-string "Command: " "" 'our-async-exec-cmd-history "")
		(read-directory-name "Directory: " default-directory 'our-async-exec-cwd-history default-directory)))
  (let ((default-directory cwd))
    (switch-to-buffer
     (funcall #'term-ansi-make-term
	      (format "%s: In %s" (car (split-string line)) (expand-file-name cwd))
	      "bash" nil "-c" line))))


(defun symdon-shell-command-retry ()
  (interactive)
  (with-current-buffer (get-buffer-create "*SHELL*")
    (goto-char (point-max))
    (apply #'make-process `(:name "*SHELL*"
				  :buffer ,(current-buffer)
				  :command ,symdon-shell-command-line
				  :filter (lambda (proc output)
					    (with-current-buffer (process-buffer proc)
					      (let ((cur (point-min)))
						(insert output)
						(ansi-color-apply-on-region cur (point-max)))))))))

(bind-keys*
 ("C-t C-c" . symdon-shell-command))

(bind-keys :map symdon-shell-mode-map
	   ("C-c C-v" . symdon-shell-command-retry))


(defun goolge-build-query-words (&optional words)
  (when (> (length words) 0)
    (url-encode-url
     (string-join
      (--filter (not (string-empty-p it))
		(s-split " " words))
      "+"))))


(defun google (&optional words)
  (interactive "sSearch Word: ")
  ;; gvg=1 javascript off
  (xwidget-webkit-browse-url
   (format "https://google.com/?gbv=1&q=%s" (or (goolge-build-query-words words) ""))
   t))


(bind-keys*
 ("C-t C-g" . google))


;; for xwidget-webkit
(require 'xwidget)

(bind-keys :map xwidget-webkit-mode-map
	   ("M-w" . xwidget-webkit-copy-selection-as-kill)  ;; Emacs Style
	   ("s-c" . xwidget-webkit-copy-selection-as-kill)  ;; Other System Style
	   )


;; Mission
(defun mission-show ()
  (interactive)
  (with-current-buffer (get-buffer-create "*MISSION*")
    (insert
     (if mission-task-list
	 (car mission-task-list)
       (substring-no-properties org-clock-current-task)))
    (display-buffer (current-buffer))))

(defvar mission-task-list nil)

(defun mission-register (&optional name)
  (interactive "sMISSION: ")
  (setq mission-task-list (cons name mission-task-list)))

(defun mission-finish ()
  (interactive)
  (setq mission-task-list (cdr mission-task-list)))


(defun http-server-start (port)
  (interactive "nPort: ")
  (with-current-buffer (get-buffer-create "*HTTP Server*")
    (goto-char (point-max))
    (make-process :name "*HTTP Server*"
		  :buffer (current-buffer)
		  :command `("python3" ,(expand-file-name "~/.emacs.d/http_server.py") ,(number-to-string port))
		  :filter (lambda (proc output)
			    (with-current-buffer (process-buffer proc)
			      (let ((cur (point-min)))
				(insert output)
				(ansi-color-apply-on-region cur (point-max))))))))


(defvar proxy-buffer-name "*PROXY haproxy*")
(defvar proxy-process-name "*PROXY haproxy*")
(defvar proxy-default-directory "/ng/symdon")


(defun proxy-start ()
  (interactive)
  (with-current-buffer (get-buffer-create proxy-buffer-name)
    (goto-char (point-max))
    (let ((default-directory proxy-default-directory))
      (make-process :name proxy-process-name
		    :buffer (current-buffer)
		    :command '("docker-compose" "up")
		    :filter (lambda (proc output)
			      (with-current-buffer (process-buffer proc)
				(let ((cur (point-min)))
				  (insert output)
				  (ansi-color-apply-on-region cur (point-max)))))))))


(defun proxy-stop ()
  (interactive)
  (with-current-buffer (get-buffer proxy-buffer-name)
    (signal-process
     (get-buffer-process (current-buffer))
     1)))

(use-package slime :ensure t)
;; (use-package slime-company :ensure t
;; (setq inferior-lisp-program "sbcl")  ;; Need SBCL http://www.sbcl.org/

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
  :init
  (custom-set-variables
   '(yas-snippet-dirs '("/ng/symdon/snippets"))))

;; for shell-mode
(custom-set-variables
 '(explicit-shell-file-name "/usr/local/bin/bash"))


(defun org-todo-list-from-buffer (&optional arg buf)
  "Generate an agenda view from the selected buffer."
  (interactive "P\nbBuffer")
  (if-let ((org-agenda-files (or (buffer-file-name buf)
				 org-agenda-files)))
      (org-todo-list arg)
    (error "%s is not visiting a file" (buffer-name buf))))

(require 'python)
(require 'eglot)

(add-hook 'python-mode-hook 'eglot-ensure)

;; (global-display-line-numbers-mode)
(use-package terraform-mode :ensure t)

;; process-environment initialization
(setq process-environment-original (copy-alist process-environment))

(defun process-environment-init ()
  "Reset process-environment to initial state"
  (interactive)
  (setq process-environment (copy-alist process-environment-original)))

(add-hook 'projectile-before-switch-project-hook #'process-environment-init)
(add-hook 'projectile-after-switch-project-hook  #'configur-after-project-for-projectile)

(setq smtpmail-smtp-server "host.docker.internal")
(setq smtpmail-smtp-service 1025)

;; Mew
(use-package mew :ensure t)
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-smtp-server "host.docker.internal")
(setq mew-smtp-port 1025)

(setq send-mail-function #'smtpmail-send-it)

;; json-mode
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(message "Finished to initialize emacs")



;;; for compilation-mode ansi escaping
(add-hook 'term-mode-hook 'compilation-shell-minor-mode)

(require 'ansi-color)


(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(progn 
  (defun regexp-alternatives (regexps)
    "Return the alternation of a list of regexps."
    (mapconcat (lambda (regexp)
		 (concat "\\(?:" regexp "\\)"))
               regexps "\\|"))

  (defvar non-sgr-control-sequence-regexp nil
    "Regexp that matches non-SGR control sequences.")

  (setq non-sgr-control-sequence-regexp
	(regexp-alternatives
	 '(;; icon name escape sequences
           "\033\\][0-2];.*?\007"
           ;; non-SGR CSI escape sequences
           "\033\\[\\??[0-9;]*[^0-9;m]"
           ;; noop
           "\012\033\\[2K\033\\[1F"
           )))

  (defun filter-non-sgr-control-sequences-in-region (begin end)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
              non-sgr-control-sequence-regexp end t)
	(replace-match ""))))

  (defun filter-non-sgr-control-sequences-in-output (ignored)
    (let ((start-marker
           (or comint-last-output-start
               (point-min-marker)))
          (end-marker
           (process-mark
            (get-buffer-process (current-buffer)))))
      (filter-non-sgr-control-sequences-in-region
       start-marker
       end-marker)))

  (add-hook 'comint-output-filter-functions
            'filter-non-sgr-control-sequences-in-output)
  )
