;; ----------------
;; toolbarの設定
;; ----------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;------------------------
;; fonts
;;------------------------
(set-face-attribute 'default nil :family "Menlo" :height 120)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))

(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; --------------
;; 起動画面の設定
;; --------------
(setq initial-buffer-choice
      (lambda ()
	(switch-to-buffer "*Messages*")))


(load-file "~/.emacs.d/env.el")

;; --------
;; 環境変数
;; --------

(setenv "LDFLAGS" "-L/usr/local/opt/libffi/lib -L/usr/local/opt/texinfo/lib -L/usr/local/opt/libxml2/lib -L/usr/local/opt/texinfo/lib -L/usr/local/opt/mysql@5.7/lib -L/usr/local/opt/openssl/lib")

(setenv "CPPFLAGS" "-I/usr/local/opt/libxml2/include -I/usr/local/opt/mysql@5.7/include -I/usr/local/opt/openssl/include")

(setenv "PKG_CONFIG_PATH" "/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/libxml2/lib/pkgconfig:/usr/local/opt/mysql@5.7/lib/pkgconfig:/usr/local/opt/openssl/lib/pkgconfig")


;; ------------
;; Yes/Noの設定
;; ------------
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------
;; our-async-exec
;; --------------
(defvar our-async-exec-cmd-history nil)
(defvar our-async-exec-cwd-history nil)

(defun our-create-buffer-name (cmd &optional cwd)
  (format "`%s`%s"
	  (let ((elms (split-string cmd)))
	    (mapconcat 'identity
		       (append (last (split-string (car elms) "/"))
			       (cdr elms))
		       " "))
	  (if (and cwd (> (length cwd) 0)) (format ": %s" cwd) "")))


(defun our-async-exec (cmd &optional cwd buffer)
  (let ((default-directory (or cwd default-directory)))
    (async-shell-command cmd (or buffer (our-create-buffer-name cmd cwd)))))


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


(defun our-get-buffer-create (&optional name)
  (interactive "sBuffer Name: ")
  (let ((buf-name (format "*%s*" name)))
    (get-buffer-create buf-name)
    (message (format "Created a buffer: %s" buf-name))))

;; -----
;; elenv
;; -----
(setq elenv-root-directory "/srv/")
(add-hook
 'elenv-initialize-package-after-hook
 (lambda ()
   (require 'use-package)

   (use-package powerline :ensure t :defer t)

   (use-package exec-path-from-shell :ensure t :defer t
     :init
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

   (use-package helm :ensure t :defer t
     :init
     (require 'helm-config)
     :config
     (helm-mode t)
     (dired-async-mode t)
     (setq helm-M-x-fuzzy-match t)
     (bind-keys :map helm-map
		("<tab>" . helm-execute-persistent-action)
		("C-i" . helm-execute-persistent-action)
		("C-z" . helm-select-action)))
   (use-package helm-ag :ensure t :defer t
     :init
     (setq helm-ag-use-agignore t))
   (use-package elscreen :ensure t
     :init
     (setq elscreen-display-tab nil)
     (setq elscreen-tab-display-kill-screen nil)
     (setq elscreen-tab-display-control nil)
     (elscreen-start)
     (elscreen-create))
   (use-package magit :ensure t :defer t)

   (use-package async-await :ensure t :defer t)
   (use-package json :ensure t :defer t)
   (use-package request :ensure t :defer t)
   (use-package async-await :ensure t :defer t)
   (use-package gist :ensure t :defer t)
   (use-package helm-themes :ensure t :defer t)
   (use-package http :ensure t :defer t)
   (use-package markdown-mode :ensure t)
   (use-package quickrun :ensure t :defer t)
   (use-package restclient :ensure t :defer t
     :config
     (add-to-list 'restclient-content-type-modes '("text/csv" . http-mode)))
   (use-package websocket :ensure t :defer t)
   (use-package yaml-mode :ensure t :defer t)
   (use-package dockerfile-mode :ensure t :defer t)
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

   (use-package spacemacs-theme :ensure t :defer t
     :no-require t
     :init
     (load-theme 'tsdh-dark t))
   ))

(progn (add-to-list 'load-path "/srv/sallies/elenv/") (require 'elenv) (elenv-activate))  ;; elenv auto inser
(toggle-frame-fullscreen)


;; -----------
;; our-package
;; -----------
(add-to-list 'load-path "/srv/sallies/nvm.el/")
(add-to-list 'load-path "/srv/sallies/our.el/")

(require 'our)
(require 'our-circleci)
(require 'our-brew)
(require 'our-org)
(require 'our-pyvenv)
(require 'our-magit)

(add-to-list 'our-org--target-dir-list "~/Dropbox/tasks")


;; --------
;; org-mode
;; --------
(defun our-org-mode-setup ()
  (org-indent-mode)  ;; org-modeの表示をインデントモードにする
  (org-display-inline-images)  ;; 画像表示
  (setq org-src-fontify-natively t)

  (setq org-todo-keywords
      '((sequence
         "TODO(t)"
	 "WIP(w)"
	 "PENDING(e)"
	 "REVIEW(r)"
	 "PROPOSAL(P)"
	 "PROBREM(p)"
	 "QUESTION(q)"
	 "RESEARCH(R)"
	 "FEEDBACK(f)"
	 "EPIC(g)"
	 "|"
         "WHY(W)"
         "DONE(x)"
	 "CANCEL(c)"
	 "RESOLVED(o)"
	 "KEEP(k)"
	 "DOC(d)"
	 "FAQ(f)"
	 "SPEC(s)"
	 "TIPS(t)")))

  (setq org-global-properties
	(quote (("Effort_ALL" . "1 2 3 5 8 13 21 34 55 89")
		("STYLE_ALL" . "habit")))))

(add-hook 'org-mode-hook 'our-org-mode-setup)

;; ----------
;; keybinding
;; ----------
(require 'simple)
(require 'macros)
(require 'newcomment)
(require 'windmove)
;; (require 'window)

(require 'elscreen)
(require 'helm-ag)
(require 'magit)

(bind-keys* ("¥" . "\\")
	    ("C-h" . backward-delete-char-untabify)
	    ("C-x g" . helm-do-ag)
	    ("C-x C-g" . goto-line)
	    ("C-x C-p" . list-processes)
	    ("C-c C-w" . comment-or-uncomment-region)

            ;; keyboard macro
            ("<f1>" . start-kbd-macro)
            ("<f2>" . end-kbd-macro)
            ("<f3>" . call-last-kbd-macro)
            ("<f4>" . name-last-kbd-macro)
            ("<f5>" . insert-kbd-macro)

	    ;; buffers
	    ("C-<backspace>" . kill-buffer)

	    ;; panes and screen
	    ("C-t C-t" . elscreen-previous)
	    ("C-t h" . windmove-left)
	    ("C-t j" . windmove-down)
	    ("C-t k" . windmove-up)
	    ("C-t l" . windmove-right)

            ;; panes size
            ("s-<left>" . shrink-window-horizontally)
            ("s-<down>" . enlarge-window)
            ("s-<up>" . shrink-window)
            ("s-<right>" . enlarge-window-horizontally)

	    ;; command
            ("M-_" . our-async-exec-interactive)
            ("C-M-_" . async-shell-command)
	    ;; other
	    ("s-t" . (lambda () (interactive) (message "Oops!")))
            ("<f9>" . google-this)
	    ("<f12>" . elenv-switch-user-init-file)
	    ("<C-f12>" . our-org-open))


;; buffer
(our-bind-key "C-x C-b" 'helm-mini)
(our-bind-key "C-x b" 'helm-buffers-list)
(our-bind-key "C-x C-f" 'helm-find-files)
(our-bind-key "M-x" 'helm-M-x)

;; git
(our-bind-key "C-x C-v" 'magit-status)

;; elscreen
;; (bind-key "C-t C-t" 'elscreen-previousch)
;; (our-bind-key "C-t C-t" 'elscreen-previous)
;; (our-bind-key "C-t C-n" 'elscreen-next)
;; (our-bind-key "C-t C-p" 'elscreen-previous)
;; (our-bind-key "C-t C-l" 'helm-elscreen)
;; (our-bind-key "C-t C-w" 'elscreen-kill)
