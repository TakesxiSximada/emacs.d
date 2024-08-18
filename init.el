(message "Loading ~/.emacs.d/init.el")
;; C-tをプレフィックスキーとして使うために潰しておく
(global-set-key (kbd "C-t") nil)

;; XperiaではなぜかC-SPCを入力したと判定されるまでに時間がかかるようだっ
;; た。さらにC-SPCではなくC-@とし扱われていた。しかたがないのでM-SPCと
;; C-t C-pに#'set-mark-commandを割り当てる事にした。
;; いつか直したい。
(global-set-key (kbd "M-SPC")  #'set-mark-command)
(global-set-key (kbd "C-t C-p") #'set-mark-command)

;; その他のキーバインドの設定
(global-set-key (kbd "C-h") #'backward-delete-char-untabify)
(global-set-key (kbd "C-x C-w") #'kill-buffer)

;; Window切り替え
(global-set-key (kbd "C-t C-h") #'windmove-left)
(global-set-key (kbd "C-t C-j") #'windmove-down)
(global-set-key (kbd "C-t C-k") #'windmove-up)
(global-set-key (kbd "C-t C-l") #'windmove-right)
(global-set-key (kbd "C-t h") #'windmove-left)
(global-set-key (kbd "C-t j") #'windmove-down)
(global-set-key (kbd "C-t k") #'windmove-up)
(global-set-key (kbd "C-t l") #'windmove-right)
(global-set-key (kbd "C-t l") #'windmove-right)

;; キーボードマクロ
(global-set-key (kbd "<f1>") #'start-kbd-macro)
(global-set-key (kbd "<f2>") #'end-kbd-macro)
(global-set-key (kbd "<f3>") #'call-last-kbd-macro)
(global-set-key (kbd "<f4>") #'insert-kbd-macro)

;; コメントアウト
(global-set-key (kbd "C-c C-w") #'comment-or-uncomment-region)

;; DDSKK
(global-set-key (kbd "C-x C-j") #'skk-mode)

;; Magit
(global-set-key (kbd "C-x C-v") #'magit-status)

;; NG: 個人用の設定ファイルなど
;;
;; これらのディレクトリは、環境によって有ったり無かったりする。存在す
;; ればそれを使用し、存在しなければ使用しせず、それでもなんとか使える
;; 程度に設定され起動する事が望ましい。
(setq ng-path (expand-file-name (if (file-directory-p "/opt/ng") "/opt/ng" "~/ng"))
      ng-cache-dir (expand-file-name (if (file-directory-p "/var/ng") "/var/ng" "~/cache/ng"))
      ng-custom-file (expand-file-name (file-name-concat ng-path
							 "symdon"
							 "emacs-custom"
							 (format "%s.el" system-configuration)))
      custom-file (if (file-exists-p ng-custom-file) ng-custom-file
		    (progn (warn "No ng custom file: %s" ng-custom-file)
			   (locate-user-emacs-file "custom.el")))
      custom-theme-directory (expand-file-name "~/.emacs.d/themes"))

;; package
(setq package-user-dir (expand-file-name (format "%s/elpa.%d" ng-cache-dir emacs-major-version))
      package-archives '(("gnu" .
			  "https://elpa.gnu.org/packages/")
			 ("org" .
			  "https://orgmode.org/elpa/")
			 ("melpa" .
			  "https://melpa.org/packages/")
  			 ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ;; 個人用リポジトリ
			 ;; ("cubelpa" . "https://sximada.github.io/cubelpa-repo/packages/")
  			 ;; marmaladeは保守されなくなった
  			 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
(package-initialize)

;; DDSKKの定設
(require 'ddskk-autoloads)

;; 基本的なパスは使えないと不便なので、あらかじめ設定しておく
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (string-join exec-path ":"))

;; カスタムファイルのロード
(when custom-file
  (condition-case err (load-file custom-file) (error err)))

;; vterm
(with-eval-after-load 'vterm
  ;; (setq vterm-environment '("LANG=ja_JP.UTF-8"))
  (global-set-key (kbd "C-t C-c") #'vterm-command)
  (define-key vterm-mode-map (kbd "C-t") nil)
  (define-key vterm-mode-map (kbd "C-c C-v") 'vterm-copy-mode)
  ;; (bind-key* "C-t C-c" #'vterm-command)

  (defun vterm-command (line &optional cwd)
    (interactive (list
		  (read-string "Command: " "" nil "")
		  (read-directory-name "Directory: " default-directory nil default-directory)))
    (let ((default-directory cwd)
	  (vterm-shell line)
	  (vterm-buffer-name (format "%s %s: In %s"
				     (car (split-string line))
				     (or (car (cdr (split-string line))) "")
				     (expand-file-name cwd)))
	  (vterm-kill-buffer-on-exit nil))
      (vterm))))

;; org-mode
(defun our-org-todo (&optional todo)
  (interactive
   (list (completing-read "Status: " (cdr (car org-todo-keywords)))))
  (org-todo todo))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") #'our-org-todo)
  (define-key org-mode-map (kbd "C-c C-,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "C-c ,") #'org-insert-structure-template)

  ;; org-clock
  (define-key org-mode-map (kbd "M-i") #'org-clock-in)
  (define-key org-mode-map (kbd "M-o") #'org-clock-out)
  )

;; org-agenda
(defun our-org-agenda-todo (&optional todo)
  (interactive
   (list (completing-read "Status: " (cdr (car org-todo-keywords)))))
  (org-agenda-todo todo))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M-p") #'org-agenda-priority-up)
  (define-key org-agenda-mode-map (kbd "M-n") #'our-org-agenda-todo)

  (define-key org-agenda-mode-map (kbd "M-i") #'org-agenda-clock-in)
  (define-key org-agenda-mode-map (kbd "M-o") #'org-agenda-clock-out))

(global-set-key (kbd "C-t C-q") #'org-agenda-list)

;; deepl
(global-set-key (kbd "C-t C-r") #'deepl)
(global-set-key (kbd "C-t C-e") #'openai-chat-question)

;; IDO
(require 'ido)

(defun configure-ido-keymap ()
  (define-key ido-completion-map (kbd "M-n") #'ido-next-match)
  (define-key ido-completion-map (kbd "M-p") #'ido-prev-match))

(condition-case err
    (progn  ;; IDO configuration
      (ido-mode 1)
      (ido-everywhere 1)
      (add-hook 'ido-setup-hook #'configure-ido-keymap)
      (setq ido-enable-flex-matching t)
      (setq ido-default-file-method 'selected-window)
      (setq ido-default-buffer-method 'selected-window)

      (require 'ido-vertical-mode-autoloads)
      (require 'ido-completing-read+-autoloads)

      (ido-vertical-mode)
      (ido-ubiquitous-mode 1))

  (error err))

;; company
(condition-case err
    (progn
      (require 'company-autoloads)
      (global-company-mode))
  (error err))

;; OpenAI
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1708258091" ng-path))
(condition-case err (require 'openai-autoloads) (error err))
(global-set-key (kbd "C-t C-e") #'openai-chat-question)

;; DeepL
(add-to-list 'load-path (expand-file-name "symdon/pages/posts/1708668537" ng-path))
(condition-case err (require 'deepl-autoloads) (error err))
(global-set-key (kbd "C-t C-r") #'deepl)

;; essay.el
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1602410851" ng-path))
(condition-case err (require 'essay-autoloads) (error err))
(global-set-key (kbd "C-t C-w") #'essay)

;; misc
(global-set-key (kbd "C-t C-c") #'async-shell-command)

;; which-command.el
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1671331641" ng-path))
(condition-case err (require 'which-command-autoloads) (error err))

;; asciidoc.el
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1711183139" ng-path))
(condition-case err (require 'asciidoc-autoloads) (error err))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . asciidoc-mode))

;; smex
(condition-case err
    (progn
      (global-set-key (kbd "M-x") #'smex)
      (global-set-key (kbd "M-X") #'smex-major-mode-commands))
  (error err))

;; custom command
(defun our-buffer-copy-current-file-path ()
  "バッファのファイルパスをクリップボードにコピーする"
  (interactive)
  (let ((path (buffer-file-name)))
    (if path
  	(progn
         (kill-new path)
         (message (format "Copied: %s" path)))
      (message (format "Cannot copied")))))

;; macos.el
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1710409633" ng-path))
(condition-case err (require 'macos-autoloads) (error err))
(global-set-key (kbd "C-t C-o") #'macos-app)
(with-eval-after-load 'macos
  (macos-app-refresh))

;; misc
(defalias 'yes-or-no-p 'y-or-n-p)

(condition-case err (load-file (expand-file-name "~/.emacs.d/vault.el")) (error err))
;; (load-file custom-file)  ;; customファイルを読み込む
(global-visual-line-mode 1)

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; SSH Port Forward
(defun port-forward-local (ssh-command remote-host remote-port local-port)
  (interactive
   (list (completing-read "SSH Command: " nil)
	 (completing-read "Remote Host: " nil)
	 (completing-read "Remote Port: " nil)
	 (completing-read "Local Port: " nil)))

  (let ((shell-command-buffer-name-async "*Port Forward*"))
    (async-shell-command
     (format "ssh -L %s:%s:%s %s"
	     local-port
	     remote-host
	     remote-port
	     ssh-command
	     ))))

;; org-agenda custom
(defcustom org-agenda-default-directory-on-agenda-view
  nil
  "Specify default directory in org-agenda agenda view.

MOTIVATION: When in the agenda view of org-agenda, I want to execute magit-status and immediately perform Git operations. For this to work, the default directory when in the agenda view needs to be the directory where the Git repository I intend to operate on is located.
")

(defun org-agenda-move-to-custom-default-directory ()
  (when org-agenda-default-directory-on-agenda-view
    (setq default-directory (expand-file-name org-agenda-default-directory-on-agenda-view))))

(add-hook 'org-agenda-mode-hook #'org-agenda-move-to-custom-default-directory)

;; Environment variables configuration
(setenv "PAGER" "cat")
(setenv "PATH" (string-join exec-path ":"))
(setenv "WORKON_HOME" (file-name-concat ng-cache-dir "python-venv"))
(put 'erase-buffer 'disabled nil)
(global-visual-line-mode 0)
(message "Loaded ~/.emacs.d/init.el")
