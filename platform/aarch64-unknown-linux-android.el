(setq ;; Env
 debug-on-error nil
 ng-path (if (file-directory-p "/opt/ng") "/opt/ng" "~/ng")
 ng-cache-dir (if (file-directory-p "/var/ng") "/var/ng" "~/cache/ng")
 custom-file (expand-file-name "~/.emacs.d/custom-android.el"))

(setq  ;; Base
 ;; packageを保持するディレクトリを指定する
 package-user-dir (expand-file-name (format "%s/elpa.%d" ng-cache-dir emacs-major-version))

 ;; package repositoryの更新
 package-archives '(("gnu" .
		     "https://elpa.gnu.org/packages/")
		    ("org" .
		     "https://orgmode.org/elpa/")
		    ("melpa" .
		     "https://melpa.org/packages/")))

(load-file custom-file)  ;; customファイルを読み込む
(load-theme 'wombat)
(package-initialize)

;; misc
(setenv "PAGER" "cat")

;; DDSKKの定設
(require 'ddskk-autoloads)
(defun skk-find-window-system () t)
(global-set-key (kbd "C-x C-j") #'skk-mode)

;; その他のキーバインドの設定
(global-set-key (kbd "C-t") nil)
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

;; コメントアウト
(global-set-key (kbd "C-c C-w") #'comment-or-uncomment-region)

;; Magit
(global-set-key (kbd "C-x C-v") #'magit-status)

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

;; ssh-agent
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1713099096" ng-path))
(condition-case err (require 'ssh-autoloads) (error err))

;; macos.el
(add-to-list 'load-path (expand-file-name "symdon/pages/emacs/1710409633" ng-path))
(condition-case err (require 'macos-autoloads) (error err))
(global-set-key (kbd "C-t C-o") #'macos-app)
(with-eval-after-load 'macos
  (macos-app-refresh))

;; misc
(defalias 'yes-or-no-p 'y-or-n-p)

(condition-case err (load-file (expand-file-name "~/.emacs.d/vault.el")) (error err))
(load-file custom-file)  ;; customファイルを読み込む
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
