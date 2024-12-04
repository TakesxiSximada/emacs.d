;; 標準のコマンドのキー割当変更。ここには基本的なキー割当の設定をする
(global-set-key (kbd "C-t") nil)      ; C-tをプレフィックスキーとして使うために潰す
(global-set-key (kbd "C-h") #'backward-delete-char-untabify)                          ; バックスペース
(global-set-key (kbd "C-x C-w") #'kill-buffer)                                        ; バッファ削除
(global-set-key (kbd "C-t C-h") #'windmove-left)                                      ; ウィンドウ切替
(global-set-key (kbd "C-t C-j") #'windmove-down)                                      ; ウィンドウ切替
(global-set-key (kbd "C-t C-k") #'windmove-up)                                        ; ウィンドウ切替
(global-set-key (kbd "C-t C-l") #'windmove-right)                                     ; ウィンドウ切替
(global-set-key (kbd "C-t h") #'windmove-left)                                        ; ウィンドウ切替
(global-set-key (kbd "C-t j") #'windmove-down)                                        ; ウィンドウ切替
(global-set-key (kbd "C-t k") #'windmove-up)                                          ; ウィンドウ切替
(global-set-key (kbd "C-t l") #'windmove-right)                                       ; ウィンドウ切替
(global-set-key (kbd "C-t l") #'windmove-right)                                       ; ウィンドウ切替
(global-set-key (kbd "<f1>") #'start-kbd-macro)                                       ; キーボードマクロ開始
(global-set-key (kbd "<f2>") #'end-kbd-macro)                                         ; キーボードマクロ終了
(global-set-key (kbd "<f3>") #'call-last-kbd-macro)                                   ; キーボードマクロ実行
(global-set-key (kbd "<f4>") #'insert-kbd-macro)                                      ; キーボードマクロ書出
(global-set-key (kbd "C-c C-w") #'comment-or-uncomment-region)                        ; コメントアウト
(global-set-key (kbd "C-t C-c") #'async-shell-command)                                ; コマンド実行
(global-set-key (kbd "M-SPC")  #'set-mark-command)                                    ; リージョン選択開始
(global-set-key (kbd "s-t") #'make-frame-on-current-monitor)                          ; ウィンドウ追加
(global-set-key (kbd "C-t C-t") #'other-frame)
(global-set-key (kbd "s-<up>")    (lambda () (interactive) (window-resize nil -1)))   ; ウィンドウサイズの変更
(global-set-key (kbd "s-<down>")  (lambda () (interactive) (window-resize nil 1)))    ; ウィンドウサイズの変更
(global-set-key (kbd "s-<right>") (lambda () (interactive) (window-resize nil 1 t)))  ; ウィンドウサイズの変更
(global-set-key (kbd "s-<left>")  (lambda () (interactive) (window-resize nil -1 t))) ; ウィンドウサイズの変更

;; XperiaではなぜかC-SPCを入力したと判定されるまでに時間がかかるようだっ
;; た。さらにC-SPCではなくC-@とし扱われていた。しかたがないのでM-SPCと
;; C-t C-pに#'set-mark-commandを割り当てる事にした。
;; いつか直したい。
(global-set-key (kbd "C-t C-p") #'set-mark-command)            ; リージョン選択開始

;; その他の基本的な設定
(defalias 'yes-or-no-p 'y-or-n-p) ; Yes/Noの省略入力
(global-visual-line-mode 0)
(put 'erase-buffer 'disabled nil)
(setenv "PAGER" "cat")            ; pagerでlessが使われないようにcatを指定しておく
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; diredの省略表示
(global-hl-line-mode t) ; 可視性の向上のためカーソル位置の行にアンダーラインを表示する

;; 個人用の基本的な環境用の変数設定
;; これらのディレクトリは、環境によって有ったり無かったりする。存在す
;; ればそれを使用し、存在しなければ使用しせず、それでもなんとか使える
;; 程度に設定され起動する事が望ましい。
(setq ng-path ; 個人用の設定のルートディレクトリ
      (expand-file-name (if (file-directory-p "/opt/ng") "/opt/ng" "~/ng"))
      ng-cache-dir ; キャッシュ用のディレクトリ
      (expand-file-name (if (file-directory-p "/var/ng") "/var/ng" "~/.cache/ng"))
      ng-custom-file ; 追加の設定置き場
      (expand-file-name (file-name-concat
                         ng-path "symdon" "emacs-custom"
                         (format "%s.el" system-configuration))))

;; Emacsの基本的な環境用の変数設定
(setq custom-file ; customizeの設定はここに保存される
      (if (file-exists-p ng-custom-file) ng-custom-file
        (progn (warn "No ng custom file: %s" ng-custom-file)
               (locate-user-emacs-file "custom.el")))
      custom-theme-directory ; テーマファイル置き場
      (expand-file-name "~/.emacs.d/themes"))

;; パッケージの設定
(setq package-user-dir ; インストールしたパッケージはここに保存される
      (expand-file-name (format "%s/elpa.%d" ng-cache-dir emacs-major-version))
      package-archives ; このリポジトリからパッケージを取得する
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ; ("melpa-stable" . "http://stable.melpa.org/packages/") ; stable
        ; ("cubelpa" . "https://sximada.github.io/cubelpa-repo/packages/") ; 個人用
        ; ("marmalade" . "http://marmalade-repo.org/packages/") ; marmaladeは保守されなくなった
        ))
(package-initialize) ; パッケージ情報を初期化する

(condition-case err (load-theme 'symdon-dark t t) (error err)) ; テーマの設定

;; どうしても必ず入れておきたいパッケージ
(progn ; SKK
  (unless (package-installed-p 'ddskk)
    (condition-case err (package-install 'ddskk) (error err)))
  (require 'ddskk-autoloads))

(progn ; smex
  (unless (package-installed-p 'smex)
    (condition-case err (package-install 'smex) (error err)))
  (require 'smex-autoloads))

(progn ; company
  (unless (package-installed-p 'company)
    (condition-case err (package-install 'company) (error err)))
  (require 'company-autoloads)
  (global-company-mode))

(progn ; Magit
  (unless (package-installed-p 'magit)
    (condition-case err (package-install 'magit) (error err)))
  (require 'magit-autoloads))

(progn ; IDO関連
  (require 'ido)
  (unless (package-installed-p 'ido-vertical-mode)
    (condition-case err (package-install 'ido-vertical-mode) (error err)))
  (require 'ido-vertical-mode-autoloads)

  (unless (package-installed-p 'ido-completing-read+)
    (condition-case err (package-install 'ido-completing-read+) (error err)))
  (require 'ido-completing-read+-autoloads)

  (defun configure-ido-keymap ()
    (define-key ido-completion-map (kbd "M-n") #'ido-next-match)
    (define-key ido-completion-map (kbd "M-p") #'ido-prev-match))

  (condition-case err
      (progn (setq ido-enable-flex-matching t
                   ido-default-file-method 'selected-window
                   ido-default-buffer-method 'selected-window)
             (ido-mode 1)
             (ido-everywhere 1)
             (ido-vertical-mode)
             (ido-ubiquitous-mode 1)
             (add-hook 'ido-setup-hook #'configure-ido-keymap))
    (error err)))

(progn ; org
  (condition-case err (require 'org) (error err))
  (condition-case err (require 'org-habit) (error err)))

(progn ; VTerm
  (unless (package-installed-p 'vterm)
    (condition-case err (package-install 'vterm) (error err)))

  (require 'vterm-autoloads)

  (defun vterm-command (line &optional cwd)
    (interactive (list (read-string "Command: " "" nil "")
                       (read-directory-name "Directory: "
                                            default-directory nil
                                            default-directory)))
    (let ((default-directory cwd)
          (vterm-shell line)
          (vterm-buffer-name (format "%s %s: In %s"
                                     (car (split-string line))
                                     (or (car (cdr (split-string line))) "")
                                     (expand-file-name cwd)))
          (vterm-kill-buffer-on-exit nil))
      (vterm)))

  (setq vterm-environment '("LANG=ja_JP.UTF-8")))

;; 追加パッケージに関するキー割当
(global-set-key (kbd "M-x") #'smex)                     ; M-x補助
(global-set-key (kbd "M-X") #'smex-major-mode-commands) ; M-x補助
(global-set-key (kbd "C-x C-j") #'skk-mode)             ; SKK切替
(global-set-key (kbd "C-x C-v") #'magit-status)         ; Git状態表示
(global-set-key (kbd "C-t C-c") #'vterm-command)
(global-set-key (kbd "C-M-i") #'company-complete)

(condition-case err
    (progn (require 'vterm)
           (define-key vterm-mode-map (kbd "C-t") nil)
           (define-key vterm-mode-map (kbd "C-c C-v") 'vterm-copy-mode))
  (error err))

;; その他の設定
(custom-set-default 'system-time-locale "C") ; org-scheduleで挿入される曜日を英語表記にする。 参考 :: https://qiita.com/tnoda_/items/9fefa1575f3bd5273b64

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; safe-local-variable-valuesを保存してしまうと、custom-fileファイルを
;; Gitに登録できなくなってしまうため、safe-local-variable-valuesは
;; custom-fileに反映しないように設定する。
(setq-default enable-local-variables :all)

;; カスタムファイルのロード
(when custom-file (condition-case err (load-file custom-file) (error err)))
(print "Okay")
