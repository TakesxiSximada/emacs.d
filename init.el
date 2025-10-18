;;; init.el --- Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 - 2025 TakesxiSximada

;; Author: TakesxiSximada
;; URL: https://github.com/TakesxiSximada/emacs.d

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; My Emacs configuration

;;; Start up view
(defvar fancy-startup-tail-original (symbol-function 'fancy-startup-tail)
  "起動画面のオリジナルの処理を変数セルに退避")
(defvar fancy-startup-text-original fancy-startup-text
  "起動画面のオリジナルの処理を変数セルに退避")

;; 起動画面をカスタマイズ
(setf (symbol-function 'fancy-startup-tail) (lambda (&optional concise) nil))
(setq fancy-startup-text nil)

;; 起動画面のオリジナルの処理を呼び出せるように関数セルに入れておく
(setf (symbol-function 'fancy-startup-tail-original) fancy-startup-tail-original)

;; Library Path
(add-to-list 'load-path (expand-file-name "~/ng/symdon/my"))
(add-to-list 'load-path (expand-file-name "~/ng/symdon/elisp"))

;;; Code:
(customize-set-variable 'custom-theme-directory
			(expand-file-name "~/.emacs.d/themes")
			"My theme files")
(load-theme 'symdon-surface t)

(set-face-attribute 'default nil :height 150)
(set-frame-parameter nil 'alpha '(100 . 100))
(set-cursor-color "red")

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
(global-set-key (kbd "<f1>") #'start-kbd-macro)                                       ; キーボードマクロ開始
(global-set-key (kbd "<f2>") #'end-kbd-macro)                                         ; キーボードマクロ終了
(global-set-key (kbd "<f3>") #'call-last-kbd-macro)                                   ; キーボードマクロ実行
(global-set-key (kbd "<f4>") #'insert-kbd-macro)                                      ; キーボードマクロ書出
(global-set-key (kbd "C-c C-w") #'comment-or-uncomment-region)                        ; コメントアウト
(global-set-key (kbd "C-t C-c") #'async-shell-command)                                ; コマンド実行
(global-set-key (kbd "M-SPC")  #'set-mark-command)                                    ; リージョン選択開始
(global-set-key (kbd "s-t") #'make-frame-on-current-monitor)                          ; ウィンドウ追加
(global-set-key (kbd "C-t C-t") #'other-frame)                                        ; フレームの移動
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
(global-visual-line-mode 0)  ; 行の表示を行わない
(put 'erase-buffer 'disabled nil)
(setenv "PAGER" "cat")            ; pagerでlessが使われないようにcatを指定しておく
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; diredの省略表示
;; (global-hl-line-mode t)  ; 可視性の向上のためカーソル位置の行にアンダーラインを表示する

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
(customize-set-variable 'custom-file
			(if (file-exists-p ng-custom-file) ng-custom-file
			  (progn (warn "No ng custom file: %s" ng-custom-file)
				 (locate-user-emacs-file "custom.el")))
			"customizeの設定はここに保存される")

;; パッケージの設定
(customize-set-variable 'package-user-dir
			(expand-file-name
			 (format "%s/elpa.%d"
				 ng-cache-dir
				 emacs-major-version))
			"パッケージのインストール先ディレクトリ

異なるバージョンのEmacsを使用できるよう、パッケージのインストール先は
Emacsのバージョン毎に分かれるようにする。
")

(customize-set-variable 'package-archives
			'(("gnu" . "https://elpa.gnu.org/packages/")
			  ("org" . "https://orgmode.org/elpa/")
			  ("melpa" . "https://melpa.org/packages/"))
			"パッケージを取得するリポジトリ

過去に使用していたリポジトリ

- melpa-stable :: MELPAの安定版
  http://stable.melpa.org/packages/

- cubelpa :: 個人用
  https://sximada.github.io/cubelpa-repo/packages/

- marmalade :: marmaladeは保守されなくなった
  http://marmalade-repo.org/packages/
")

(package-initialize)

(condition-case err (load-theme 'symdon-surface t t) (error err)) ; テーマの設定

;; どうしても必ず入れておきたいパッケージ
(progn ; SKK
  (unless (package-installed-p 'ddskk)
    (condition-case err (package-install 'ddskk) (error err)))
  (require 'ddskk-autoloads))

(progn ; smex (disabled: migrated to Vertico/Consult)
  (unless (package-installed-p 'smex)
    (condition-case err (package-install 'smex) (error err)))
  (require 'smex-autoloads))

;; company
(add-to-list 'load-path (expand-file-name "company-mode" ng-path))
(progn ; company
  (unless (package-installed-p 'company)
    (condition-case err (package-install 'company) (error err)))
  (require 'company-autoloads)
  (global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  )

(progn ; Magit
  (unless (package-installed-p 'magit)
    (condition-case err (package-install 'magit) (error err)))
  (require 'magit-autoloads))

(progn ; IDO関連 (disabled: migrated to Vertico)
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

;; ;; Vertico + Orderless + Marginalia + Consult
;; (progn
;;   (dolist (pkg '(vertico orderless marginalia consult))
;;     (unless (package-installed-p pkg)
;;       (condition-case err (package-install pkg) (error err))))

;;   (condition-case err
;;       (progn
;;         ;; Completion behavior
;;         (setq completion-styles '(orderless basic)
;;               completion-category-defaults nil
;;               completion-category-overrides '((file (styles . (partial-completion)))))
;;         ;; Enable UIs
;;         (require 'vertico)
;;         (vertico-mode 1)
;;         (require 'marginalia)
;;         (marginalia-mode 1)
;;         (require 'consult))
;;     (error (message "Failed to setup Vertico stack: %s" err))))

(progn ; VTerm
  (unless (package-installed-p 'vterm)
    (condition-case err (package-install 'vterm) (error err)))

  (require 'vterm-autoloads)

  (defun our-async-shell-command (line &optional cwd)
    (interactive (list (progn
			 (when (use-region-p) (clipboard-kill-ring-save (region-beginning) (region-end)))
			 (read-shell-command "SHELL$ "))
                       (read-directory-name "DIRECTORY: "
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
(global-set-key (kbd "M-x") #'smex)                        ; M-x補助
(global-set-key (kbd "M-X") #'smex-major-mode-commands)    ; M-x補助
;; 追加パッケージに関するキー割当（安全なフォールバック付き）
;; M-x: smex が利用可能なら使用し、なければ標準コマンドにフォールバック
;; (global-set-key
;;  (kbd "M-x")
;;  (cond
;;   ((fboundp 'smex) #'smex)
;;   (t #'execute-extended-command)))

;; M-X: メジャーモード限定コマンド。smex があれば smex を、なければ Emacs 28+ の
;; execute-extended-command-for-buffer があればそれを、最後の手段として通常の M-x
;; (global-set-key
;;  (kbd "M-X")
;;  (cond
;;   ((fboundp 'smex-major-mode-commands) #'smex-major-mode-commands)
;;   ((fboundp 'execute-extended-command-for-buffer) #'execute-extended-command-for-buffer)
;;   (t #'execute-extended-command)))

;; 追加パッケージに関するキー割当（Vertico/Consult）
;; (global-set-key (kbd "M-x") #'consult-M-x)                 ; M-x補助
;; (global-set-key (kbd "M-X") #'consult-mode-command)        ; メジャーモードコマンド
(global-set-key (kbd "C-x C-j") #'skk-mode)                ; SKK切替
(global-set-key (kbd "C-x C-v") #'magit-status)            ; Git状態表示
(global-set-key (kbd "C-t C-c") #'our-async-shell-command) ; Shell実行
;; 検索系: consult-line があれば利用、無ければ isearch-forward
;; (global-set-key (kbd "C-s") (if (fboundp 'consult-line) #'consult-line #'isearch-forward))
(global-set-key (kbd "C-M-i") #'company-complete)          ; サジェスト

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

(condition-case err
    (progn
      (require 'doctor-quack)
      (with-eval-after-load 'doctor-quack
	(define-key doctor-mode-map (kbd "C-j") #'electric-newline-and-maybe-indent)
	(define-key doctor-mode-map (kbd "RET") #'newline)
	(define-key doctor-mode-map (kbd "M-RET") #'doctor-quack-read-print)))
  (error err))

;; カスタムファイルのロード
(when custom-file (condition-case err (load-file custom-file) (error err)))
(print "Okay")
(put 'list-timers 'disabled nil)

;; tinyscheme用の動的ライブラリ探索
(add-to-list 'process-environment
	     (format "DYLD_LIBRARY_PATH=%s"
		     (string-join `(,(expand-file-name "~/ng/tinyscheme")
				    ,(expand-file-name "~/.emacs.d/tinyscheme_clib"))
				  ":")))

(defun my/vterm-copy-mode-open-python-traceback-from-line ()
  "In vterm copy-mode, obtain the current line content and jump to the corresponding line in a Python file."
  (interactive)
  (let* ((line-str  ;; get line string
          (string-replace
           "\n" ""
           (buffer-substring-no-properties
            (vterm--get-beginning-of-line)
            (vterm--get-end-of-line))))

         ;; Extract filename and line number by regexp
         (match-data (string-match "File \"\\([^\"]+\\)\"[, ]* line \\([0-9]+\\)" line-str)))

    (if match-data
        (let* ((file-path (match-string 1 line-str))
               (line-num (string-to-number (match-string 2 line-str)))
               (full-path file-path))

          ;; Convert absolute path
          (unless (file-name-absolute-p full-path)
            (setq full-path (expand-file-name full-path)))

          ;; Open file and goto line
          (if (file-exists-p full-path)
              (progn
                (find-file-other-window full-path)
                (goto-line line-num))
            (message "File not found: %s" full-path)))

      (message "Unmatch line string: %s" line-str))))

(condition-case err
    (progn
      (defvar my/macos-started-applications nil "")

      (defun my/macos-refresh-started-applications ()
	(interactive)
	(with-current-buffer "*macOS Started Applications*"
	  (erase-buffer))

	(call-process "osascript" nil "*macOS Started Applications*" t "-e"
		      "tell application \"System Events\" to get name of every application process")

	(with-current-buffer "*macOS Started Applications*" ;; cleanup buffer
	  (replace-string "," "\n" nil (point-min) (point-max))
	  (indent-region (point-min) (point-max))
	  (delete-trailing-whitespace))

	(setq my/macos-started-applications
	      (with-current-buffer "*macOS Started Applications*"
		(split-string (buffer-substring (point-min) (point-max)) "\n"))))

      )
  (error "faild to support macos started applications"))

;; (defvar my/copy-and-paste-other-application--application-history nil)
;; (defun my/copy-and-paste-region-to-other-application (application)
;;   (interactive
;;    (list (completing-read "Application: "
;; 			  my/macos-started-applications
;; 			  (lambda (str) (not (string-blank-p str)))
;; 			  t
;; 			  (or (car my/copy-and-paste-other-application--application-history) "")
;; 			  'my/copy-and-paste-other-application--application-history
;; 			  (car my/copy-and-paste-other-application--application-history))))
;; (customize-set-value 'my/colab--browser-application "Brave Browser")
;; copy-and-paste-to-other-application

(condition-case err
    (progn
      (defvar my/colab--osascript-executable (executable-find "osascript"))
      (defcustom my/colab--browser-application "Safari" "Browser application name")

      (defun my/colab ()
	(interactive)
	(clipboard-kill-ring-save (region-beginning) (region-end))
	(call-process my/colab--osascript-executable nil nil nil "-e"
		      (format "tell application \"%s\" to activate"
			      my/colab--browser-application))
	(sleep-for 0.5)
	(call-process my/colab--osascript-executable nil nil nil "-e"
		      "tell application \"System Events\" to keystroke \"v\" using {command down}")
	(call-process my/colab--osascript-executable nil nil nil "-e"
		      "
tell application \"System Events\" to keystroke \"v\" using {command down}
tell application \"System Events\" to keystroke return using {option down}
tell application \"Emacs\" to activate")))
  (error "Failed to support google colab extention"))

(defun my/apply-shell-command(cmd)
  (interactive (list (read-shell-command "Shell Command: ")))
  (when (y-or-n-p (format "Execute and apply[$ %s] ?" cmd))
    (shell-command-on-region (point-min) (point-max) cmd nil t)))


;;  (t "No terminal")))
(condition-case err
    (progn
      (unless (package-installed-p 'gcmh)
	(package-install 'gcmh))
      (require 'gcmh)
      (gcmh-mode 1))
  (warn "Failed to configuration: gcmh: It might be better to lower the GC threshold.: %s" err))

;; SKK
(defun disable-mode-line ()
  (setq-local mode-line-format nil))

;; 辞書の設定
;; (setq skk-user-directory "~/.cache/skk")
;; (setq skk-extra-jisyo-file-list
;;       '("Library/Application Support/AquaSKK/SKK-JISYO.L"))

(with-eval-after-load 'skk
  ;; モードラインを表示しない
  (add-hook 'skk-mode-hook 'disable-mode-line)
  (setq mode-line-format nil)
  (setq skk-modeline-input-mode nil)

  ;; 絶対にモードラインを表示させたくないため
  ;; モードラインの設定関数を上書きする。
  (defun skk-setup-modeline () nil)

  ;; SKKの候補の表示方法
  (setq skk-show-tooltip nil)
  (setq skk-show-inline 'vertical)
  (setq skk-egg-like-newline nil)
  (setq skk-dcomp-activate t)
  (setq skk-dcomp-multiple-activate t)
  (setq skk-henkan-strict-okuri-precedence t)

  ;; カーソルの色を変更する
  (setq skk-cursor-latin-color "turquoise")
  (setq skk-cursor-hiragana-color "orange")
  (setq skk-cursor-katakana-color "systemGreenColor")

  (setq skk-show-mode-show t)
  (setq skk-show-mode-style "tooltip")

  (defun skk-isearch-setup-maybe ()
    (require 'skk-vars)
    (when (or (eq skk-isearch-mode-enable 'always)
  	      (and (boundp 'skk-mode)
  		   skk-mode
  		   skk-isearch-mode-enable))
      (skk-isearch-mode-setup)))

  (defun skk-isearch-cleanup-maybe ()
    (require 'skk-vars)
    (when (and (featurep 'skk-isearch)
  	       skk-isearch-mode-enable)
      (skk-isearch-mode-cleanup)))

  (add-hook 'isearch-mode-hook #'skk-isearch-setup-maybe)
  (add-hook 'isearch-mode-end-hook #'skk-isearch-cleanup-maybe)

  (require 'skk-study)  ;; 辞書の学習
  )

(defun my/set-alpha (alpha)
  (interactive
   (list
    (string-to-number
     (completing-read
      "Alpha: "
      '("10" "20" "30" "40" "50" "60" "70" "80" "90" "100")))))
  (set-frame-parameter nil 'alpha
		       (cons alpha alpha)))

(defun my/super-laser-beam-focus ()
  (interactive)
  (make-frame)
  (doctor)
  (insert "次やる事を指示して")
  (doctor-quack-read-print))

;; EWW
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "b") #'eww-back-url))

;; Agentic
(add-to-list 'load-path (expand-file-name "~/ng/symdon/agentic"))

;; key customize
(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "C-t") nil))


;;; init.el ends here
