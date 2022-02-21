(require 'org)

(defconst essay-buffer-name "*ESSAY*")

(defvar essay-base-directory (expand-file-name "~")
  "このディレクトリにessayが保存されます。")

(defvar essay-file-path-directory-style nil
  "TRUEにするとディレクトリスタイルでessayを記録します。")

(define-derived-mode essay-mode org-mode
  "Essay mode"
  nil)

(defun essay-create-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create essay-buffer-name))
  (when (= 0 (buffer-size))
    (save-excursion
      (goto-char 0)
      (insert (format "#+DATE: %s\n#+TAGS[]: comment\n\n"
		      (format-time-string "%+FT%T%z")))))
  (kill-all-local-variables)
  (use-local-map essay-mode-map)
  (essay-mode))

(defun essay-create-id ()
  "essayの番号を生成します。"
  (truncate (float-time)))

(defun essay-make-new-file-path ()
  "保存先ファイルのパス返す。

  通常ではファイルスタイルorgファイル (XXXX.org) のパスを返す。
  `essay-file-path-directory-style` をNILにするとディレクトリスタ
  イルのパス(XXXX/index.org)を返す。
  "
  (expand-file-name
    (format (if essay-file-path-directory-style "%s/index.org" "%s.org")
	    (essay-create-id))
    essay-base-directory))

(defvar essay-project-alist nil)

(defvar essay-new-file-path nil)

(defvar essay-after-save-hook nil)

(defun essay-close-essay-buffer ()
  (kill-buffer essay-buffer-name))

(defun essay-save (&optional project directory-style)
  (interactive
   (list (completing-read "PROJECT: " essay-project-alist)
	 (y-or-n-p "Directory style?")))
  (let* ((essay-base-directory (cdr (assoc project essay-project-alist)))
	 (essay-file-path-directory-style directory-style)
	 (essay-new-file-path (essay-make-new-file-path)))
    (make-directory (file-name-directory essay-new-file-path) t)
    (switch-to-buffer
     (with-current-buffer (find-file-noselect essay-new-file-path)
       (insert-buffer-substring (get-buffer essay-buffer-name))
       (save-buffer)
       (current-buffer)))
    (run-hooks 'essay-after-save-hook)))

(defun essay-git-commit ()
  (interactive)
  (when essay-new-file-path
    (shell-command (format "git add %s" essay-new-file-path))
    (shell-command (format "git commit -m 'Add new essay.' %s" essay-new-file-path))))

(defalias #'essay #'essay-create-buffer)

(add-hook 'essay-after-save-hook #'essay-close-essay-buffer)

(bind-keys :map essay-mode-map
  	   ("C-x C-s" . essay-save))

(provide 'essay)
