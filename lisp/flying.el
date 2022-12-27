;; - [ ] mypy
;; - [ ] black
;; - [ ] isort
;; - [ ] flake8
;; - [ ] pytest
;; - [ ] pyflakes
;; - [ ] unittest
;; - [ ] coverage
;; - [ ] django unittest

(require 'traverse-directory)



(require 'flymake-collection)  ;; flymake関連で適切に保全されていそうなもの
(require 'reformatter)

(defun python-autoflake-reformat ()
  (interactive)

  (let ((before-save-hook nil))
    (save-buffer))
  (let ((proc (make-process :name "*AUTOFLAKE*"
                            :buffer (get-buffer-create "*AUTOFLAKE*")
                            :command `("autoflake" "-i" "--remove-all-unused-imports" ,buffer-file-name)
                            :sentinel (lambda (process _)
                                         (print "Ok")
                                         (when (eq 'exit (process-status process))
                                               (revert-buffer t t t))))))
     (sleep-for 2)  ;; とりあえず2秒程度待ってみる
     ))
;; (sleep-for 
;; (reformatter-define python-autoflake4
;;   :program "autoflake"
;;   :args '("--remove-all-unused-imports" "-i")
;;   :stdin nil
;;   :input-file (reformatter-temp-file-in-current-directory)
;;   )

;; (defun python-remove-unused-imports ()
;;   "Use Autoflake to remove unused function"
;;   (interactive)
;;   (when (eq major-mode 'python-mode)
;;     (shell-command (format "%s --remove-all-unused-imports -i %s"
;; 			   python-autoflake-path
;; 			   (shell-quote-argument (buffer-file-name))))
;;     (revert-buffer t t t))
;;   nil)

;; (eval-after-load 'python
;;   '(if python-autoflake-path
;;        (add-hook 'before-save-hook 'python-remove-unused-imports)

;; 初期化処理
(defun flymake-python-setup ()
  (flycheck-mode 0)

  ;; バッファ保存時にフォーマットする
  (add-hook 'before-save-hook #'python-isort-buffer nil t)
  (add-hook 'before-save-hook #'python-autoflake-reformat nil t)
  (add-hook 'before-save-hook #'blacken-buffer nil t)

  
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  
  (setq flymake-diagnostic-functions nil)
  (add-hook 'flymake-diagnostic-functions 'flymake-collection-flake8 nil t)
  (flymake-mode-on)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-mypy nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-isort nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-black nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-doctest nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-unittest nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-django-test nil t)
  ;; (add-hook 'flymake-diagnostic-functions 'on-the-fly-django-doctest nil t)
  )

(setq python-mode-hook nil)
(add-hook 'python-mode-hook #'flymake-python-setup)


(file-name-directory nil

(defun run-flake8 ()
  (interactive)
  (let* ((default-directory (file-name-directory buffer-file-name))
	 (vterm-shell (format "flake8 %s" buffer-file-name))
	 (vterm-buffer-name "*FLAKE8*")
	 (vterm-kill-buffer-on-exit nil))
    (vterm)))


(defun run-mypy ()
  (interactive)
  (let* ((default-directory (file-name-directory buffer-file-name))
	 (vterm-shell (format "mypy %s" buffer-file-name))
	 (vterm-buffer-name "*MYPY*")
	 (vterm-kill-buffer-on-exit nil))
    (vterm)))

(defun run-isort ()
  (interactive)
  (let* ((default-directory (file-name-directory buffer-file-name))
	 (vterm-shell (format "mypy %s" buffer-file-name))
	 (vterm-buffer-name "*MYPY*")
	 (vterm-kill-buffer-on-exit nil))
    (vterm)))





	  ex-file-name (org-latex-export-to-latex))
	 (base-file-name (file-name-base tex-file-name))
	 (dvi-file-name (format "%s.dvi" base-file-name))
	 (pdf-file-name (format "%s.pdf" base-file-name))
	 (vterm-shell (format "bash -c 'platex %s && dvipdfmx %s'"
			      tex-file-name
			      dvi-file-name))
	 (vterm-buffer-name (format "*Org PDF Exporting: %s" pdf-file-name))
	 (vterm-kill-buffer-on-exit nil))
    (vterm)
    pdf-file-name))


flymake-diagnostic-functions

python-mode

(defun python-flymake (report-fn &rest _args)
  (let
  (print "Ok"))

(defun python-flymake-other (report-fn &rest _args)
  (print "Ng"))

(add-hook 'flymake-diagnostic-functions #'python-flymake-other nil t)



(our-url-encode





(setq python-custom-project-root-directory nil)


(defun run-custom-flake8 (report-fn &rest _args)
  (interactive)
  (let ((default-directory python-custom-project-root-directory)
	(process-environment (append process-environment
				     '(
				       ))))

    (make-process :name "*FLAKE8*"
		  :buffer (get-buffer-create "*FLAKE8*")
		  :command
		  `("flake8"
		    ,buffer-file-name)
		  :type 'pipe
		  )))



(setq run-custom-mypy-executable "mypy")

(defun run-custom-mypy ()
  (interactive)
  (let ((default-directory python-custom-project-root-directory)
	(process-environment (append process-environment
				     '(
				       ))))

    (make-process :name "*MYPY*"
		  :buffer (get-buffer-create "*MYPY*")
		  :type 'pipe
		  :command
		  `(,run-custom-mypy-executable ,buffer-file-name)
		  )))


(defun run-custom-django-unittest ()
  (interactive)
  (let ((default-directory python-custom-project-root-directory)
	(process-environment (append process-environment
				     '(
				       ))))

    (make-process :name "*DJANGO*"
		  :buffer (get-buffer-create "*DJANGO*")
		  :type 'pipe
		  :command
		  `("python" "manage.py" "test" "--keepdb")
		  )))

					;-------------------------

(add-hook 'python-mode-hook #'flymake-mode-on)
(add-hook 'python-mode-hook (defun flycheck-mode nil)
(add-hook 'python-mode-hook (defun flymake-install-flake8 () (add-hook 'flymake-diagnostic-functions 'run-custom-flake8)))
(add-hook 'python-mode-hook (defun flymake-install-flake8 () (add-hook 'flymake-diagnostic-functions 'run-custom-mypy)))


(defun on-the-fly-flake8 (report-fn &rest _args)
  (print "flake8")
  )
(defun on-the-fly-mypy (report-fn &rest _args) (print "mypy"))
(defun on-the-fly-isort (report-fn &rest _args) (print "isort"))
(defun on-the-fly-black (report-fn &rest _args) (print "black"))
(defun on-the-fly-doctest (report-fn &rest _args) (print "doctest"))
(defun on-the-fly-unittest (report-fn &rest _args) (print "unittest"))
(defun on-the-fly-django-test (report-fn &rest _args) (print "djang-test"))
(defun on-the-fly-django-doctest (report-fn &rest _args) (print "djang-doctest"))

(defun foo1 (cwd) (print 1) nil)
(defun foo2 (cwd) (print 2) t)
(defun foo3 (cwd) (print 3) t)


(seq-some (lambda (predicate-fn)
	    (funcall predicate-fn ""))
	  '(foo1 foo2 foo3))

(or
 t
 nil
 (print 2))

(defun traverse-directory-to-up (target-directory predicate-fn-list)
  (let ((cwd (file-name-directory (expand-file-name target-directory))))
    (if (seq-some (lambda (predicate-fn)
		    (funcall predicate-fn cwd))
		  predicate-fn-list)
	cwd
      (traverse-directory-to-up
       (string-remove-suffix "/" cwd) predicate-fn-list))))

(defun traverse-directory-exist-readme-md-p (cwd)
  (file-exists-p (file-name-concat cwd "README.md")))

(defun traverse-directory-super-root-p (cwd)
  (string-equal "/" cwd ))


(traverse-directory-to-up
 "/foo/bar/baz/"
 '(traverse-directory-exist-readme-md-p
   traverse-directory-super-root-p))
(defun traverse-directory-to-up (target-directory predicate-p)
  (let ((cwd (file-name-directory (expand-file-name target-directory))))
    (if (funcall predicate-p cwd)
	cwd
      (traverse-directory-to-up (string-remove-suffix "/" cwd)
				predicate-p))))


(defun testingp (cwd)
  (file-exists-p (file-name-concat cwd "README.md")))



(traverse-directory-to-up
 "/foo/bar/baz/"
 'traverse-directory-exist-readme-md-p)
 (lambda (cwd) (file-exists-p (format "%s/README.md" cwd))))
 'testingp)
