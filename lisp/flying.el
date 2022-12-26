;; - [ ] mypy
;; - [ ] black
;; - [ ] isort
;; - [ ] flake8
;; - [ ] pytest
;; - [ ] pyflakes
;; - [ ] unittest
;; - [ ] coverage
;; - [ ] django unittest



(setq python-custom-project-root-directory nil)


(defun run-custom-flake8 (report-fn &rest _args)
  (interactive)
  (let ((default-directory python-custom-project-root-directory)
	(process-environment (append process-environment
				     '("APP_KEY=test"
				       "APP_ENV=test"
				       ))))

    (make-process :name "*FLAKE8*"
		  :buffer (get-buffer-create "*FLAKE8*")
		  :command
		  `("/Users/sximada/.venv/hkn/bin/flake8"
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

(defun flymake-python-setup ()
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-flake8 nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-mypy nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-isort nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-black nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-doctest nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-unittest nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-django-test nil t)
  (add-hook 'flymake-diagnostic-functions 'on-the-fly-django-doctest nil t)
  )


(add-hook 'python-mode-hook #'flymake-python-setup)


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

   
