;;; django-migration --- Support django migration. -*- lexical-binding: t -*-

;; Copyright (C) 2023 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Version: 1
;; Package-Version: 20230121.0000
;; Package-Requires: ((emacs "28.1"))
;; Date: 2023-01-21

;; This file is part of django-migration.

;; django-migration is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; django-migration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:
(require 'vterm)

(defcustom django-migration-squash-python-command "python" "")
(defcustom django-migration-module-directory "." "")
(defcustom django-migration-squash-database-name "testing" "")

(defvar django-migration-squash-app-migration-alist nil)
(defvar django-migration-squash-buffer-name "*DJANGO SQUASH*")
(defvar django-migration-dependencies-buffer-name "*DJANGO MIGRATION DEPS*")
(defvar django-migration-dependencies-buffer-name-temp "*DJANGO MIGRATION DEPS TEMP*")

(defun django-migration-squash-convert-to-app-migration-alist (files)
  (interactive)
  (let* ((last-file (car (seq-reverse files))))
    `((migration-begin . ,(if (eq 1 (seq-length files))
			      "" (car (seq-reverse (string-split (car files) "/")))))
      (migration-end . ,(car (seq-reverse (string-split last-file "/"))))
      (app-label . ,(nth 2 (seq-reverse (string-split last-file "/")))))))

;;;###autoload
(defun django-migration-create-database ()
  "Recreate database"
  (interactive)
  (sql-send-string
   (format "DROP DATABASE IF EXISTS %s;" django-migration-squash-database-name))

  (sql-send-string
   (format "CREATE DATABASE %s DEFAULT CHARACTER SET utf8mb4;" django-migration-squash-database-name)))

;;;###autoload
(defun django-migration-select ()
  "Select django migration files"
  (interactive)
  (setq django-migration-squash-app-migration-alist
	(django-migration-squash-convert-to-app-migration-alist
	 (dired-get-marked-files))))

;;;###autoload
(defun django-migration-squash ()
  "Squash django migration files"
  (interactive)
  (let ((vterm-shell (format "%s manage.py squashmigrations --noinput %s %s %s"
			     django-migration-squash-python-command
			     (cdr (assoc 'app-label django-migration-squash-app-migration-alist))
			     (file-name-base (cdr (assoc 'migration-begin django-migration-squash-app-migration-alist)))
			     (file-name-base (cdr (assoc 'migration-end django-migration-squash-app-migration-alist)))))
	(vterm-buffer-name django-migration-squash-buffer-name)
	(vterm-kill-buffer-on-exit nil))
    (vterm)))


;;;###autoload
(defun django-migration-delete-squash ()
  "Delete unnessesary squashed migration file

It need Django Extentions https://django-extensions.readthedocs.io/en/latest/index.html.
See https://django-extensions.readthedocs.io/en/latest/index.html.
"
  (interactive)
  (let ((vterm-shell (format "%s manage.py delete_squashed_migrations --noinput %s"
			     django-migration-squash-python-command
			     (cdr (assoc 'app-label django-migration-squash-app-migration-alist))))
	(vterm-buffer-name django-migration-squash-buffer-name)
	(vterm-kill-buffer-on-exit nil))
    (vterm)))

(defun django-migration-dependencies-format-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create django-migration-dependencies-buffer-name)
    (insert (with-current-buffer
		(get-buffer django-migration-dependencies-buffer-name-temp)
	      (buffer-substring-no-properties (point-min) (point-max))))
    (replace-regexp ".*\(\'" "" nil (point-min) (point-max))
    (sort-lines nil (point-min) (point-max))
    (delete-duplicate-lines (point-min) (point-max))))

;;;###autoload
(defun django-migration-dependencies ()
  (interactive)
  (let ((vterm-shell (format "grep \"('%s', '0\" %s/*/migrations/*.py"
			     (cdr (assoc 'app-label django-migration-squash-app-migration-alist))
			     django-migration-module-directory))
	(vterm-buffer-name django-migration-dependencies-buffer-name-temp)
	(vterm-kill-buffer-on-exit nil)
	(vterm-exit-functions nil))
    (vterm)))

(provide 'django-migration)
;;; django-migration.el ends here
