;;; traverse-directory --- traverse diretory -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository:
;; Version: 1
;; Package-Version: 20221226.0000
;; Package-Requires: ((emacs "28")
;; Date: 2022-12-26

;; This file is part of traverse-directory.

;; traverse-directory is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; traverse-directory is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

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

(defun traverse-directory-django-manage-py-p (cwd)
  (file-exists-p (file-name-concat cwd "manage.py")))

(defun traverse-directory-super-root-p (cwd)
  (string-equal "/" cwd ))

(require 'traverse-directory)
;;; traverse-directory.el ends here

