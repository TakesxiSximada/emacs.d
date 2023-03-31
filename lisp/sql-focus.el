;;; sql-focus --- Focus on that SQL -*- lexical-binding: t -*-

;; Copyright (C) 2023 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository:
;; Version: 1
;; Package-Version: 20230331.0000
;; Package-Requires: ((emacs "28.0")
;; Date: 2023-03-31

;; This file is part of sql-focus.

;; sql-focus is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; sql-focus is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

(require 'edit-indirect)

(defvar-local sql-focus-hook nil)
(defvar sql-focus-delay 1)
(defvar sql-focus-timer nil)
(defvar sql-focus-explain-buffer \"*SQL: Postgres*\")

(defun sql-focus-get-explain-sql ()
  (format "EXPLAIN %s;\n"
	  (buffer-substring-no-properties (point-min) (point-max))))

(defun sql-focus-explain ()
  (interactive)
  (let ((proc (get-buffer-process (get-buffer sql-focus-explain-buffer))))
    (comint-send-string proc "\n")
    (comint-send-string proc (sql-focus-get-explain-sql))))

(defun sql-focus-activate ()
  (interactive)
  (add-hook 'sql-focus-hook #'sql-focus-explain nil t)
  (when (timerp sql-focus-timer)
    (cancel-timer sql-focus-timer))
  (setq sql-focus-timer (run-with-idle-timer sql-focus-delay t #'run-hooks 'sql-focus-hook)))

(defun sql-focus-deactivate ()
  (interactive)
  (remove-hook 'sql-focus-hook #'sql-focus-explain t)
  (when (timerp sql-focus-timer)
    (cancel-timer sql-focus-timer)))

(defun sql-focus (beg end)
  (interactive "r")
  (let ((buf (edit-indirect-region beg end t)))
    (with-current-buffer buf
      (sql-focus-activate))
    (display-buffer sql-focus-explain-buffer '(display-buffer-below-selected))))

(provide 'sql-focus)
;;; sql-focus ends here
