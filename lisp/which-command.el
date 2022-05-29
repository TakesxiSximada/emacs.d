;;; which-command.el --- Run which command on Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Maintainer: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Repository:
;; Version: 1
;; Package-Version: 20220426.0000
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))
;; Date: 2022-04-26

;; This package is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with change-case.el.  If not, see https://www.gnu.org/licenses/agpl-3.0.html.

;;; Commentary:

;; which command.

;;; Code:

(require 's)

(defun which-command--run-which (&optional name)
  (with-temp-buffer
    (call-process "which" nil (current-buffer) t name)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun which-command (&optional name)
  (interactive "MCommand name: ")
  (if-let ((path (s-trim (which-command--run-which name))))
      (progn
	(kill-new path)
	(message (format "Copied: %s" path)))))

(provide 'which-command)
;;; which-command.el ends here
