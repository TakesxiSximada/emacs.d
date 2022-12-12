;;; aws-cli.el --- AWS CLI for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Version: 1
;; Package-Version: 20221203.0000
;; Package-Requires: ((emacs "27.1"))
;; Date: 2022-12-03

;; This file is part of aws cli.el.

;; aws-cli.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; aws-cli.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

;;; Customization
(require 'simple)

(defcustom aws-cli-command "aws" "")

(defcustom aws-cli-current-profile nil "")

;;;###autoload
(defun aws-cli-switch-profile (profile-name)
  (interactive "s:AWS_PROFILE: ")
  (setq aws-cli-current-profile profile-name)
  (message (format "Change aws profile configuration: %s"
		   aws-cli-current-profile)))

;;;###autoload
(defun aws-cli-switch-cli (command)
  (interactive "s:AWS CLI COMMAND: ")
  (setq aws-cli-command command)
  (message (format "Change aws cli base command: %s"
		   aws-cli-command)))

;;;###autoload
(defun aws-cli-apply-profile-to-global ()
  (interactive)
  (when aws-cli-current-profile
    (setenv "AWS_PROFILE" aws-cli-current-profile)
    (message (format "Change `AWS_PROFILE` environment variable: %s"
		     aws-cli-current-profile))))

(defun aws-cli--process-environment ()
  (if aws-cli-current-profile
      (append `(,(format "AWS_PROFILE=%s" aws-cli-current-profile)) process-environment)
    process-environment))

;;;###autoload
(defun aws-cli-help (subcmd)
  (interactive "s[aws] ")
  (let ((process-environment (aws-cli--process-environment)))
    (async-shell-command (format "%s %s help" aws-cli-command subcmd))))

;;;###autoload
(defun aws-cli-print-skeleton (subcmd)
  (interactive "s[aws] ")
  (let ((process-environment (aws-cli--process-environment)))
    (async-shell-command (format "%s %s --generate-cli-skeleton" aws-cli-command subcmd))))

;;;###autoload
(defun aws-cli-execute-skelton (subcmd)
  (interactive "s[aws] ")
  (let ((process-environment (aws-cli--process-environment)))
    (async-shell-command
     (format "%s %s --cli-input-json file://%s"
	     aws-cli-command
	     subcmd
	     buffer-file-name))))

(provide 'aws-cli)
;;; aws-cli.el ends here
