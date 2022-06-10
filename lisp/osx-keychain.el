;;; osx-keychain --- OS X Keychain Utility on Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository:
;; Version: 1
;; Package-Version: 20220610.0000
;; Package-Requires: ((emacs "28.0")
;; Date: 2022-06-10

;; This file is part of osx-keychain.

;; osx-keychain is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; osx-keychain is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

(defgroup osx-keychain nil
  "OS X Keychain Utility on Emacs"
  :prefix "osx-keychain-"
  :group 'osx
  :link '(url-link :tag "Source" "https://github.com/TakesxiSximada/emacs.d/blob/main/lisp/osx-keychain.el"))

(defcustom osx-keychain-security-executable "/usr/bin/security"
  "security command."
  :type 'string)

;;;###autoload
(defun osx-keychain-get (service account)
  (interactive)
  (with-temp-buffer
    (let ((ret (call-process osx-keychain-security-executable
			     nil (current-buffer) nil
			     "find-generic-password"
			     "-s" service
			     "-a" account
			     "-w")))
      (when (eq ret 0)
	(string-trim
	 (buffer-substring-no-properties (point-min) (point-max)))))))

;;;###autoload
(defun osx-keychain-add (service account value)
  (interactive)
  (make-process
   :name "*Keychain*"
   :command `("/usr/bin/security"
	      "add-generic-password"
	      "-U"
	      "-A"
	      "-s" ,service
	      "-a" ,account
	      "-w")
   :filter `(lambda (proc output)
	      (when (or (string-match "password data for new item: " output)
			(string-match "retype password for new item: " output))
		(process-send-string proc (format "%s\n" ,value))))))

(provide 'osx-keychain)
;;; osx-keychain.el ends here
