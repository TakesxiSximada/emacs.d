;;; weasyprint for Emacs --- WeasyPrint for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository: https://github.com/TakesxiSximada/emacs.d
;; Version: 1
;; Package-Version: 20220603.0000
;; Package-Requires: ((emacs "27.1"))
;; Date: 2022-05-29

;; This file is part of weasyprint for Emacs.

;; weasyprint for Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; weasyprint for Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

(defgroup weasyprint nil
  "WeasyPRint interface for Emacs."
  :prefix "weasyprint-"
  :group 'tools
  :link '(url-link :tag "Source" "https://github.com/TakesxiSximada/emacs.d/blob/main/lisp/weasyprint.el"))

(defcustom weasyprint-executable "weasyprint"
  "Executables of weasyprint command."
  :type 'string)

(defcustom weasyprint-process-name "WeasyPrint"
  "Name of weasyprint subprocess."
  :type 'string)

(defcustom weasyprint-output-buffer-name "*WeasyPrint Output*"
  "Output buffer name of weasyprint subprocess."
  :type 'string)

(defcustom weasyprint-error-buffer-name "*WeasyPrint Error*"
  "Error buffer name of weasyprint subprocess."
  :type 'string)

(defcustom weasyprint-styleseets '("main.css" "ext.css")
  "Output buffer name of weasyprint subprocess."
  :type 'list)

(defcustom weasyprint-content-file "index.html"
  "Output buffer name of weasyprint subprocess."
  :type 'string)

(defvar weasyprint-after-build-hook nil
  "Call after build for weasyprint.")

(defcustom weasyprint-content-file "index.html"
  "Output buffer name of weasyprint subprocess."
  :type 'string)

(defun weasyprint-generate-command ()
  `(,weasyprint-executable
    "--encoding" "UTF-8"
    "--media-type" "page"
    "--base-url" ,default-directory
    ,@(seq-reduce (lambda (options file) (append options  `("--stylesheet" ,file)))  weasyprint-styleseets nil)
    ,weasyprint-content-file
    "-"
    ))


;;;###autoload
(defun weasyprint-open-pdf-buffer ()
  (interactive)
  (with-current-buffer weasyprint-output-buffer-name
    (setq-local buffer-file-coding-system 'binary)
    (doc-view-mode)
    (display-buffer-at-bottom (current-buffer) t)))

;;;###autoload
(defun weasyprint ()
  (interactive)

  (when (get-buffer weasyprint-output-buffer-name)
    (kill-buffer weasyprint-output-buffer-name))

  (make-process :name weasyprint-process-name
		:buffer (get-buffer-create weasyprint-output-buffer-name)
		:command (weasyprint-generate-command)
		:coding 'utf-8-unix
		:stderr (get-buffer-create weasyprint-error-buffer-name)
		:connection-type 'pipe
		:sentinel (lambda (process event)
			    (when (string-equal event "finished\n")
			      (run-hooks 'weasyprint-after-build-hook)))
		))

(add-hook 'weasyprint-after-build-hook 'weasyprint-open-pdf-buffer)

(provide 'weasyprint)
;;; weasyprint.el ends here
