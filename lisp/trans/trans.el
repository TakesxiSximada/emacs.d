;;; trans.el --- Translation functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023 TakesxiSximada
;;
;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Keywords: Translation
;; Homepage: https://github.com/TakesxiSximada/emacs.d/tree/main/lisp/trans

;; Version: 2

;; trans.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; trans.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trans.el. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; trans.el is translation utility using by translation-shell.


;; Install by package.el
;; ---------------------
;; (This package is not yet melpa packaging.)
;;
;; 1. Download trans.el
;; 2. M-x package-install-file RET /PATH/TO/DOWNLOADED/trans.el RET

;; Testing
;; -------
;; This package has no test yet.

;; Bug report and Contributing
;; ---------------------------
;; We haven't prepared those flows yet. For the time being, please write the steps
;; to reproduce the bug in this gist comment, or attach the patch file in diff format.
;; Welcome them.

;;; Code:

(defvar trans-output-buffer-name "*Trans*")
(defvar trans-process-name "*Trans Process")
(defvar trans-process-buffer-name " *Transprocess*")
(defvar trans-process nil)

(defcustom trans-current-lang-mode "ja:en" "")
(defcustom trans-started-lang-mode "" "")

(defun trans-output (process output)
  (with-output-to-temp-buffer trans-output-buffer-name
    (princ output)))

(defun trans-get-sentence ()
  (if (region-active-p)
      `(buffer-substring-no-properties
	(region-beginning)
	(region-end))
    `(buffer-substring-no-properties
      (point-min)
      (point-max))))

(defun trans-cleanup-sentence (sentence)
  (with-temp-buffer
    (insert sentence)
    (delete-trailing-whitespace)
    (replace-regexp "\n" " " nil (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun trans-start ()
  (interactive)
  (unless trans-process
    (setq trans-process
	  (make-process :name trans-process-name
			:buffer (get-buffer-create trans-process-buffer-name)
			:command `("trans"
				   ,trans-current-lang-mode
				   "-show-original" "n"
				   "-show-prompt-message" "n"
				   "-brief"
				   "-no-pager"
				   "-no-ansi"
				   )
			:filter 'trans-output
			)
	  trans-started-lang-mode trans-current-lang-mode)))

;;;###autoload
(defun trans-switch-lang (lang-mode)
  (interactive (list (completing-read "sLang mode(BASE:TRNAS): " '("ja:en" "en:ja"))))
  (setq trans-current-lang-mode lang-mode))

;;;###autoload
(defun trans-restart ()
  (interactive)
  (trans-stop)
  (trans-start))

;;;###autoload
(defun trans-stop ()
  (interactive)
  (when trans-process
    (signal-process (get-process trans-process) 15)
    (setq trans-process nil
	  trans-started-lang-mode ""
	  )))

;;;###autoload
(defun trans-region ()
  (interactive)
  (unless (string-equal trans-current-lang-mode trans-started-lang-mode)
    (trans-stop))
  (trans-start)
  (if trans-process
      (process-send-string trans-process
			   (concat
			    (trans-cleanup-sentence
			     (eval (trans-get-sentence)))
			    "\n"))
    (error "No translation process")))

(provide 'trans)
;;; trans.el ends here
