;;; trans.el --- Translation functions -*- lexical-binding: t; -*-

;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2021-2021 Symdon GA
;;
;; Author: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Maintainer: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Keywords: Translation
;; Homepage: https://symdon.ga/

;; Version: 1

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

(setq trans-output-buffer-name "*Trans*")
(setq trans-process-name "*Trans Process")
(setq trans-process-buffer-name " *Transprocess*")
(setq trans-process-command '("trans"
				   "en:ja"
				   "-show-original" "n"
				   "-show-prompt-message" "n"
				   "-brief"
				   "-no-pager"
				   "-no-ansi"
				   ))

(defvar trans-process nil)

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



;;;###autoload
(defun trans-start ()
  (interactive)
  (unless trans-process
    (setq trans-process
	  (make-process :name trans-process-name
			:buffer (get-buffer-create trans-process-buffer-name)
			:command trans-process-command
			:filter 'trans-output
			))
    (set-process-filter-multibyte trans-process t)))


;;;###autoload
(defun trans-restart ()
  (interactive)
  (trans-stop)
  (trans-start))


;;;###autoload
(defun trans-stop ()
  (interactive)
  (when trans-process
    (signal-process (get-process trans-process) 15)  ;; Send SIGTERM
    (setq trans-process nil)))


;;;###autoload
(defun trans-region ()
  (interactive)
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
