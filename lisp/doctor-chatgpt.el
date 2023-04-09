;;; doctor-chatgpt --- Talk to doctor(backend OpenAI ChatGPT)  -*- lexical-binding: t -*-

;; Copyright (C) 2023 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository:
;; Version: 2
;; Package-Version: 20230409.0000
;; Package-Requires: ((emacs "28.0") (plz "0.3") (plz "0.3") (json "1.5"))
;; Date: 2023-04-09

;; This file is part of doctor-chatgpt.

;; doctor-chatgpt is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; doctor-chatgpt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

(require 'plz)
(require 'json)
(require 'doctor)

(defvar doctor-current-answer-marker (make-marker))
(defvar doctor-current-talk-end-marker (make-marker))

(defvar doctor-chatgpt-buffer-name "*doctor*"
  "name of doctor buffer")

(defcustom doctor-chatgpt-api-origin "https://api.openai.com"
  "API Origin of OpenAI ChatGPT")

(defcustom doctor-chatgpt-access-token nil
  "API access token of OpenAI ChatGPT")

(defun doctor-chatgpt-api-get-url (path)
  (concat doctor-chatgpt-api-origin path))

(defun doctor-chatgpt-activate ()
  (interactive)
  (define-key doctor-mode-map (kbd "C-j") nil)
  (define-key doctor-mode-map (kbd "RET") nil)
  (define-key doctor-mode-map (kbd "C-c C-c") #'doctor-read-print))

(defun doctor-chatgpt-parse-response-get-text (data)
  "ChatGPT Response parser"
  (cdr (assoc 'text (elt (cdr (assoc 'choices data)) 0))))

;; backup original function
(unless (symbol-function 'doctor-read-print-original)
  (defalias 'doctor-read-print-original (symbol-function #'doctor-read-print)))

(unless (symbol-function 'doctor-readin-original)
  (defalias 'doctor-readin-original (symbol-function #'doctor-readin)))

(unless (symbol-function 'doctor-doc-original)
  (defalias 'doctor-doc-original (symbol-function #'doctor-doc)))

;; extend doctor functions
(defun doctor-readin-all-sentences ()
  (interactive)
  (list
   (buffer-substring-no-properties
    (marker-position doctor-current-talk-end-marker)
    (point-max))))

(defun doctor-read-print-extra ()
  "Top level loop."
  (interactive nil doctor-mode)
  (setq doctor-sent (doctor-readin))
  (insert (if (equal (line-beginning-position) (point))
    "\n" "\n\n"))
  (insert "#+begin_example\n")
  (set-marker doctor-current-answer-marker (point))
  (goto-char (+ (point) 1))
  (insert "#+end_example\n\n")
  (set-marker doctor-current-talk-end-marker (point))
  (doctor-doc))

(defun doctor-doc-chatgpt ()
  (let* ((sentence (car doctor-sent))
         (endpoint (doctor-chatgpt-api-get-url "/v1/completions"))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" doctor-chatgpt-access-token))))
         (body (json-encode `(("model" . "text-davinci-003")
                              ("prompt" . ,sentence)
                              ("temperature" . 0.9)
                              ("max_tokens" . 300)
                              ("top_p" . 1)
                              ("frequency_penalty" . 0.0)
                              ("presence_penalty" . 0.6)
                              ("stop" . (" Human:" " AI:"))))))
    (plz 'post endpoint :headers headers :body body
      :as #'json-read
      :then (lambda (d)
              (let ((answer (doctor-chatgpt-parse-response-get-text d)))
                (with-current-buffer (marker-buffer doctor-current-answer-marker)
                  (save-excursion
                    (goto-char (marker-position doctor-current-talk-end-marker))
                    (insert answer)))))
      :else (lambda (d)
        (princ d)))))

(defalias 'doctor-readin (symbol-function #'doctor-readin-all-sentences))
(defalias 'doctor-read-print (symbol-function #'doctor-read-print-extra))
(defalias 'doctor-doc (symbol-function #'doctor-doc-chatgpt))

(defun doctor-chatgpt-recovery ()
  (interactive)
  (defalias 'doctor-read-print (symbol-function #'doctor-read-print-original))
  (defalias 'doctor-readin (symbol-function #'doctor-readin-original))
  (defalias 'doctor-doc (symbol-function #'doctor-doc-original)))

(provide 'doctor-chatgpt)
;;; doctor-chatgpt ends here
