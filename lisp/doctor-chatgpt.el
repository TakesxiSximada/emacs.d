;;; doctor-chatgpt --- Talk to doctor(backend OpenAI ChatGPT)  -*- lexical-binding: t -*-

;; Copyright (C) 2023 TakesxiSximada


;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository:
;; Version: 1
;; Package-Version: 20230115.0000
;; Package-Requires: ((emacs "28.1") (plz "0.3") (plz "0.3") (json "1.5"))
;; Date: 2023-01-15

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

(defcustom doctor-chatgpt-api-origin "https://api.openai.com"
  "API Origin of OpenAI ChatGPT")

(defcustom doctor-chatgpt-access-token nil
  "API access token of OpenAI ChatGPT")

(defun doctor-chatgpt-api-get-url (path)
  (concat doctor-chatgpt-api-origin path))

(defun doctor-read-print ()
  "Top level loop."
  (interactive nil doctor-mode)
  (backward-sentence 1)
  (let* ((doctor-buf (current-buffer))
         (sentence (buffer-substring-no-properties (point) (point-max)))
         (endpoint (doctor-chatgpt-api-get-url "/v1/completions"))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" doctor-chatgpt-access-token))))
         (body (json-encode `(("model" . "text-davinci-003")
                              ("prompt" . ,sentence)
                              ("temperature" . 0.9)
                              ("max_tokens" . 150)
                              ("top_p" . 1)
                              ("frequency_penalty" . 0.0)
                              ("presence_penalty" . 0.6)
                              ("stop" . (" Human:" " AI:"))))))
    (goto-char (point-max))

    (plz 'post endpoint :headers headers :body body
      :as #'json-read
      :then (lambda (d)
              (with-current-buffer doctor-buf
                (insert
                 (format "\nDoctor: %s\n\n"
                         (cdr (assoc 'text (elt (cdr (assoc 'choices
                                                            d
                                                            ))
                                                0))))))))))


(provide 'doctor-chatgpt)
;;; doctor-chatgpt ends here
