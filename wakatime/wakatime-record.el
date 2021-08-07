;; wakatime-record.el --- Yet Another Wakatime plugin for Emacs.

; Copyright (C) 2021 TakesxiSximada <sximada@gmail.com>

; Author: TakesxiSximada <sximada@gmail.com>
; Maintainer: TakesxiSximada <sximada@gmail.com>
; Website: https://github.com/TakesxiSximada/emacs.d/master/wakatime/
; Keywords: calendar, comm
; Package-Version: 20210730.240
; Package-Commit: 5e6deddda7a70f4b79832e9e8b6636418812e162
; Version: 1.0.0

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:

; wakatime-record.el is an unofficial wakatime plugin for Emacs.
; It was implemented based on a different design philosophy.
; Heartbeats are recorded as Line-delimited JSON in a temporary journal.

;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-clock)

(defvar wakatime-record-buffer-name "*WAKATIME RECORD*")
(defvar wakatime-record-file-path (expand-file-name "~/.wakatime.heartbeat.json"))
(defvar wakatime-record-timer nil)

(setq wakatime-record-language-alist
      '(
	(Info-mode . "Emacs")
	(c-mode . "C")
	(compilation-mode . "Completion")
	(completion-list-mode . "Completion")
	(conf-toml-mode . "TOML")
	(css-mode . "CSS")
	(dired-mode . "Dired")
	(dockerfile-mode . "Docker")
	(editor-mode . "Org")
	(emacs-lisp-mode . "Emacs Lisp")
	(foreman-mode . "Foreman")
	(fundamental-mode . "Emacs Lisp")
	(go-mode . "Go")
	(html-mode . "HTML")
	(js-mode . "JavaScript")
	(js2-mode . "JavaScript")
	(json-mode . "JSON")
	(lisp-interaction-mode . "Emacs Lisp")
	(magit-refs-mode . "Git")
	(magit-status-mode . "Git")
	(messages-buffer-mode . "Emacs")
	(mhtml-mode . "HTML")
	(org-agenda-mode . "Org")
	(org-mode . "Org")
	(python-mode . "Python")
	(restclient-mode . "HTTP")
	(rustic-mode . "Rust")
	(scss-mode . "CSS")
	(shell-script-mode . "Sehll")
	(sql-interactive-mode . "SQL")
	(sql-mode . "SQL")
	(typescript-mode . "TypeScript")
	(vterm-mode . "Shell")
	(vue-html-mode . "Vue")
	(vue-mode . "Vue")
	(xwidget-webkit-mode . "HTML")
	(yaml-mode . "YAML")
	))

(setq wakatime-record-category-alist
      '(
	(Info-mode . "coding")
	(c-mode . "coding")
	(compilation-mode . "building")
	(compilation-mode . "coding")
	(completion-list-mode . "coding")
	(conf-toml-mode . "coding")
	(css-mode . "coding")
	(dired-mode . "planning")
	(dockerfile-mode . "coding")
	(editor-mode . "writing docs")
	(emacs-lisp-mode . "coding")
	(eww-mode . "eww-mode")
	(fundamental-mode . "coding")
	(go-mode . "coding")
	(html-mode . "coding")
	(js-mode . "coding")
	(js2-mode . "coding")
	(json-mode . "coding")
	(lisp-interaction-mode . "coding")
	(magit-refs-mode . "coding")
	(magit-status-mode . "coding")
	(messages-buffer-mode . "planning")
	(mhtml-mode . "coding")
	(org-agenda-mode . "planning")
	(org-mode . "writing docs")
	(python-mode . "coding")
	(restclient-mode . "coding")
	(rustic-mode . "coding")
	(scss-mode . "coding")
	(shell-script-mode . "coding")
	(sql-interactive-mode . "coding")
	(sql-mode . "coding")
	(typescript-mode . "coding")
	(vterm-mode . "coding")
	(vue-html-mode . "coding")
	(vue-mode . "coding")
	(xwidget-webkit-mode . "coding")
	(yaml-mode . "coding")
	(nil . "code reviewing")
	(nil . "debugging")
	(nil . "designing")
	(nil . "indexing")
	(nil . "learning")
	(nil . "manual testing")
	(nil . "meeting")
	(nil . "researching")
	(nil . "running tests")
	(nil . "writing tests")))


(cl-defstruct wakatime-record-heartbeat
  time
  user_agent
  entity
  type
  category
  is_write
  project
  ;; branch
  language
  ;; dependencies
  ;; lines
  ;; lineno
  ;; cursorpos
  )

(defun wakatime-record-get-category-by-major-mode ()
  (or
   (cdr (assoc major-mode wakatime-record-category-alist))
   "planning"))

(defalias 'wakatime-record-get-category 'wakatime-record-get-category-by-major-mode)

(defun make-wakatime-record-current-heartbeat ()
  (make-wakatime-record-heartbeat
   :time (float-time)
   :type "file"
   :user_agent "emacs"
   :entity (buffer-name)
   :language (or (cdr (assoc major-mode wakatime-record-language-alist)) major-mode)
   :project (if-let ((current-task-buffer (org-clock-is-active)))
		(with-current-buffer current-task-buffer
		  (org-get-category))
	      "GLOBAL")
   :is_write t
   :category (wakatime-record-get-category)
   ))

(defun wakatime-record-serialize (heatbeat)
  (concat
   (json-encode-alist
    (let ((typ (type-of heatbeat)))
      (mapcar (lambda (key) `(,key . ,(cl-struct-slot-value typ key heatbeat)))
	      (mapcar 'car (cdr (cl-struct-slot-info typ))))))
   "\n"))


(defun wakatime-record-save-heatbeat ()
  (interactive)
  (let ((serialized-heatbeat (wakatime-record-serialize
			      (make-wakatime-record-current-heartbeat))))
    (with-current-buffer (get-buffer-create wakatime-record-buffer-name)
      (insert serialized-heatbeat)
      (write-region (point-min) (point-max)
		    wakatime-record-file-path
		    t)
      (kill-buffer))))

(defun wakatime-record-tunrn-off ()
  (interactive)
  (when (timerp wakatime-record-timer)
    (cancel-timer wakatime-record-timer)))


(defun wakatime-record-tunrn-on ()
  (interactive)
  (wakatime-record-tunrn-off)
  (setq wakatime-record-timer
	(run-with-idle-timer 20 t #'wakatime-record-save-heatbeat)))

(provide 'wakatime-record)
;;; wakatime-record.el ends here
