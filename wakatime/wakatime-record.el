;; wakatime-record.el --- Yet Another Wakatime plugin for Emacs.

; Copyright (C) 2021 TakesxiSximada <sximada@gmail.com>

; Author: TakesxiSximada <sximada@gmail.com>
; Maintainer: TakesxiSximada <sximada@gmail.com>
; Website: https://wakatime.com
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

; wakatime-record.el is Unofficial wakatime plugin for Emacs.
; It was implemented based on a different design philosophy.
; Heartbeats are recorded as Line-delimited JSON in a temporary journal.

;; Code:

(require 'json)
(require 'cl-lib)

(defvar wakatime-record-buffer-name "*WAKATIME RECORD*")
(defvar wakatime-record-file-path (expand-file-name "~/.wakatime.heartbeat.json"))
(defvar wakatime-record-timer nil)

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


(defun make-wakatime-record-current-heartbeat ()
  (make-wakatime-record-heartbeat
   :time (float-time)
   :type "file"
   :user_agent "Emacs"
   :entity (buffer-name)
   :language major-mode
   :project "GLOBAL"
   :is_write t
   :category "coding"
   ))

(defun wakatime-record-serialize (heatbeat)
  (concat
   (json-encode-alist
    (let ((typ (type-of heatbeat)))
      (mapcar (lambda (key) `(,key . ,(cl-struct-slot-value typ key heatbeat)))
	      (mapcar 'car (cdr (cl-struct-slot-info typ))))))
   "\n"))


(defun wakatime-record-save-heatbeat ()
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
