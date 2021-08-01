;;; yawakatime.el --- Yet Another Wakatime plugin for Emacs.

;; Copyright (C) 2021 TakesxiSximada <sximada@gmail.com>

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Website: https://wakatime.com
;; Keywords: calendar, comm
;; Package-Version: 20210730.240
;; Package-Commit: 5e6deddda7a70f4b79832e9e8b6636418812e162
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yawakatime is Unofficial wakatime plugin for Emacs.
;; It was implemented based on a different design philosophy.
;; Heartbeats are recorded as Line-delimited JSON in a temporary journal.

;;; Code:

(require 'cl-lib)
(require 'json)

(defcustom yawakatime-buffer-name "*WAKATIME HEARTBEATS*"
  "")

(defcustom yawakatime-heartbeat-file-path (expand-file-name "~/.wakatime.heartbeat.json")
  "")

(cl-defstruct yawakatime-heartbeat
  time
  entity
  type
  category
  is_write
  project
  branch
  language
  dependencies
  lines
  lineno
  cursorpos
  user_agentl
  )

(defvar yawakatime-record-timer nil)


(defun yawakatime-tunrn-on ()
  (interactive)
  (yawakatime-tunrn-off)
  (setq yawakatime-record-timer
	(run-with-idle-timer 20 t #'yawakatime-save-heartbeats)))			     


(defun yawakatime-tunrn-off ()
  (interactive)
  (when (timerp yawakatime-record-timer)
    (cancel-timer yawakatime-record-timer)))
    
(defun yawakatime-util-convert-struct-to-alist (instance)
  (let ((struct (type-of instance)))
    (mapcar (lambda (key)
	      (cons key
		    (cl-struct-slot-value struct key instance)))
	    (mapcar 'car (cdr (cl-struct-slot-info struct))))))

(defun yawakatime-serialize-heartbeat (heartbeat)
  (concat
   (json-encode-alist (convert-struct-to-alist heartbeat))
   "\n"))

(defun make-yawakatime-current-heartbeat ()
  (make-yawakatime-heartbeat
   :time (float-time)
   :user_agent "Emacs"))

(defun yawakatime-save-heartbeats ()
  (let ((serialized-heartbeat (yawakatime-serialize-heartbeat
			       (make-yawakatime-current-heartbeat))))
    (with-current-buffer (get-buffer-create yawakatime-buffer-name)
      (insert serialized-heartbeat)
      (write-region (point-min) (point-max)
		    yawakatime-heartbeat-file-path
		    t)
      (kill-buffer))))

(provide 'yawakatime)
;;; yawakatime.el ends here
