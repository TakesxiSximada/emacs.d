;; wakatime-transport.el --- Yet Another Wakatime plugin for Emacs.

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

; wakatime-transport.el is Unofficial wakatime plugin for Emacs.  
; It was implemented based on a different design philosophy.
; wakatime-transport.el is an unofficial wakatime plugin for
; Emacs. Send the Heartbeat Journal to https://wakatime.com. This
; package does not save the heatbeat journal, wakatime-record.el does
; it instead.

; Code:

(require 'json)
(require 'seq)
(require 'timer)

(require 'restclient)

(defvar wakatime-transport-buffer-name "*WAKATIME TRANSPORT*")
(defvar wakatime-transport-file-path (expand-file-name "~/.wakatime.heartbeat.json"))
(defvar wakatime-transport-response-buffer nil)
(defvar wakatime-transport-restclient-file (expand-file-name
					    "~/.emacs.d/wakatime.heartbeat.bulk.http"))
(defvar wakatime-transport-timer nil)

(defun wakatime-transport-get-heartbeeats-cache ()
  (json-encode-array
   (mapcar #'json-parse-string
	   (seq-filter
	    (lambda (elt) (> (length elt) 0))
	    (split-string
	     (with-current-buffer (get-buffer-create wakatime-transport-buffer-name)
	       (buffer-substring-no-properties (point-min) (point-max)))
	     "\n")))))

(defun wakatime-transport-send-request ()
  (when (buffer-live-p wakatime-transport-response-buffer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer wakatime-transport-response-buffer)))

  (setq wakatime-transport-response-buffer
	(with-current-buffer (find-file-noselect wakatime-transport-restclient-file)
	  (restclient-http-send-current-stay-in-window))))


(defun wakatime-transport-load-heatbeats ()
  (let ((buf (get-buffer-create wakatime-transport-buffer-name)))
    (with-current-buffer buf (erase-buffer))

    (make-process
     :name "WAKATIME TRANSPORT"
     :buffer buf
     :command `("/usr/local/bin/gsed" "-i"
      		"-e" "1,10 w /dev/stdout"
    		"-e" "1,10d"
		,wakatime-transport-file-path))))

(defun wakatime-transport-heartbeats ()
  (let ((proc (wakatime-transport-load-heatbeats)))
    (set-process-sentinel
     proc (lambda (proc sig)
	    (wakatime-transport-send-request)))))


(defun wakatime-transport-turn-off ()
  (interactive)
  (when (timerp wakatime-transport-timer)
    (cancel-timer wakatime-transport-timer)))


(defun wakatime-transport-turn-on ()
  (interactive)
  (wakatime-transport-turn-off)
  (setq wakatime-transport-timer
	(run-with-idle-timer 60 t #'wakatime-transport-heartbeats)))

(provide 'wakatime-transport)
;;; wakatime-transport.el ends here
