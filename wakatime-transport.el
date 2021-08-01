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
	(run-with-idle-timer 20 t #'wakatime-transport-heartbeats)))


(provide 'wakatime-transport)
