(setq sound-play-macos-afplay-executable "afplay")

(defun sound-play-macos-afplay (file &optional repeat)
  (interactive (list
		(read-file-name  "Audio file: ")
		(yes-or-no-p "Repeat?: ")))
  (if repeat
      (async-shell-command
       (format "while true; do %s %s; sleep 1; done"
	       sound-play-macos-afplay-executable file))
    (start-process
     "*SOUND PLAY*" nil sound-play-macos-afplay-executable file)))

(provide 'sound)
