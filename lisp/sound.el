(setq sound-play-macos-afplay-executable "afplay")

(defun sound-play-macos-afplay (file &optional repeat)
  (interactive (list
		(read-file-name  "Audio file: ")
		(yes-or-no-p "Repeat?: ")))
  (start-process "*SOUND PLAY*" nil sound-play-macos-afplay-executable file))

(provide 'sound)
