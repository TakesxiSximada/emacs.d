(require 'vterm)

(defun prettier-buffer ()
  (interactive)
  (let ((vterm-shell (format "npx prettier -w %s" (buffer-file-name)))
	(vterm-buffer-name (format "*PRETTIER*"))
	(vterm-exit-functions (lambda (&optional eve)
				(revert-buffer t t t))))
    (vterm)))

(provide 'prettier-buffer)
