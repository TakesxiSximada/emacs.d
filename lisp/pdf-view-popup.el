;;; pdf-view-popup
(require 'pdf-view)

(defun pdf-view-popup-text ()
  (interactive)
  (pdf-view-mark-whole-page)
  (pdf-view-kill-ring-save)
  (pdf-view-deactivate-region)
  (let ((buf (get-buffer-create "*PDF TEXT*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (yank)
    (goto-char (point-min))
    (display-buffer buf)))


(provide 'pdf-view-popup)
;;; pdf-view-popup.el ends here
