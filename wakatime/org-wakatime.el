(defvar org-wakatime-category-property-name "WAKATIME_CATEGORY")

(defun org-wakatime-set-category (wakatime-category)
  (interactive (list (completing-read
		      "WAKATIME_CATEGORY: "
		      (delete-dups (mapcar #'cdr wakatime-record-category-alist)))))
  (org-set-property org-wakatime-category-property-name
		    wakatime-category))

(defun org-wakatime-get-category ()
  (interactive)
  (if-let ((current-task-buffer (org-clock-is-active)))
      (with-current-buffer current-task-buffer
	(save-excursion
	  (goto-char (marker-position org-clock-marker))
	  (cdr (assoc org-wakatime-category-property-name (org-entry-properties)))))))

(provide 'org-wakatime)

