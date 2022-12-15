(defun calendar-google ()
  (interactive)
  (async-shell-command "gcalcli agenda $(date +'%Y-%m-%dT00:00:00') $(date +'%Y-%m-%dT23:59:59') --tsv"))

(provide 'calendar-google)
