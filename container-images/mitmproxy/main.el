(defun my/mitmproxy (port-number)
  "Start MITMProxy"
  (interactive "nPort Number: ")
  (let ((vterm-shell
	 (format
	  "docker run --publish %d:8080 --volume $PWD:$PWD --workdir $PWD -it %s %s"
	  port-number "mitmproxy:202509" "bash"))
        (vterm-buffer-name
	 (format "*mitmproxy: %s: %s*" port-number (expand-file-name default-directory)))
        (vterm-kill-buffer-on-exit nil))
    (vterm)))
