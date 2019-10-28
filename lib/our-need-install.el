;; (our-need-install "jq package" "jq"
;; 		  :darwin "brew install jq"
;; 		  :windows-nt "ninst install jq")


(defmacro our-need-install (name test &rest install-docs)
  `(let ((installed? (cond ((booleanp ,test)
			    ,test)
			   ((stringp ,test)
			    (executable-find ,test))
			   (t
			    (eval ,test)))))
     (if (not installed?)
	 (message
	  (format "Need install: %s: %s" ,name
		  ,(plist-get install-docs (intern (concat ":" (symbol-name system-type)))))))))

(provide 'our-need-install)
