;; My Configuration

;; (pcase system-name
;;   ("ng7.local"
;;    1)
;;   ("localhost"
;;    2))


(if (string-equal system-name "ng7.local") ;; main machine
    (custom-set-variables
     '(exec-path
       '("/Applications/KeePassXC-2.7.6.app/Contents/MacOS" "/opt/X11/bin"
	 "/opt/ng/minetest/bin" "/opt/ng/symdon/bin" "/Users/sximada/.rbenv/shims"
	 "/opt/ng/symdon/pages/posts/1697247010/bin"
	 "/Applications/Emacs30-202402.app/Contents/MacOS/bin"
	 "/Users/sximada/Library/Android/sdk/emulator" "/Users/sximada/.tiup/bin"
	 "/Users/sximada/google-cloud-sdk/bin"
	 "/Users/sximada/development/flutter/bin"
	 "/Users/sximada/Library/Python/.bin"
	 "/Users/sximada/Library/Android/sdk/platform-tools"
	 "/Users/sximada/.whalebrew-bin/bin" "/Users/sximada/.poetry/bin"
	 "/Users/sximada/.nvm/versions/node/v8.15.0/bin" "/Users/sximada/.local/bin"
	 "/Users/sximada/.goenv/shims" "/Users/sximada/.goenv/bin"
	 "/Users/sximada/.emacs.d/whalebrew" "/Users/sximada/.emacs.d/bin"
	 "/Users/sximada/.cargo/bin" "/usr/local/texlive/2021/bin/universal-darwin"
	 "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"
	 "/usr/local/opt/openjdk/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin"
	 "/sbin" "/Applications/Emacs30-202402.app/Contents/MacOS/libexec"
	 "/Applications/Docker.app/Contents/Resources/bin"))))

;; Emacs Custom COnfiguration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-tooltip-idle-delay 0.5)
 '(epg-gpg-program "/usr/local/opt/gnupg/bin/gpg")
 '(org-agenda-files '("~/.emacs.d/doc/test.org"))
 '(org-agenda-prefix-format
   '((agenda .
	     "%4.4(org-entry-get-with-inheritance \"ASSIGNEE\") %-5.5c %-15.15b")
     (todo . " %i %-12:c") (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-global-properties '(("Effort_ALL" . "5 13 21 34 55 89 144 233 377 610 987")))
 '(org-startup-indented t)
 '(org-todo-keywords
   '((sequence "TODO" "EPIC" "MEET" "WAIT" "|" "DONE" "CANCEL")))
 '(package-selected-packages
   '(visual-fill-column smex company ido-completing-read+
			ido-vertical-mode magit ddskk))
 '(select-enable-clipboard nil)
 '(skk-cursor-default-color "gray")
 '(skk-use-color-cursor nil)
 '(ssh-keys
   '(("~/.ssh/id_rsa" . "~/.ssh/id_rsa.pub")
     ("~/.ssh/id_ed25519" . "~/.ssh/id_ed25519.pub")))
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white"))))
 '(flycheck-error ((t (:background "Red" :foreground "alternatingContentBackgroundColor"))))
 '(flycheck-info ((t (:underline (:color "selectedTextBackgroundColor" :style wave)))))
 '(flycheck-warning ((t (:background "systemOrangeColor" :foreground "alternateSelectedControlTextColor"))))
 '(focus-unfocused ((t (:inherit org-hide))))
 '(vertical-border ((t (:foreground "gray35"))))
 '(window-divider ((t (:foreground "gray35")))))
