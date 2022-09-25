(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e704d8997ed724206e11721a9297a5b7d909301b238768ad17431b70f19ccc62" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default))
 '(fill-column 80)
 '(gnus-select-method '(nnml ""))
 '(mail-sources
   `((pop :server ,(osx-keychain-get "gnus.server" user-login-name)
	  :port ,(string-to-number
		  (osx-keychain-get "gnus.port" user-login-name))
	  :user ,(osx-keychain-get "gnus.user" user-login-name)
	  :password ,(osx-keychain-get "gnus.password" user-login-name)
	  :stream ssl)))
 '(package-selected-packages
   '(visual-fill-column python-isort hmac indent-guide edit-indirect wgrep compat quelpa anaphora eglot magit-popup kubernetes eslint-fix flycheck-mypy ob-http add-node-modules-path adoc-mode ag avy-menu blacken company csv-mode db ddskk dired-filter docker docker-compose-mode dockerfile-mode editorconfig el-get fakir flycheck flycheck-pos-tip github-review go-mode google-translate grip-mode haskell-mode http ido-completing-read+ ido-vertical-mode js2-mode markdown-mode mew monky nginx-mode ob-async ob-restclient org-agenda org-agenda-property org-re-reveal org-super-agenda ox-gfm pcre2el py-isort pyvenv quelpa-use-package request s3ed slime smex solarized-theme terraform-mode twilight-bright-theme typescript-mode unicode-escape vagrant-tramp vterm vue-mode web wgrep-ag))
 '(skk-jisyo-edit-user-accepts-editing t)
 '(warning-suppress-log-types '((initialization) (initialization) (use-package)))
 '(warning-suppress-types '((initialization) (use-package)))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places 'bottom-only)
 '(window-divider-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "Red" :foreground "alternatingContentBackgroundColor"))))
 '(flycheck-info ((t (:underline (:color "selectedTextBackgroundColor" :style wave)))))
 '(flycheck-warning ((t (:background "systemOrangeColor" :foreground "alternateSelectedControlTextColor"))))
 '(hl-line ((t (:inherit highlight :extend t :background "#000066"))))
 '(vertical-border ((t (:foreground "gray35"))))
 '(window-divider ((t (:foreground "gray35")))))
