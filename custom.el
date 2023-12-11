(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("25fd8ff5839b30a105cee5998dd03ad11eb020bb0580fa9465f3a684475cd7e2"
     "3216fa3c524213bdb35794b0dfaeb675518d4ccdbb7c54099d5a389f403f778a"
     "09a8d5bfc866d1b9b06b1f3887ce58e66b2695d2ffda6544bfc387e5e0adb230"
     "08a8ebdf8f7e4cf409758b31cad551d3853070a8404b6ddc0d36526a2dfec8bd"
     "3c8d2998c29afc102af21639c866ab933f050d70fdc3da6be47347dbe060e7e5"
     "e69e879542a3dc5f241dcb4d4798f50e2d5a737ec575530c17ccaf37e141d05a"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077"
     "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659"
     "47d17213c927e5d4b3508026a27c472998450e220fb8ce06b33bb5f78becd252"
     "b99bdd148cf7468b34ff1cf048b11ee34e4044903eadc1e51b61906ea212e0cc"
     "994b969d5ee8f235f2e466902a8cd4a5b610d99d54728891a1bbfe8a8c9220a1"
     "840c1572a7b034d5629a4189589a1707f988b8ad0fb4836f55f44132347ab4e8"
     "3bb4c0af5272fa8e9890e3a1cd0c33d0eae2b55f0a91084447b107cf52a75e5d"
     "b9e32e6195a5d43988aeaa7a64258ca157d25a59446aaae143e9cc12294d6ec5"
     "4b1dcde784b856bdc05830501b27291c5a5a74e8daacc09c0e096cb5f250c2a9"
     "154181b1295df2e8ffd0ec53e00c8cd9b0c861e414b72ee5b263de4e99b1047b"
     "4001810ad4b1535ffa9315b6092401df2151039c38ced22fcc1f85a8a8be12c2"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "c376e68aa20c8648d8b797cdf62e067761721e0b5d365f7957ad5a17504b1e61"
     "ecd6fd761d40e92686bd1599df3d97a04a2ee05b901da0fea4d595ca52a0ca0f"
     "7e117696caf3ae65ff1badeb2b0cec3be2a433e748cd10b85099e2c7ef098f5d"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "e704d8997ed724206e11721a9297a5b7d909301b238768ad17431b70f19ccc62"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default))
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
   '("slime" vline magit cider swift3-mode python-mode ox-pandoc focus
     python-coverage coverage yaml plz flymake-collection
     flymake-diagnostic-at-point hide-lines lua-mode visual-fill-column
     python-isort hmac indent-guide edit-indirect wgrep compat quelpa anaphora
     eglot magit-popup kubernetes eslint-fix flycheck-mypy ob-http
     add-node-modules-path adoc-mode ag avy-menu blacken company csv-mode db
     ddskk dired-filter docker docker-compose-mode dockerfile-mode editorconfig
     el-get fakir flycheck flycheck-pos-tip github-review go-mode
     google-translate grip-mode haskell-mode http ido-completing-read+
     ido-vertical-mode js2-mode markdown-mode mew monky nginx-mode ob-async
     ob-restclient org-agenda org-agenda-property org-re-reveal org-super-agenda
     ox-gfm pcre2el py-isort pyvenv quelpa-use-package request s3ed slime smex
     solarized-theme terraform-mode twilight-bright-theme typescript-mode
     vagrant-tramp vterm vue-mode web wgrep-ag))
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
 '(focus-unfocused ((t (:inherit org-hide))))
 '(vertical-border ((t (:foreground "gray35"))))
 '(window-divider ((t (:foreground "gray35")))))
