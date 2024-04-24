(pcase system-configuration
  ("x86_64-apple-darwin22.6.0"
   (message "GNU Emacs on macOS")
   (load-file (expand-file-name "~/.emacs.d/init-darwin.el")))

  ("aarch64-unknown-linux-android"
   (message "GNU Emacs on Android")
   (load-file (expand-file-name "~/.emacs.d/init-android.el")))

  (t
   (warn "Unkown system")))
