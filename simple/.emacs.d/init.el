;; -*- lexical-binding: t -*-
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-common-dir (or (getenv "OUR_EMACS") nil))
(setq user-before-file (if user-common-dir (format "%s/before.el" user-common-dir) nil))
(setq user-additional-file (if user-common-dir (format "%s/additional.el" user-common-dir) nil))
(setq user-after-file (if user-common-dir (format "%s/after.el" user-common-dir) nil))
(when (file-exists-p user-before-file) (load-file user-before-file))
(when (file-exists-p user-additional-file) (load-file user-additional-file))
(when (file-exists-p user-after-file)
  (add-hook 'emacs-startup-hook (lambda () (load-file user-after-file))))

