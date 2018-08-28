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


(require 'use-package)

(use-package slack
  :ensure t :defer t
  :commands (slack-start)
  :init
  ;; (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (mapc (lambda (args) (apply 'slack-register-team args)) our-secrets-slack-team-alist))

;; example
;; (slack-register-team
;;  :name "emacs-slack"
;;  :default t
;;  :client-id "aaaaaaaaaaa.00000000000"
;;  :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
;;  :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
;;  :subscribed-channels '(test-rename rrrrr)
;;  :full-and-display-names t)

;; (slack-register-team
;;  :name "test"
;;  :client-id "3333333333.77777777777"
;;  :client-secret "cccccccccccccccccccccccccccccccc"
;;  :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
;;  :subscribed-channels '(hoge fuga))

;; (evil-define-key 'normal slack-info-mode-map
;;   ",u" 'slack-room-update-messages)
;; (evil-define-key 'normal slack-mode-map
;;   ",c" 'slack-buffer-kill
;;   ",ra" 'slack-message-add-reaction
;;   ",rr" 'slack-message-remove-reaction
;;   ",rs" 'slack-message-show-reaction-users
;;   ",pl" 'slack-room-pins-list
;;   ",pa" 'slack-message-pins-add
;;   ",pr" 'slack-message-pins-remove
;;   ",mm" 'slack-message-write-another-buffer
;;   ",me" 'slack-message-edit
;;   ",md" 'slack-message-delete
;;   ",u" 'slack-room-update-messages
;;   ",2" 'slack-message-embed-mention
;;   ",3" 'slack-message-embed-channel
;;   "\C-n" 'slack-buffer-goto-next-message
;;   "\C-p" 'slack-buffer-goto-prev-message)
;;  (evil-define-key 'normal slack-edit-message-mode-map
;;   ",k" 'slack-message-cancel-edit
;;   ",s" 'slack-message-send-from-buffer
;;   ",2" 'slack-message-embed-mention
;;   ",3" 'slack-message-embed-channel))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
