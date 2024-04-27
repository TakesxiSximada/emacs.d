(pcase system-configuration
  ("x86_64-apple-darwin22.6.0"
   (message "GNU Emacs on macOS")
   (load-file (expand-file-name "~/.emacs.d/init-darwin.el")))

  ("aarch64-unknown-linux-android"
   (message "GNU Emacs on Android")
   (load-file (expand-file-name "~/.emacs.d/init-android.el")))

  (t
   (warn "Unkown system")))

;; org-mode
(defun our-org-todo (&optional todo)
  (interactive
   (list (completing-read "Status: " (cdr (car org-todo-keywords)))))
  (org-todo todo))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") #'our-org-todo)
  (define-key org-mode-map (kbd "C-c C-,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "C-c ,") #'org-insert-structure-template)

  ;; org-clock
  (define-key org-mode-map (kbd "M-i") #'org-clock-in)
  (define-key org-mode-map (kbd "M-o") #'org-clock-out)
  )

;; org-agenda
(defun our-org-agenda-todo (&optional todo)
  (interactive
   (list (completing-read "Status: " (cdr (car org-todo-keywords)))))
  (org-agenda-todo todo))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M-p") #'org-agenda-priority-up)
  (define-key org-agenda-mode-map (kbd "M-n") #'our-org-agenda-todo)

  (define-key org-agenda-mode-map (kbd "M-i") #'org-agenda-clock-in)
  (define-key org-agenda-mode-map (kbd "M-o") #'org-agenda-clock-out))

(global-set-key (kbd "C-t C-q") #'org-agenda-list)
;; deepl
(global-set-key (kbd "C-t C-r") #'deepl)
(global-set-key (kbd "C-t C-e") #'openai-chat-question)
