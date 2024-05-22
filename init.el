(setq ng-path (if (file-directory-p "/opt/ng") "/opt/ng" "~/ng")
      ng-cache-dir (if (file-directory-p "/var/ng") "/var/ng" "~/cache/ng")
      ng-custom-file (file-name-concat ng-path "symdon/custom.el"))

;; customize platform
(progn
  (setq platform-configuration-file
	(expand-file-name
	 (format "~/.emacs.d/platform/%s.el" system-configuration)))
  (if (file-exists-p platform-configuration-file)
      (progn
	(message "Platform configuration file exists: %s" platform-configuration-file)
	(condition-case err (load-file platform-configuration-file) (error err)))
    (warn "No platform configuration file: %s" platform-configuration-file)))

;; load custom file
(when (file-exists-p ng-custom-file)
  (setq custom-file ng-custom-file)
  (message "cusotm file exists: %s" custom-file)
  (condition-case err (load-file custom-file) (error err)))

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

(progn
  ;; XperiaではなぜかC-SPCを入力したと判定されるまでに時間がかかるようだっ
  ;; た。さらにC-SPCではなくC-@とし扱われていた。しかたがないのでM-SPCと
  ;; C-t C-pに#'set-mark-commandを割り当てる事にした。
  ;; いつか直したい。
  (global-set-key (kbd "M-SPC")  #'set-mark-command)
  (global-set-key (kbd "C-t C-p") #'set-mark-command))
