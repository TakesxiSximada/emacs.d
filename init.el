(setq find-function-C-source-directory "/opt/ng/emacs/src")
(setq debug-on-error t)

(setq custom-theme-directory (expand-file-name "~/.emacs.d/themes"))
(load-theme 'sximada-dark t)
(toggle-frame-fullscreen)

;; -----------------------------
;; package.el
;; -----------------------------
(setq package-user-dir (expand-file-name "~/.elpa")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  			 ("melpa" . "https://melpa.org/packages/")
  			 ("org" . "https://orgmode.org/elpa/")
  			 ("melpa-stable" . "http://stable.melpa.org/packages/")
  			 ;; marmalade is already not mainted
  			 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
  			 ))
(package-initialize)

;; -----------------------------
;; Other packaging library
;; -----------------------------
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package quelpa :ensure t :defer t)
(use-package quelpa-use-package :ensure t :defer t)
(use-package el-get :ensure t :defer t
  :init
  (setq el-get-dir (expand-file-name "/opt/ng")))

;; -----------------------------
;; Base Key binding
;; ----------------------------
(bind-key* "C-t" nil)

(bind-key* "<f1>" #'start-kbd-macro)
(bind-key* "<f2>" #'end-kbd-macro)
(bind-key* "<f3>" #'call-last-kbd-macro)
(bind-key* "<f4>" #'insert-kbd-macro)
(bind-key* "C-c C-w" 'comment-or-uncomment-region)
(bind-key* "C-h" #'backward-delete-char-untabify)
(bind-key* "C-t C-h" 'windmove-left)
(bind-key* "C-t C-j" 'windmove-down)
(bind-key* "C-t C-k" 'windmove-up)
(bind-key* "C-t C-l" 'windmove-right)
(bind-key* "C-t h" 'windmove-left)
(bind-key* "C-t j" 'windmove-down)
(bind-key* "C-t k" 'windmove-up)
(bind-key* "C-t l" 'windmove-right)
(bind-key* "C-x C-w" 'kill-buffer)

;; -----------------------------
;; Emacs UI
;; -----------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------
;; Misc
;; -----------------------------
(setq make-backup-files nil
      auto-save-default nil
      custom-file (locate-user-emacs-file "custom.el")
      )

;; ------------------------------
;; environment variable utilities
;; ------------------------------
(defun getenv+ (name)
  "環境変数から値を取得し、さもなければシンボルから値を取得する"
  (or (getenv name)
      (symbol-value (intern-soft name))))
;; -----------------------------
;; wakatime
;; -----------------------------
;; (setq wakatime-response-buffer nil)

;; (defun restclient-http-handle-response (status method url bufname raw stay-in-window)
;;   "Switch to the buffer returned by `url-retreive'.
;; The buffer contains the raw HTTP response sent by the server."
;;   (setq restclient-within-call nil)
;;   (setq restclient-request-time-end (current-time))
;;   (if (= (point-min) (point-max))
;;       (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
;;     (when (buffer-live-p (current-buffer))
;;       (with-current-buffer (restclient-decode-response
;;                             (current-buffer)
;;                             bufname
;;                             restclient-same-buffer-response)
;;         (run-hooks 'restclient-response-received-hook)
;;         (unless raw
;;           (restclient-prettify-response method url))
;;         (buffer-enable-undo)
;; 	(restclient-response-mode)
;;         (run-hooks 'restclient-response-loaded-hook))))
;;   (current-buffer))

;; (defun wakatime-update-response-buffer ()
;;   (setq wakatime-response-buffer (current-buffer)))

;; (setq waka-work-type-list
;;       '("browsing"
;;         "building"
;;         "code reviewing"
;;         "coding"
;;         "debugging"
;;         "designing"
;;         "indexing"
;;         "learning"
;;         "manual testing"
;;         "meeting"
;;         "planning"
;;         "researching"
;;         "running tests"
;;         "writing docs"
;;         "writing tests"
;;         ))

;; (setq org-waka-work-type-property-name "WAKATIME_WORK_TYPE")

;; (defun org-waka-set-work-type (work-type)
;;   (interactive (list (completing-read "WORK TYPE: "
;; 				      waka-work-type-list)))
;;   (org-set-property org-waka-work-type-property-name work-type))


;; (defun waka-get-category ()
;;   (interactive)
;;   (if-let ((current-task-buffer (org-clock-is-active)))
;;       (with-current-buffer current-task-buffer
;; 	(save-excursion
;; 	  (goto-char (marker-position org-clock-marker))
;; 	  (cdr (assoc org-waka-work-type-property-name (org-entry-properties)))))
;;     "planning"))

;; (defun waka-get-entity ()
;;   (interactive)
;;   (buffer-name))

;; (defun waka-get-language ()
;;   (interactive)
;;   major-mode)


;; (defun wakatime-send-heatbeat ()
;;   (interactive)
;;   (with-current-buffer (find-file-noselect
;; 			(expand-file-name "~/.emacs.d/wakatime.http"))
;;     (if (buffer-live-p wakatime-response-buffer)
;; 	(let ((kill-buffer-query-functions nil))
;; 	  (kill-buffer wakatime-response-buffer))
;;       (setq wakatime-response-buffer (restclient-http-send-current-stay-in-window)))))

;; (add-hook 'restclient-response-loaded-hook 'wakatime-update-response-buffer)
;; (setq wakatime-timer (run-with-idle-timer 20 t 'wakatime-send-heatbeat))
;; (define-key org-mode-map (kbd "C-c C-x C-w") #'org-waka-set-work-type)


;; -------------------------
;; Load README configuration
;; -------------------------
(require 'org)

(save-window-excursion
  ;; Almost the same as org-babel-load-file, But the tangled filenames
  ;; are added dot to prefix for make it easier to choose in dired.
  (let ((tangled-file (org-babel-tangle-file
		       (expand-file-name "README.org" user-emacs-directory)
		       ".README.el"
		       "emacs-lisp\\|elisp")))
    (load-file (car tangled-file))))

;; -------------------------
;; Load README configuration
;; -------------------------
(use-package dockerfile-mode :ensure t :defer t)

(require 'dockerfile-mode)

(defun dockerfile-get-docker-image-from-inbuffer ()
  "# iamge: DockerImageName"
  (interactive)
  (let ((image-name-line (save-excursion
		      (goto-char (point-min))
		      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (s-trim (car (cdr (s-split ":" image-name-line))))))


(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (ido-completing-read "Image name: "
		       dockerfile-image-name-history
		       nil nil nil nil
		       (dockerfile-get-docker-image-from-inbuffer)))


(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If prefix arg NO-CACHE is set, don't cache the image.
The build string will be of the format:
`sudo docker build --no-cache --tag IMAGE-NAME --build-args arg1.. -f filename directory`"
  (interactive (list (dockerfile-read-image-name)
		     (not (y-or-n-p "Using cache?"))))
  (save-buffer)
  (vterm-command
   (format
    "%s%s build --ssh=default %s %s %s -f %s %s"  ;; FIX
    (if dockerfile-use-sudo "sudo " "")
    dockerfile-mode-command
    (if no-cache "--no-cache" "")
    (dockerfile-tag-string image-name)
    (dockerfile-build-arg-string)
    (shell-quote-argument (dockerfile-standard-filename (buffer-file-name)))
    (shell-quote-argument (dockerfile-standard-filename default-directory)))
   default-directory))
;; nil
   ;; (lambda (_) (format "*docker-build-output: %s *" image-name))))


(define-key dockerfile-mode-map (kbd "C-c C-c") #'dockerfile-build-buffer)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; load custom file.
(load-file custom-file)

;; Setup PATH environment variable
(setenv "PATH" (string-join exec-path ":"))
