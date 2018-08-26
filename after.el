;; -*- lexical-binding: t -*-
(setq user-common-after-file (buffer-file-name))

(defun our-bind-key (ch sym)
  (if (fboundp sym)
      (bind-key ch sym)
    (message (format "Need function: %s" sym))))

(defun our-toggle-open-init-file ()
  (interactive)
  (let ((name (buffer-file-name)))
    (cond ((equal name user-init-file) (find-file user-after-file))
	  ((equal name user-after-file) (find-file user-before-file))
	  ((equal name user-before-file) (find-file (or user-additional-file user-init-file)))
	  ((equal name user-additional-file) (find-file user-init-file))
	  ((equal name nil) (find-file user-init-file))
	  ((equal "" nil) (find-file user-init-file))
	  (t (find-file user-init-file)))))


(bind-keys* ("¥" . "\\")
	    ("C-h" . backward-delete-char-untabify)
	    ("C-x g" . find-grep)
	    ("C-x C-g" . goto-line)
	    ("C-x C-p" . list-processes)
	    ("C-c C-w" . comment-or-uncomment-region)

            ;; keyboard macro
            ("<f1>" . start-kbd-macro)
            ("<f2>" . end-kbd-macro)
            ("<f3>" . call-last-kbd-macro)
            ("<f4>" . name-last-kbd-macro)
            ("<f5>" . insert-kbd-macro)

	    ;; buffers
	    ("C-<backspace>" . kill-buffer)

	    ;; panes
	    ("C-t h" . windmove-left)
	    ("C-t j" . windmove-down)
	    ("C-t k" . windmove-up)
	    ("C-t l" . windmove-right)

            ;; panes size
            ("s-<left>" . shrink-window-horizontally)
            ("s-<down>" . enlarge-window)
            ("s-<up>" . shrink-window)
            ("s-<right>" . enlarge-window-horizontally)
	    
	    ;; command
            ("M-_" . async-shell-command)
	    ;; other
	    ("s-t" . (lambda () (interactive) (message "Oops!")))
            ("<f11>" . (lambda () (interactive) (find-file "~/Dropbox/tasks/sximada.org")))
	    ("<f12>" . our-toggle-open-init-file))

;; buffer
(our-bind-key "C-x C-b" 'helm-mini)
(our-bind-key "C-x b" 'helm-buffers-list)
(our-bind-key "C-x C-f" 'helm-find-files)
(our-bind-key "M-x" 'helm-M-x)

;; git
(our-bind-key "C-x C-v" 'magit-status)

;; elscreen
(our-bind-key "C-t C-c" 'elscreen-create)
(our-bind-key "C-t C-n" 'elscreen-next)
(our-bind-key "C-t C-p" 'elscreen-previous)
(our-bind-key "C-t C-l" 'helm-elscreen)
(our-bind-key "C-t C-w" 'elscreen-kill)

