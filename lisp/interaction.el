;;; inteaction.el -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Repository: https://github.com/TakesxiSximada/emacs.d
;; Version: 1
;; Package-Version: 20221203.0000
;; Package-Requires: ((emacs "28.1"))
;; Date: 2022-12-03

;; Interaction mode.

;; interaction.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; interaction.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:
(setq interaction-output nil)

(setq interaction-prompt nil)


(setq interaction-overlay-timer nil)  ;; オーバーレイ表示関数のタイマーを保持する

(defun interaction-cancel-timer ()
  "Timerとして設定されているオーバーレイ表示をキャンセルする。

設定されていない場合は何もしない。
"
  (interactive)
  (when (and (boundp 'interaction-overlay-timer)
	     (timerp interaction-overlay-timer))
    (cancel-timer interaction-overlay-timer)))



(setq-local interaction-end-character "\n")

(defun interaction-process-filter (process output)
  (setq-local interaction-output (concat interaction-output output))
  (when interaction-overlay-timer (cancel-timer interaction-overlay-timer))
  (setq interaction-overlay-timer (run-at-time "1 sec" 1 #'interaction-display-overlay)))

(defun interaction-open-redis ()
  (setq-local interaction-process
	      (make-process  :name "*REDIS CLIENT*"
			     :buffer "*REDIS CLIENT*"
			     :command '("/usr/local/bin/redis-cli")
			     :filter #'interaction-process-filter)))


(setq interaction-alist '(("redis" . interaction-open-redis)))


(defun interaction-is-active (proc)
  (and proc
       (equal 'run (process-status proc))))

(defun interaction-close ()
  (interactive)
  (when (interaction-is-active interaction-process)
    (process-send-eof interaction-process)))

(defun interaction-substring-no-property ()
  (interactive)
  (concat
   (apply #'buffer-substring-no-properties
	  (if (region-active-p)
	      (list (region-beginning) (region-end))
	    (list (line-beginning-position) (line-end-position))))
   interaction-end-character))

(defun interaction-send-string (txt)
  (when (interaction-is-active interaction-process)
    (process-send-string interaction-process txt)))

(defun interaction-send-region ()
  (interactive)
  (setq interaction-output nil)
  (interaction-remove-overlays)
  (when (interaction-is-active interaction-process)
    (process-send-string interaction-process (interaction-substring-no-property))))



(defun interaction-detect-prompt ()
  (interactive)
  (when interaction-end-character
    (setq-local interaction-output nil)
    (interaction-send-string interaction-end-character)
    (sleep-for 1)
    (setq-local interaction-prompt interaction-output)))


(defun interaction-display-overlay ()
  (interactive)
  (when (s-contains-p interaction-prompt interaction-output)
    (when interaction-overlay-timer (cancel-timer interaction-overlay-timer))
    (let ((display-text (format
			 " => %s"
			 (string-trim
			  (string-replace interaction-prompt "" interaction-output)))))

      (overlay-put (make-overlay (point) (point))
		   'after-string
		   (propertize display-text
			       'face 'custom-modified
			       'display '())))))

(defun interaction-remove-overlays ()
  (interactive)
  (remove-overlays))

(global-set-key "\C-x\C-d" #'interaction-send-region)

(defun interaction (target)
  (interactive (list
		(ido-completing-read "Target"
   				     (mapcar #'car interaction-alist)
				     nil nil nil nil
				     "redis")))
  (setq-local interaction-output nil)
  (setq-local interaction-prompt nil)
  (setq-local interaction-overlay-timer nil)
  (setq-local interaction-end-character "\n")
  (funcall (cdr (assoc target interaction-alist))))

(provide 'interaction)
;;; interaction.el ends here
