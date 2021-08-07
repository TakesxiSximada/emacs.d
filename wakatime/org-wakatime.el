;;; org-wakatime.el --- Wakatime Org Mode Support.

;; Copyright (C) 2021 TakesxiSximada <sximada@gmail.com>

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Website: https://github.com/TakesxiSximada/emacs.d/master/wakatime/
;; Keywords: calendar, comm
;; Package-Version: 20210730.240
;; Package-Commit: 5e6deddda7a70f4b79832e9e8b6636418812e162
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org-wakatime.el is an unofficial package related to wakatime.  It save
;; category for wakatimed in the org property. wakatime-record.el can
;; detect the current working category from there.

;;; Code:
(defvar org-wakatime-category-property-name "WAKATIME_CATEGORY")

(defun org-wakatime-set-category (wakatime-category)
  (interactive (list (completing-read
		      "WAKATIME_CATEGORY: "
		      (delete-dups (mapcar #'cdr wakatime-record-category-alist)))))
  (org-set-property org-wakatime-category-property-name
		    wakatime-category))

(defun org-wakatime-get-category ()
  (interactive)
  (if-let ((current-task-buffer (org-clock-is-active)))
      (with-current-buffer current-task-buffer
	(save-excursion
	  (goto-char (marker-position org-clock-marker))
	  (cdr (assoc org-wakatime-category-property-name (org-entry-properties)))))))

(provide 'org-wakatime)
;;; org-wakatime.el ends here
