;;; calendar-google.el --- Google Calendar for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Version: 1
;; Package-Version: 20221216.0000
;; Package-Requires: ((emacs "30"))
;; Date: 2022-12-16

;; This file is part of aws cli.el.

;; calendar-google.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; calendar-google.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

;;; Customization
(defun calendar-google ()
  (interactive)
  (async-shell-command "gcalcli agenda $(date +'%Y-%m-%dT00:00:00') $(date +'%Y-%m-%dT23:59:59') --tsv"))

(provide 'calendar-google)
;;; calendar-google.el ends here
