;;; magh --- the MAgical GitHub command line interface for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Version: 1
;; Package-Version: 20220601.0000
;; Package-Requires: ((emacs "27.1"))
;; Date: 2022-06-01

;; This file is part of magh.

;; magh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; magh is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

;;; Customization
(defgroup magh nil
  "the MAgical GitHub command line interface for Emacs."
  :prefix "magh-"
  :group 'tools
  :link '(url-link :tag "Source" "https://github.com/TakesxiSximada/emacs.d/blob/main/lisp/magh.el"))

(defcustom magh-gh-executable "gh"
  "Executables of gh command."
  :type 'string)

;;;###autoload
(defun magh ()
  (interactive)
  nil)


;;;###autoload
(defun magh-pr-list ()
  (interactive)
  (let ((vterm-shell "gh pr list")
	(vterm-buffer-name "*Github*")
	(vterm-kill-buffer-on-exit nil))
    (vterm)))

(provide 'magh)
;;; magh.el ends here
