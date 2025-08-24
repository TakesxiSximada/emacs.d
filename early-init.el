;; -*- lexical-binding: t -*-

;; Copyright (C) 2016 - 2025 TakesxiSximada

;; Author: TakesxiSximada
;; URL: https://github.com/TakesxiSximada/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:
(setq-default byte-compile-warnings nil
	      debug-on-error t
	      eval-expression-debug-on-error t)
;; (push '(fullscreen . fullboth) default-frame-alist)

;; Disable GUI Menu
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;;; early-init.el ends here
