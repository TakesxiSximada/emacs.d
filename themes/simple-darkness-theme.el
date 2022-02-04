;;; simple-darkness-theme.el --- In the beginning there was darkness.

;; Copyright (C) 2011-2021 TakesxiSximada

;; Author: TakesxiSximada
;; Keywords: lisp, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This theme created from tsdh-dark base.

(require 'solarized)
(deftheme simple-darkness "The simple-darkness colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'simple-darkness
  '((base03 . "#000000")
    (base02 . "#0b0b0b")
    (base01 . "#525252")
    (base00 . "#5e5e5e")
    (base0 . "#7c7c7c")
    (base1 . "#8b8b8b")
    (base2 . "#eeeeee")
    (base3 . "#ffffff")
    (yellow . "#dbb32d")
    (orange . "#e67f43")
    (red . "#ed4a46")
    (magenta . "#eb6eb7")
    (violet . "#a580e2")
    (blue . "#368aeb")
    (cyan . "#3fc5b7")
    (green . "#70b433")
    (yellow-d . "#2c2512")
    (yellow-l . "#fcefd6")
    (orange-d . "#2e1d14")
    (orange-l . "#ffe5d8")
    (red-d . "#301714")
    (red-l . "#ffddd7")
    (magenta-d . "#2e1b25")
    (magenta-l . "#fee3f0")
    (violet-d . "#231d2c")
    (violet-l . "#eee5fa")
    (blue-d . "#171e2e")
    (blue-l . "#dfe6fc")
    (cyan-d . "#172825")
    (cyan-l . "#def4f0")
    (green-d . "#1c2512")
    (green-l . "#e4f0d6")
    (yellow-1bg . "#231d0f")
    (orange-1bg . "#251810")
    (red-1bg . "#261310")
    (magenta-1bg . "#24171e")
    (blue-1bg . "#141924")
    (cyan-1bg . "#13201e")
    (green-1bg . "#171e0f")
    (violet-1bg . "#1c1823")
    (yellow-1fg . "#ebc972")
    (orange-1fg . "#f4a57a")
    (red-1fg . "#fc867a")
    (magenta-1fg . "#f49ccc")
    (violet-1fg . "#c2a5eb")
    (blue-1fg . "#86abf2")
    (cyan-1fg . "#86d7cc")
    (green-1fg . "#9ecb73")
    (yellow-2bg . "#54451a")
    (orange-2bg . "#58331f")
    (red-2bg . "#5b241f")
    (magenta-2bg . "#582f46")
    (violet-2bg . "#413455")
    (blue-2bg . "#223758")
    (cyan-2bg . "#234b46")
    (green-2bg . "#30451b")
    (yellow-2fg . "#f1d490")
    (orange-2fg . "#f9b895")
    (red-2fg . "#ffa195")
    (magenta-2fg . "#f8b2d7")
    (violet-2fg . "#cfb8f0")
    (blue-2fg . "#a3bcf5")
    (cyan-2fg . "#a2e0d7")
    (green-2fg . "#b4d690"))
  'nil)
(provide-theme 'simple-darkness)
(provide 'simple-darkness-theme)
;; simple-darkness-theme.el ends here
