;;; make-possess-preset --- Make process preset of configuration. -*- lexical-binding: t -*-

;;                          ⢠⣤⡀
;;                          ⣛⠷⣴⣶⣶⢶⣤⣄ ⣀
;;                          ⠈⠙⠻⡏⠈⠛⠛⠾⣾⣽⣷⣠⡄
;;                       ⣀⣤⠤⠴⠖⠒⠒⠶⠦⢤⣀⡋⠐⠦⠟⠘⠶⠄
;;                    ⣠⠴⠛⠉         ⠈⠙⠲⢤⡀
;;                 ⢀⣴⠛⠁                ⠙⢦⡀
;;                ⢠⠞⠁                    ⠹⣄
;;               ⢠⠏   ⢀⣇⢠     ⣀⣀⡀         ⠘⣆
;;               ⡟    ⠈⠉⠉   ⢠⣾⣿⣿⣿⣷⡄  ⢀⣤⣤⡄⠠⣼⢽⡄
;;              ⢸⠁         ⢀⣾⣿⣿⣿⣿⣿⣿  ⣿⣿⣿⣿⣆  ⣇
;;              ⡟         ⢀⣘⣿⣿⣿⣿⣿⣿⣿⠰⢲⣿⣿⣿⣿⣿  ⣿
;;             ⣸⠁         ⠘⠟⠚⣿⣿⣿⣿⣿⠏ ⢸⣿⣿⣿⣿⣿  ⡟
;;            ⢠⡏            ⢺⣿⣿⣿⠛⠁⣀⣀⠈⠿⣿⣿⣿⣕⠇⢰⡇
;;           ⢀⡾              ⢳⡀   ⠻⡿⠁ ⠹⠿⠟⠋ ⢸⡇
;;          ⣠⠟⢀⡀              ⢻⡄           ⢸⡁
;;       ⢀⡴⠞⠁⣠⠏               ⠈⡇           ⡼⠉⠳⣄
;;    ⣀⡴⠞⠉  ⢰⣇⣠⠤⠒         ⠰⢦⣦⡤⠾⠃          ⢠⠇  ⠸⡇
;; ⢠⠖⠋⢁          ⢠⡦         ⠈⠁        ⡤  ⢀⡞⠲⣶⡶⠶⠃
;; ⠸⡆  ⠑⠢⢤⣀⡀    ⢀⡾⠁        ⢀        ⣇⢠⠇  ⣾⠁
;;  ⠹⣄     ⠉  ⣀⡴⢟⡼         ⣿ ⣠⠄   ⣤ ⠉⠉  ⡼⠁
;;   ⠈⠉⠑⠒⠒⠒⠒⠒⣻⠋ ⡎  ⣀⠄ ⢠⡼⠃  ⠉⠉⠁  ⢀⡾⠁   ⢀⡞⠁
;;          ⢰⠃  ⠉⠉⠉⠁ ⣠⠟       ⣠⡶⠋    ⢠⠞
;;          ⡏⢀       ⡏    ⢀⣀⣠⡾⠋ ⡀  ⢀⡴⠃
;;          ⣇⠈⠁⢀⣠⠤⠖⠒⠚⣇  ⣠⡴⠛⠁ ⢷⣀  ⣀⡾⠋
;;          ⠸⣦⠴⠋     ⠉⠙⠋⠁     ⠈⠛⠛⠁
;;           ⣀⣀  Image: https://emojicombos.com/ghost-ascii-art
;;
;; Copyright (C) 2022 TakesxiSximada

;; Author: TakesxiSximada <sximada@gmail.com>
;; Maintainer: TakesxiSximada <sximada@gmail.com>
;; Version: 1
;; Package-Version: 20220528.0000
;; Package-Requires: ((emacs "27.1"))
;; Date: 2022-05-28

;; This file is part of make-possess-preset.

;; make-possess-preset is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; make-possess-preset is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(defun make-possess-preset-of-spacemacs ()
  (interactive)
  (setq package-user-dir (format "%s.spacemacs" package-user-dir)
	spacemacs-start-directory (expand-file-name "~/.emacs.d/distributions/spacemacs/")
	spacemacs-bootstrap-file (file-name-concat spacemacs-start-directory "init.el")
	custom-file (locate-user-emacs-file "custom-spacemacs.el")
	)
  (setenv "SPACEMACSDIR" (expand-file-name "~/.emacs.d/spacemacs.d/"))
  (package-initialize t)
  (load spacemacs-bootstrap-file nil nil))

;;;###autoload
(defun make-possess-preset-of-doom-emacs ()
  (interactive)
  (setenv "EMACSDIR" (expand-file-name "~/.emacs.d/distributions/doom-emacs/"))
  (setenv "DOOMLOCALDIR" (expand-file-name "~/.emacs.d/doom.d/"))
  (setenv "DOOMDIR" (expand-file-name "~/.emacs.d/doom.d/"))

  (setq package-user-dir (format "%s.doom-emacs" package-user-dir)
	custom-file (locate-user-emacs-file "custom-doom-emacs.el")
	)

  (setq user-emacs-directory (expand-file-name "~/.emacs.d/distributions/doom-emacs/"))
  (load (concat user-emacs-directory "core/core") nil 'nomessage)
  (load (expand-file-name "~/.emacs.d/distributions/doom-emacs/init.el") nil 'nomessage)
  (switch-to-buffer (get-buffer "*doom*")))

(provide 'make-possess-preset)
;;; make-possess-preset.el ends here
