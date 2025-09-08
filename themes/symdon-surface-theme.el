;;; symdon-surface-theme --- Emacs transpalent surface theme.  -*- lexical-binding: t; -*-
(deftheme symdon-surface "Display surface")

(custom-theme-set-faces
 'symdon-surface
 '(default ((t (:background "#000000" :foreground "#ffffff")))))

(set-face-attribute 'default nil :height 200)
(set-frame-parameter nil 'alpha '(40 . 40))

(provide 'symdon-surface)
;;; symdon-surface-theme.el ends here
