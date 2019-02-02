(load-file "~/.emacs.d/env.el")
(setq elenv-root-directory "/srv/")
(progn (add-to-list 'load-path "/srv/sallies/elenv/") (require 'elenv) (elenv-activate))  ;; elenv auto inser
(toggle-frame-fullscreen)
