(use-package pyvenv :ensure t :defer t)

(setenv "GIT_PAGER" "cat")  ;; Do not use the git command pager

(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (require 'pyvenv)
     (require 'our-pyvenv)
     (our-pyvenv-setup (expand-file-name "var/pyvenvs" (elenv-env-root-directory))))))
