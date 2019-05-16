(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (require 'pyvenv)
     (require 'our-pyvenv)
     (our-pyvenv-setup (expand-file-name "var/pyvenvs" (elenv-env-root-directory))))))
