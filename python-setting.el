(autoload 'python-mode "python-mode" "Python editing mode." t)

(custom-set-variables
 '(py-indent-offset 4)
 )

(add-hook 'python-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq indent-tabs-mode nil)

             )
          )

(setq py-shell-name "ipython")

;; ** for python **
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (c-set-offset 'statement-cont '++)
;;             (c-set-offset 'arglist-intro '+)
;;             (c-set-offset 'arglist-close 0)
;;             (define-key python-mode-map "\"" 'electric-pair)
;;             (define-key python-mode-map "\'" 'electric-pair)
;;             (define-key python-mode-map "(" 'electric-pair)
;;             (define-key python-mode-map "[" 'electric-pair)
;;             (define-key python-mode-map "{" 'electric-pair)))
;; (defun electric-pair ()
;;   "Insert character pair without sournding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;     (insert-pair)))


;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (define-key python-mode-map "\C-m" 'newline-and-indent)))

(bind-keys* :map python-mode-map
           ("\C-j" . newline-and-indent)
           )

;; flake8
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
; (setq flymake-python-pyflakes-extra-arguments '("--ignore=W806"))
; (custom-set-variables
                                        ;  '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128"))))


;; flycheck
; (require 'python)
; (defun tnoda/turn-on-flycheck-mode ()
;   (flycheck-mode 1))
; (add-hook 'python-mode-hook 'tnoda/turn-on-flycheck-mode)


;; keyboard macros
;; (fset 'py-script-scaffold
;;    "\C-[<#! /sr\C-?\C-?usr/bin/env python\C-j# -*- coding: utf-8 -:-\C-b\C-b\C-d*\C-e\C-jimport\C-a\C-k\"\"\"\C-j\C-e\C-j\C-jimport sys\C-jimport argparse\C-j\C-j\C-jdef main(argv=sys.argv[1\C-e\C-b\C-b\C-b\C-f:\C-e:\C-mparser = argparse.ArgumentParser(\C-e\C-j\C-iargs = parser.parse_args(argv\C-e\C-j\C-j\C-j\C-aif __name__ == ''\C-b\C-d\C-d__main__\C-e:\C-[b\C-[f\C-f\C-k:\C-mj\C-?main(\C-e\C-j")
;; (define-key global-map [f12] 'py-script-scaffold)
