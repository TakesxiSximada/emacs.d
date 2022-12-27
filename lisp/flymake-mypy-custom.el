;;; flymake-mypy-custom -*- lexical-binding: t -*-
(require 'flymake-collection-define)
(require 'flymake-collection-mypy)

(defcustom flymake-mypy-custom-executable "mypy"
  "Mypy command")

(flymake-collection-define-rx flymake-mypy-custom
  "Custom Mypy Check"
  :title "mypy"
  :pre-let ((mypy-exec flymake-mypy-custom-executable)
            (default-directory (flymake-collection-mypy--default-directory
                                flymake-collection-source)))
  :pre-check (progn
               (flymake-log :debug "Working dir is %s" default-directory)
               (unless mypy-exec
                 (error "Cannot find mypy executable"))
               (unless default-directory
                 (error "Default dir is nil: check `flymake-collection-mypy-project-root'")))
  :write-type 'file
  :source-inplace t
  :command `(,mypy-exec
             "--show-column-numbers"
             "--show-absolute-path"
             "--no-error-summary"
             "--no-color-output"
             ,@flymake-collection-mypy-args
             ,@(if-let ((source-file (buffer-file-name
                                      flymake-collection-source))
                        ((file-exists-p source-file)))
                   (list
                    "--shadow-file" source-file flymake-collection-temp-file
                    source-file)
                 (list flymake-collection-temp-file)))
  ;; Temporary file cannot start with a dot for mypy, see
  ;; https://github.com/mohkale/flymake-collection/pull/9
  :temp-file-prefix "flymake_mypy_"
  :regexps
  ((error   bol (file-name) ":" line ":" column ": error: "
            (message) (opt "  [" (id (* graph)) "]") eol)
   (warning bol (file-name) ":" line ":" column ": warning: "
            (message) (opt "  [" (id (* graph)) "]") eol)
   (note    bol (file-name) ":" line ":" column ": note: "
            (message) (opt "  [" (id (* graph)) "]") eol)))

(provide 'flymake-mypy-custom)
;;; flymake-mypy-custom ends here
