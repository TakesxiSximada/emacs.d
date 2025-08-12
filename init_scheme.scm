(display "
###############################################
#         HELLO LISP WORLD!!                  #
#                                             #
# This session is working on TinyScheme 1.42. #
###############################################
")

(define (type-of val)
  (cond
   ((boolean? val) 'boolean)
   ((integer? val) 'integer)
   ((string? val) 'string)
   ((symbol? val) 'symbol)
   ((pair? val) 'pair)
   ((null? val) 'null)
   ((procedure? val) 'procedure)
   (else 'unknown)))
