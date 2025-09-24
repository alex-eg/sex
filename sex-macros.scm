(declare (unit sex-macros))

(import
  (chicken plist)
  (chicken string))

(define (cat-syms s-1 s-2)
  (import fmt)
  (fmt #f s-1 s-2))

(define (cat sym-1 sym-2)
  (string->symbol (cat-syms sym-1 sym-2)))

(define (register-macro name arglist body)
  (put! name 'sex-macro
        `(lambda ,arglist
           ,@body)))

(define (get-macro name)
  (eval (get name 'sex-macro)))

(define (sex-macro? form)
  (and (list? form)
       (symbol? (car form))
       (get (car form) 'sex-macro)))

(define (apply-macro form)
  (assert (sex-macro? form)
          (fmt #f (car form) " is not a macro"))
  (apply (get-macro (car form))
         (cdr form)))

(define (defmacro form)
  (let ((arglist (car form))
        (body (cdr form)))
    (register-macro (car arglist) (cdr arglist) body)))
