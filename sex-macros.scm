(import
  scheme
  (only fmt fmt)
  (chicken base)
  (chicken plist)
  (chicken string))

(define (cat-syms s-1 s-2)
  (fmt #f s-1 s-2))

(define (cat sym-1 sym-2)
  (string->symbol (cat-syms sym-1 sym-2)))

;;; The reader keeps `;' comments as (comment "...") forms so they can
;;; be re-emitted into the generated C. In a macro body a comment
;;; should be a call which does nothing, hence this one
(define (comment . _)
  (void))

(define (register-macro name arglist body)
  (put! name 'sex-macro
        `(lambda ,arglist
           ;; A macro body is ordinary Scheme, evaluated at compile
           ;; time. It gets `cat' for building names, and read access to
           ;; the type database
           (import scheme
                   (scheme base)
                   (only sex-macros cat comment)
                   (only types get-type-info get-fields get-underlying-type
                         type-match map-fields))
           ,@body)))

(define (get-macro name)
  (eval (get name 'sex-macro)))

(define (macro? form)
  (and (list? form)
       (symbol? (car form))
       (get (car form) 'sex-macro)))

(define (apply-macro form)
  (assert (macro? form)
          (fmt #f (car form) " is not a macro"))
  (apply (get-macro (car form))
         (cdr form)))

(define (defmacro form)
  (let ((arglist (car form))
        (body (cdr form)))
    (register-macro (car arglist) (cdr arglist) body)))
