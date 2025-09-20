(declare
  (unit sex-types)
  (uses fmt-c))

(import fmt
        srfi-69)

(define +all-types+ (make-hash-table))

(define (normalize-type type)
  )

(define (add-pointer type)
  (list 'pointer type))

(define (add-const type)
  (list 'const type))

(define (get-types-from-arglist arglist)
  (list))

(define (to-c-type type)
  (fmt #f (c-type type)))
