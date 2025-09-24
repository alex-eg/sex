;;; Sex fmt-c output writer

(declare (unit fmt-c-writer)
         (uses fmt-c
               semen))

(import (chicken string)
        brev-separate
        fmt
        regex
        srfi-1                          ; lists
        srfi-13                         ; strings
        )

(define (unkebabify sym)
  (case sym
    ((-) sym)
    ((--) sym)
    ((->) sym)
    ((-=) sym)
    (else
     (string->symbol
      (string-substitute "-(?!>)" "_"
                         (symbol->string sym) #t)))))

(define (atom-to-fmt-c atom)
  (case atom
    ((fn) '%fun)
    ((prototype) '%prototype)
    ((var) '%var)
    ((begin) '%block-begin)
    ((define) '%define)
    ((pointer) '%pointer)
    ((array) '%array)
    ((attribute) '%attribute)
    ((@) 'vector-ref)
    ((include) '%include)
    ((cast) '%cast)
    ;; uh things we do for c89 compatibility
    ((bool) 'int)
    ((true) 1)
    ((false) 0)
    (else
     (if (symbol? atom)
         (unkebabify atom)
         atom))))

(define (make-field-access form)
  (assert
   (= 2 (length form)) "Wrong field access format")
  (unkebabify
   (string->symbol
    (fmt #f (cadr form) (car form)))))

(define (walk-generic form acc)
  (cond
   ((null? form) (cons '() acc))

   ;; vector, e.g. {}-initializer
   ((vector? form)
    (cons
     (list->vector
      (car (walk-generic (vector->list form) (list))))
     acc))

   ;; atom (hopefully)
   ((not (list? form)) (cons (atom-to-fmt-c form) acc))

   ;; another special case - field access
   ((and (symbol? (car form))
         (char=? #\. (string-ref (symbol->string (car form)) 0)))
    (cons (make-field-access form) acc))

   ;; toplevel, or a start of a regular list form
   (else
    (let ((new-acc (list)))
      (cons (fold-right
             walk-generic
             new-acc
             form)
            acc)))))

(define (normalize-fn-form form)
  ;; (fn ret-type name arglist body) -> normal function
  ;; (fn ret-type name arglist) -> prototype
  (if (>= (length form) 5)
      form
      (cons 'prototype (cdr form))))

(define (walk-function form static)
  (if static
      (walk-generic (list 'static (normalize-fn-form form))
                    (list))
      (walk-generic (normalize-fn-form (cdr form))
                    (list))))

(define (walk-extern form)
  (case (cadr form)
    ((fn)
     (list (cons 'extern (walk-function form #f))))
    ((var)
     (list (cons 'extern (walk-generic (cdr form) (list)))))
    (else (error "Extern what?"))))

(define (walk-public form)
  (case (cadr form)
    ((fn)
     (walk-function form #f))
    ((var)
     (walk-generic (list 'static (cdr form)) (list)))
    ((define defmacro import include struct typedef union var)
     ;; ignore here, used in generating public interface
     (process-toplevel-form (cdr form)))
    (else
     (error "Pub what?" (cadr form)))))

(define (process-toplevel-form form)
  ;; todo: rewrite to match
  (case (car form)
    ((fn) (walk-function form #t))
    ((extern) (walk-extern form))
    ((pub) (walk-public form))
    (else (walk-generic form (list)))))

(define (emit-c sex-forms)
  (for-each (lambda (form)
              (fmt #t (c-expr (car (process-toplevel-form form))) nl))
            sex-forms))
