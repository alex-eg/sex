;;; Sex fmt-c output writer

(declare (unit fmt-c-writer)
         (uses fmt-c
               semen
               utils))

(import (chicken string)
        brev-separate
        fmt
        matchable
        regex
        srfi-1                          ; lists
        srfi-13                         ; strings
        tree
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
    ((begin) '%block-begin)
    ((define) '%define)
    ((pointer) '%pointer)
    ((array) '%array)
    ((attribute) '%attribute)
    ((¤) 'vector-ref)
    ((include) '%include)
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

   ;; variable declaration inside function
   ((eq? (car form) 'var)
    (cons (walk-var form) acc))

   ;; (cast expr type) -> (%cast type expr)
   ((eq? (car form) 'cast)
    (cons (list '%cast
                (walk-type (drop form 2))
                (car (walk-generic (second form) (list))))
          acc))

   ;; toplevel, or a start of a regular list form
   (else
    (cons (fold-right
           walk-generic
           (list)
           form)
          acc))))

(define (walk-var form)
  ;; (var a int) -> (%var int a)
  ;; (var a (const int) 32) -> (%var (const int) a 32)
  ;; (var b [const char 512]) -> (%var (%array (const char) 512) b)
  ;; note: [...] is actually (¤ ...) after reading
  ;; (var c (fn ((int) (float)) void)) -> (%var (%fun void ((int) (float))) c)
  (append
   (list '%var)
   (list (walk-type (flatten (third form))))
   (list (atom-to-fmt-c (second form)))

   (if (null? (drop form 3))
       (list)
       (car (walk-generic (drop form 3) (list))))  ; optional init expression
   ))

(define (walk-type form)
  ;; int -> int
  ;; (const int) -> const int
  ;; [const char 512] ->(¤ (const char) 512) -> (%array (const char) 512)
  ;; [float 8] -> (%array float 8)
  ;; (* const char) -> (const char *)
  ;; (const * const * const char) -> (const char * const * const)
  ;; (fn ((int) (float)) void) -> (%fun void ((int) (float)))
  (match form
    (('¤ . array-type)
     (if (integer? (last array-type))
         ;; sized array
         (let ((type (drop-right array-type 1))
               (size (last array-type)))
           `(%array ,(walk-type (flatten type))
                    ,size))
         ;; sugar for pointer... Do we really need it? Guess why not,
         ;; it's a strong semantic cue
         `(%array ,(walk-type (flatten array-type)))))
    (('fn arglist ret-type)
     `(%fun ,(walk-type ret-type) ,(walk-arglist arglist)))

    ;; Special case: nested structs/unions
    (('struct ((field-names field-types) ...) . attrs)
     (append `(struct ,(process-struct-fields field-names field-types))
             (if (null? attrs)
                 (list)
                 (list (map atom-to-fmt-c attrs)))))
    (('union ((field-names field-types) ...) . attrs)
     (append `(union ,(process-struct-fields field-names field-types))
             (if (null? attrs)
                 (list)
                 (list (map atom-to-fmt-c attrs)))))

    (else
     (type-convert-to-c form))))

(define (type-convert-to-c type)
  ;; Our pointers to C pointers
  ;; int -> int
  ;; * const char -> const char *
  ;; const * const char -> const char * const
  (if (atom? type) (atom-to-fmt-c type)
      (tree-map atom-to-fmt-c
                (fold-right append (list)
                            (list-join (reverse (list-split type '*))
                                       '(*))))))

(define (walk-fn-def form)
  (match form
    (('fn name args ret-type . maybe-body)
     `(%fun
       ,(walk-type ret-type)
       ,(car (walk-generic name (list)))
       ,(walk-arglist args)
       .
       ,maybe-body))))

;;; Todo: isn't there a better way?
(define (is-probably-type form)
  (case (car form)
    ((¤ * const volatile struct union) #t)
    (else #f)))

(define (walk-arglist form)
  ;; E.g.:
  ;; ((float) (int) (const char) (* const char) (¤ (* const struct res) 32))
  ;; ((f1 float) (f2 float) (f3 float) (res (¤ float 4)))
  (map (fn
        (match x
          (('¤ . _) (walk-type x))

          ;; yeah shitty, but I don't know yet how to determine if the
          ;; first entry is part of the type and not an argument name
          ;; :(
          ((? is-probably-type) (walk-type x))

          ;; 1 element args are always type
          ((_) (walk-type x))

          ((var . type) (append (list (walk-type (if (= 1 (length type))
                                                     (car type)
                                                     type)))
                                (list (walk-type var))))))
       form))

(define (normalize-fn-form form)
  ;; (fn ret-type name arglist body) -> normal function
  ;; (fn ret-type name arglist) -> prototype
  (if (>= (length form) 5)
      (walk-fn-def form)
      (cons 'prototype (cdr (walk-fn-def form)))))

(define (walk-function form static)
  (if static
      (walk-generic (list 'static (normalize-fn-form form))
                    (list))
      (walk-generic (normalize-fn-form (cdr form))
                    (list))))

(define (process-struct-fields names types)
  (zip (map walk-type types) (map atom-to-fmt-c names)))

(define (walk-struct form)
  (match form
    ((type name ((field-names field-types) ...) . attrs)
     (list (append `(,type ,(atom-to-fmt-c name)
                           ,(process-struct-fields field-names field-types))
                   (if (null? attrs)
                       (list)
                       (list (map atom-to-fmt-c attrs))))))
    (else (error "Malformed aggregate definition " form))))

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
     (walk-generic (walk-var (cdr form)) (list)))
    ((define defmacro import include struct typedef union var)
     ;; ignore here, used in generating public interface
     (process-toplevel-form (cdr form)))
    (else
     (error "Pub what?" (cadr form)))))

(define (process-toplevel-form form)
  ;; todo: rewrite to match
  ;; for some reason should return list in a list; TODO: rewrite
  (case (car form)
    ((fn) (walk-function form #t))
    ((var) (walk-generic (list 'static (walk-var form)) (list)))
    ((extern) (walk-extern form))
    ((pub) (walk-public form))
    ((struct union) (walk-struct form))
    (else (walk-generic form (list)))))

(define (emit-c sex-forms)
  (for-each (lambda (form)
              (fmt #t (c-expr (car (process-toplevel-form form))) nl))
            sex-forms))
