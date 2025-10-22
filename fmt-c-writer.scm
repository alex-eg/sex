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

(define (walk-generic-toplevel form)
  (cond ((atom? form) (atom-to-fmt-c form))
        ((list? form) (map walk-generic-toplevel form))
        (else (error "Malformed form " form))))

(define (field-access-form? form)
  (and (symbol? (car form))
       (char=? #\. (string-ref (symbol->string (car form)) 0))))

(define (walk-expr form)
  (match form
    ((? vector?)
     (list->vector
      (walk-expr (vector->list form))))
    ((? atom?)
     (atom-to-fmt-c form))
    ((? field-access-form?)
     (make-field-access form))
    (('var . _) (walk-var form))
    (('cast expr type) (list '%cast
                             (walk-type type)
                             (walk-expr expr)))
    ;; | is problematic... And c-or/bit-or/etc are actually
    ;; procedures, so we have to call the procedure itself
    (('c-or . rest) (apply c-or (map walk-expr rest)))
    (('c-bit-or . rest) (apply c-bit-or (map walk-expr rest)))
    (('c-bit-or= . rest) (apply c-bit-or= (map walk-expr rest)))
    (else (map walk-expr form))))

(define (walk-var form)
  ;; (var a int) -> (%var int a)
  ;; (var a (const int) 32) -> (%var (const int) a 32)
  ;; (var b [const char 512]) -> (%var (%array (const char) 512) b)
  ;; note: [...] is actually (¤ ...) after reading
  ;; (var c (fn ((int) (float)) void)) -> (%var (%fun void ((int) (float))) c)
  `(%var
    ,(walk-type (third form))
    ,(atom-to-fmt-c (second form))
    .
    ,(if (null? (drop form 3))
         (list)
         (walk-expr (drop form 3)))      ; optional init expression
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
         (let* ((type-list (drop-right array-type 1))
                (type (if (and (list? (car type-list))
                               (= 1 (length type-list)))
                          (car type-list)
                          type-list))
                (size (last array-type)))
           `(%array ,(walk-type type)
                    ,size))
         ;; sugar for pointer... Do we really need it? Guess why not,
         ;; it's a strong semantic cue
         `(%array ,(walk-type (if (and (list? (car array-type))
                                       (= 1 (length array-type)))
                                  (car array-type)
                                  array-type)))))
    (('fn arglist ret-type)
     `(%fun ,(walk-type ret-type) ,(walk-arglist arglist)))

    ;; Special case: nested structs/unions
    ((or ('struct . _)
         ('union . _)) (walk-struct form))

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
       ,(atom-to-fmt-c name)
       ,(walk-arglist args)
       .
       ,(walk-expr maybe-body)))))

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

(define (walk-function form)
  ;; (fn ret-type name arglist body) -> normal function
  ;; (fn ret-type name arglist) -> prototype
  (if (>= (length form) 5)
      (walk-fn-def form)
      (cons '%prototype (cdr (walk-fn-def form)))))

(define (process-struct-fields fields)
  (map (fn
        (let ((type (walk-type (last x))))
          (cons type (map atom-to-fmt-c (drop-right x 1)))))
       fields))

(define (walk-struct form)
  (match form
    ((type (fields ...) . attrs)        ; anonymous struct
     `(,type ,(process-struct-fields fields)
             . ,(tree-map atom-to-fmt-c attrs)))
    ((type name)                        ; simple 'struct whatever', like in variable def
     `(,type ,(atom-to-fmt-c name)))
    ((type name (fields ...) . attrs)
     `(,type ,(atom-to-fmt-c name)
             ,(process-struct-fields fields)
             . ,(tree-map atom-to-fmt-c attrs)))
    (else (error "Malformed aggregate definition " form))))

(define (walk-extern form)
  (match form
    (('fn . _)
     ;; extern function?.. What
     (list 'extern (walk-function form)))
    (('var . _)
     (list 'extern (walk-var form)))
    (else (error "Extern what?"))))

(define (walk-public form)
  (match form
    (('fn . _)
     (walk-function form))
    (('var . _)
     (walk-var form))
    ((or ('define . _)
         ('defmacro . _)

         ('import . _)
         ('include . _)

         ('struct . _)
         ('union . _)

         ('typedef . _))
     ;; ignore here, used in generating public interface
     (process-toplevel-form form))
    (else
     (error "Pub what?" (cadr form)))))

(define (process-toplevel-form form)
  (match form
    (('fn . _) (list 'static (walk-function form)))
    (('var . _) (list 'static (walk-var form)))
    (('extern . rest) (walk-extern rest))
    (('pub . rest) (walk-public rest))
    ((or ('struct . _)
         ('union . _)) (walk-struct form))
    (else (walk-expr form))))

(define (emit-c sex-forms)
  (for-each (lambda (form)
              (fmt #t (c-expr (process-toplevel-form form)) nl))
            sex-forms))
