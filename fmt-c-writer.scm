;;; Sex fmt-c output writer

(import
  scheme
  (scheme base)                         ; make-parameter
  (chicken base)
  (chicken string)
  (chicken syntax)
  brev-separate                         ; fn, flatten
  fmt
  sex-fmt-c
  matchable
  (chicken irregex)                     ; unkebabify
  srfi-1                                ; lists
  srfi-13                               ; strings
  utils)

;;; Map a procedure over every leaf of a tree, preserving its shape.
;;; Was the `tree' egg, which has not been ported to CHICKEN 6. (Its
;;; `flatten', also used here, comes from brev-separate.)
(define (tree-map f tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons (tree-map f (car tree))
                            (tree-map f (cdr tree))))
        (else (f tree))))

(define (unkebabify sym)
  (case sym
    ((-) sym)
    ((--) sym)
    ((->) sym)
    ((-=) sym)
    (else
     (string->symbol
      (irregex-replace/all "-(?!>)" (symbol->string sym) "_")))))

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

(define (maybe-unwrap-type type)
  (if (and (list? type)
           (= 1 (length type)))
      (car type)
      type))

(define (comment-form? f)
  (and (pair? f) (eq? (car f) 'comment)))

(define (walk-generic-toplevel form)
  (cond ((atom? form) (atom-to-fmt-c form))
        ((list? form) (map walk-generic-toplevel form))
        (else (error "Malformed form " form))))

(define (walk-expr form)
  (match form
    ((? vector?)
     (list->vector
      (walk-expr (vector->list form))))
    ((? atom?)
     (atom-to-fmt-c form))
    ;; (comment "text") -> /* text */. `%comment' is the fmt-c directive.
    (('comment . text) (cons '%comment text))
    ;; (dot-access obj field ...) -> obj.field... member access. `%.'
    ;; is the fmt-c directive for the `.' operator (see sex-fmt-c).
    (('dot-access . rest) (cons '%. (map walk-expr rest)))
    (('var . _) (walk-var form))
    (('cast expr type) (list '%cast
                             (walk-type type)
                             (walk-expr expr)))
    (('enum . _) (walk-enum form))
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
                (type (maybe-unwrap-type type-list))
                (size (last array-type)))
           `(%array ,(walk-type type)
                    ,size))
         ;; sugar for pointer... Do we really need it? Guess why not,
         ;; it's a strong semantic cue
         `(%array ,(walk-type (maybe-unwrap-type array-type)))))
    (('fn arglist ret-type)
     `(%fun ,(walk-type ret-type) ,(walk-arglist arglist)))
    (('fn . _)
     (assert #f "Malformed function type form"))

    ;; Special case: nested structs/unions
    ((or ('struct . _)
         ('union . _)) (walk-struct form))

    (('enum . _) (walk-enum form))
    (else
     (type-convert-to-c form))))

(define (type-convert-to-c type)
  ;; Our pointers to C pointers
  ;; int -> int
  ;; * const char -> const char *
  ;; const * const char -> const char * const
  (if (atom? type) (atom-to-fmt-c type)
      (flatten
       (tree-map atom-to-fmt-c
                 (flatten
                  (list-join (reverse (list-split type '*))
                             '(*)))))))

(define (walk-fn-def form)
  (match form
    (('fn name args ret-type . maybe-body)
     `(%fun
       ,(walk-type ret-type)
       ,(atom-to-fmt-c name)
       ,(walk-arglist args)
       .
       ,(walk-expr maybe-body)))))

;;; TODO: isn't there a better way?
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

          ((var . type) (append (list (walk-type (maybe-unwrap-type type)))
                                (list (walk-type var))))))
       (remove comment-form? form)))

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
       (remove comment-form? fields)))

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

(define (walk-enum form)
  (match form
    (('enum (values ...))
     `(enum ,(map atom-to-fmt-c values)))
    (('enum name (values ...))
     `(enum ,(atom-to-fmt-c name) ,(map atom-to-fmt-c values)))))

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
    (('comment . text) (cons '%comment text))
    (('fn . _) (list 'static (walk-function form)))
    (('var . _) (list 'static (walk-var form)))
    (('extern . rest) (walk-extern rest))
    (('pub . rest) (walk-public rest))
    ((or ('struct . _)
         ('union . _)) (walk-struct form))
    (('enum . _) (walk-enum form))
    (('define name . rest) `(%define ,(atom-to-fmt-c name) ,@rest))
    (else (walk-expr form))))

(define (get-line-num form)
  ;; Source line recorded by our reader (see utils' form-line), or #f
  ;; for forms the reader did not produce (prelude, macro expansions).
  (form-line form))

(define sex-fmt-current-file (make-parameter "/dev/null"))
(define sex-fmt-line-num (make-parameter 0))

(define (line-directive-string)
  (fmt #f #\# "line " (sex-fmt-line-num) " " #\" (sex-fmt-current-file) #\"))

(define (emit-c sex-forms)
  (for-each (lambda (form)
              (let ((start-line (get-line-num form)))
                (when start-line
                  (sex-fmt-line-num start-line)
                  (fmt #t (line-directive-string) nl)))
              (fmt #t (c-expr (process-toplevel-form form)) nl))
            sex-forms))
