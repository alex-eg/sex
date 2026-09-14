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

;;; egg `tree' not ported to CHICKEN 6 yet
(define (tree-map f tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons (tree-map f (car tree))
                            (tree-map f (cdr tree))))
        (else (f tree))))

;;; How much #line information to emit:
;;;
;;;   statement -- before every statement.
;;;   toplevel  -- one directive per toplevel form.
;;;   none      -- none at all, for reading -C output by eye.
(define sex-line-directives (make-parameter 'statement))

(define (anchor-statements?)
  (eq? (sex-line-directives) 'statement))

(define (line-directive src)
  ;; `%line' is fmt-c's #line directive. cpp-line concatenates its
  ;; second argument verbatim, so the file name arrives already quoted.
  `(%line ,(cdr src) ,(fmt #f #\" (car src) #\")))

(define (walk-body stmts)
  "Walk a statement list, re-anchoring each statement that has a known
source location. Only statement positions may be walked this way: a
#line inside an expression is a C syntax error."
  (if (anchor-statements?)
      (append-map (lambda (s)
                    (let ((src (form-source s)))
                      (if src
                          (list (line-directive src) (walk-expr s))
                          (list (walk-expr s)))))
                  stmts)
      (map walk-expr stmts)))

(define (walk-stmt s)
  "A statement in a slot that holds exactly one form -- an `if' arm.
Splicing is not possible there, since c-if reads anything past the arm
as an `else if' chain, so the anchor and the statement are wrapped in
`%begin': a statement sequence that emits no braces of its own (the
surrounding c-block supplies them)."
  (let ((src (and (pair? s) (anchor-statements?) (form-source s))))
    (if src
        `(%begin ,(line-directive src) ,(walk-expr s))
        (walk-expr s))))

(define (walk-if-clauses clauses)
  "(test stmt test stmt ... [else-stmt]) -- tests stay expressions."
  (let loop ((cs clauses) (acc (list)))
    (cond ((null? cs) (reverse acc))
          ((null? (cdr cs))                    ; trailing else statement
           (reverse (cons (walk-stmt (car cs)) acc)))
          (else (loop (cddr cs)
                      (cons (walk-stmt (cadr cs))
                            (cons (walk-expr (car cs)) acc)))))))

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
    ((do) '%block-begin)
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

    ;; Statement positions. These are the only places a #line may go,
    ;; and each is spliced or wrapped according to what the
    ;; corresponding fmt-c procedure accepts.
    (('do . stmts) (cons '%block-begin (walk-body stmts)))
    (('if . clauses) (cons 'if (walk-if-clauses clauses)))
    (('while test . body)
     (cons* 'while (walk-expr test) (walk-body body)))
    (('for init test step . body)
     (cons* 'for (walk-expr init) (walk-expr test) (walk-expr step)
            (walk-body body)))
    ;; No anchor *between* switch clauses: c-switch requires every clause
    ;; to be a case/default form and errors on anything else. The clause
    ;; bodies are anchored from inside, which is what a debugger steps
    ;; onto -- a `case' label is not a statement.
    (('switch e . clauses)
     (cons* 'switch (walk-expr e) (map walk-expr clauses)))
    (('case v . body) (cons* 'case (walk-expr v) (walk-body body)))
    (('case/fallthrough v . body)
     (cons* 'case/fallthrough (walk-expr v) (walk-body body)))
    (('default . body) (cons 'default (walk-body body)))

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
       ,(walk-body maybe-body)))))

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

(define (emit-c sex-forms)
  (for-each (lambda (form)
              ;; Forms the reader did not produce -- the prelude, and
              ;; anything a macro built that we could not attribute --
              ;; have no location and get no directive.
              (let ((src (and (not (eq? (sex-line-directives) 'none))
                              (form-source form))))
                (when src
                  (fmt #t (c-expr (line-directive src)))))
              (fmt #t (c-expr (process-toplevel-form form)) nl))
            sex-forms))
