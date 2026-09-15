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
                  (pack-comments stmts))
      (map walk-expr (pack-comments stmts))))

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

;;; A `;' comment reads as a form, so one written inside a construct
;;; with positional slots lands in a slot and shifts everything after
;;; it. So take the positional slots by skipping comments, and hand
;;; the comments back to be emitted just before the statement
(define (take-slots forms n)
  "Three values: the comment forms skipped over, the next N non-comment
forms, and what remains."
  (let loop ((fs forms) (n n) (comments (list)) (slots (list)))
    (cond ((or (= n 0) (null? fs))
           (values (reverse comments) (reverse slots) fs))
          ((comment-form? (car fs))
           (loop (cdr fs) n (cons (car fs) comments) slots))
          (else
           (loop (cdr fs) (- n 1) comments (cons (car fs) slots))))))

;;; `%begin' is a statement sequence that emits no braces of its own, so
;;; the comments simply precede the statement.
(define (with-comments comments form)
  (if (null? comments)
      form
      `(%begin ,@(map walk-expr comments) ,form)))

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
    ;; a `|' inside a symbol has to be escaped to be written in
    ;; a Scheme source, so we just rename it in fmt-c compatible
    ;; way
    ((|\||) 'bit-or)
    ((|\|\||) '%or)
    ((|\|=|) 'bit-or=)
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

;;; Strip ;s so ;;; Foo becomes /* Foo */ and not /* ;; Foo */
(define (strip-comment-marker text)
  (string-trim-both (string-trim text #\;)))

(define (walk-comment texts)
  (list '%comment
        (string-append " "
                       (string-intersperse (map strip-comment-marker texts)
                                           "\n ")
                       " ")))

;;; Merge multiple lines of /* */ into single block
(define (pack-comments forms)
  (let loop ((fs forms) (acc (list)))
    (cond
     ((null? fs) (reverse acc))
     ((comment-form? (car fs))
      (let ((first (car fs)))
        (let gather ((rest (cdr fs)) (texts (cdr first)) (line (form-line first)))
          (if (and (pair? rest)
                   (comment-form? (car rest))
                   line
                   (equal? (form-file first) (form-file (car rest)))
                   (eqv? (form-line (car rest)) (+ line 1)))
              (gather (cdr rest)
                      (append texts (cdr (car rest)))
                      (form-line (car rest)))
              (loop rest
                    (cons (if (eq? texts (cdr first))
                              first          ; a run of one, left alone
                              (copy-form-source! first (cons 'comment texts)))
                          acc))))))
     (else (loop (cdr fs) (cons (car fs) acc))))))

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
    (('comment . text) (walk-comment text))
    ;; (dot-access obj field ...) -> obj.field... member access. `%.'
    ;; is the fmt-c directive for the `.' operator (see sex-fmt-c).
    (('dot-access . rest) (cons '%. (map walk-expr rest)))
    (('var . _) (walk-var form))
    ;; An expression has no room for a statement, so a comment in a
    ;; cast is dropped rather than relocated.
    (('cast . rest)
     (let-values (((comments slots _) (take-slots rest 2)))
       (list '%cast (walk-type (cadr slots)) (walk-expr (car slots)))))
    (('enum . _) (walk-enum form))
    (('c-or . rest) (apply c-or (map walk-expr rest)))
    (('c-bit-or . rest) (apply c-bit-or (map walk-expr rest)))
    (('c-bit-or= . rest) (apply c-bit-or= (map walk-expr rest)))

    ;; Statement positions. These are the only places a #line may go,
    ;; and each is spliced or wrapped according to what the
    ;; corresponding fmt-c procedure accepts.
    (('do . stmts) (cons '%block-begin (walk-body stmts)))
    (('if . clauses)
     (with-comments (filter comment-form? clauses)
       (cons 'if (walk-if-clauses (remove comment-form? clauses)))))
    (('while . rest)
     (let-values (((comments slots body) (take-slots rest 1)))
       (with-comments comments
         (cons* 'while (walk-expr (car slots)) (walk-body body)))))
    (('for . rest)
     (let-values (((comments slots body) (take-slots rest 3)))
       (with-comments comments
         (cons* 'for (walk-expr (car slots)) (walk-expr (cadr slots))
                (walk-expr (caddr slots))
                (walk-body body)))))
    ;; No anchor *between* switch clauses: c-switch requires every clause
    ;; to be a case/default form and errors on anything else. The clause
    ;; bodies are anchored from inside, which is what a debugger steps
    ;; onto -- a `case' label is not a statement.
    ;; A comment between clauses has to go too: c-switch requires every
    ;; clause to be a case/default form and errors on anything else.
    (('switch . rest)
     (let-values (((comments slots clauses) (take-slots rest 1)))
       (with-comments (append comments (filter comment-form? clauses))
         (cons* 'switch (walk-expr (car slots))
                (map walk-expr (remove comment-form? clauses))))))
    (('case . rest)
     (let-values (((comments slots body) (take-slots rest 1)))
       (with-comments comments
         (cons* 'case (walk-expr (car slots)) (walk-body body)))))
    (('case/fallthrough . rest)
     (let-values (((comments slots body) (take-slots rest 1)))
       (with-comments comments
         (cons* 'case/fallthrough (walk-expr (car slots)) (walk-body body)))))
    (('default . body) (cons 'default (walk-body body)))

    ;; Drop comments so they will not generate additional comma
    (else (map walk-expr (remove comment-form? form)))))

(define (walk-var form)
  ;; (var a int) -> (%var int a)
  ;; (var a (const int) 32) -> (%var (const int) a 32)
  ;; (var b [const char 512]) -> (%var (%array (const char) 512) b)
  ;; note: [...] is actually (¤ ...) after reading
  ;; (var c (fn ((int) (float)) void)) -> (%var (%fun void ((int) (float))) c)
  ;; Likewise a declaration: drop any comment rather than shift the
  ;; name and type apart.
  (let ((form (cons (car form) (remove comment-form? (cdr form)))))
    `(%var
      ,(walk-type (third form))
      ,(atom-to-fmt-c (second form))
      .
      ,(if (null? (drop form 3))
           (list)
           (walk-expr (drop form 3)))))) ; optional init expression

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
    (('comment . text) (walk-comment text))
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
            (pack-comments sex-forms)))
