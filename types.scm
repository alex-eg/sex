;;; The type database.
;;;
;;; Every named aggregate, typedef and define the semantic engine
;;; walks past is recorded here, so that macros (or other forms) can
;;; ask what a type is made of. That is what lets a macro generate
;;; code from a struct's fields given nothing but its name.
;;;
;;; Entries are filled in as toplevel forms are processed, in order, so
;;; a type has to be declared before the macro that asks about it.

(import
  scheme
  (scheme base)
  (chicken base)
  srfi-1
  srfi-69)

(define +type-db+ (make-hash-table))

(define (strip-pub form)
  (if (eq? (car form) 'pub) (cdr form) form))

(define (comment-form? f)
  (and (pair? f) (eq? (car f) 'comment)))

;;; Fields are written with the type last and one or more names before
;;; it, so ((x y int) (p (* char))) describes three fields. Flatten that
;;; into one (name type) per field, which is what a caller wants.
(define (normalize-fields fields)
  (append-map
   (lambda (field)
     (if (comment-form? field)
         (list)
         (let ((type (last field))
               (names (drop-right field 1)))
           (map (lambda (name) (list name type)) names))))
   (remove comment-form? fields)))

;;; ([pub] struct name (fields ...) . attrs)
(define (aggregate-fields form)
  (let ((f (strip-pub form)))
    (if (and (pair? (cddr f)) (list? (caddr f)))
        (caddr f)
        (list))))

(define (add-struct name form)
  (hash-table-set! +type-db+ name
                   (list 'struct name (normalize-fields (aggregate-fields form)))))

(define (add-union name form)
  (hash-table-set! +type-db+ name
                   (list 'union name (normalize-fields (aggregate-fields form)))))

;;; ([pub] enum name (value ...))
(define (add-enum name form)
  (let ((f (strip-pub form)))
    (hash-table-set! +type-db+ name
                     (list 'enum name (if (and (pair? (cddr f)) (list? (caddr f)))
                                          (caddr f)
                                          (list))))))

;;; ([pub] typedef new-name target)
(define (add-typedef name form)
  (hash-table-set! +type-db+ name
                   (list 'typedef name (last (strip-pub form)))))

;;; (define name value ...) -- a C #define, kept so a macro can read a
;;; compile-time constant rather than re-parse the source.
(define (add-define name form)
  (hash-table-set! +type-db+ name
                   (list 'define name (cddr (strip-pub form)))))

(define (get-type-info name)
  (hash-table-ref/default +type-db+ name #f))

;;; ((name type) ...) for a struct or union, #f for anything else --
;;; including a name that was never declared. Callers give the better
;;; error, since they know what they wanted it for.
;;;
;;; A typedef is followed to what it stands for, so reflection over an
;;; alias works exactly as it does over the name it aliases.
(define (get-fields name)
  (let ((info (resolve-type-info name)))
    (and info
         (memq (car info) '(struct union))
         (caddr info))))

;;; Follow a typedef chain to the name it stands for. #f if NAME is
;;; not a typedef. A typedef that leads back to itself stops rather
;;; than spinning: nothing prevents one from being written.
(define (get-underlying-type name)
  (let follow ((name name) (seen (list)))
    (and (not (member name seen))
         (let ((info (get-type-info name)))
           (and info
                (eq? (car info) 'typedef)
                (let ((target (caddr info)))
                  (or (and (symbol? target) (follow target (cons name seen)))
                      target)))))))

;;; The declaration NAME ultimately names. For a typedef that is the
;;; entry of whatever it stands for, and for anything else it is
;;; NAME's own info.
(define (resolve-type-info name)
  (let ((info (get-type-info name)))
    (and info
         (if (eq? (car info) 'typedef)
             (let* ((target (get-underlying-type name))
                    (tag (cond ((symbol? target) target)
                               ((and (pair? target)
                                     (pair? (cdr target))
                                     (symbol? (cadr target)))
                                (cadr target))
                               (else #f))))
               (and tag (get-type-info tag)))
             info))))

;;; Type matcher macro
;;; (type-match type
;;;   (int ...)
;;;   ((* const char) ...)
;;;   ([int 10] ...)
;;;   (else ...))
;;;
;;; A type is a form, not an atom, so this compares with equal? rather
;;; than dispatching like `case'. Patterns are literal types and are not
;;; evaluated; `else' is optional and the whole thing is #f when nothing
;;; matches and there is no else.
(define-syntax type-match
  (syntax-rules (else)
    ((_ type) #f)
    ((_ type (else body ...)) (begin body ...))
    ((_ type (pattern body ...) clause ...)
     (if (equal? type 'pattern)
         (begin body ...)
         (type-match type clause ...)))))

;;; Map function to each field/value of a structure/union/enum
;;; For enums, field-type is the type of the enum (since C 23)
;;; (map-fields type-name
;;;     (lambda (field-name field-type) ...))
;;;
;;; Returns #f if nothing of that name was declared
(define (map-fields struct-union-enum fn)
  (let ((info (resolve-type-info struct-union-enum)))
    (and info
         (case (car info)
           ((struct union)
            (map (lambda (field) (fn (car field) (cadr field)))
                 (caddr info)))
           ;; An enumerator's type is the enum itself -- named as it was
           ;; declared, since `enum some-typedef' is not a C type.
           ((enum)
            (let ((type (list 'enum (cadr info))))
              (map (lambda (value) (fn value type))
                   (caddr info))))
           (else #f)))))
