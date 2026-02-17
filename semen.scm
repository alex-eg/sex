;;; Sex semantic engine

(import
  scheme
  (chicken base)
  (chicken keyword)
  (chicken string)
  (chicken module)
  fmt
  sex-macros
  sex-modules
  matchable                         ; pattern matching
  srfi-1                            ; list routines
  srfi-69                           ; hash tables
  utils
  )

(export/rename (process semen-process))

;;; for lambda extraction, docstring processing, macro expansion,
;;; injection of module headers, i.e. all things that rearrange code
;;; structurally, add or remove forms
;;;
;;; The algorithm: feed toplevel forms to appropriate handlers, then
;;; append their return to the resulting list. Each handler can return
;;; multiple forms, e.g. lambdas collected from a function may result
;;; in auxiliary structures and functions.
(define (process raw-sex-forms)
  (process-rec raw-sex-forms (list)))

(define (process-rec forms acc)
  (cond
   ((null? forms) (reverse acc))
   ((macro? (car forms))
    (process-rec
     (macroexpand (car forms) (cdr forms))
     acc))
   (else
    (process-rec (cdr forms)
                 (match-sex-form (car forms) acc)))))

(define (macroexpand macro-form rest-forms)
  ;; We want to replace macro with its expansion. The problem is,
  ;; top-level macro can return either a single form, or a list of
  ;; forms, when it for example generates some aux
  ;; structures/functions/typedefs.
  ;;
  ;; Single form we just cons to the top of rest-forms, but multiple
  ;; forms have to be appended to the rest-forms.
  (let ((res (apply-macro macro-form)))
    (if (list? (car res))
        (append res rest-forms)
        (cons res rest-forms))))

(define (match-sex-form sex-form acc)
  (match sex-form
    ((or ('fn . _)
         ('pub 'fn . _)
         ('extern 'fn . _)) (process-fn sex-form acc))
    ((or ('struct . _)
         ('pub 'struct . _)) (process-struct sex-form acc))
    ((or ('union . _)
         ('pub 'union . _)) (process-struct sex-form acc))
    ((or ('enum . _)
         ('pub 'enum . _)) (process-struct sex-form acc))
    ((or ('var . _)
         ('pub 'var . _)
         ('extern 'var . _)) (process-global-var sex-form acc))
    (('include _) (cons sex-form acc))
    (('define . _) (cons sex-form acc))

    (('import . modules)
     (process-imports (get-modules-public-forms modules) acc))

    ((or ('defmacro . rest)
         ('pub 'defmacro . rest)) (defmacro rest) acc)

    ((or ('typedef new-type target)
         ('pub 'typedef new-type target))
     (process-typedef new-type target acc))

    (else (assert #f (fmt #f "Unknown top level form " sex-form)))))

(define (process-imports module-public-forms acc)
  ;; Recursively process imports: register public macros, cons all
  ;; other public things to our acc
  (if (null? module-public-forms) acc
      (match (car module-public-forms)
        (('defmacro . rest)
         (defmacro rest)
         (process-imports (cdr module-public-forms) acc))
        (else
         (process-imports (cdr module-public-forms)
                          (cons (car module-public-forms)
                                acc))))))

(define (macro-expand form)
  "Walk the form recursively and expand all macros, until none is left."
  (walk-form
   form
   (lambda (subform env)
     (if (macro? subform)
         (cons walk-embed-result (macroexpand subform (list)))
         subform))
   #f))

;;; walk-form and friends: form walker with various abilities.
;;; By default, replaces walked form with walk-fn result But may
;;; perform additional operations depending of what the walk function
;;; has requested.

;;; For inspiration, see SBCL's walk.lisp and their template
;;; system.

(define walk-embed-result (gensym)
  ;; For cases when result is a list which must be embedded in the
  ;; form, e.g. when it returned from a macro
  )

(define (walk-form form walk-fn env)
  (if (atom? form) form
      (let ((new-form (walk-fn form env)))
        (cond ((not (eq? form new-form))
               (walk-form new-form walk-fn env))
              (else
               (let ((new-car (walk-form (car new-form) walk-fn env))
                     (new-cdr (walk-form (cdr new-form) walk-fn env)))
                 (cond ((and (pair? new-car)
                             (eq? (car new-car) walk-embed-result))
                        (append (cdr new-car) new-cdr))
                       (else
                        (recons new-form new-car new-cdr)))))))))

;;; Typdef

(define (process-typedef new-type target acc)
  (cons `(typedef ,target ,new-type) acc))

;;; Fn processing

(define (process-fn sex-fn acc)
  (let* ((expanded (macro-expand sex-fn))
         (env (make-hash-table))
         (processed
          (walk-form
           expanded
           fn-walker
           (begin
             (set! (hash-table-ref env :fn-name) (sex-fn-name expanded))
             (set! (hash-table-ref env :lambda-counter) 0)
             (set! (hash-table-ref env :lambda-aux-code) (list))
             env))))

    (cons processed
          (append (hash-table-ref env :lambda-aux-code) acc))))

(define (fn-walker form env)
  (if (eq? 'lambda (car form))
      (let ((lambda-name (make-lambda-name (hash-table-ref env :fn-name)
                                           (hash-table-ref env :lambda-counter))))
        (set! (hash-table-ref env :lambda-aux-code)
          (append (make-aux-lambda-struct lambda-name form)
                  (hash-table-ref env :lambda-aux-code)))
        (set! (hash-table-ref env :lambda-counter)
          (+ (hash-table-ref env :lambda-counter) 1))
        lambda-name)
      form))

(define (make-lambda-name enclosing-fn-name counter)
  (string->symbol
   (fmt #f "__lambda_" counter "_" enclosing-fn-name)))

(define (make-aux-lambda-struct name form)
  (match form
    (('lambda ret-type arglist captures . body)
     ;; Captures are ignored for now, but
     ;; we'll need them for TODO: closures support
     (process-fn `(fn ,ret-type ,name ,arglist ,@body) (list)))
    (else (assert #f (fmt #f "Malformed lambda " form)))))

;;; Structs

(define (process-struct sex-struct acc)
  (cons sex-struct acc))

(define (process-global-var sex-var acc)
  (cons sex-var acc))

;;; Utils
(define (non-empty-list? form)
  (and (list? form)
       (not (null? form))))

(define (sex-fn? form)
  "The `form` must be toplevel.
Returns #f if the form is not a function, returns the form otherwise"
  (match form
    ((fn . _) form)
    ((pub fn . _) form)
    (else #f)))

(define (sex-fn-public? fn-form)
  (eq? (car fn-form) 'pub))

(define (sex-fn-return-type fn-form)
  (assert (sex-fn? fn-form)
          (fmt #f "Form " fn-form " is not a function"))
  (if (sex-fn-public? fn-form)
      (third fn-form)
      (second fn-form)))

(define (sex-fn-name fn-form)
  (assert (sex-fn? fn-form)
          (fmt #f "Form " fn-form " is not a function"))
  (if (sex-fn-public? fn-form)
      (fourth fn-form)
      (third fn-form)))

(define (sex-fn-arglist fn-form)
  (assert (sex-fn? fn-form)
          (fmt #f "Form " fn-form " is not a function"))
  (if (sex-fn-public? fn-form)
      (fifth fn-form)
      (fourth fn-form)))

(define (sex-fn-prototype fn-form)
  "Returns all except body"
  (assert (sex-fn? fn-form)
          (fmt #f "Form " fn-form " is not a function"))
  (if (sex-fn-public? fn-form)
      (take fn-form 5)
      (take fn-form 4)))

(define (sex-fn-body fn-form)
  (assert (sex-fn? fn-form)
          (fmt #f "Form " fn-form " is not a function"))
  (if (sex-fn-public? fn-form)
      (drop fn-form 5)
      (drop fn-form 4)))
