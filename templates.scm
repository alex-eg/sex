(declare (unit templates))

(import
  (chicken plist)
  brev-separate
  fmt
  regex
  srfi-1                                ; list routines
  tree)

(define (register-template name)
  (put! name 'sex-template #t))

(define-syntax template
  (syntax-rules ()
    ((template (name . args) . body)
     (begin
       (register-template 'name)
       (define-syntax name
         (syntax-rules ()
           ((name . applied-args)
            (let* ((subst-alist (map cons 'args 'applied-args))
                   (replaced-body (apply-substitution `body subst-alist)))
              replaced-body))))))))

(define (apply-symbol-substitution sym subst-alist)
  ;; All non-symbol substitutions will be filtered.
  ;; E.g. if the subst-alist is ((T + 1 2) (U . w) (W . e)),
  ;; only ((U . w) (W . e)) will be applied to symbols.
  (let ((str (symbol->string sym))
        (subst-map (map (fn
                         (if (symbol? (car x))
                             (cons
                              (fmt #f "([^\\-]?)"
                                   (regexp-escape (symbol->string (car x)))
                                   "([\\-$]?)")
                              (fmt #f "\\1" (symbol->string (cdr x)) "\\2"))
                             x))
                        (filter (fn (symbol? (cdr x))) subst-alist))))
    (string->symbol
     (string-substitute* str subst-map))))

(define (maybe-replace-symbol sym subst-alist)
  (call/cc
   (lambda (return)
     (for-each (fn (when (eq? (car x) sym)
                     (return (cdr x))))
               subst-alist)
     (return sym))))

(define (apply-substitution target subst-alist)
  ;; Subsitute free symbols and -/$/^ separated parts
  ;; of symbols with provided forms.
  ;; E.g. with substitution (T int):
  ;; list-T -> list-int ; by apply-symbol-substitution
  ;; (var T data) -> (var int data) ; by maybe-replace-symbol
  ;; see respective functions for further details.
  (tree-map
   (fn
    (if (symbol? x)
        (let ((st (maybe-replace-symbol x subst-alist)))
          (if (eq? st x)
              (apply-symbol-substitution x subst-alist)
              st))
        x))
   target))
