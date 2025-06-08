(declare (unit templates))

(import brev-separate
        fmt
        regex
        srfi-1                          ; list routines
        tree)

;;; template structure storage example:
;;; (name (T U) ((T field-1) (U field-2))))
;;;
;;; template function storage example:
;;; (name (T U) public? T-ret ((T arg-1) (U arg-2)) body)

(define template-functions (list))
(define template-structures (list))

(define (template-name template)
  (first template))

(define (template-subst-list template)
  (second template))

(define (template-struct-fields template)
  (third template))

(define (template-fn-public? template)
  (third template))

(define (template-fn-return-type template)
  (fourth template))

(define (template-fn-arglist template)
  (fifth template))

(define (template-fn-body template)
  (sixth template))

(define (find-template name subst-list template-storage)
  (find (fn (and (eq? name (template-name x))
                 (= (length subst-list)
                    (length (template-subst-list x)))))
        template-storage))

(define (check-template-doesnt-exist name subst-list)
  (when (find-template
         name subst-list
         template-functions)
    (error "Template function with this name and number of parameters already exists"))
  (when (find-template
         name subst-list
         template-structures)
    (error "Template structure with this name and number of parameters already exists")))

(define (register-template-fn subst-list public? return-type name arg-list body)
  (check-template-doesnt-exist name subst-list)
  (when +debug+
    (fmt #t (fmt-join dsp (list "Adding template fn" public? return-type name subst-list) " ") nl))
  (set! template-functions
    (cons (list name subst-list public? return-type arg-list body) template-functions)))

(define (register-template-struct subst-list name fields)
  (check-template-doesnt-exist name subst-list)
  (when +debug+
    (fmt #t (fmt-join dsp (list "Adding template structure" name subst-list fields) " ") nl))
  (set! template-structures
    (cons (list name subst-list fields) template-structures)))

(define-syntax template
  (syntax-rules (pub fn struct)
    ((_ (T ...) pub fn ret-type name args body ...)
     (register-template-fn (list 'T ...) #t 'ret-type 'name 'args (list 'body ...)))
    ((_ (T ...) fn ret-type name args body ...)
     (register-template-fn (list 'T ...) #f 'ret-type 'name 'args (list 'body ...)))
    ((_ (T ...) struct name fields)
     (register-template-struct (list 'T ...) 'name 'fields))))

(define (apply-symbol-substitution sym subst-alist)
  ;; All non-symbol substitutions will be filtered.
  ;; E.g. if the subst-alist is ((T + 1 2) (U . w) (W . e)),
  ;; only ((U . w) (W . e)) will be applied to symbols.
  (let ((str (symbol->string sym))
        (subst-map (map (fn
                         (if (symbol? (car x))
                             (cons
                              (fmt #f "([^\\-]?)" (symbol->string (car x)) "([\\-$]?)")
                              (fmt #f "\\1" (symbol->string (cdr x)) "\\2"))
                             x))
                        (filter (fn (not (list? (cdr x)))) subst-alist))))
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

(define (instantiate-template-fn instance-name fun subst-alist)
  `(,@((fn (if (template-fn-public? fun) '(pub) '())))
    fn
    ,(apply-substitution (template-fn-return-type fun) subst-alist)
    ,instance-name
    ,(apply-substitution (template-fn-arglist fun) subst-alist)
    ,@(apply-substitution (template-fn-body fun) subst-alist)))

(define (instantiate-template-struct instance-name struct subst-alist)
  (list 'struct instance-name
        (apply-substitution (template-struct-fields struct)
                            subst-alist)))

(define (instantiate-template instance-name template subst-list)
  (let ((struct (find-template template subst-list template-structures))
        (fn (find-template template subst-list template-functions)))
    (when (and struct fn)
      (error (fmt #f "Both template structure and function are defined. Shouldn't have happened\n"
                  "Struct: " struct nl
                  "Fn: " fn nl)))
    (unless (or struct fn)
      (error (fmt #f "Template with name " template " was not found." nl
                  "Known template fns: " (fmt-join dsp (map template-name template-functions)
                                                   "\n                    ")
                  nl
                  "Known template structs: " (fmt-join dsp (map template-name template-structures)
                                                       "\n                       "))))
    (let ((subst-alist
           (map cons (template-subst-list (or struct fn)) subst-list)))
      (if struct
          (instantiate-template-struct instance-name struct subst-alist)
          (instantiate-template-fn instance-name fn subst-alist)))))

(define-syntax instance
  (syntax-rules ()
    ((_ instance-name template subst-list)
     (instantiate-template 'instance-name 'template 'subst-list))))
