(import
  scheme
  (scheme base)                         ; make-parameter
  (chicken base)
  (chicken pathname)
  (chicken process-context)
  srfi-1
  srfi-69)

(define-syntax prog1
  (syntax-rules ()
    ((prog1 form . forms)
     (let ((res form))
       (begin . forms)
       res))))

(define-syntax with-directory
  (syntax-rules ()
    ((with-directory path form . forms)
     (let ((current-dir (current-directory)))
       (set-working-directory path)
       (prog1
        (begin form . forms)
        (change-directory current-dir))))))

(define (get-env-var name)
  (get-environment-variable name))

(define (set-working-directory file)
  (change-directory
   (normalize-pathname
    (if (absolute-pathname? file)
        (pathname-directory file)
        (make-absolute-pathname
         (current-directory)
         (pathname-directory file))))))

(define (to-absolute-pathname pathname)
  (if (absolute-pathname? pathname)
      pathname
      (make-absolute-pathname
       (current-directory)
       pathname)))

(define (list-split src-list split-elt)
  ;; split '(1 2 / 3 4 / 5 6) by '/ -> '((1 2) (3 4) (5 6))
  (fold (lambda (elt acc)
          (if (eq? elt split-elt)
              (append acc (list (list)))
              (append (drop-right acc 1)
                      (list (append (last acc) (list elt))))))
        (list (list))
        src-list))

(define (list-join lists join-by)
  (drop-right
   (fold (lambda (elt acc)
           (append acc (list elt) (list join-by)))
         (list)
         lists)
   1))

;;; Reconstruct form
(define (recons old-cons new-car new-cdr)
  (if (and (eq? new-car (car old-cons))
           (eq? new-cdr (cdr old-cons)))
      old-cons
      ;; A rebuilt cell is still the same source form, so it keeps the
      ;; same location
      (copy-form-source! old-cons (cons new-car new-cdr))))

;;; Source-location map.
;;; Our hand-written reader records the source location of each form
;;; here, keyed by the form's cons cell (eq?). This replaces CHICKEN's
;;; read-with-source-info / get-line-number, which only works when forms
;;; are produced by the built-in `read'.
;;;
;;; A location is (file . line). The file matters because imported
;;; modules paste their public forms into the current unit: those forms
;;; originate in another file and must be reported as such
(define +form-sources+ (make-hash-table eq?))

;;; The file `parse-all' is currently reading. Bound by the reader
(define current-source-file (make-parameter "<unknown>"))

(define (set-form-source! form file line)
  (hash-table-set! +form-sources+ form (cons file line)))

(define (form-source form)
  (hash-table-ref/default +form-sources+ form #f))

(define (form-file form)
  (let ((src (form-source form)))
    (and src (car src))))

(define (form-line form)
  (let ((src (form-source form)))
    (and src (cdr src))))

(define (copy-form-source! from to)
  "Give TO the location of FROM, if FROM has one. Returns TO, so it can
wrap a form-building expression."
  (let ((src (form-source from)))
    (when (and src (pair? to))
      (hash-table-set! +form-sources+ to src)))
  to)

;;; Diagnostics
;;;
;;; Every form carries a location now, so an error can say where the
;;; wrong code was written
(define (form-location form)
  "\"file:line: \" for FORM, or \"\" when it has none"
  (let ((src (form-source form)))
    (if src
        (string-append (car src) ":" (number->string (cdr src)) ": ")
        "")))

(define (sex-error form message . args)
  "Signal an error about FORM, prefixed with where it was written."
  (apply error (string-append (form-location form) message) args))

(define (stamp-form-source! form src)
  "Give FORM and every subform that has none the location SRC. Used for
macro expansions, which inherit the location of the call site the way a
cpp macro does. Forms that already have a location keep it."
  (when (and src (pair? form))
    (unless (form-source form)
      (hash-table-set! +form-sources+ form src))
    (stamp-form-source! (car form) src)
    (stamp-form-source! (cdr form) src))
  form)
