(import
  scheme
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
      (cons new-car new-cdr)))

;;; Source-location map.
;;; Our hand-written reader records the source line of each form here,
;;; keyed by the form's cons cell (eq?). This replaces CHICKEN's
;;; read-with-source-info / get-line-number, which only works when
;;; forms are produced by the built-in `read'. The key survives
;;; semantic processing because `recons' preserves the original cons
;;; cell for forms it does not structurally change.
(define +form-lines+ (make-hash-table eq?))

(define (set-form-line! form line)
  (hash-table-set! +form-lines+ form line))

(define (form-line form)
  (hash-table-ref/default +form-lines+ form #f))
