(declare (unit utils))

(include "utils.macros.scm")

(import
  (chicken pathname)
  (chicken process-context)
  srfi-1)

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
