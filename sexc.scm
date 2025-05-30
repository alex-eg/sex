(import fmt fmt-c (chicken string))

(define (unkebabify sym)
  (string->symbol
   (string-translate (symbol->string sym) #\- #\_)))

; Maybe rewrite with tree inversions?
(define (map-filter-tree map-proc filter-proc tree)
  "Walk the tree, checking each element with filter-proc.
If filter-proc returns #t:
  - if the element is list, process it recursively,
  - if the element is an atom, process it with map-proc.

If filter-proc returns #f, ignore the element.

If filter-proc returns something other, use it instead of processing the element."
  (let ((cont (cut map-filter-tree map-proc filter-proc <>)))
    (if (null? tree) '()
        (let ((filter-res (filter-proc (car tree))))
          ;(fmt #t "Filter res for " tree " is " filter-res "\n")
          (case filter-res
            ((#t) (cons
                   (if (atom? (car tree))
                       (map-proc (car tree))
                       (cont (car tree)))
                   (cont (cdr tree))))
            ((#f) (cont (cdr tree)))
            (else (cons filter-res (cont (cdr tree)))))))))

(define (walk-tree tree)
  (map-filter-tree
   (lambda (elem)
     (case elem
       ((fn) '%fun)
       ((var) '%var)
       ((begin) '%begin)
       ((pointer) '%pointer)
       ((array) '%array)
       (([]) 'vector-ref)
       ((include) '%include)
       ((cast) '%cast)
       (else
        (if (symbol? elem)
            (unkebabify elem)
            elem))))
   (lambda (subtree)
     (if (list? subtree)
         (case (car subtree)
           ((unquote)
            (eval (cadr subtree)))
           (else #t))
         #t))
   tree))

(define (loop)
  (call/cc
   (lambda (return)
     (let ((r (read)))
       (when (eof-object? r)
         (return '()))
       (case (car r)
         ((define)
          (eval r)
          (fmt #t "\n"))
         (else
          (let ((result (walk-tree r)))
            (when result
              (fmt #t (c-expr
                       result))))))
       (loop)))))

(loop)
