(import (chicken pretty-print)
        (chicken process-context)
        (chicken string)
        fmt
        fmt-c
        getopt-long
        srfi-1 ; list routines
        )

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

(define (read-forms collect)
  (call/cc
   (lambda (return)
     (read-forms
      (cons
       (let ((r (read)))
         (when (eof-object? r)
           (return (filter (lambda (e) (not (null? e)))
                           (reverse collect))))
         (case (car r)
           ((define)
            (eval r)
            '())
           (else
            (let ((result (walk-tree r)))
              (if result result
                  '())))))
       collect)))))

(define (emit-c forms)
  (map (lambda (form)
         (fmt #t (c-expr form))
         (fmt #t "\n"))
       forms))

;;; Main function facilities

(define opts-grammar
  `((output "Write output to file"
            (required #f)
            (value #t)
            (single-char #\o))
    (help "Show this help"
          (required #f)
          (value #f)
          (single-char #\h))
    (macro-expand "Emit macro-expanded Sex code instead of C"
                  (required #f)
                  (value #f)
                  (single-char #\m))))

(define (print-help)
  (fmt #t "Usage: sexc [OPTIONS] [FILE]\n")
  (fmt #t "Options: -o, --output <file>        Write output to file. If omitted, write to stdout\n")
  (fmt #t "         -h, --help                 Show this help\n")
  (fmt #t "         -m, --macro-expand         Emit macro-expanded Sex code instead of C\n"))

(define (help-arg? args)
  (assoc 'help args))

(define (get-arg args arg-name default)
  (let ((arg (assoc arg-name args)))
    (if arg (cdr arg)
        default)))

(define (get-input-file args)
  (let ((rest-args (assoc '@ args)))
    (if (= 1 (length rest-args))
        'stdin
        (cadr rest-args))))

(define (main)
  (let* ((raw-args (command-line-arguments))
         (args (getopt-long raw-args
                            opts-grammar))
         (output (get-arg args 'output 'stdout))
         (help (help-arg? args))
         (input (get-input-file args)))
    (if help (print-help)
        (let ((sex-forms
               (if (eq? input 'stdin)
                   (read-forms (list))
                   (with-input-from-file input
                     (lambda () (read-forms (list)))))))

          (if (get-arg args 'macro-expand #f)
              (map pp sex-forms)
              (if (not (eq? output 'stdout))
                  (with-output-to-file output
                    (lambda ()
                      (emit-c sex-forms)))
                  (emit-c sex-forms)))))))

(main)
