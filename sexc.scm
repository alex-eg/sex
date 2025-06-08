(import brev-separate
        (chicken pathname)
        (chicken pretty-print)
        (chicken process-context)
        (chicken string)
        fmt
        fmt-c
        getopt-long
        srfi-1                          ; list routines
        tree)

(define (unkebabify sym)
  (string->symbol
   (string-translate (symbol->string sym) #\- #\_)))

(define (atom-to-fmt-c atom)
  (case atom
    ((fn) '%fun)
    ((var) '%var)
    ((begin) '%begin)
    ((pointer) '%pointer)
    ((array) '%array)
    (([]) 'vector-ref)
    ((include) '%include)
    ((cast) '%cast)
    (else
     (if (symbol? atom)
         (unkebabify atom)
         atom))))

(define (tree-finder symbol)
  (lambda (node)
    (or (and (tree? node)
             (eq? (car node) symbol))
        #f)))

(define (walk-generic form)
  (tree-map
   atom-to-fmt-c
   (let ((ret-form form))
     (let loop ((unquote-form (tree-find (tree-finder 'unquote) form #f)))
       (if unquote-form
           (begin
             (set! ret-form (tree-replace (invert-tree form)
                                          unquote-form
                                          (eval (cadr unquote-form))))
             (loop (tree-find (tree-finder 'unquote)
                              ret-form
                              #f)))
           ret-form)))))

(define (walk-function form static)
  (if static
      (list 'static (walk-generic form))
      (walk-generic (cdr form))))

(define (walk-struct form acc)
  (let ((name (unkebabify (cadr form))))
    (cons (walk-generic form)
          (cons `(typedef struct ,name ,name) acc))))

(define (walk-sex-tree form acc)
  (case (car form)
    ((fn) (cons (walk-function form #t) acc))
    ((pub) (cons (walk-function form #f) acc))
    ((struct) (walk-struct form acc))
    (else (cons (walk-generic form) acc))))

(define (process-form form acc)
  (case (car form)
    ((define) (eval form) acc)
    ((load) (eval form) acc)
    (else
     (walk-sex-tree form acc))))

(define (process-raw-forms raw-forms acc)
  (if (null? raw-forms) (filter (fn (not (null? x)))
                                (reverse acc))
      (process-raw-forms (cdr raw-forms)
                         (process-form (car raw-forms) acc))))

(define (read-forms acc)
  (let ((r (read)))
    (if (eof-object? r) (reverse acc)
        (read-forms (cons r acc)))))

(define (emit-c forms)
  (for-each (lambda (form)
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

(define (read-from-file file)
  (with-input-from-file (pathname-strip-directory file)
    (fn (read-forms (list)))))

(define (set-working-directory file)
  (change-directory
   (normalize-pathname
    (make-absolute-pathname
     (current-directory)
     (pathname-directory file)))))

(define (main)
  (let* ((raw-args (command-line-arguments))
         (current-dir (current-directory))
         (args (getopt-long raw-args
                            opts-grammar))
         (output (get-arg args 'output 'stdout))
         (help (help-arg? args))
         (input (get-input-file args)))
    (if help (print-help)
        (let* ((raw-forms
                (if (eq? input 'stdin)
                    (read-forms (list))
                    (begin
                      (set-working-directory input)
                      (read-from-file input))))
               (sex-forms (process-raw-forms raw-forms (list))))
          (change-directory current-dir)
          (if (get-arg args 'macro-expand #f)
              (map pp sex-forms)
              (if (not (eq? output 'stdout))
                  (with-output-to-file output
                    (lambda ()
                      (emit-c sex-forms)))
                  (emit-c sex-forms)))))))

(main)
