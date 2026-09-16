(import scheme
        brev-separate
        (chicken base)
        (chicken file)
        (chicken io)
        (chicken pathname)
        (chicken port)
        (chicken process)
        (chicken process-context)
        (chicken string)                ; string-split
        fmt
        getopt-long
        reader                          ; read-raw-forms, shared with sexc
        srfi-1
        srfi-13)                        ; string-prefix?

(define (print-help)
  (fmt #t "Usage: sextest [options] filename" nl
       "Options:" nl
       (usage opts-grammar) nl))

(define (split-settings contents)
  (foldl (lambda (acc elt)
           (case (car elt)
             ((compilation input output return)
              (cons
               (append (car acc) (list elt))
               (cdr acc)))
             (else
              (cons
               (car acc)
               (append (cdr acc) (list elt))))))
         (cons (list) (list))
         contents))

;;; --features from the (compilation ...) form, which we have to honour
;;; ourselves: the program is read here and printed back out for sexc,
;;; so #+ and #- are resolved on this side
(define (compilation-features settings)
  (let ((compilation (assoc 'compilation settings)))
    (if compilation
        (append-map (lambda (flag)
                      (if (string-prefix? "--features=" flag)
                          (map string->symbol
                               (string-split (substring flag 11) ","))
                          (list)))
                    (string-split (cadr compilation)))
        (list))))

(define (process-file target-path)
  (let* ((first-pass (split-settings (read-raw-forms target-path)))
         (features (compilation-features (car first-pass))))
    (if (null? features)
        first-pass
        (parameterize ((current-features (append (platform-features) features)))
          (split-settings (read-raw-forms target-path))))))

(define (compile src compilation sexc)
  (let ((compiler (or
                   (and sexc (cdr sexc))
                   (get-environment-variable "SEXC")
                   "sexc"))
        ;; (compilation "--features=x -- -O2") -- one string
        (flags (if compilation
                   (string-split (cadr compilation))
                   (list)))
        (compiled-file (create-temporary-file)))
    ;; `process' returns one record; `process-input-port' is named from
    ;; the child's side, so it is the port we write to.
    (let* ((proc (process compiler (append (list "-o" compiled-file) flags)))
           (sexc-stdin (process-input-port proc)))
      (with-output-to-port sexc-stdin
        (fn (map (fn (fmt #t x)) src)))
      (close-output-port sexc-stdin)
      (call-with-values
          (fn (process-wait proc))
        (lambda (pid exited retcode)
          (if (= 0 retcode)
              compiled-file
              #f))))))

(define (run-and-check file in out ret)
  (let* ((proc (process file))
         (out-port (process-output-port proc))   ; the program's stdout
         (in-port  (process-input-port proc)))   ; the program's stdin
    (let ()
      (when in
        (with-output-to-port in-port
          (fn (map (fn (fmt #t x))
                   (cdr in))))
        (close-output-port in-port))
      (let ((out-lines
             (with-input-from-port out-port
               (fn
                (let loop ((line (read-line))
                           (lines (list)))
                  (if (eof-object? line)
                      (reverse lines)
                      (loop (read-line)
                            (cons line lines)))))))
            (ret-code
             (call-with-values
                 ;; TODO: what if the program hangs
                 ;; we need some kind of timeout mechanism
                 (fn
                  (process-wait proc))
               (lambda (pid exited retcode)
                 retcode))))
        (and
         (if (not (= ret-code (cadr ret)))
             (begin
               (fmt #t "Retcode differs. Got " ret-code ", expected " (cadr ret) nl)
               #f)
             #t)

         (if (not (equal? out-lines (cdr out)))
             (begin
               (fmt #t "Output differs. Got " out-lines " expected " (cdr out) nl)
               #f)
             #t))))))

(define opts-grammar
  `((sexc ,(fmt #f "Path to sexc compiler. Defaults to value of SEXC" nl
                (pad 26) "environment variable, ot if it's empty, to sexc" nl )
          (required #f)
          (value #t))
    (help "Show this help"
          (required #f)
          (value #f)
          (single-char #\h))))

(define (process-test-file sexc path)
  (set-environment-variable! "SEX_MODULE_PATH"
                             (normalize-pathname (make-absolute-pathname
                                                  (current-directory)
                                                  (pathname-directory path))))
  (let* ((settings-and-src (process-file path))
         (settings (car settings-and-src))
         (src (cdr settings-and-src))
         (compiled-file (compile src (assoc 'compilation settings) sexc)))
    (if (not compiled-file)
        (begin (fmt #t "Failed to compile " path nl)
               #f)
        (if (run-and-check
             compiled-file
             (assoc 'input settings)
             (assoc 'output settings)
             (assoc 'return settings))
            (begin (fmt #t ".")
                   #t)
            (begin (fmt #t ",")
                   #f)))))

(define (main)
  (let ((args (getopt-long (command-line-arguments)
                           opts-grammar)))
    (when (assoc 'help args)
      (print-help)
      (exit 0))
    (when (null? (cdr (assoc '@ args)))
      (fmt #t "Missing target file" nl)
      (print-help)
      (exit 1))
    (unless
        (foldl (lambda (a b) (and a b))
               #t
               (map (fn (process-test-file (assoc 'sexc args) x))
                    (cdr (assoc '@ args))))
      ;; TODO: add more verbose and human readable output and reporting
      (fmt #t nl)
      (exit 2))
    (fmt #t nl)
    (exit 0)))

(main)
