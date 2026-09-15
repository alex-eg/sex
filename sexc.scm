(import scheme
        (scheme base)                   ; call/cc
        brev-separate
        (chicken base)
        (chicken condition)             ; handle-exceptions
        (chicken file)
        (chicken plist)
        (chicken pretty-print)
        (chicken process)
        (chicken process-context)
        (chicken port)
        fmt
        fmt-c-writer
        getopt-long
        sex-macros
        sex-modules
        reader
        semen
        srfi-1                          ; list routines
        utils)

;;; Main function facilities

(define opts-grammar
  (let ((padding 26))
    `((c-compiler ,(fmt #f "Select C compiler. Defaults to value of SEX_CC" nl
                        (pad padding) "environment variable, or if it is empty, to cc")
                  (required #f)
                  (value #t))
      (compile-object "Compile object file instead of executable program"
                      (required #f)
                      (value #f)
                      (single-char #\c))
      (emit-c "Emit C code"
              (required #f)
              (value #f)
              (single-char #\C))
      (public-interface "Get module's public interface"
                        (required #f)
                        (value #f))
      (help "Show this help"
            (required #f)
            (value #f)
            (single-char #\h))
      (macro-expand "Emit macro-expanded semantically processed Sex code"
                    (required #f)
                    (value #f)
                    (single-char #\m))
      (output ,(fmt #f "Write output to file. Default file name is a.out." nl
                    (pad padding) "If -E or -m options are provided, defaults to stdout")
              (required #f)
              (value #t)
              (single-char #\o))
      (line-directives
       ,(fmt #f "How much #line information to emit: statement (default)," nl
             (pad padding) "toplevel, or none. `statement' is what makes a debugger" nl
             (pad padding) "land on the right source line; `none' is for reading -C" nl
             (pad padding) "output by eye")
       (required #f)
       (value #t)))))

(define (print-help)
  (fmt #t "Usage: sexc [options] filename [-- options-for-c-compiler]\n")
  (fmt #t "Options:\n")
  (fmt #t (usage opts-grammar))
  (fmt #t ""))

(define (help-arg? args)
  (assoc 'help args))

(define (get-arg args arg-name default)
  (let ((arg (assoc arg-name args)))
    (if arg (cdr arg)
        default)))

;;; Everything before the first `--' is ours to parse, everything after
;;; is handed to the C compiler verbatim

(define (separator? a)
  (string=? a "--"))

(define (args-before-separator argv)
  (take-while (complement separator?) argv))

(define (args-after-separator argv)
  (let ((tail (drop-while (complement separator?) argv)))
    (if (null? tail)
        (list)
        (cdr tail))))

(define (get-rest-args args)
  (cdr (assoc '@ args)))

(define (line-directives-arg args)
  (let ((v (get-arg args 'line-directives "statement")))
    (cond ((equal? v "statement") 'statement)
          ((equal? v "toplevel") 'toplevel)
          ((equal? v "none") 'none)
          (else (error "--line-directives must be statement, toplevel or none, got" v)))))

(define (get-input-file args)
  (let ((rest-args (get-rest-args args)))
    (if (null? rest-args)
        'stdin
        (car rest-args))))

(define (write-to-file-or-stdout output what)
  (if (eq? output 'default)
      (what)
      (with-output-to-file output
        (fn (what)))))

(define (emit-c-or-sex sex-forms output args)
  (write-to-file-or-stdout output
                           (lambda ()
                             (if (get-arg args 'macro-expand #f)
                                 (map pp sex-forms)
                                 (emit-c sex-forms)))))

(define (compile-to-file sex-forms output args cc-args)
  "Hand the generated C to the C compiler. Returns the compiler's exit
status, which is ours to pass on."
  (let ((compiler (or (get-arg args 'c-compiler #f)
                      (get-env-var "SEX_CC")
                      "cc"))
        (out-file (if (eq? output 'default)
                      "a.out"
                      output))
        ;; The generated C goes to a temporary .c file rather than the
        ;; compiler's stdin. It is removed however we leave -- emit-c
        ;; can throw, and used to leave the file behind when it did
        (c-file (create-temporary-file "c")))
    ;; An unhandled error ends the process without unwinding, so the
    ;; cleanup cannot be left to dynamic-wind
    (handle-exceptions exn
        (begin (delete-file* c-file) (abort exn))
      (with-output-to-file c-file
        (lambda () (emit-c sex-forms)))
      (let ((proc (process compiler (append (list "-o" out-file)
                                            (if (get-arg args 'compile-object #f)
                                                (list "-c")
                                                (list))
                                            (list c-file)
                                            cc-args))))
        (call-with-values (lambda () (process-wait proc))
          (lambda (pid normal-exit? status)
            (delete-file* c-file)
            (if normal-exit? status 1)))))))

(define (semantic-process-forms raw-forms input-source)
  (if (eq? input-source 'stdin)
      (semen-process raw-forms)
      (with-directory input-source
                      (semen-process raw-forms))))

(define prelude
  '((include inttypes.h)

    (typedef u8 uint8-t)
    (typedef i8 int8-t)
    (typedef u16 uint16-t)
    (typedef i16 int16-t)
    (typedef u32 uint32-t)
    (typedef i32 int32-t)
    (typedef u64 uint64-t)
    (typedef i64 int64-t)))

(define (main)
  (let* ((argv (command-line-arguments))
         (raw-args (args-before-separator argv))
         (cc-args (args-after-separator argv))
         (args (getopt-long raw-args
                            opts-grammar))
         (output (get-arg args 'output 'default))
         (help (help-arg? args))

         (input (get-input-file args))
         (current-dir (current-directory)))
    (call/cc
     (lambda (return)
       (when help
         (print-help)
         (return #f))
       (when (get-arg args 'public-interface #f)
         (assert (not (eq? input 'stdin)) "Error: --public-interface requires file argument")

         (write-to-file-or-stdout
          output
          (fn
           (map pp (reverse
                    (read-public-interface input)))))
         (return #f))
       (load-persistent-module-paths)

       ;; The file name in a #line directive now comes from the form's
       ;; own recorded location, so imported modules report themselves
       ;; rather than the unit that imported them
       (sex-line-directives (line-directives-arg args))
       (when (and (get-arg args 'emit-c #f)
                  (not (get-arg args 'line-directives #f)))
         (sex-line-directives 'none))

       (let* ((raw-forms (append prelude (read-raw-forms input)))
              (sex-forms (semantic-process-forms raw-forms input)))
         (if (or (get-arg args 'macro-expand #f)
                 (get-arg args 'emit-c #f))
             ;; Emit processed and macro-expanded sex code, or emit C code
             (emit-c-or-sex sex-forms output args)
             ;; Compile file! The C compiler's status is ours too
             (let ((status (compile-to-file sex-forms output args cc-args)))
               (unless (zero? status)
                 (exit status)))))))))
