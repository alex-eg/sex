(declare (unit sexc)
         (uses fmt-c-writer
               sex-reader
               semen))

(include "utils.macros.scm")

(import brev-separate
        (chicken file)
        (chicken plist)
        (chicken pretty-print)
        (chicken process)
        (chicken process-context)
        (chicken port)
        fmt
        getopt-long
        srfi-1                          ; list routines
        srfi-13
        tree)

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
              (single-char #\o)))))

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

(define (get-rest-args args)
  (cdr (assoc '@ args)))

(define (get-c-compiler-args args)
  (filter (fn (string-prefix? "-" x)) (get-rest-args args)))

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

(define (compile-to-file sex-forms output args)
  (let ((compiler (or (get-arg args 'c-compiler #f)
                      (get-env-var "SEX_CC")
                      "cc"))
        (out-file (if (eq? output 'default)
                      "a.out"
                      output)))
    (call-with-values
        (lambda ()
          (process compiler (append (list "-o" out-file "-x" "c")
                                    (if (get-arg args 'compile-object #f)
                                        (list "-c")
                                        (list))
                                    (list "-")  ; read stdin
                                    (get-c-compiler-args args))))
      (lambda (out-port in-port pid)
        (with-output-to-port in-port
          (lambda () (emit-c sex-forms)))
        (close-output-port in-port)
        (process-wait pid)))))

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
  (let* ((raw-args (command-line-arguments))
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

       (let* ((raw-forms (append prelude (read-raw-forms input)))
              (sex-forms (semantic-process-forms raw-forms input)))
         (if (or (get-arg args 'macro-expand #f)
                 (get-arg args 'emit-c #f))
             ;; Emit processed and macro-expanded sex code, or emit C code
             (emit-c-or-sex sex-forms output args)
             ;; Compile file!
             (compile-to-file sex-forms output args)))))))
