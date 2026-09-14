;;; Splitting the command line at `--'.
;;;
;;; getopt-long cannot do this: it consumes the separator and merges
;;; everything after it into `@' alongside the input file. So sexc
;;; splits the raw argv first, and only the head is parsed as options.
;;; Everything else reaches the C compiler exactly as written --
;;; including the words that do not start with a dash, which a previous
;;; leading-dash heuristic used to drop.

(import sexc)

(define full '("foo.sex" "-o" "bar" "--" "-framework" "OpenGL" "-Wall"))

(test-group "argument separator"

  (test "options and input file stay with sexc"
        '("foo.sex" "-o" "bar")
        (args-before-separator full))

  (test "the tail reaches the compiler verbatim"
        '("-framework" "OpenGL" "-Wall")
        (args-after-separator full))

  ;; The case that motivated this: `OpenGL' has no leading dash and was
  ;; silently dropped, leaving `-framework' to swallow whatever flag
  ;; came next.
  (test "a word without a dash survives"
        '("-framework" "OpenGL")
        (args-after-separator '("x.sex" "--" "-framework" "OpenGL")))

  (test "no separator means nothing for the compiler"
        '()
        (args-after-separator '("foo.sex" "-o" "bar")))

  (test "no separator leaves every argument with sexc"
        '("foo.sex" "-o" "bar")
        (args-before-separator '("foo.sex" "-o" "bar")))

  (test "a trailing separator is allowed"
        '()
        (args-after-separator '("foo.sex" "--")))

  (test "a leading separator leaves no input file"
        '()
        (args-before-separator '("--" "-lm")))

  ;; Only the first `--' separates; a later one is an ordinary compiler
  ;; argument (ld takes several).
  (test "only the first separator counts"
        '("-Wl,--as-needed" "--" "-lm")
        (args-after-separator '("x.sex" "--" "-Wl,--as-needed" "--" "-lm")))

  (test "an empty command line is handled"
        '()
        (args-before-separator '())))
