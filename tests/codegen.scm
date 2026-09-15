;;; Codegen details that are easy to get subtly wrong, and that the
;;; walk-* unit tests cannot see: they check the intermediate form we
;;; hand to fmt-c, not the C that fmt-c renders from it.
;;;
;;; Both cases below were found by writing an OpenGL example, not by
;;; the existing suite, because both need an operand shape that no
;;; earlier test program happened to use.

(import (chicken port)
        (chicken string)
        srfi-13
        fmt-c-writer
        reader
        semen
        utils)

(define (sex->c source)
  "Compile SOURCE, a string of Sex, and return the generated C."
  (let ((forms (with-input-from-string source
                 (lambda ()
                   (parameterize ((current-source-file "codegen.sex"))
                     (parse-all (current-input-port)))))))
    (with-output-to-string
      (lambda ()
        (parameterize ((sex-line-directives 'none))
          (emit-c (semen-process forms)))))))

(define (emits? source fragment)
  (and (string-contains (sex->c source) fragment) #t))

(define (in-fn body)
  (string-append "(fn f ((a int) (b int)) void " body ")"))

(test-group "codegen"

  ;; c-switch handed its scrutinee straight to `cat', which only works
  ;; when it is an atom. Anything else was displayed as a raw
  ;; s-expression: `switch ((%. e type))'.
  (test-group "switch scrutinee"
    (test-assert "member access"
      (emits? "(struct s ((type int))) (fn f ((e (struct s))) void (switch (. e type) (case 1 (g))))"
              "switch (e.type)"))
    (test-assert "call"
      (emits? (in-fn "(switch (g a) (case 1 (h)))")
              "switch (g(a))"))
    (test-assert "arithmetic"
      (emits? (in-fn "(switch (+ a b) (case 1 (h)))")
              "switch (a + b)")))

  ;; A cast binds tighter than every binary operator, so an operand
  ;; that is itself a binary expression has to be parenthesised --
  ;; otherwise the cast silently applies to the first operand only.
  (test-group "cast precedence"
    (test-assert "binary operand is parenthesised"
      (emits? (in-fn "(var p (* void) (cast (* 2 (sizeof int)) (* void)))")
              "(void *)(2 * sizeof(int))"))
    (test-assert "subtraction operand is parenthesised"
      (emits? (in-fn "(var f float (cast (- a b) float))")
              "(float)(a - b)"))
    ;; ...but exactly once. The operand used to parenthesise itself
    ;; again inside the parens the cast had just added.
    (test-assert "and not parenthesised twice"
      (not (emits? (in-fn "(var f float (cast (- a b) float))")
                   "(float)((a - b))")))
    ;; Unary operands are already unary-expressions and must be left
    ;; alone, or every existing cast in the tree gains noise.
    (test-assert "identifier is left bare"
      (emits? (in-fn "(var f float (cast a float))")
              "(float)a"))
    (test-assert "address-of is left bare"
      (emits? (in-fn "(var p (* int) (cast (& a) (* int)))")
              "(int *)&a"))
    (test-assert "sizeof is left bare"
      (emits? (in-fn "(var n int (cast (sizeof int) int))")
              "(int)sizeof(int)")))

  ;; A `;' comment is a form, so one written inside a construct with
  ;; positional slots used to land in a slot and shift everything after
  ;; it -- silently. In an `if' the comment became the then-arm and the
  ;; then-arm became an `else if' condition, and it still compiled.
  ;; Comments are now taken out of the slots and emitted just before the
  ;; statement; comments in a body stay where they were written.
  (test-group "comments in positional slots"
    (test-assert "an if arm is not shifted"
      (not (emits? (in-fn "(if 1 ;; c\n (g 1) (g 2))") "else if")))
    (test-assert "and both arms survive"
      (emits? (in-fn "(if 1 ;; c\n (g 1) (g 2))") "g(1)"))
    (test-assert "the comment survives too"
      (emits? (in-fn "(if 1 ;; kept here\n (g 1) (g 2))") "kept here"))
    (test-assert "a for header is not shifted"
      (emits? (in-fn "(for ;; c\n (var i int 0) (< i 2) (++ i) (g i))")
              "for (int i = 0; i < 2; ++i)"))
    (test-assert "a while condition is not shifted"
      (emits? (in-fn "(while ;; c\n (< a b) (g 1))") "while (a < b)"))
    (test-assert "a var is not shifted"
      (emits? (in-fn "(var ;; c\n x int 5)") "int x = 5"))
    (test-assert "a cast is not shifted"
      (emits? (in-fn "(var y int (cast ;; c\n a int))") "(int)a"))
    ;; Bodies are a statement sequence, so comments there stay put.
    (test-assert "a comment in a body stays in the body"
      (emits? (in-fn "(while (< a b) ;; inside\n (g 1))")
              "while (a < b) {"))))
