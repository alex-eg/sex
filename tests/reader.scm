(import (chicken port)
        reader)

(define-syntax reader-test
  (syntax-rules ()
    ((reader-test result string)
     (test result
           (with-input-from-string string
             (lambda () (read-raw-forms 'stdin)))))))

(test-group "reader"
  ;; []-syntax. For array types and array access expressions
  (reader-test '((¤ * char)) "[* char]")
  (reader-test '((¤ * * char const 512)) "[* * char const 512]")
  (reader-test '((¤)) "[]")
  (reader-test '((¤ (¤))) "[[]]")
  (reader-test '((¤ (¤ const char))) "[[const char]]")

  ;; leading `.' becomes the dot-access operator
  (reader-test '((dot-access obj field)) "(. obj field)")
  (reader-test '((dot-access obj method a b)) "(. obj method a b)")
  (reader-test '((dot-access a b)) "(.  a  b)")
  ;; nested leading dot
  (reader-test '((foo (dot-access a b))) "(foo (. a b))")

  ;; dotted pairs are preserved (only a *leading* dot is special)
  (reader-test '((a . b)) "(a . b)")
  (reader-test '((a b . c)) "(a b . c)")
  (reader-test '((quote (a . b))) "'(a . b)")
  ;; a `.'-prefixed symbol is an ordinary symbol, not dot-access
  (reader-test '((.field obj)) "(.field obj)")

  ;; `;' comments are preserved as (comment "...") forms
  (reader-test '((comment " hi")) "; hi")
  (reader-test '((comment ";; Prototypes")) ";;; Prototypes")
  (reader-test '((foo (comment " c") bar)) "(foo ; c\n bar)")
  ;; a trailing top-level comment is its own form
  (reader-test '((a b) (comment " t")) "(a b) ; t")
  ;; a `;' inside a string is not a comment
  (reader-test '("a;b") "\"a;b\"")
)
