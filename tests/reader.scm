(import (chicken port))

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
)
