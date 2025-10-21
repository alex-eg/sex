(import (chicken port))

(test-group "reader"
  ;; []-syntax. For array types and array access expressions
  (test '((¤ * char))
        (with-input-from-string "[* char]"
          (lambda ()
            (read-raw-forms 'stdin))))

  (test '((¤ * * char const 512))
        (with-input-from-string "[* * char const 512]"
          (lambda ()
            (read-raw-forms 'stdin))))

  (test '((¤))
        (with-input-from-string "[]"
          (lambda ()
            (read-raw-forms 'stdin))))

  (test '((¤ (¤)))
        (with-input-from-string "[[]]"
          (lambda ()
            (read-raw-forms 'stdin))))

  (test '((¤ (¤ const char)))
        (with-input-from-string "[[const char]]"
          (lambda ()
            (read-raw-forms 'stdin)))))
