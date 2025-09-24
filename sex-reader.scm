(declare (unit sex-reader))

(include "utils.macros.scm")

(import (chicken pathname)
        brev-separate
        fmt)

(define (read-forms acc)
  (let ((r (read)))
    (if (eof-object? r) (reverse acc)
        (read-forms (cons r acc)))))

(define (read-from-file file)
  (with-directory file
    (with-input-from-file (pathname-strip-directory file)
      (fn (read-forms (list))))))

(define (read-raw-forms input-source)
  (if (eq? input-source 'stdin)
      (read-forms (list))
      (read-from-file input-source)))
