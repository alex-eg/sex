(declare (unit sex-reader))

(include "utils.macros.scm")

(import
  (chicken base)
  (chicken io)
  (chicken pathname)
  (chicken port)
  (chicken read-syntax)
  (chicken string)
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

(define (read-bracket port)
  (let loop ((c (read-char port))
             (str (string)))
    (cond ((char=? c #\])
           (cons '¤
                 (with-input-from-string str
                   (fn (port-map identity read)))))
          ((char=? c #\[)
           (loop port (conc )))
          (else
           (loop (read-char port)
                 (conc str c))))))

(define open-bracket-counter (make-parameter 0))

(define (read-raw-forms input-source)
  (let ((bracket-end (gensym)))
   (set-read-syntax!
    #\]
    (lambda (port)
      (when (= 0 (open-bracket-counter))
        (error "Unmatched closing bracket"))
      (open-bracket-counter (- (open-bracket-counter) 1))
      bracket-end))

   (set-read-syntax!
    #\[
    (lambda (port)
      (open-bracket-counter (+ (open-bracket-counter) 1))
      (let loop ((r (read port))
                 (acc (list)))
        (if (eq? r bracket-end)
            (cons '¤ (reverse acc))
            (loop (read port)
                  (cons r acc)))))))
  (if (eq? input-source 'stdin)
      (read-forms (list))
      (read-from-file input-source)))
