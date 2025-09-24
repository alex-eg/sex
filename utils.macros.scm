(import (chicken process-context))

(define-syntax prog1
  (syntax-rules ()
    ((prog1 form . forms)
     (let ((res form))
       (begin . forms)
       res))))

(define-syntax with-directory
  (syntax-rules ()
    ((with-directory path form . forms)
     (let ((current-dir (current-directory)))
       (set-working-directory path)
       (prog1
        (begin form . forms)
        (change-directory current-dir))))))
