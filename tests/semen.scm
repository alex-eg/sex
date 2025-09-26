(define print-str-fn
  '(fn void print-str ((string s))
       (printf "%s" s)))

(define sum-fn
  '(pub fn float sum ((int a) (int b))
        (return (cast float (+ a b)))))

(test-begin "semen")
(test-assert (sex-fn? print-str-fn))
(test #f (sex-fn-public? print-str-fn))
(test 'void (sex-fn-return-type print-str-fn))
(test 'print-str (sex-fn-name print-str-fn))
(test '((string s)) (sex-fn-arglist print-str-fn))
(test '(fn void print-str ((string s))) (sex-fn-prototype print-str-fn))
(test '((printf "%s" s)) (sex-fn-body print-str-fn))

(test-assert (sex-fn? sum-fn))
(test #t (sex-fn-public? sum-fn))
(test 'float (sex-fn-return-type sum-fn))
(test 'sum (sex-fn-name sum-fn))
(test '((int a) (int b)) (sex-fn-arglist sum-fn))
(test '(pub fn float sum ((int a) (int b))) (sex-fn-prototype sum-fn))
(test '((return (cast float (+ a b)))) (sex-fn-body sum-fn))

(let ((sex-code
       '((defmacro (sum-var name a b c)
           `(var ,name ,(+ a b c)))

         (sum-var v 1 2 3))))

  (test '((var v 6)) (semen-process sex-code)))

;;; Macro expansion

(test 'a (semen-walk-form 'a identity))
(test '(a b c) (semen-walk-form '(a b c) identity))

(test 'a (semen-macro-expand 'a))
(test '(a b c) (semen-macro-expand '(a b c)))

(let ((sex-code-macro
       '((defmacro (x10 a)
           `(* 10 ,a))

         (fn void foo ((int a) (int b))
             (return (+ a (x10 b)))))))

  (test '((fn void foo ((int a) (int b))
              (return (+ a (* 10 b)))))
        (semen-process sex-code-macro)))

(test-end)
