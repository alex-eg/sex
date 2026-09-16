(import (chicken port)
        reader)

(define-syntax feature-test
  (syntax-rules ()
    ((feature-test result features string)
     (test result
           (parameterize ((current-features 'features))
             (with-input-from-string string
               (lambda () (read-raw-forms 'stdin))))))))

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

  ;; #+ / #- feature expressions. What does not apply is read and
  ;; dropped, so it never reaches the compiler at all
  (feature-test '((a)) (linux) "#+linux (a)")
  (feature-test '()    (macosx) "#+linux (a)")
  (feature-test '()    (linux) "#-linux (a)")
  (feature-test '((a)) (macosx) "#-linux (a)")
  ;; the guarded datum can be anything, not only a list
  (feature-test '(42) (x) "#+x 42")
  (feature-test '("s") (x) "#+x \"s\"")

  ;; and / or / not
  (feature-test '((a)) (unix linux) "#+(and unix linux) (a)")
  (feature-test '()    (unix) "#+(and unix linux) (a)")
  (feature-test '((a)) (unix) "#+(or linux unix) (a)")
  (feature-test '()    (bsd)  "#+(or linux unix) (a)")
  (feature-test '((a)) (unix) "#+(and unix (not macosx)) (a)")
  (feature-test '()    (unix macosx) "#+(and unix (not macosx)) (a)")
  ;; (and) is true and (or) is false, as they are in CL
  (feature-test '((a)) () "#+(and) (a)")
  (feature-test '()    () "#+(or) (a)")

  ;; a guard inside a form, including as the last element -- dropping
  ;; continues with the next token, so the closing paren still arrives
  (feature-test '((f 1 3)) (a) "(f #+a 1 #-a 2 3)")
  (feature-test '((f 2 3)) (b) "(f #+a 1 #-a 2 3)")
  (feature-test '((f 1))   (a) "(f #+a 1 #+b 2)")
  (feature-test '((f))     (b) "(f #+a 1)")
  ;; ...and as the last form in the file
  (feature-test '((a)) (x) "(a) #+y (b)")

  ;; guards nest
  (feature-test '((a)) (x y) "#+x #+y (a)")
  (feature-test '((b)) (x) "#+x #+y (a) (b)")

  ;; a feature the program was not given is simply absent
  (feature-test '() () "#+anything (a)")
)
