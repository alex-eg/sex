;;; Source-line mapping.
;;;
;;; Every construct in the generated C must be attributed, through
;;; #line directives, to the source line of the Sex form it came from.
;;; That mapping is the whole basis of source-level debugging.
;;;
;;; It cannot be left to the C compiler's implicit line counting,
;;; because a Sex form and its C rendering may or may not occupy the
;;; same number of lines. A call written across four lines renders as
;;; one C line; a one-line `for' renders as a braced block of
;;; four. Either way every following line drifts, and the drift
;;; accumulates over a function body.
;;;
;;; The tests below pin one construct per statement kind. Each carries
;;; a unique numeric marker chosen so that it lands on the first C line
;;; that construct emits; the marker is then located in the fixture (to
;;; get the true source line) and in the C output (to get the line the
;;; directives claim). The two must agree.

(import (chicken port)
        (chicken string)
        srfi-1
        srfi-13
        fmt-c-writer
        reader
        semen
        utils)

;;; ------------------------------------------------------------------
;;; Fixture. Line numbers are in the trailing comments; keep them
;;; correct when editing. Markers are distinct 3-digit integers, and no
;;; other literal in the fixture contains one as a substring.

(define fixture-lines
  '("(include stdio.h)"                   ;  1
    ""                                    ;  2
    "(struct pt"                          ;  3  multi-line toplevel
    "  ((x int)"                          ;  4
    "   (y int)))"                        ;  5
    ""                                    ;  6
    "(enum color (red green blue))"       ;  7
    ""                                    ;  8
    "(typedef byte u8)"                   ;  9
    ""                                    ; 10
    "(define MAXN 101)"                   ; 11
    ""                                    ; 12
    "(var gvar int 102)"                  ; 13
    ""                                    ; 14
    "(extern var evar int)"               ; 15
    ""                                    ; 16
    "(fn helper ((a int)) int)"           ; 17  prototype
    ""                                    ; 18
    "(pub fn main () int"                 ; 19
    "  (var mvar int 103)"                ; 20
    "  (var p (struct pt))"               ; 21
    "  (var arr [int 4])"                 ; 22
    "  (= (. p x) 104)"                   ; 23
    "  (+= mvar 105)"                     ; 24
    "  (++ mvar)"                         ; 25
    "  (= [arr 0] 106)"                   ; 26
    "  (putchar (+ 107"                   ; 27  form spanning 3 lines,
    "             mvar"                   ; 28  emitted as one C line:
    "             0))"                    ; 29  everything after drifts
    "  (var aftercall int 108)"           ; 30
    "  (if (< mvar 109)"                  ; 31
    "      (putchar 110))"                ; 32
    "  (if (< mvar 111)"                  ; 33
    "      (putchar 112)"                 ; 34
    "      (putchar 113))"                ; 35
    "  (while (< mvar 114)"               ; 36
    "    (++ mvar))"                      ; 37
    "  (for (var i int 115)"              ; 38
    "       (< i 116)"                    ; 39
    "       (++ i)"                       ; 40
    "    (continue))"                     ; 41
    "  (switch 117"                       ; 42
    "    (case 118"                       ; 43
    "      (putchar 119)"                 ; 44
    "      (break))"                      ; 45
    "    (default"                        ; 46
    "      (putchar 120)))"               ; 47
    "  (begin"                            ; 48
    "    (var bvar int 121)"              ; 49
    "    (putchar bvar))"                 ; 50
    "  (goto done)"                       ; 51
    "  (: done)"                          ; 52
    "  (var svar u64 (sizeof (struct pt)))" ; 53
    "  (var cvar int (cast mvar int))"    ; 54
    "  (return 122))"                     ; 55
    ))

(define fixture (string-intersperse fixture-lines "\n"))

;;; ------------------------------------------------------------------
;;; Pipeline and #line accounting

(define (compile-to-c source)
  "Run reader -> semen -> writer on SOURCE, returning the generated C.
parse-all is called directly rather than through read-raw-forms so the
fixture can be given a file name: a location is (file . line), and the
file is what makes an imported module report itself rather than the unit
that imported it."
  (let ((forms (with-input-from-string source
                 (lambda ()
                   (parameterize ((current-source-file "fixture.sex"))
                     (parse-all (current-input-port)))))))
    (with-output-to-string
      (lambda () (emit-c (semen-process forms))))))

(define (split-lines s)
  (let loop ((i 0) (start 0) (acc (list)))
    (cond
     ((= i (string-length s))
      (reverse (if (> i start) (cons (substring s start i) acc) acc)))
     ((char=? (string-ref s i) #\newline)
      (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
     (else (loop (+ i 1) start acc)))))

(define (directive-line text)
  "The N of a `#line N \"file\"' directive, or #f if TEXT is not one."
  (let ((t (string-trim text)))
    (and (string-prefix? "#line " t)
         (string->number (car (string-split (substring t 6) " "))))))

(define (directive-file text)
  "The file name of a `#line N \"file\"' directive, or #f."
  (let ((t (string-trim text)))
    (and (string-prefix? "#line " t)
         (let ((parts (string-split (substring t 6) " ")))
           (and (pair? (cdr parts)) (cadr parts))))))

(define (attributed-lines c-source)
  "Pair every non-directive C line with the source line it is
attributed to. `#line N' says the *next* physical line is N; each line
after that is one more. Lines before the first directive get #f."
  (let loop ((lines (split-lines c-source)) (cur #f) (acc (list)))
    (if (null? lines)
        (reverse acc)
        (cond
         ((directive-line (car lines))
          => (lambda (n) (loop (cdr lines) n acc)))
         (else
          (loop (cdr lines)
                (and cur (+ cur 1))
                (cons (cons (car lines) cur) acc)))))))

(define (sex-line token)
  "1-based fixture line containing TOKEN."
  (let loop ((lines fixture-lines) (n 1))
    (cond ((null? lines) #f)
          ((string-contains (car lines) token) n)
          (else (loop (cdr lines) (+ n 1))))))

(define (claimed-line attributed token)
  "The source line the generated C attributes TOKEN to."
  (let ((hit (find (lambda (p) (string-contains (car p) token)) attributed)))
    (and hit (cdr hit))))

;;; ------------------------------------------------------------------

(define c-out (compile-to-c fixture))
(define attributed (attributed-lines c-out))

;;; MAPS checks a construct found under the same token on both sides.
;;; MAPS/TOKENS is for constructs that are spelled differently in Sex
;;; and in C (`(break)' -> `break;', `(: done)' -> `done:').
(define-syntax maps
  (syntax-rules ()
    ((maps name token)
     (test name (sex-line token) (claimed-line attributed token)))))

(define-syntax maps/tokens
  (syntax-rules ()
    ((maps/tokens name sex-token c-token)
     (test name (sex-line sex-token) (claimed-line attributed c-token)))))

(test-group "line-directives"

  ;; Toplevel forms
  (test-group "toplevel"
    (maps/tokens "include"        "(include stdio.h)" "#include")
    (maps/tokens "struct"         "(struct pt"        "struct pt {")
    (maps/tokens "enum"           "(enum color"       "enum color {")
    (maps/tokens "typedef"        "(typedef byte u8)" "typedef u8 byte;")
    (maps         "define"        "101")
    (maps         "global var"    "102")
    (maps/tokens "extern var"     "(extern var evar" "extern int evar")
    (maps/tokens "prototype"      "(fn helper"       "int helper (int a)")
    (maps/tokens "function"       "(pub fn main"     "int main (void)"))

  ;; Declarations and expression statements
  (test-group "statements"
    (maps "var decl"              "103")
    (maps "member assignment"     "104")
    (maps "compound assignment"   "105")
    (maps/tokens "increment"      "(++ mvar)" "++mvar;")
    (maps "array assignment"      "106")

    ;; The point of the whole exercise: a call spread over three source
    ;; lines collapses to one C line, so the statement after it must be
    ;; re-anchored or it is reported two lines too early. The call
    ;; itself anchors to the line it *starts* on, which is where a
    ;; debugger should report it.
    (maps "multi-line call"       "107")
    (maps "statement after it"    "108"))

  ;; Control flow
  (test-group "control flow"
    (maps "if"                    "109")
    (maps "if body"               "110")
    (maps "if/else"               "111")
    (maps "then branch"           "112")
    (maps "else branch"           "113")
    (maps "while"                 "114")
    (maps "for"                   "115")
    (maps/tokens "continue"       "(continue)" "continue;")
    (maps "switch"                "117")
    ;; The `case 118:' label line itself is deliberately not pinned.
    ;; c-switch requires every clause to be a case/default form and
    ;; rejects anything else, so no anchor can be placed between
    ;; clauses. The clause *bodies* are anchored from inside, which is
    ;; what matters -- a label is not a statement a debugger stops on.
    (maps "case body"             "119")
    (maps/tokens "break"          "(break)" "break;")
    (maps "default body"          "120")
    (maps "block"                 "121")
    (maps/tokens "goto"           "(goto done)" "goto done;")
    (maps/tokens "label"          "(: done)"    "done:")
    (maps "return"                "122"))

  ;; Expressions that are their own statement
  (test-group "expressions"
    (maps/tokens "sizeof" "(var svar" "sizeof")
    (maps/tokens "cast"   "(var cvar" "(int)mvar"))

  ;; A location is (file . line); every directive must name the file the
  ;; form was read from.
  (test-group "file name"
    (test "every directive names the fixture"
          (list "\"fixture.sex\"")
          (delete-duplicates
           (filter values (map directive-file (split-lines c-out)))))))
