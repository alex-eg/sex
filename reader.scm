;;; Sex reader
;;;
;;; A hand-written tokenizer + recursive-descent parser that replaces
;;; CHICKEN's built-in `read'. We need our own reader because the
;;; features Sex requires cannot be expressed on top of `read':
;;;   - [ ... ] array/pointer sugar, read as (¤ ...)
;;;   - a leading `.' rewritten to the symbol `dot-access'
;;;   - `;' comments preserved as (comment "...") forms, so they can be
;;;     re-emitted into the generated C (keeping the source mapping)
;;; It also records the source location of every form it reads (see
;;; utils' form-source), so the C writer can emit #line directives.

(import
  scheme
  (scheme base)                         ; make-parameter
  (chicken base)
  (chicken pathname)
  utils)

;;; Sentinels for structural tokens
(define close-paren   (list '%close-paren))
(define close-bracket (list '%close-bracket))
(define dot-token     (list '%dot))

;;; Current source line. Tracked as characters are consumed
(define current-line (make-parameter 1))

(define (get-ch port)
  (let ((c (read-char port)))
    (when (and (char? c) (char=? c #\newline))
      (current-line (+ 1 (current-line))))
    c))

(define (peek port)
  (peek-char port))

;;; Record where a form started. Called with the line of the opening
;;; delimiter, sampled before it is consumed
(define (stamp form line)
  (when (pair? form)
    (set-form-source! form (current-source-file) line))
  form)

(define (delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\" #\; #\' #\` #\,))))

;;; Skip whitespace. `;' comments are NOT skipped here: they are read
;;; as (comment "...") forms by the tokenizer. Block comments (#| |#)
;;; and datum comments (#;) are discarded in the tokenizer's `#'
;;; dispatch, since `#' also introduces real data (#t, #f, #\c, #(...))
(define (skip-whitespace port)
  (let ((c (peek port)))
    (cond
     ((eof-object? c) #t)
     ((char-whitespace? c) (get-ch port) (skip-whitespace port))
     (else #t))))

;;; A `;' comment, read as (comment "<rest of line>"). The leading `;'
;;; is consumed; the newline is left in the stream so line tracking and
;;; the surrounding parser see it normally
(define (read-comment port)
  (get-ch port)                         ; consume the leading ;
  (let loop ((chars (list)))
    (let ((c (peek port)))
      (if (or (eof-object? c) (char=? c #\newline))
          (list 'comment (list->string (reverse chars)))
          (begin (get-ch port)
                 (loop (cons c chars)))))))

;;; Read the next token: a datum, one of the structural sentinels
;;; (close-paren / close-bracket / dot-token), or the eof-object
(define (next-token port)
  (skip-whitespace port)
  (let ((c (peek port)))
    (cond
     ((eof-object? c) c)
     ((char=? c #\()
      (let ((line (current-line)))
        (get-ch port)
        (stamp (read-list port close-paren) line)))
     ((char=? c #\[)
      (let ((line (current-line)))
        (get-ch port)
        (stamp (cons '¤ (read-list port close-bracket)) line)))
     ((char=? c #\)) (get-ch port) close-paren)
     ((char=? c #\]) (get-ch port) close-bracket)
     ((char=? c #\;)
      (let ((line (current-line)))
        (stamp (read-comment port) line)))
     ((char=? c #\") (read-string-lit port))
     ((char=? c #\') (get-ch port) (list 'quote (read-datum port)))
     ((char=? c #\`) (get-ch port) (list 'quasiquote (read-datum port)))
     ((char=? c #\,)
      (get-ch port)
      (if (eqv? (peek port) #\@)
          (begin (get-ch port) (list 'unquote-splicing (read-datum port)))
          (list 'unquote (read-datum port))))
     ((char=? c #\#) (get-ch port) (read-hash port))
     (else (read-atom port)))))

;;; Like next-token, but a full datum is required: the structural
;;; sentinels and eof are errors here (e.g. after a quote or `.')
(define (read-datum port)
  (let ((tok (next-token port)))
    (cond
     ((eof-object? tok) (error "Unexpected end of input"))
     ((eq? tok close-paren) (error "Unexpected )"))
     ((eq? tok close-bracket) (error "Unexpected ]"))
     ((eq? tok dot-token) (error "Unexpected ."))
     (else tok))))

;;; Read list elements up to close-paren or close-bracket,
;;; honoring dotted-pair notation (a b . c)
(define (read-list port closer)
  (let loop ((acc (list)))
    (let ((tok (next-token port)))
      (cond
       ((eof-object? tok) (error "Unexpected end of input inside list"))
       ((eq? tok close-paren)
        (if (eq? closer close-paren)
            (reverse acc)
            (error "Unmatched closing bracket")))
       ((eq? tok close-bracket)
        (if (eq? closer close-bracket)
            (reverse acc)
            (error "Unmatched closing bracket")))
       ((eq? tok dot-token)
        (if (null? acc)
            ;; Leading `.': the member/method access operator. It reads
            ;; as an ordinary `dot-access' symbol in first position.
            (loop (cons 'dot-access acc))
            ;; Otherwise: ordinary dotted-pair notation (a b . c).
            (let ((tail (read-datum port))
                  (end (next-token port)))
              (unless (eq? end closer)
                (error "Malformed dotted list"))
              (append-reverse acc tail))))
       (else (loop (cons tok acc)))))))

;;; Append the reversed list `rev' in front of `tail', producing a
;;; possibly-improper list (used for dotted pairs)
(define (append-reverse rev tail)
  (if (null? rev)
      tail
      (append-reverse (cdr rev) (cons (car rev) tail))))

;;; A bare atom: symbol or number, or the dot token when it is exactly "."
(define (read-atom port)
  (let loop ((chars (list)))
    (let ((c (peek port)))
      (if (delimiter? c)
          (finish-atom (list->string (reverse chars)))
          (begin (get-ch port) (loop (cons c chars)))))))

(define (finish-atom s)
  (cond
   ((string=? s ".") dot-token)
   ((string->number s) => identity)
   (else (string->symbol s))))

;;; #-dispatch: booleans, characters, vectors, block/datum comments
(define (read-hash port)
  (let ((c (get-ch port)))
    (cond
     ((eof-object? c) (error "Unexpected end of input after #"))
     ((or (char=? c #\t) (char=? c #\T)) (read-bool port #t))
     ((or (char=? c #\f) (char=? c #\F)) (read-bool port #f))
     ((char=? c #\\) (read-char-lit port))
     ((char=? c #\() (list->vector (read-list port close-paren)))
     ((char=? c #\|) (skip-block-comment port 1) (next-token port))
     ((char=? c #\;) (read-datum port) (next-token port)) ; datum comment
     (else (error "Unsupported # syntax" c)))))

;;; #t / #true / #f / #false. `val' is the boolean; consume any
;;; trailing name characters and validate
(define (read-bool port val)
  (let ((rest (read-atom-string port)))
    (cond
     ((string=? rest "") val)
     ((and val (string=? rest "rue")) val)
     ((and (not val) (string=? rest "alse")) val)
     (else (error "Malformed boolean literal" rest)))))

(define (read-atom-string port)
  (let loop ((chars (list)))
    (let ((c (peek port)))
      (if (delimiter? c)
          (list->string (reverse chars))
          (begin (get-ch port) (loop (cons c chars)))))))

(define named-chars
  '(("space" . #\space) ("newline" . #\newline) ("tab" . #\tab)
    ("return" . #\return) ("nul" . #\nul) ("null" . #\nul)
    ("delete" . #\delete) ("escape" . #\escape) ("alarm" . #\alarm)
    ("backspace" . #\backspace)))

(define (read-char-lit port)
  (let ((first (get-ch port)))
    (when (eof-object? first)
      (error "Unexpected end of input in character literal"))
    (if (char-alphabetic? first)
        (let ((rest (read-atom-string port)))
          (if (string=? rest "")
              first
              (let ((name (string-append (string first) rest)))
                (cond
                 ((assoc name named-chars) => cdr)
                 (else (error "Unknown character name" name))))))
        first)))

;;; String literal with escape processing, matching the common escapes
;;; the previous reader (CHICKEN `read') interpreted
(define (read-string-lit port)
  (get-ch port)                         ; consume opening quote
  (let loop ((chars (list)))
    (let ((c (get-ch port)))
      (cond
       ((eof-object? c) (error "Unterminated string literal"))
       ((char=? c #\") (list->string (reverse chars)))
       ((char=? c #\\) (loop (cons (read-escape port) chars)))
       (else (loop (cons c chars)))))))

(define (read-escape port)
  (let ((c (get-ch port)))
    (cond
     ((eof-object? c) (error "Unterminated string literal"))
     ((char=? c #\n) #\newline)
     ((char=? c #\t) #\tab)
     ((char=? c #\r) #\return)
     ((char=? c #\a) #\alarm)
     ((char=? c #\b) #\backspace)
     ((char=? c #\f) (integer->char 12))
     ((char=? c #\v) (integer->char 11))
     ((char=? c #\0) #\nul)
     (else c))))                        ; \" \\ and anything else: literal

(define (skip-block-comment port depth)
  (if (= depth 0)
      #t
      (let ((c (get-ch port)))
        (cond
         ((eof-object? c) (error "Unterminated block comment"))
         ((and (char=? c #\|) (eqv? (peek port) #\#))
          (get-ch port) (skip-block-comment port (- depth 1)))
         ((and (char=? c #\#) (eqv? (peek port) #\|))
          (get-ch port) (skip-block-comment port (+ depth 1)))
         (else (skip-block-comment port depth))))))

;;; Read every top-level form from `port'. Locations are recorded by
;;; next-token, for every form rather than only these
(define (parse-all port)
  (parameterize ((current-line 1))
    (let loop ((acc (list)))
      (skip-whitespace port)
      (let ((tok (next-token port)))
        (cond
         ((eof-object? tok) (reverse acc))
         ((or (eq? tok close-paren)
              (eq? tok close-bracket))
          (error "Unmatched closing bracket at top level"))
         ((eq? tok dot-token)
          (error "Unexpected . at top level"))
         (else
          (loop (cons tok acc))))))))

;;; Entry point: read all forms from a file, or from the current input
;;; port when the source is 'stdin
(define (read-from-file file)
  ;; Resolve the name before with-directory moves us, so an imported
  ;; module's forms carry that module's path rather than the importer's
  (let ((source-file (to-absolute-pathname file)))
    (with-directory file
      (with-input-from-file (pathname-strip-directory file)
        (lambda ()
          (parameterize ((current-source-file source-file))
            (parse-all (current-input-port))))))))

(define (read-raw-forms input-source)
  (if (eq? input-source 'stdin)
      (parameterize ((current-source-file "stdin"))
        (parse-all (current-input-port)))
      (read-from-file input-source)))
