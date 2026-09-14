;;; The type database.
;;;
;;; Names here are prefixed so they cannot collide with the types the
;;; other suites register: the database is one table in the linked
;;; binary, and semen fills it in whenever a suite compiles a struct.

(import types)

(test-group "types"

  (add-struct 't-point '(struct t-point ((x int) (y int))))
  (test "fields come back as (name type)"
        '((x int) (y int))
        (get-fields 't-point))
  (test "and the whole entry is tagged"
        '(struct t-point ((x int) (y int)))
        (get-type-info 't-point))

  ;; Fields are written with the type last, so one entry can declare
  ;; several names. They come back as one field each.
  (add-struct 't-settings
              '(pub struct t-settings ((x y w h u32) (title (* const char)))))
  (test "names sharing a type are split apart"
        '((x u32) (y u32) (w u32) (h u32) (title (* const char)))
        (get-fields 't-settings))

  (add-struct 't-commented '(struct t-commented ((comment " hi") (a int))))
  (test "a comment among the fields is not a field"
        '((a int))
        (get-fields 't-commented))

  (add-union 't-value '(union t-value ((i int) (f float))))
  (test "unions have fields too"
        '((i int) (f float))
        (get-fields 't-value))

  (add-enum 't-color '(enum t-color (red green blue)))
  (test "enums keep their values"
        '(enum t-color (red green blue))
        (get-type-info 't-color))
  (test "but have no fields"
        #f
        (get-fields 't-color))

  (add-typedef 't-u8 '(typedef t-u8 uint8-t))
  (test "a typedef resolves to its target"
        'uint8-t
        (get-underlying-type 't-u8))
  (add-typedef 't-byte '(typedef t-byte t-u8))
  (test "and chains are followed to the end"
        'uint8-t
        (get-underlying-type 't-byte))
  (test "a struct is not a typedef"
        #f
        (get-underlying-type 't-point))

  ;; A #define keeps its value forms -- there can be more than one.
  (add-define 't-maxn '(define t-maxn 8))
  (test "defines are recorded"
        '(define t-maxn (8))
        (get-type-info 't-maxn))

  ;; map-fields walks an aggregate, handing each field to a function.
  (test "map-fields visits every field"
        '((x int) (y int))
        (map-fields 't-point (lambda (name type) (list name type))))
  (test "and splits shared names apart too"
        '(x y w h title)
        (map-fields 't-settings (lambda (name type) name)))
  ;; An enumerator's type is the enum itself.
  (test "enum values are fields whose type is the enum"
        '((red (enum t-color)) (green (enum t-color)) (blue (enum t-color)))
        (map-fields 't-color (lambda (name type) (list name type))))
  (test "a typedef has no fields to map"
        #f
        (map-fields 't-u8 (lambda (name type) name)))
  (test "nor does an undeclared name"
        #f
        (map-fields 't-nothing (lambda (name type) name)))

  ;; type-match compares whole types, since a type is a form.
  (test "a bare type matches"      'yes (type-match 'int (int 'yes) (else 'no)))
  (test "so does a compound one"   'yes (type-match '(* const char)
                                                    (int 'no)
                                                    ((* const char) 'yes)
                                                    (else 'no)))
  ;; The [int 10] of the docstring is Sex notation: the Sex reader turns
  ;; brackets into a ¤ form, while CHICKEN reads them as plain parens.
  ;; In a .scm file the array type has to be written out.
  (test "and an array"             'yes (type-match '(¤ int 10)
                                                    ((¤ int 10) 'yes)
                                                    (else 'no)))
  (test "else catches the rest"    'no  (type-match '(* void) (int 'yes) (else 'no)))
  (test "a near miss does not match" 'no (type-match '(¤ int 20)
                                                     ((¤ int 10) 'yes)
                                                     (else 'no)))
  (test "no clause matching and no else is #f"
        #f
        (type-match 'float (int 'yes)))

  (test "an undeclared name has no entry"
        #f
        (get-type-info 't-never-declared))
  (test "and no fields"
        #f
        (get-fields 't-never-declared)))
