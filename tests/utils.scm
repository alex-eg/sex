(test-group "utils"

  (test
   '((1) (2) (3))
   (list-split '(1 * 2 * 3) '*))

  (test
   '((1 2 3))
   (list-split '(1 2 3) '*))

  (test
   '(() (1) (2) (3) ())
   (list-split '(* 1 * 2 * 3 *) '*))

  (test
   '((const) (const struct something))
   (list-split '(const * const struct something) '*))

  (test
   '(1 * 2 * 3)
   (list-join '(1 2 3) '*))
)
