(declare (uses sexc))

(import
  (chicken process)
  (chicken process-context)
  srfi-1
  test)

(include "basic.scm")
(include "types.scm")

;;; Should be the last in the test suite
(test-exit)
