(declare (uses fmt-c-writer
               semen))

(import
  (chicken process)
  (chicken process-context)
  srfi-1
  test)

(include "basic.scm")
(include "semen.scm")
(include "reader.scm")
(include "fmt-c-writer.scm")
(include "utils.scm")

;;; Should be the last in the test suite
(test-exit)
