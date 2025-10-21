(test-begin "basic")

;;; unkebabify
(test '- (unkebabify '-))
(test '-- (unkebabify '--))
(test '-> (unkebabify '->))
(test '-= (unkebabify '-=))
(test 'kebab_case (unkebabify 'kebab-case))
(test '_what_ (unkebabify '-what-))
(test 'this->member (unkebabify 'this->member))
(test '_this_->_member_ (unkebabify '-this-->-member-))
(test '__->>> (unkebabify '--->>>))

;;; atom-to-fmt-c
(test '%fun (atom-to-fmt-c 'fn))
(test '%prototype (atom-to-fmt-c 'prototype))
(test '%var (atom-to-fmt-c 'var))
(test '%block-begin (atom-to-fmt-c 'begin))
(test '%define (atom-to-fmt-c 'define))
(test '%pointer (atom-to-fmt-c 'pointer))
(test '%array (atom-to-fmt-c 'array))
(test 'vector-ref (atom-to-fmt-c '¤))
(test '%include (atom-to-fmt-c 'include))
(test '%cast (atom-to-fmt-c 'cast))

;;; c89 stuff
(test 'int (atom-to-fmt-c 'bool))
(test 1 (atom-to-fmt-c 'true))
(test 0 (atom-to-fmt-c 'false))

;;; make-field-access
(test 'a.b (make-field-access '(.b a)))
(test 'a.b.c (make-field-access '(.c a.b)))

(test-end)
