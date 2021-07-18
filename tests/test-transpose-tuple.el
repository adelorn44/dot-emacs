(require 'ert)
(require 'my-helpers)

(ert-deftest my-test-transpose-python-tuple ()
  "Tests the transposition of python tuples."
  (should (equal (my-transpose-tuple "(1, 2, 3)") "(3, 2, 1)"))
  (should (equal (my-transpose-tuple "(1, hello, \"hello world\")") "(\"hello world\", hello, 1)"))
  (should (equal (my-transpose-tuple "(1, hello, 1.0238734)") "(1.0238734, hello, 1)")))
