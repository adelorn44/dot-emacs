;;; This script loads all test cases
(dolist (it (file-expand-wildcards "~/.emacs.d/tests/test*.el"))
  (message it)
  (load it))
