(require 'ert)
(require 'my-helpers)

(ert-deftest my-test-transpose-python-tuple ()
  "Tests the transposition of python tuples."
  (should (equal (my-transpose-tuple "(1, 2, 3)") "(3, 2, 1)"))
  (should (equal (my-transpose-tuple "(1, hello, \"hello world\")") "(\"hello world\", hello, 1)"))
  (should (equal (my-transpose-tuple "(1, hello, 1.0238734)") "(1.0238734, hello, 1)")))

(ert-deftest my-kill-pwd ()
  "Test copying the current working directory"
  (with-temp-buffer
    (pwd-kill)
    (yank)
    (should (string-match-p (expand-file-name default-directory) (buffer-string)))))

(ert-deftest python-copy-next-docstring ()
  "Test copying the next python doctring after point"
  (cd "~/.emacs.d/tests/assets")
  (let* ((buff1 (find-file "docstring_1.txt"))
	 (res1 (buffer-string))
	 (buff2 (find-file "docstring_2.txt"))
	 (res2 (buffer-string))
	 (python (find-file "docstring.txt")))
    (switch-to-buffer python)
    (python-copy-next-docstring)
    (with-temp-buffer
      (yank)
      (should (string-equal (buffer-string) res1)))

    ;; Point after first doctring
    (set-window-point (selected-window) 312)
    (python-copy-next-docstring)
    (with-temp-buffer
      (yank)
      (should (string-equal (buffer-string) res2)))
    (dolist (it `(,buff1 ,buff2 ,python)) (kill-buffer it))))
