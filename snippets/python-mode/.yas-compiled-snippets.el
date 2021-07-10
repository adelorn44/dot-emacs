;;; Compiled snippets and support files for `python-mode'
;;; contents of the .yas-setup.el support file:
;;;
(require 'yasnippet)
(defvar yas-text)

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Args:" formatted-args) indent))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("fd" "def ${1:name}($2):\n \\\"\\\"\\\"$3\n\n ${2:$(python-args-to-docstring)}\n \\\"\\\"\\\"\n $0" "function_docstring" nil
			("definitions")
			nil "/home/jules/.emacs.d/snippets/python-mode/ds" nil nil)
		       ("csv" "$1.to_csv(\"$1.csv\")" "Export DataFrame to CSV" nil nil nil "/home/jules/.emacs.d/snippets/python-mode/csv" nil nil)))


;;; Do not edit! File generated at Mon Mar 22 11:09:44 2021
