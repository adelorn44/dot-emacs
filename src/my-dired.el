;;; Dired configuration

(use-package dired
  :bind
  (:map dired-mode-map
   ("w" . my-dired-copy-filename-as-kill)))

(defun my-dired-copy-filename-as-kill (&optional arg)
  "By default, return the absolute path."
  (interactive "P")
  (if arg
      (dired-copy-filename-as-kill arg)
    ;; 0 means kill the absolute path
    (dired-copy-filename-as-kill 0)))

(provide 'my-dired)
