;;; Dired configuration

(use-package dired
  :bind
  (:map dired-mode-map
	("w" . my-dired-copy-filename-as-kill)
	("C-c C-c" . my-dired-keep-extensions-at-point)))

(defun my-dired-copy-filename-as-kill (&optional arg)
  "By default, return the absolute path."
  (interactive "P")
  (if arg
      (dired-copy-filename-as-kill arg)
    ;; 0 means kill the absolute path
    (dired-copy-filename-as-kill 0)))

(defun my-dired-keep-extensions-at-point (&optional arg)
  "Keep only the files with the same extension as at point. With any args, do not keep directories."
  (interactive "P")
  (if (not (string-equal major-mode "dired-mode")) (error "Not in Dired"))
  (let* ((filename (dired-copy-filename-as-kill)) (extension-regexp "\\(\\.[a-zA-Z]+\\)+") extension)
    (if (not filename) (error "No file at point"))
    (if (string-match-p extension-regexp filename)
	(progn
	  (setq extension (substring filename (string-match extension-regexp filename) (match-end 0)))
	  (revert-buffer)
	  (dired-unmark-all-marks)
	  (dired-mark-files-regexp (concat (regexp-quote extension) "$"))
	  (dired-toggle-marks)
	  (if (not arg) (dired-mark-directories t))
	  (dired-do-kill-lines))
      (message "This file has not any extension or is a directory"))))

(provide 'my-dired)
