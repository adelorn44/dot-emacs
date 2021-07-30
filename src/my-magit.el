;;; Magit configuration

(defun my-ask-pre-commit ()
  "Ask the user if he wants to run pre-commit."
  (let (pre-commit-found pre-commit-buffer)
    (setq pre-commit-found
	  (zerop (call-process "which" nil nil nil "pre-commit")))

    (if (and pre-commit-found (y-or-n-p "Run pre-commit ?"))
	(progn
	  (setq pre-commit-buffer (get-buffer-create "*pre-commit*"))
	  (with-current-buffer
	      "*pre-commit*"
	    (erase-buffer)
	    (if (zerop (call-process "pre-commit" nil (current-buffer) nil))
		(message "Pre Commit succeeded")
	      (message "Pre Commit failed. See *pre-commit* for details.")))))))

(use-package magit
  :ensure t)

;; Ask to run pre-commit
(advice-add 'magit-commit :before #'my-ask-pre-commit)

(provide 'my-magit)
