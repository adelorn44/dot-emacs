;;; My functions :)

(defun python-shell-restart ()
  (interactive)
  (let ((buffer-process)
	(buffer-beginning (current-buffer))
	(window-beginning (selected-window)))
    (switch-to-buffer "*Python*")
    (setq buffer-process (get-buffer-process (current-buffer)))
    (if buffer-process (set-process-query-on-exit-flag buffer-process nil))
    (kill-buffer "*Python*")
    (run-python)
    (switch-to-buffer "*Python*")
    (setq buffer-process (get-buffer-process (current-buffer)))
    (set-process-query-on-exit-flag buffer-process t)
    ;; Rearanging windows
    (select-window window-beginning)
    (switch-to-buffer buffer-beginning)
    ;; Opening python in other window if there is one
    (my-open-buffer-split-below "*Python*")
    (select-window window-beginning)
    (switch-to-buffer buffer-beginning)))

(defun my-open-buffer-split-below (buffer)
  (let ((window-beginning (selected-window)))
    (if (= (count-windows) 1)
	(split-window-below))
    (other-window 1)
    (switch-to-buffer buffer)
    (select-window window-beginning)))
  
(defun pwd-kill ()
  "Add the current directory to kill ring."
  (interactive)
  (message (expand-file-name default-directory))
  (kill-new (expand-file-name default-directory)))

(defun python-copy-next-docstring ()
  "Copy next Python docstring."
  (interactive)
  (let (beg-docstring end-docstring)
    (save-excursion
      (search-forward "\"\"\"")
      (move-beginning-of-line nil)
      (setq beg-docstring (point))
      (search-forward "\"\"\"" nil nil 2)
      (next-line)
      (move-beginning-of-line nil)
      (setq end-docstring (point)))
    (kill-ring-save beg-docstring end-docstring)))

(defun pytest-command-at-point ()
  (interactive)
  (let* ((function-name (symbol-name (symbol-at-point)))
	(command (concat "pytest " buffer-file-name "::" function-name)))
    (if (get-buffer "*shell*")
	(progn
	  (my-open-buffer-split-below "*shell*")
	  (other-window 1)
	  (switch-to-buffer "*shell*")
	  (insert command)
	  (comint-send-input))
      (shell-command (concat "pytest " buffer-file-name "::" function-name)))))


(defun kill-ring-save-up-to-char (arg char)
  (interactive "p\ncSave up to char: ")
  (save-excursion
    (zap-up-to-char arg char)
    (yank)))

(defun my/display-line-length ()
  (interactive)
  (message (number-to-string (- (line-end-position) (line-beginning-position)))))

;; TODO: la coder en Lisp
(fset 'transpose-tuple
   (kmacro-lambda-form [?\C-r ?\( return ?\M-x ?s ?e ?a ?r ?c ?h ?- ?f ?o ?r ?w ?a ?r ?d ?- ?r ?e ?g ?e ?x ?p return ?\[ ?0 ?- ?9 return left ?\C-  ?\C-s ?, return left right ?\M-x ?s ?e ?a ?r ?c ?h ?- ?b ?a ?c ?k ?w ?a ?r ?d ?- ?r ?e ?g ?e ?x return ?\[ ?0 ?- ?9 return right ?\C-w ?\C-s ?, return backspace ?\M-x ?s ?e ?a ?r ?c ?h ?- ?f ?r backspace ?o ?r ?w ?a ?r ?d ?- ?r ?e ?g ?e ?x return ?\[ ?0 ?- ?9 return left ?\C-  ?\C-s ?\) return ?\M-x ?s ?e ?a ?r ?c ?h ?- ?b ?a ?c ?k ?w ?a ?r ?d ?- ?r ?e ?g ?e ?x return ?\[ ?0 ?- ?9 return right ?\C-w ?\C-s ?\) return left ?\C-  ?\C-r ?\( return right backspace ?\C-y ?, ?  ?\C-y ?\M-y] 0 "%d"))

(defun yank-with-indent ()
  (interactive)
  (let ((indent
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message indent)
    (yank)
    (narrow-to-region (mark t) (point))
    (pop-to-mark-command)
    (replace-string "\n" (concat "\n" indent))
    (widen)))

(defun my-dot-py-buffers ()
  "Returns a list of already opened buffers ending with .py"
  (let (py-buffers)
  (dolist (it (buffer-list))
    (if (string-match-p "\\.py$" (buffer-name it))
	(setq py-buffers (append (cons it nil) py-buffers))))
  py-buffers))

(defun my-blacken-dot-py-buffers ()
  "Blacken already opened buffers ending with .py"
  (interactive)
  (dolist (it (my-dot-py-buffers))
    (with-current-buffer it
      (blacken-buffer t)
      (message (concat "Blackened buffer " (buffer-name it)))))
  (message (concat "Blackened " (int-to-string (length (my-dot-py-buffers))) " buffer(s)")))

(defun my-pytest-redo ()
  (interactive)
  (if (not (string-equal "*shell*" (buffer-name (current-buffer))))
      (progn (my-open-buffer-split-below (current-buffer))
	     (other-window 1)
	     (switch-to-buffer "*shell*")))
  (if (not (string-equal "shell-mode" major-mode))
      (shell (current-buffer)))
  (end-of-buffer)
  (comint-previous-matching-input "^pytest" 1)
  (comint-send-input))
