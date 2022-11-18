;;; My helper/useful functions

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
      (forward-line)
      (move-beginning-of-line nil)
      (setq end-docstring (point)))
    (kill-ring-save beg-docstring end-docstring)))

(defun kill-ring-save-up-to-char (arg char)
  (interactive "p\ncSave up to char: ")
  (barf-if-buffer-read-only)
  (let ((buffer-modified (buffer-modified-p)))
    (save-excursion
      (zap-up-to-char arg char)
      (yank)
      (restore-buffer-modified-p buffer-modified))))

(defun my-display-line-length ()
  (interactive)
  (message (number-to-string (- (line-end-position) (line-beginning-position)))))

(defun my-transpose-tuple (str)
  "Transpose a Python tuple. The input string should look like \"(1, 1.02, variable)\"."
  (when (not (char-equal ?\( (aref str 0)))
    (error "Error in tuple format: %c instead of ( at str pos 0" (aref str 0)))
  (when (not (char-equal ?\) (aref str (- (length str) 1))))
    (error "Error in tuple format: %c instead of ) at str pos -1" (aref str (- (length str) 1))))
  (let (tuple-words tuple-words-trim res)
    (setq tuple-words (split-string (substring str 1 -1) ","))
    (dolist (it tuple-words tuple-words-trim)
      (setq tuple-words-trim (append tuple-words-trim `(,(string-trim it)))))
    (setq tuple-words (reverse tuple-words-trim))
    (setq res "(")
    (dolist (it tuple-words res)
      (when (char-equal ?\( (aref it 0))
	(error "Error: nested tuple transposition is not supported"))
      (setq res (concat res it ", ")))
    (setq res (substring res 0 -2))
    (concat res ")")))

(defun my-transpose-tuple-at-point ()
  "Transpose a Python tuple at point. The point should be inside of a Python tuple."
  (interactive)
  (let (init-point point-tuple-beg point-tuple-end tuple)
    (setq init-point (point))
    (re-search-backward "(")
    (setq point-tuple-beg (point))
    (re-search-forward ")")
    (setq point-tuple-end (point))
    (setq tuple (buffer-substring-no-properties point-tuple-beg point-tuple-end))
    (setq tuple (my-transpose-tuple tuple))
    (delete-region point-tuple-beg point-tuple-end)
    (insert tuple)
    (set-window-point (selected-window) init-point))
  nil)

(defun yank-with-indent (&optional indent-length)
  "Yank text with indent-length spaces or with the current indent line before point."
  (interactive "P")
  (let (indent bound)
    (if indent-length
	(progn (setq indent (make-string indent-length ? ))
		     (insert indent))
      (setq indent (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (yank)
    (setq bound (point))
    (pop-to-mark-command)
    (while (search-forward "\n" bound t)
      (progn (replace-match (concat "\n" indent))
	     (setq bound (+ bound (length indent))))))
  (if indent-length (backward-delete-char indent-length)))

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

(defun my-insert-hugo-date ()
"Insert the current date at point

The date format matches Hugo blogging system format

The minutes and seconds are both set to `00' since they are not relevant
as an article takes multiple hours to be completed
"
  (interactive)
  (insert (format-time-string (concat "%F" "T" "%H:00:00" "%z"))))

(defun my-hugo-server ()
  (interactive)
  (switch-to-buffer "*hugo*")
  (switch-to-prev-buffer)
  (with-current-buffer "*hugo*"
    (cd "~/Documents/freelance/blog")
    (make-process
     :name "hugo"
     :buffer (current-buffer)
     :command '("hugo" "server"))))

(provide 'my-helpers)
