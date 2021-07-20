;;; Spell checking configuration

(use-package flyspell
  :ensure t
  :custom
  (flyspell-default-dictionary "en")
  :hook (latex-mode . flyspell-mode)
  :bind
  (:map flyspell-mode-map
	("C-c C-f t" . my-ispell-toggle-dictionnary)
	("C-c C-f b" . flyspell-buffer)))

(defun my-ispell-toggle-dictionnary ()
  "Toggle the current dictionnary between english and french."
  (interactive)
  (if (equal ispell-current-dictionary "en")
      (ispell-change-dictionary "fr" 1)
    (ispell-change-dictionary "en" 1)))

(provide 'my-ispell)
