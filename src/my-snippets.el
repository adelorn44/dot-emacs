;; Snippets configuration

(use-package yasnippet
  :config
  ;; Add my snippets
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets") t)
  (yas-load-directory (expand-file-name "~/.emacs.d/snippets"))
  :bind
  (:map yas-minor-mode-map
	("C-;" . yas-expand)
	("C-c C-y t" . yas-describe-tables))
  :hook
  (python-mode . yas-minor-mode)
  (emacs-lisp-mode . yas-minor-mode)
  (latex-mode . yas-minor-mode)
  :ensure t)

(use-package yasnippet-snippets :ensure t)

(provide 'my-snippets)
