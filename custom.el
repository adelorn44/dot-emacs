(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   '(("Pytest"
      ("Pytest"
       (name . "test_.*.py$\\|tests\\|conftest.py"))
      ("Python"
       (or
	(name . "\\(\\.py$\\)\\|\\(shell\\)\\|\\(Python\\*\\)")
	(mode . dired-mode))))))
 '(ibuffer-saved-filters
   '(("python"
      (or
       (name . "\\(\\.py$\\)\\|\\(shell\\)\\|\\(Python\\*\\)")
       (mode . dired-mode)))
     ("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
	(starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(package-selected-packages
   '(ox-md yasnippet-snippets yaml-mode which-key web-mode vue-mode use-package unicode-fonts pyenv-mode-auto projectile powerline magit lsp-ui lsp-pyright ivy flycheck exec-path-from-shell dockerfile-mode diminish dap-mode csv-mode company cmake-mode blacken)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

