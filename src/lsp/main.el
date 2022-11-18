(use-package lsp-mode
  :ensure t
  :bind (:map lsp-command-map ("C-c l" . lsp-command-map))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil "Disable breadcrumb")
  (lsp-treemacs-sync-mode -1 "Disable Treemacs integration")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-enable nil)
  ;;; Memory usage & misc (see lsp performance & M-x lsp-doctor)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))
  :bind
  (:map lsp-ui-mode-map
	("C-c i" . lsp-ui-imenu)
	("C-c f" . lsp-ui-flycheck-list)))

(use-package lsp-treemacs :ensure t)

(require 'my-lsp-c "lsp/c.el")
(require 'my-lsp-php "lsp/php.el")
(require 'my-lsp-python "lsp/python.el")
(require 'my-lsp-typescript "lsp/typescript.el")
(provide 'my-lsp)
