;;; LSP mode configuration

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil "Disable breadcrumb")
  (lsp-treemacs-sync-mode -1 "Disable Treemacs integration")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-enable nil)
  :bind
  (:map lsp-ui-mode-map
	("C-c i" . lsp-ui-imenu)
	("C-c f" . lsp-ui-flycheck-list)))

;;; Memory usage & misc (see lsp performance & M-x lsp-doctor)
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(provide 'my-lsp)
