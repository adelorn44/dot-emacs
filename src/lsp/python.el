(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-python-ms-auto-install-server t)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration)
  :hook (python-mode . (lambda ()
			 (setq fill-column 120)
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp

(use-package python
  :bind
  (:map python-mode-map ("C-c C-b" . blacken-buffer)))

(provide 'my-lsp-python)
