(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package python
  :bind
  (:map python-mode-map ("C-c C-b" . blacken-buffer)))

(provide 'my-lsp-python)
