;;; Python mode configuration
(require 'my-dap-templates)

(use-package dap-python
  :custom
  (dap-python-debugger 'debugpy "Use debugpy instead of deprecated ptvsd")
  :config
  (my-register-python-dap-templates))

(use-package blacken :ensure t :custom (blacken-line-length 120))

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
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Python mode custom keys
(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "C-c M-r") 'python-shell-restart)
  (define-key python-mode-map (kbd "C-c C-d") 'python-copy-next-docstring)
  (define-key python-mode-map (kbd "C-c C-b") 'blacken-buffer)
  (define-key python-mode-map (kbd "C-c C-t") 'my-transpose-tuple-at-point))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(provide 'my-python-mode)
