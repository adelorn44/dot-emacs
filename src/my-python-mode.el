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
			 (setq fill-column 120)
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; Python mode custom keys
(use-package python
  :bind
  (:map python-mode-map
	("C-c M-r" . python-shell-restart)
	("C-c C-d" . python-copy-next-docstring)
	("C-c C-b" . blacken-buffer)
	("C-c C-t" . my-transpose-tuple-at-point)))

(provide 'my-python-mode)
