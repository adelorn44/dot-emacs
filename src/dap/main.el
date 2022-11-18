(use-package dap-mode
  :ensure t
  :bind
  (:map dap-mode-map
	("C-c h" . dap-hydra)
	("C-c r" . dap-ui-repl)))

(require 'my-dap-python "dap/python.el")
(provide 'my-dap)
