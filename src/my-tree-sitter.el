;;; Tree sitter configuration
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :requires tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'my-tree-sitter)
