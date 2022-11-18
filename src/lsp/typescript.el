;; Typescript
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . lsp))

(provide 'my-lsp-typescript)
