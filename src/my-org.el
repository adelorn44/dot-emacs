;;; Org mode configuration
(require 'my-customize-init)

(use-package org
  :bind
  (:map org-mode-map
	("C-c i" . org-insert-item))
  :config
  (company-mode)
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  ;; Markdown export
  (require 'ox-md)
  ;;; Open .pdf with evince
  (add-to-list 'org-file-apps '(("\\.pdf\\'" "evince %s")) t)
  :custom
  ;; Agenda
  (org-agenda-files (file-expand-wildcards (concat my-org-dir "*.org") t))
  (org-export-default-language my-language)
  (org-todo-keywords my-org-keywords)
  ;; Org Capture
  (org-directory (expand-file-name my-org-dir))
  (org-default-notes-file (concat org-directory my-org-capture))
  :hook
  (org-mode . auto-fill-mode))


(provide 'my-org)
