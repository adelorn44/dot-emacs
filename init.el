;;; Make use-package available
(let ((package 'use-package))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p package) (package-install package)))

;;; Add src directories to load path
(dolist (path '("~/.emacs.d/src" "~/.emacs.d/src/dap" "~/.emacs.d/src/lsp"))
  (add-to-list 'load-path path :append nil))

;;; Require custom Elisp code
(require 'my-helpers "helpers.el")

;;; Configure packages using use-package
;; Ansible
(use-package ansible :ensure t)
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;;; Bind key
(use-package bind-key :ensure t)

;; Company
(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode 1)
  :diminish)

;;; Diminish
(use-package diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode))

;;; Docker
(use-package dockerfile-mode :ensure t)

;;; Magit
(use-package magit :ensure t)

;; Ivy
(use-package ivy :ensure t :config (ivy-mode 1) :diminish)

;;; Org
(use-package org
  :bind
  (:map org-mode-map
	("C-c i" . org-insert-item))
  :config
  (company-mode)
  ;;; Org babel
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (http . t)))
  ;;; Markdown export
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

;;; Flycheck
(use-package flycheck :ensure t)

;;; Flyspell
(use-package flyspell
  :ensure t
  :hook (latex-mode . flyspell-mode))

;;; Powerline
(use-package powerline :ensure t :config (powerline-default-theme))

;;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy)
  :diminish)

;;; Tree sitter (programming language syntax highlighting)
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :requires tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :diminish)

;;; Web mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :diminish)

;;; Yaml
(use-package yaml-mode :ensure t)

;;; Require LSP/DAP configurations
(require 'my-dap "dap/main.el")
(require 'my-lsp "lsp/main.el")

;;; Customize UI
;;; Color theme
(load-theme 'tango-dark)

;;; Font
(condition-case nil
    (set-frame-font "Ubuntu Mono:pixelsize=23:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)
  ((error) (message "You should install the Ubuntu font located in ~/.emacs.d/UbuntuMono-R.ttf")))

;;; Srollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Startup screen
(setq inhibit-startup-screen t)

;;; Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Keybindings
;;; Global Shortcuts
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c m") 'menu-bar-mode)

(global-set-key (kbd "C-c u") 'emacs-uptime)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "C-M-z") 'kill-ring-save-up-to-char)
(global-set-key (kbd "C-c y") 'yank-with-indent)

(global-set-key (kbd "<f5>") 'treemacs)

;;; Emacs builtin Customize config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Misc
;;; Allow region narrowing
(put 'narrow-to-region 'disabled nil)

;;; Enable upcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Global electric pairs
(electric-pair-mode t)

;;; Indent
(setq indent-tabs-mode nil)
