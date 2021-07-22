(require 'package)

;;; List of required packages (used without customizations)
(setq package-list '(cmake-mode magit dockerfile-mode vue-mode
flycheck use-package yaml-mode lsp-treemacs))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Require my custom functions and customizations
(add-to-list 'load-path "~/.emacs.d/src")
(require 'my-helpers)
(require 'my-customize-init)

;; Same PATH as shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; Color theme
(load-theme 'tango-dark)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy))

;; Ivy
(use-package ivy :ensure t :config (ivy-mode 1))

;; Company
(use-package company :ensure t)

;; Dap Mode
(use-package dap-mode
  :ensure t
  :bind
  (:map dap-mode-map ("C-c h" . dap-hydra)))

;; Shell Script
'(sh-basic-offset 8)
'(sh-indentation 8)
'(smie-indent-basic 8)

;;; C-mode
(add-hook 'c-mode-hook 'lsp)
(add-hook 'cpp-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq c-default-style '((c-mode . "linux")
			(c++-mode . "gnu")
			(other . "cc-mode")))

;;; Global electric pairs
(electric-pair-mode t)

;;; Global which key mode
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Web configuration
(require 'my-web-mode)

;; Python configuration
(require 'my-python-mode)

;; LSP configuration
(require 'my-lsp)

;; Org Mode configuration
(require 'my-org)

;; Spell checking configuration
(require 'my-ispell)

;; Snippets configuration
(require 'my-snippets)

;; Dired configuration
(require 'my-dired)

;;; Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Startup screen
(setq inhibit-startup-screen t)

;;; Global Shortcuts
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c m") 'menu-bar-mode)

(global-set-key (kbd "C-c g g") 'beginning-of-buffer)
(global-set-key (kbd "C-c g e") 'end-of-buffer)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c t") 'org-todo-list)
(global-set-key (kbd "C-c c") 'my/display-line-length)
(global-set-key (kbd "C-c u") 'emacs-uptime)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "C-M-z") 'kill-ring-save-up-to-char)
(global-set-key (kbd "C-c y") 'yank-with-indent)

;; Multiple screen setup
(global-set-key (kbd "C-x <down>") 'other-frame)
(global-set-key (kbd "C-x <up>") 'other-frame)
(global-set-key (kbd "C-x C-<down>") 'other-frame)
(global-set-key (kbd "C-x C-<up>") 'other-frame)

(global-set-key (kbd "<f5>") 'treemacs)
(global-set-key (kbd "<f6>") 'python-mode)
(global-set-key (kbd "<f7>") 'my-pytest-redo)

;;; Font
(condition-case nil
    (set-frame-font "Ubuntu Mono:pixelsize=23:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)
  (warn (message "You should install the Ubuntu font located in ~/.emacs.d/UbuntuMono-R.ttf")))

;;; Docview
(setq doc-view-resolution 250)

;;; Disable sroll bar (for gtk only)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Open two frame at startup
(if my-open-two-windows (add-hook 'emacs-startup-hook 'make-frame-command))

(use-package powerline :ensure t :config (powerline-default-theme))

(use-package diminish
  :ensure t
  :config
  ;; Diminish global modes since they are always present
  (diminish 'projectile-mode)
  (diminish 'ivy-mode)
  (diminish 'which-key-mode))

(use-package csv-mode
  :custom
  ;;; French CSV format
  (csv-separators '(";" "," ":")))

;;; Tabs are usually bad
(setq indent-tabs-mode nil)

;;; Enable upcase region (bound on C-x C-u)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Custom in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
