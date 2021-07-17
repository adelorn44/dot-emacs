(require 'package)

;;; List of required packages
(setq package-list '(cmake-mode magit dockerfile-mode vue-mode flycheck which-key use-package yaml-mode projectile exec-path-from-shell lsp-treemacs csv-mode))
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
(load (expand-file-name "~/.emacs.d/functions.el"))
(load (expand-file-name "~/.emacs.d/my-customize-init.el"))

;; Same PATH as shell
(exec-path-from-shell-initialize)

;;; Color theme
(load-theme 'tango-dark)
(setq inhibit-startup-screen t)

;; Yasnippets
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets") t)
  (yas-load-directory (expand-file-name "~/.emacs.d/snippets"))
  :bind
  (:map yas-minor-mode-map ("C-;" . yas-expand))
  :ensure t)

(use-package yasnippet-snippets :ensure t)

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

;; Python
(use-package dap-python
  :custom
  (dap-python-debugger 'debugpy "Use debugpy instead of deprecated ptvsd")
  :config
  (dap-register-debug-template
   "Raccordement"
   (list :type "python"
	 :args "-i"
	 :cwd nil
	 :env '(("DEBUG" . "1"))
	 :target-module (expand-file-name "~/Documents/raccordement/example_nantes.py")
	 :request "launch"
	 :name "Raccordement"))

  (dap-register-debug-template
   "Python :: Run sirao_cli"
   (list :type "python"
         :args "network connection-options Nantes_Metropole.sqlite sites_nantes_id.csv \"Situation été\" Original REDACTED full_output"
         :cwd nil
         :module "sirao_cli"
         :program nil
         :request "launch"
         :name "Python :: Run sirao_cli"))

  (dap-register-debug-template
   "Core :: Run pytest (at point)"
   (list :type "python-test-at-point"
         :args ""
	 :env '(("GMAPS_API_KEY" . "REDACTED"))
         :program nil
         :module "pytest"
         :request "launch"
         :name "Core :: Run pytest (at point)")))

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
  (define-key python-mode-map (kbd "C-c C-b") 'blacken-buffer))
(add-hook 'python-mode-hook 'my-python-mode-hook)


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

;;; General config
(electric-pair-mode t)

;;; Org mode
(add-hook 'org-mode-hook (lambda ()
			   (company-mode)
			   (org-babel-do-load-languages
			    'org-babel-load-languages
			    '((python . t)))))
(setq org-agenda-files (file-expand-wildcards "~/Documents/organisation/*.org" t))
(setq org-export-default-language "fr")
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "|" "DONE")))
;;; (setq org-cycle-emulate-tab 'white)
;;; Open .pdf with evince
(eval-after-load "org"
  '(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))
;; Markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))
;; Org Capture
(setq org-directory (expand-file-name "~/Documents/organisation"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;; LSP mode
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil "Disable breadcrumb")
  (lsp-treemacs-sync-mode -1 "Disable Treemacs integration")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-enable nil)
  :bind
  (:map lsp-ui-mode-map
	("C-c i" . lsp-ui-imenu)
	("C-c f" . lsp-ui-flycheck-list)))

;;; Global which key mode
(which-key-mode)

;; Web configuration
(require 'web-init "~/.emacs.d/web-init.el")

;;; Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; User defined Shortcuts
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

;;; Fonts
(condition-case nil
    (set-frame-font "Ubuntu Mono:pixelsize=23:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)
  (error (message "Warning: You should install the Ubuntu font located in ~/.emacs.d/UbuntuMono-R.ttf")))

;;; Docview
(setq doc-view-resolution 250)

;;; Memory usage & misc

(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Lecture des fichiers .csv de Roseau (format français)
(setq csv-separators '(";" "," ":"))
;(setq csv-separators '(";"))

;;; Ouverture d'une fenêtre au démarrage
(if my-open-two-windows (add-hook 'emacs-startup-hook 'make-frame-command))

(use-package powerline :ensure t :config (powerline-default-theme))

(use-package diminish
  :ensure t
  :config
  ;; Diminish global modes since they are always present
  (diminish 'projectile-mode)
  (diminish 'ivy-mode)
  (diminish 'which-key-mode))

;;; Tabs are usually bad
(setq indent-tabs-mode nil)

;;; Enable upcase region (bound on C-x C-u)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   '(("Pytest"
      ("Pytest"
       (name . "test_.*.py$\\|tests\\|conftest.py"))
      ("Python"
       (or
	(name . "\\(\\.py$\\)\\|\\(shell\\)\\|\\(Python\\*\\)")
	(mode . dired-mode))))))
 '(ibuffer-saved-filters
   '(("python"
      (or
       (name . "\\(\\.py$\\)\\|\\(shell\\)\\|\\(Python\\*\\)")
       (mode . dired-mode)))
     ("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
	(starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode))))))
