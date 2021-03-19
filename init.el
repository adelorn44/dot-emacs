(require 'package)

;;; Emacs < 26.3 TLS bug (problem when installing melpa packages)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;; List of required packages
(setq package-list '(lsp-mode lsp-ui ac-php cmake-mode magit dockerfile-mode vue-mode company company-lsp flycheck which-key use-package typescript-mode yaml-mode projectile pyenv-mode-auto exec-path-from-shell lsp-treemacs dap-mode csv-mode))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Require my custom functions
(load "~/.emacs.d/functions.el")

;; Same PATH as shell
(exec-path-from-shell-initialize)

;;; Color theme
(load-theme 'tango-dark)
(setq inhibit-startup-screen t)

;; Yasnippets
(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets")))

;; Python
(use-package dap-python
  :custom
  (dap-python-debugger 'debugpy "Use debugpy instead of deprecated ptvsd")
  :config
  (dap-register-debug-template "Raccordement"
			       (list :type "python"
				     :args "-i"
				     :cwd nil
				     :env '(("DEBUG" . "1"))
				     :target-module (expand-file-name "~/Documents/raccordement/example_nantes.py")
				     :request "launch"
				     :name "Raccordement"))
  :bind
  ("C-c h" . dap-hydra))

(add-hook 'python-mode-hook 'my-python-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)

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
  (auto-complete-mode -1)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Shell Script
'(sh-basic-offset 8)
'(sh-indentation 8)
'(smie-indent-basic 8)

;;; EDE
(global-ede-mode t)

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
			   (auto-complete-mode)))
(setq org-agenda-files (file-expand-wildcards "~/Documents/organisation/*.org" t))
(setq org-export-default-language "fr")
;;; (setq org-cycle-emulate-tab 'white)
;;; Open .pdf with evince
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "gedit %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "gedit %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))
;;; Markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

;;; LSP mode
(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil "Disable breadcrumb")
  (lsp-treemacs-sync-mode 1 "Enable Treemacs integration")
  :config
  (lsp-enable-which-key-integration)
  (auto-complete-mode -1))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  :bind
  ("C-c i" . lsp-ui-imenu))

;;; Pyenv mode
(use-package pyenv-mode
  :init
  ;; Ajout du PATH pour pyenv
  (setenv "PATH" (concat (getenv "PATH") ":/home/jules/.pyenv/bin/")))

;;; Global which key mode
(which-key-mode)

;;; Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; User defined Shortcuts
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c m") 'menu-bar-mode)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c t") 'org-todo-list)

;; Multiple screen setup
(global-set-key (kbd "C-c p") 'previous-multiframe-window)
(global-set-key (kbd "C-x <down>") 'other-frame)
(global-set-key (kbd "C-x <up>") 'other-frame)
(global-set-key (kbd "C-x C-<down>") 'other-frame)
(global-set-key (kbd "C-x C-<up>") 'other-frame)

(global-set-key (kbd "C-c r") 'eval-region)

(global-set-key (kbd "<f5>") 'treemacs)

(global-set-key (kbd "<f5>") 'treemacs)
(global-set-key (kbd "C-c g g") 'beginning-of-buffer)
(global-set-key (kbd "C-c g e") 'end-of-buffer)

;;; Fonts

(set-frame-font "Ubuntu Mono:pixelsize=23:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)

;;; Docview
(setq doc-view-resolution 250)

;;; Memory usage & misc

(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(scroll-bar-mode -1)

;;; Lecture des fichiers .csv de Roseau
(setq csv-separators '(";" "," ":"))

;;; Ouverture d'une fenêtre au démarrage
(add-hook 'emacs-startup-hook 'make-frame-command)
