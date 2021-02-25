(require 'package)

;;; Emacs > 26.3 TLS bug (problem with installing packages)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;; List of required packages
(setq package-list '(lsp-mode lsp-ui ac-php cmake-mode magit dockerfile-mode vue-mode company company-lsp flycheck which-key use-package typescript-mode yaml-mode projectile pyenv-mode-auto exec-path-from-shell lsp-treemacs))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Same PATH as shell
(exec-path-from-shell-initialize)

;;; Color theme
(load-theme 'tango-dark)
(setq inhibit-startup-screen t)

;; Python
(add-hook 'python-mode-hook 'pyenv-mode)
(add-hook 'python-mode-hook 'lsp)

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
(setq org-cycle-emulate-tab 'white)
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

;;; LSP mode
(use-package lsp-mode
  :init
  (progn
    (setq lsp-keymap-prefix "C-c C-l")
    (setq lsp-completion-provider :capf)
    (setq lsp-idle-delay 0.500)
    (setq lsp-headerline-breadcrumb-enable nil)
    )
  :config
  (progn
    (lsp-enable-which-key-integration)
    (auto-complete-mode -1)
    )
  )
;;; Treemacs integration
(setq lsp-treemacs-sync-mode 1)

;;; Pyenv mode
(use-package pyenv-mode
  :init
  (lambda ()
    ;; Ajout du PATH pour pyenv
    (setenv "PATH" (concat (getenv "PATH") ":/home/jules/.pyenv/bin/"))))

;;; Global which key mode
(which-key-mode)

;;; Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Custom Shortcuts
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c m") 'menu-bar-mode)

(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)

(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;;; Fonts

(set-frame-font "Ubuntu Mono:pixelsize=23:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)

;;; Memory usage & misc

(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
