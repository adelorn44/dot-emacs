(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)

;;; List of required packages
(setq package-list '(lsp-mode lsp-ui ac-php cmake-mode magit dockerfile-mode vue-mode company company-lsp flycheck which-key use-package typescript-mode yaml-mode projectile))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Color theme
(load-theme 'tango-dark)
(setq inhibit-startup-screen t)

;; Python
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
(setq org-agenda-files (file-expand-wildcards "~/Documents/organisation/*.org" t))
(setq org-export-default-language "fr")
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

(use-package lsp-mode
  :config
  (progn
    (setq lsp-keymap-prefix "C-c C-l")
    (lsp-enable-which-key-integration)
    (auto-complete-mode -1)
    )
)

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
