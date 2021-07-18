;;; Variable to customize my emacs config (multi screen, folders etc)

;; nil -> single screen, t -> two screens
(setq my-open-two-windows nil) ;; Single screen here

;;; Path to your emacs source dir (clone emacs repo and point to its src)
(setq find-function-C-source-directory (expand-file-name "~/.emacs.d/src"))

;;; Org main directory where notes are stored (trailing / mandatory)
(setq my-org-dir "~/Documents/")
(setq my-org-capture "notes.org")
(setq my-org-keywords '((sequence "TODO" "FEEDBACK" "|" "DONE")))

;;; Language setting for org
(setq my-language "fr")

(provide 'my-customize-init)
