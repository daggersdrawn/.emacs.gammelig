;; Using Emacs as a Server
(if (eq window-system 'x) server-start)

;; ========== Apperance ==========

;; Fonts
(set-face-attribute 'default nil :family "Inconsolata" :height 140)

;; Color Theme 
(require 'color-theme) 
(color-theme-initialize)
(load-file "~/.emacs.d/src/color-theme-sanityinc/color-theme-sanityinc.el")
(load-file "~/.emacs.d/src/zenburn/zenburn.el")

;;(color-theme-sanityinc)
(color-theme-zenburn)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; ========== Whitespace =========

(require 'whitespace)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
