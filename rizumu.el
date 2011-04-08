;; Using Emacs as a Server
(if (eq window-system 'x) (server-start))

;; Fonts
(set-face-attribute 'default nil
                    :family "Inconsolata" :height 100)

;(set-face-attribute 'font-lock-comment-face nil
;                    :family "Optima" :height 140 :slant 'italic
;                    :foreground "#000000" :background "ddddee")
;(set-face-attribute 'font-lock-string-face nil
;                    :family "Anonymous Pro" :height 130 :weight 'bold
;                    :foreground "black" :background 'unspecified)

;; Color Theme 
(load-file "~/.emacs.d/elpa/zenburn-1.8/zenburn.el")
(color-theme-zenburn)

;; Disable backup files.
(setq make-backup-files nil)
;; Enable versioning with default values (keep five last versions, I think!)
;(setq version-control t)
;; Save all backlkup file in this directory.
;(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
;; Delete unnecesary auto-save files (ex. #%*mail*#')
(setq delete-auto-save-files t)

;; Column and line numbers
(setq-default column-number-mode t)
(setq-default line-number-mode t)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq standard-indent 4)
(setq default-tab-width 4)

;; Highlight +80 lines
(setq whitespace-style '(lines))
(setq whitespace-style '(empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Automatically re-visiting the file in current buffer when it was
;; modified by an external program
(global-auto-revert-mode 1)

;; anything
;(require 'anything-startup)
;(require 'anything-config)

