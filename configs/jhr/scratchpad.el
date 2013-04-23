;;; scratchpad.el -- personal config dump
;;
;;; Commentary:
;;
;; A gateway or dumping ground for capturing, trying out, messing with
;; etc., configuration settings before they are subsumed into the system
;; depths in a more organised fashion.
;;
;;; Code:

;; Color Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/zenburn/")
(load-theme 'zenburn t)

(require 'info)
(require 'cl)
(require 'slime)
(require 'cl-info)
(require 'w3m-load)
(require 'org-drill)
;(require 'sclang)
(require 'iso-transl)
(require 'linum-off)
(require 'edbi)

;; Add all subdirs in the info folder
(let ((info-base "~/.emacs.d/info"))
  (add-to-list 'Info-additional-directory-list info-base)
  (dolist (f (directory-files info-base))
    (let ((name (concat info-base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'Info-additional-directory-list name)))))


;; Display files paths in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Auto-completion
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs/el-get/auto-complete/ac-dict")

;; Enable rainbows
(require 'rainbow-mode)
(rainbow-mode 1)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Run an interactive ruby session - TODO: replace with irbsh
(require 'inf-ruby)
(global-set-key (kbd "<f6>") 'run-ruby)


;;; Variable setting


;; Empty scratch buffer
(setq initial-scratch-buffer nil)

;; Use firefox to open links
(setq browse-url-browser-function 'browse-url-firefox)

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
(global-linum-mode)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq standard-indent 4)
(setq default-tab-width 4)

;; Highlight +80 lines
(setq whitespace-style '(lines))
(setq whitespace-style '(empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Push the mouse out of the way on cursor approach
(mouse-avoidance-mode 'jump)

;; Delete selection on a key press
(delete-selection-mode t)

;; Cursor in same relative row and column during PgUP/DN
(setq scroll-preserve-screen-position t)

;; Automatically re-visiting the file in current buffer when it was
;; modified by an external program
(global-auto-revert-mode 1)

;; Ask whether or not to close, and then close if y was pressed
(defun ask-before-closing ()
  "Cofirm - y or n - before quitting EMACS."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to quit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
          (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; Prompt before quit. Only under X.
(when (eq window-system 'x)
    (global-set-key (kbd "C-x C-c") 'ask-before-closing)
    (global-set-key (kbd "C-z") 'ask-before-closing))

;; Final newline
(setq-default require-final-newline t)

;; Show and delete trailing whitespace (on save)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; http://words.bighugelabs.com/
(setq *synonym-api-key* (getenv "BIGHUGETHESAURUS"))

;; Follow symlinks and don't ask
(setq vc-follow-symlinks t)


;; Set dictionary
(setq-default ispell-dictionary "en_GB")

;; GNU/Linux systems only
(when linux-p
  ;; Set font
  (setq default-frame-alist '((font . "Inconsolata-11")))

  ;; Set flyspell binary
  (setq-default ispell-program-name "/usr/bin/aspell"))

;; Mac OSX only
(when macosx-p
  ;; Set font
  (set-face-attribute 'default nil
                      :family "Inconsolata" :height 145 :weight 'normal)
  ;; Set flyspell binary
  (setq-default ispell-program-name "/usr/local/bin/aspell")
  ;; Set window sizes
  (setq ns-pop-up-frames nil)
  (add-hook 'window-setup-hook 'maximize-frame t))

;; Mac OSX only when running emacs in a GUI on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Disable the scroll bar
(scroll-bar-mode -1)

;; Set keywords and agenda commands
(setq org-todo-keywords
      '((type "TODO" "NEXT" "WAITING" "DONE")))
(setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil)
      ("n" todo "NEXT" nil)
      ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))

;; Use color-coded task types.
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
        ("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("DONE" . (:foregorund "green" :weight bold))
        ("CANCEL" . (:foreground "blue" :weight bold))))

;; Insert a timestamp when I mark something as done
(setq org-log-done 'note)
(setq org-log-done t)

;; Disable ffap to avoid abnormal C-x C-f behaviour in python-mode
(add-hook 'python-mode-hook (lambda ()
  (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
  (setq ffap-alist (remove '(python-mode . py-module-path) ffap-alist))
  (setq ffap-alist (remove '(inferior-python-mode . py-ffap-module-path) ffap-alist))))


;;; Keybindings


;; Use ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)

;; Slime startup options
(slime-setup '(slime-fancy slime-banner))

;; Steve Yegge Keybindings - https://sites.google.com/site/steveyegge2/effective-emacs
;; Avoid using meta key as much as possible
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Remap backwards kill word to avoid using backspace which is to far from home-row
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region) ;; we lose edit-kbd-macro with this
(global-set-key "\C-c\C-k" 'kill-region)

;; Join following line to preceding line function
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Rename buffer keybinding
(global-set-key (kbd "<f7>") 'yegge-rename)

;; Launch ansi-term
(global-set-key (kbd "<f5>") 'visit-ansi-term)

;; edbi
(global-set-key (kbd "<f11>") 'edbi:open-db-viewer)

;; Timestamp function
(defun insertdate ()
  "Insert a timestamp in ISO 8601 format."
      (interactive)
      (insert (format-time-string "%Y.%m.%d")))
(global-set-key (kbd "<f2>") 'insertdate)

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

(provide 'scratchpad.el)

;;; scratchpad.el ends here
