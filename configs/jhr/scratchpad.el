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

;; Common Lisp functions
(require 'cl)

;; w3m browser
(require 'w3m-load)

;; Org-drill
(require 'org-drill)

;; Supercollider
(require 'sclang)

;; Empty scratch buffer
(setq initial-scratch-buffer nil)

;; Use firefox to open link
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

;; Remove all files from recent list
(defun recentf-nuke ()
  "Remove all files from `recentf-list'."
  (interactive)
  (let ((count (length recentf-list)))
    (setq recentf-list
          (delq nil
                (mapcar (function
                         (lambda (filename)))
                        recentf-list)))
    (setq count (- count (length recentf-list)))
    (message "%s removed from the list"
             (cond ((= count 0) "No file")
                   ((= count 1) "One file")
                   (t (format "%d files" count)))))
  (setq recentf-update-menu-p t))

;; Kill all buffers except scratch
(defun nuke ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows)
  (recentf-nuke)
)

;; Enable dead-keys. It works with layouts such as: setxkbmap -layout us -variant intl
(require 'iso-transl)

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

;; Kill all other buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

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

;; Linum-off.el
(require 'linum-off)

;; Enable rainbow mode
(require 'rainbow-mode)
(rainbow-mode 1)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

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

;; Run an interactive ruby session - TODO: replace with irbsh
(require 'inf-ruby)
(global-set-key (kbd "<f6>") 'run-ruby)

;; Timestamp function
(defun insertdate ()
  "Insert a timestamp in ISO 8601 format."
      (interactive)
      (insert (format-time-string "%Y.%m.%d")))
(global-set-key (kbd "<f2>") 'insertdate)

;; Disable ffap to avoid abnormal C-x C-f behaviour in python-mode
(add-hook 'python-mode-hook (lambda ()
  (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
  (setq ffap-alist (remove '(python-mode . py-module-path) ffap-alist))
  (setq ffap-alist (remove '(inferior-python-mode . py-ffap-module-path) ffap-alist))))

;; Put a clock in the mode-line
(defface egoge-display-time
   '((((type x w32 mac))
      (:foreground "red" :inherit bold))
     (((type tty))
      (:foreground "red")))
   "Face used to display the time in the mode line.")

;; Display the time using `egoge-display-time-face'
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
 		   'face 'egoge-display-time)))
(display-time)

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs/el-get/auto-complete/ac-dict")




(provide 'scratchpad.el)

;;; scratchpad.el ends here
