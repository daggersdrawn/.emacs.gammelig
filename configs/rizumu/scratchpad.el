;; Color Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/zenburn/")
(load-theme 'zenburn t)

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
(global-linum-mode 1)

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

;; Disable the scroll bar
(toggle-scroll-bar -1)

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

;; Turns on flymake for all files which have a flymake mode
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Final newline
(setq-default require-final-newline t)

;; Show and delete trailing whitespace (on save)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; http://words.bighugelabs.com/
(setq *synonym-api-key* (getenv "BIGHUGETHESAURUS"))

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; kill all other buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Set dictionary
(setq-default ispell-dictionary "en_US")

;; GNU/Linux systems only
(when linux-p
  ;; Set font
  (setq default-frame-alist '((font . "Inconsolata-dz-10")))

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
