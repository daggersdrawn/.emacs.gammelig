;; Color Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/zenburn/")
(load-theme 'zenburn t)

;; CL fucntions
(require 'cl)

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

;; Final newline
(setq-default require-final-newline t)

;; Show and delete trailing whitespace (on save)
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; enable rainbow mode
(require 'rainbow-mode)
(rainbow-mode 1)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; replace buffer-menu with ibuffer
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

;; Steve Yegge's function to rename a file that you're editing along
;; with its corresponding buffer. TODO: Keybinding
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
 (filename (buffer-file-name)))
    (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
   (message "A buffer named '%s' already exists!" new-name)
 (progn
   (rename-file name new-name 1)
   (rename-buffer new-name)
   (set-visited-file-name new-name)
   (set-buffer-modified-p nil))))))

;; Join following line to preceding line function
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Ansi-term launcher function
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))
(global-set-key (kbd "<f2>") 'visit-ansi-term)

;; Timestamp function
(defun insertdate ()
      (interactive)
      (insert (format-time-string "%Y.%m.%d")))
(global-set-key (kbd "<f5>") 'insertdate)
