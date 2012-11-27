;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"


(require 'cl) ; common lisp goodies, loop

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Install el-get on first run
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

;; Set a global if running a GNU/Linux distro or Mac OSX
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))

;; Set locations for base, user and system configuration files and folders
(defvar configs-dir (expand-file-name (concat user-emacs-directory (file-name-as-directory "configs"))))
(defvar base-dir (concat configs-dir (file-name-as-directory "base")))
(defvar base-packages (concat base-dir "packages.el"))
(defvar base-behaviours-dir (concat base-dir (file-name-as-directory "behaviours")))
(defvar base-behaviours-files (directory-files base-behaviours-dir t "\.el$"))
(defvar base-keybindings (concat base-dir "keybindings.el"))
(defvar system-dir (concat configs-dir (file-name-as-directory system-name)))
(defvar system-packages (concat system-dir "packages.el"))
(defvar system-behaviours-dir (concat system-dir (file-name-as-directory "behaviours")))
(if (file-exists-p system-behaviours-dir)
    (defvar system-behaviours-files (directory-files system-behaviours-dir t "\.el$"))
    (defvar system-behaviours-files nil))
(defvar system-keybindings (concat system-dir "keybindings.el"))
(defvar system-scratchpad (concat system-dir "scratchpad.el"))
(defvar user-dir (concat configs-dir (file-name-as-directory user-login-name)))
(defvar user-packages (concat user-dir "packages.el"))
(defvar user-behaviours-dir (concat user-dir (file-name-as-directory "behaviours")))
(if (file-exists-p user-behaviours-dir)
    (defvar user-behaviours-files (directory-files user-behaviours-dir t "\.el$"))
    (defvar user-behaviours-files nil))
(defvar user-keybindings (concat user-dir "keybindings.el"))
(defvar user-scratchpad (concat user-dir "scratchpad.el"))

;; Install packages
(load-file base-packages)
(when (file-exists-p system-packages) (load-file system-packages)
      (el-get 'sync base-packages))
(when (file-exists-p user-packages) (load-file user-packages)
      (el-get 'sync my-packages))
(el-get 'sync)
(el-get 'wait)

;; Load behaviours
(defun load-behaviours-files (filelist)
  (dolist (file filelist)
    (load file)
    (message "Loaded behaviour file: %s" (file-name-nondirectory file))))

(load-behaviours-files base-behaviours-files)
(if system-behaviours-files (load-behaviours-files system-behaviours-files))
(if user-behaviours-files (load-behaviours-files user-behaviours-files))

;; Load some starter-kit helpers
(when (file-exists-p "~/.emacs.d/starter-kit-defuns.el") (load "~/.emacs.d/starter-kit-defuns.el"))
(when (file-exists-p "~/.emacs.d/starter-kit-misc.el") (load "~/.emacs.d/starter-kit-misc.el"))

;; Load keybindings
(when (file-exists-p base-keybindings) (load base-keybindings))
(when (file-exists-p system-keybindings) (load system-keybindings))
(when (file-exists-p user-keybindings) (load user-keybindings))

;; Load scratchpads
(when (file-exists-p system-scratchpad) (load system-scratchpad))
(when (file-exists-p user-scratchpad) (load user-scratchpad))
