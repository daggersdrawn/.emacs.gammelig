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

;; Determine if running a GNU/Linux distro or Mac OSX
(setq macosx-p (string-match "darwin" (symbol-name system-type)))
(setq linux-p (string-match "gnu/linux" (symbol-name system-type)))

;; Set locations for base, user and system configuration files and folders
(setq configs-dir (expand-file-name (concat user-emacs-directory (file-name-as-directory "configs"))))
(setq base-dir (concat configs-dir (file-name-as-directory "base")))
(setq system-dir (concat configs-dir (file-name-as-directory system-name)))
(setq user-dir (concat configs-dir (file-name-as-directory user-login-name)))
(setq directory-structure (list base-dir system-dir user-dir))

;; verify existence of a file and load it
(defun check-and-load (file)
  (if (and (not (listp file)) (not (file-directory-p file)) (file-readable-p file))
      (load file)))

;; Loads one file, a list of files or all files in a directory.
(defun load-files (files)
  (check-and-load files)
  (if (listp files)
      (dolist (file files)
        (check-and-load file))
    (if (file-directory-p files)
        (dolist (file (directory-files files t "^[^#].*el$"))
          (check-and-load file)))))

;; Install packages
(loop for dir in directory-structure
      do (load-files (concat dir "packages.el")))
(el-get 'sync)
(el-get 'wait)

;; Load behaviours
(loop for dir in directory-structure
      do (load-files (concat dir (file-name-as-directory "behaviours"))))

;; Load starterkits
(load-files (list (concat user-emacs-directory "starter-kit-defuns.el")
                  (concat user-emacs-directory "starter-kit-misc.el")))

;; Load keybindings
(loop for dir in directory-structure
      do (load-files (concat dir "keybindings.el")))

;; Load snippets
(require 'yasnippet)
(setq snippets (list (concat user-emacs-directory (file-name-as-directory "el-get/yasnippet/snippets"))
                     (concat base-dir (file-name-as-directory "snippets"))
                     (concat system-dir (file-name-as-directory "snippets"))
                     (concat user-dir (file-name-as-directory "snippets"))))
(dolist (file snippets)
  (if (file-exists-p file)
      (yas-load-directory file)))
(yas-global-mode 1)

;; Load scratchpads
(loop for dir in directory-structure
      do (load-files (concat dir "scratchpad.el")))
