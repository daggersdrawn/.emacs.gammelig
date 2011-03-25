
(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(require 'dominating-file)

(defun starter-kit-load (file)
  "This function is to be used to load starter-kit-*.org files."
  (org-babel-load-file (expand-file-name file
                                         dotfiles-dir)))

(defun starter-kit-compile (&optional arg)
  "Tangle and Byte compile all starter-kit files."
  (interactive "P")
  (flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (mapc
     (lambda (file)
       (when (string= "org" (file-name-extension file))
         (let ((el-file (concat (file-name-sans-extension file) ".el")))
           (when (or arg
                     (not (and (file-exists-p el-file)
                               (> (age file) (age el-file)))))
             (org-babel-tangle-file file el-file "emacs-lisp")
             (byte-compile-file el-file)))))
     (apply #'append
            (mapcar
             (lambda (d)
               (when (and (file-exists-p d) (file-directory-p d))
                 (mapcar (lambda (f) (expand-file-name f d)) (directory-files d))))
             (list (concat dotfiles-dir user-login-name) dotfiles-dir))))))

(require 'package)
(add-to-list 'package-archives
             '("original" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t) 
(package-initialize)
(starter-kit-load "starter-kit-elpa.org")

(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

(setq system-specific-config (concat dotfiles-dir system-name ".el")
      system-specific-literate-config (concat dotfiles-dir system-name ".org")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-literate-config (concat dotfiles-dir user-login-name ".org")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(setq elisp-source-dir (concat dotfiles-dir "src"))
(add-to-list 'load-path elisp-source-dir)

(starter-kit-load "starter-kit-defuns.org")

(starter-kit-load "starter-kit-bindings.org")

(starter-kit-load "starter-kit-misc.org")

(starter-kit-load "starter-kit-registers.org")

(add-to-list 'load-path
             (expand-file-name  "yasnippet"
                                (expand-file-name "src"
                                                  dotfiles-dir)))
(require 'yasnippet)
(yas/initialize)

(yas/load-directory (expand-file-name "snippets" dotfiles-dir))

;;(starter-kit-load "starter-kit-org.org")

(starter-kit-load "starter-kit-eshell.org")

(starter-kit-load "starter-kit-lisp.org")

(starter-kit-load "starter-kit-haskell.org")

(starter-kit-load "starter-kit-ruby.org")

(starter-kit-load "starter-kit-js.org")

(starter-kit-load "starter-kit-perl.org")

;;(starter-kit-load "starter-kit-python.org")

;;(starter-kit-load "starter-kit-latex.org")

(load custom-file 'noerror)

(if (file-exists-p elisp-source-dir)
    (let ((default-directory elisp-source-dir))
      (normal-top-level-add-subdirs-to-load-path)))
(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p system-specific-literate-config)
    (org-babel-load-file system-specific-literate-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-literate-config)
    (org-babel-load-file user-specific-literate-config))
(when (file-exists-p user-specific-dir)
  (let ((default-directory user-specific-dir))
    (mapc #'load (directory-files user-specific-dir nil ".*el$"))
    (mapc #'org-babel-load-file (directory-files user-specific-dir nil ".*org$"))))
