;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))
;; Load up Org Mode and Babel
(require 'org-install)

;; load up the main file
;;(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

(setq load-path (cons "~/.emacs.d/configs/" load-path))
(setq load-path (cons "~/.emacs.d/elpa/" load-path))
(setq load-path (cons "~/.emacs.d/src/" load-path))

(defun get-subdirs (directory)
  "Get a list of subdirectories under a given directory"
  (apply 'nconc (mapcar (lambda (fa)
                        (and
                         (eq (cadr fa) t)
                         (not (equal (car fa) "."))
                         (not (equal (car fa) ".."))
                         (list (car fa))))
                        (directory-files-and-attributes directory))))

(defconst emacs-config-dir "~/.emacs.d/configs/")
(defconst emacs-elpa-dir "~/.emacs.d/elpa/")
(defconst emacs-src-dir "~/.emacs.d/src/")
(defun add-dirs-to-loadpath (dir-name)
  "add subdirs of your src directory to the load path"
  (dolist (subdir (get-subdirs dir-name))
    (setq load-path (cons (concat dir-name subdir) load-path))
    (message "Added %s to load path" subdir)))

(add-dirs-to-loadpath emacs-config-dir)
(add-dirs-to-loadpath emacs-src-dir)
(add-dirs-to-loadpath emacs-elpa-dir)

(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (let ((filename (expand-file-name (concat emacs-config-dir file ".el"))))
      (if (file-exists-p filename)
          (progn
            (load (concat filename))
            (message "Loaded config file: %s" filename))
       (message "Could not load file: %s" filename)))))

;;; init.el ends here

