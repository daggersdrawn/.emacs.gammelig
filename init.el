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

;; Load up ELPA, the package manager:
(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Load up src packages:
(defun get-subdirs (directory)
  "Get a list of subdirectories under a given directory"
  (apply 'nconc (mapcar (lambda (fa)
                        (and
                         (eq (cadr fa) t)
                         (not (equal (car fa) "."))
                         (not (equal (car fa) ".."))
                         (list (car fa))))
                        (directory-files-and-attributes directory))))

(defun add-dirs-to-loadpath (dir-name)
  "add subdirs of your src directory to the load path"
  (dolist (subdir (get-subdirs dir-name))
    (setq load-path (cons (concat dir-name subdir) load-path))
    (message "Added %s to load path" subdir)))

(add-dirs-to-loadpath "~/.emacs.d/src/")

;; check marmalade for packages and install
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
    anything
    anything-complete
    anything-config
    anything-ipython
    anything-match-plugin
    auctex
    auto-complete
    auto-indent-mode
    autopair
    buffer-move
    clojure-mode
    clojure-test-mode
    color-theme
    color-theme-zenburn
    csv-mode
    descbinds-anything
    dictionary
    dired-isearch
    dired-single
    find-file-in-git-repo
    find-file-in-project
    flymake-cursor
    furl
    gist
    haml-mode
    haskell-mode
    highlight-parentheses
    htmlize
    idle-highlight
    iresize
    javascript
    keywiz
    kill-ring-search
    linum-off
    lua-mode
    lusty-explorer
    magit
    markdown-mode
    marmalade
    nav
    oddmuse
    org
    paredit
    project
    python
    rainbow-mode
    redo+
    sass-mode
    scpaste
    slime
    smart-tab
    smex
    starter-kit
    starter-kit-bindings
    starter-kit-lisp
    starter-kit-eshell
    starter-kit-js
    starter-kit-lisp
    starter-kit-ruby
    tuareg
    synonyms
    worklog
    wtf
    yaml-mode
    yasnippet-bundle
))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load up configuration files:
(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (let ((filename (expand-file-name (concat "~/.emacs.d/configs/" file ".el"))))
      (if (file-exists-p filename)
          (progn
            (load (concat filename))
            (message "Loaded config file: %s" filename))
       (message "Could not load file: %s" filename)))))

(load-cfg-files '("cfg_autopair"
                  "cfg_buffer-move"
                  "cfg_calfw"
                  "cfg_diredx"
                  "cfg_hideshow"
                  "cfg_html"
                  "cfg_jsbeautify"
                  "cfg_jshint"
                  "cfg_keybinding"
                  "cfg_maxframe"
                  "cfg_org"
                  "cfg_pyflake"
                  "cfg_python"
                  "cfg_tramp"
                  "cfg_twitter"
                  "cfg_uniquify"
                  "cfg_yasnippet"
                  "cfg_zenburn"))
(require 'find-file-in-git-repo)
(require 'google-contacts)
(require 'google-maps) (require 'org-location-google-maps)
(require 'linum-off)
(require 'maxframe)
(require 'oauth2)
(require 'pomodoro)
(require 'pony-mode)
(require 'redo+)
(require 'tea-time)
