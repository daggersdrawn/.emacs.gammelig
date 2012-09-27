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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

;; local sources
(setq el-get-sources
      '((:name calfw-gcal
           :type github
           :pkgname "myuhe/calfw-gcal.el")
        (:name color-dired
           :type github
           :pkgname "emacsmirror/color-dired")
        (:name dpastede
           :type github
           :pkgname "emacsmirror/dpastede")
        (:name find-file-in-git-repo
           :type github
           :pkgname "re5et/find-file-in-git-repo")
        (:name flymake-coffee
           :type github
           :pkgname "purcell/flymake-coffee")
        (:name flymake-haml
           :type github
           :pkgname "purcell/flymake-haml")
        (:name flymake-jslint
           :type github
           :pkgname "purcell/flymake-jslint")
        (:name less-css-mode
           :type github
           :pkgname "purcell/less-css-mode")
        (:name flymake-python
           :type github
           :pkgname "akaihola/flymake-python")
        (:name flymake-ruby
           :type github
           :pkgname "purcell/flymake-ruby")
        (:name flymake-shell
           :type github
           :pkgname "purcell/flymake-shell")
        (:name furl
           :type github
           :pkgname "jaalto/emacs-epackage--lib-furl")
        (:name grin
           :type hg
           :url "https://bitbucket.org/dariusp686/emacs-grin")
        (:name helm-ipython
           :type github
           :pkgname  "emacs-helm/helm-ipython")
        (:name idle-highlight-mode
           :type github
           :pkgname  "nonsequitur/idle-highlight-mode")
        (:name iresize
           :type github
           :pkgname  "emacsattic/iresize")
        (:name js-beautify
           :type github
           :pkgname "ostrovok-team/js-beautify")
        (:name jshint-mode
           :type github
           :pkgname "daleharvey/jshint-mode")
        (:name kill-ring-search.el
           :type github
           :pkgname "nschum/kill-ring-search.el")
        (:name nose
           :type github
           :pkgname "emacsmirror/nose")
        (:name pomodoro.el
           :type github
           :pkgname "docgnome/pomodoro.el")
        ;(:name project                    :type elpa)
        (:name python-extras
           :type github
           :pkgname "emacsmirror/python-extras")
        (:name tea-time
           :type github
           :pkgname "gabrielsaldana/tea-time")
        (:name tuareg                     :type elpa)
        (:name undo-tree
           :type github
           :pkgname "emacsmirror/undo-tree")
        (:name wtf                        :type elpa)
        (:name yasnippet
           :type github
           :pkgname "capitaomorte/yasnippet")
        (:name zenburn
           :type github
           :pkgname "bbatsov/zenburn-emacs")
))

(setq my-packages
  (append
    '(;auctex
      auto-complete
      auto-indent-mode
      autopair
      buffer-move
      calfw-gcal
      clojure-mode
      color-dired
      coffee-mode
      csv-mode
      dictionary
      dired+
      diredful
      dired-details
      dired-details+
      dired-isearch
      dired-single
      django-mode
      dpastede
      find-file-in-git-repo
      find-file-in-project
      flymake-coffee
      flymake-cursor
      flymake-haml
      flymake-jslint
      flymake-python
      flymake-ruby
      flymake-shell
      furl
      ghc-mod
      ;; gist
      google-contacts
      google-maps
      google-weather
      grep+
      grin
      haml-mode
      haskell-mode
      helm
      helm-ipython
      highlight-parentheses
      highlight-symbol
      htmlize
      icomplete+
      idle-highlight-mode
      ido-ubiquitous
      ipython
      iresize
      js-beautify
      jshint-mode
      keywiz
      less-css-mode
      kill-ring-search.el
      linum-off
      lua-mode
      lusty-explorer
      magit
      magithub
      markdown-mode
      maxframe
      nav
      nose
      oauth2
      org-mode
      paredit
      pony-mode
      pomodoro.el
      ;; project
      python
      python-extras
      rainbow-delimiters
      rainbow-mode
      redo+
      sass-mode
      skype
      slime
      smart-tab
      ;; smex
      synonyms
      tea-time
      tuareg
      twittering-mode
      undo-tree
      virtualenv
      worklog
      wtf
      yaml-mode
      yasnippet
      zenburn
      zencoding-mode)
   (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)
(el-get 'sync)
(el-get 'wait)

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
                  "cfg_dired"
                  "cfg_django"
                  "cfg_flymake-python"
                  "cfg_gnus"
                  "cfg_hideshow"
                  "cfg_html"
                  "cfg_jsbeautify"
                  "cfg_jshint"
                  "cfg_keybinding"
                  "cfg_maxframe"
                  "cfg_org"
                  "cfg_pony"
                  "cfg_python"
                  "cfg_rst"
                  "cfg_tramp"
                  "cfg_twitter"
                  "cfg_uniquify"
                  "cfg_yasnippet"
                  "cfg_yaml"
                  "cfg_zenburn"))

;; Load up personalization files:
(setq system-config (concat user-emacs-directory system-name ".el"))
(setq user-config (concat user-emacs-directory user-login-name ".el"))
(setq user-dir (concat user-emacs-directory user-login-name))
(when (file-exists-p user-config) (load user-config))
(when (file-exists-p system-config) (load system-config))
(when (file-exists-p user-dir) (mapc 'load (directory-files user-dir t "^[^#].*el$")))
(when (file-exists-p "~/.emacs.d/starter-kit-defuns.el") (load "~/.emacs.d/starter-kit-defuns.el"))
(when (file-exists-p "~/.emacs.d/starter-kit-misc.el") (load "~/.emacs.d/starter-kit-misc.el"))
