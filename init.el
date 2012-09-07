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
           :type git
           :url "git://github.com/myuhe/calfw-gcal.el.git")
        (:name color-dired
           :type git
           :url "git://github.com/emacsmirror/color-dired.git")
        (:name dpastede
           :type git
           :url "git://github.com/emacsmirror/dpastede.git")
        (:name find-file-in-git-repo
           :type git
           :url "git://github.com/re5et/find-file-in-git-repo.git")
        (:name flymake-coffee
           :type git
           :url "git://github.com/purcell/flymake-coffee.git")
        (:name flymake-haml
           :type git
           :url "git://github.com/purcell/flymake-haml.git")
        (:name flymake-python
           :type git
           :url "git://github.com/akaihola/flymake-python.git")
        (:name flymake-sass
           :type git
           :url "git://github.com/purcell/flymake-sass.git")
        (:name flymake-shell
           :type git
           :url "git://github.com/purcell/flymake-shell.git")
        (:name furl
           :type git
           :url "git://github.com/jaalto/emacs-epackage--lib-furl.git")
        (:name grin
           :type hg
           :url "https://bitbucket.org/dariusp686/emacs-grin")
        (:name helm-ipython
           :type git
           :url  "git://github.com/emacs-helm/helm-ipython.git")
        (:name idle-highlight-mode
           :type git
           :url  "git://github.com/nonsequitur/idle-highlight-mode.git")
        (:name iresize
           :type git
           :url  "git://github.com/emacsattic/iresize.git")
        (:name js-beautify
           :type git
           :url "git://github.com/einars/js-beautify.git")
        (:name jshint-mode
           :type git
           :url "git://github.com/daleharvey/jshint-mode.git")
        (:name kill-ring-search.el
           :type git
           :url "git://github.com/nschum/kill-ring-search.el.git")
        (:name nose
           :type git
           :url "git://github.com/emacsmirror/nose.git")
        (:name pomodoro.el
           :type git
           :url "git://github.com/docgnome/pomodoro.el.git")
        ;(:name project                    :type elpa)
        (:name python-extras
           :type git
           :url "git://github.com/emacsmirror/python-extras.git")
        (:name tea-time
           :type git
           :url "git://github.com/gabrielsaldana/tea-time.git")
        (:name tuareg                     :type elpa)
        (:name wtf                        :type elpa)
        (:name zenburn
           :type git
           :url "git://github.com/bbatsov/zenburn-emacs.git")
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
      flymake-python
      flymake-sass
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
