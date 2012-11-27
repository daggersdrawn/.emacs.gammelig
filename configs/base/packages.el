;; Define sources
(setq el-get-sources
      '((:name auctex
           :type github
           :pkgname "emacsmirror/auctex")
        (:name calfw-gcal
           :type github
           :pkgname "myuhe/calfw-gcal.el")
        (:name color-dired
           :type github
           :pkgname "emacsmirror/color-dired")
        (:name dpastede
           :type github
           :pkgname "emacsmirror/dpastede")
        (:name exec-path-from-shell
           :type github
           :pkgname "purcell/exec-path-from-shell")
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
           :pkgname "einars/js-beautify")
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
        (:name processing-mode
           :type github
           :pkgname "ptrv/processing2-emacs")
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

(setq base-packages
  (append
    '(auctex
      ace-jump-mode
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
      exec-path-from-shell
      expand-region
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
      gist
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
      mark-multiple
      markdown-mode
      maxframe
      multiple-cursors
      nav
      nose
      oauth2
      org-mode
      paredit
      pony-mode
      pomodoro.el
      processing-mode
      pylookup
      pymacs
      python
      python-extras
      rainbow-delimiters
      rainbow-mode
      redo+
      rst-mode
      rope
      ropemacs
      ropemode
      sass-mode
      ;; skype http://blog.mediaonfire.com/?p=60
      slime
      smart-tab
      smex
      space-chord
      swank-clojure
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
