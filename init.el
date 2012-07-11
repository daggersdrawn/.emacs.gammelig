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


(require 'cl) ; common lisp goodies, loop

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)


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
      '(;(:name auctex                     :type elpa)
        (:name auto-indent-mode           :type elpa)
        (:name buffer-move                :type elpa)
        (:name calfw-gcal                 :type elpa)
        (:name color-dired
	       :type git
	       :url "git://github.com/emacsmirror/color-dired.git")
        (:name clojure-test-mode          :type elpa)
        (:name diredful
	       :type git
	       :url "git://github.com/emacsmirror/diredful.git")
        (:name dired-details+             :type elpa)
        (:name dired-isearch              :type elpa)
        (:name dpastede
	       :type git
	       :url "git://github.com/emacsmirror/dpastede.git")
        (:name find-file-in-git-repo      :type elpa)
        (:name find-file-in-project       :type elpa)
        (:name flymake-coffee             :type elpa)
        (:name flymake-cursor             :type elpa)
        (:name flymake-cursor             :type elpa)
        (:name flymake-haml               :type elpa)
        (:name flymake-jshint             :type elpa)
        (:name flymake-python
	       :type git
	       :url "git://github.com/akaihola/flymake-python.git")
        (:name flymake-sass               :type elpa)
        (:name flymake-shell              :type elpa)
        (:name furl                       :type elpa)
        (:name ghc                        :type elpa)
        (:name grin                       :type elpa)
        (:name idle-highlight             :type elpa)
        (:name ipython                    :type elpa)
        (:name iresize                    :type elpa)
        (:name javascript                 :type elpa)
        (:name js-beautify
	       :type git
	       :url "git://github.com/einars/js-beautify.git")
        (:name jshint-mode
	       :type git
	       :url "git://github.com/daleharvey/jshint-mode.git")
        (:name kill-ring-search           :type elpa)
        (:name lusty-explorer             :type elpa)
        (:name markdown-mode              :type elpa)
        (:name oauth2                     :type elpa)
        (:name pomodoro.el
           :type git
           :url "git://github.com/docgnome/pomodoro.el.git")
        (:name project                    :type elpa)
        (:name python-extras
           :type git
           :url "git://github.com/emacsmirror/python-extras.git")
        (:name redo+                      :type elpa)
        (:name nose                       :type elpa)
        (:name smex                       :type elpa)
        (:name starter-kit                :type elpa)
        (:name synonyms                   :type elpa)
        (:name tea-time
           :type git
           :url "git://github.com/gabrielsaldana/tea-time.git")
        (:name tuareg                     :type elpa)
        (:name twittering-mode            :type elpa)
        (:name worklog                    :type elpa)
        (:name wtf                        :type elpa)
        (:name yasnippet-bundle           :type elpa)
))

(setq my-packages
  (append
    '(;el-get
      ;auctex
      auto-complete
      auto-indent-mode
      autopair
      buffer-move
      calfw-gcal
      clojure-mode
      clojure-test-mode
      color-dired
      coffee-mode
      color-theme
      color-theme-zenburn
      csv-mode
      dictionary
      dired+
      diredful
      dired-details
      dired-details+
      dired-isearch
      dired-single
      dpastede
      find-file-in-git-repo
      find-file-in-project
      flymake-coffee
      flymake-cursor
      flymake-haml
      flymake-jshint
      flymake-python
      flymake-sass
      flymake-shell
      furl
      ghc
      gist
      google-contacts
      google-maps
      google-weather
      grep+
      grin
      haml-mode
      haskell-mode
      highlight-parentheses
      highlight-symbol
      htmlize
      icomplete+
      idle-highlight
      ipython
      iresize
      javascript
      jshint-mode
      js-beautify
      keywiz
      kill-ring-search
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
      project
      python
      python-extras
      rainbow-mode
      redo+
      sass-mode
      skype
      slime
      smart-tab
      smex
      starter-kit
      synonyms
      tea-time
      tuareg
      twittering-mode
      undo-tree
      virtualenv
      worklog
      wtf
      yaml-mode
      yasnippet-bundle
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
                  "cfg_diredx"
                  "cfg_flymake-python"
                  "cfg_gnus"
                  "cfg_hideshow"
                  "cfg_html"
                  "cfg_jsbeautify"
                  "cfg_jshint"
                  "cfg_keybinding"
                  "cfg_maxframe"
                  "cfg_org"
                  "cfg_python"
                  "cfg_rst"
                  "cfg_tramp"
                  "cfg_twitter"
                  "cfg_uniquify"
                  "cfg_yasnippet"
                  "cfg_zenburn"))
