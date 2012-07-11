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
      '(;;(:name auctex                     :type elpa)
        ;(:name calfw-gcal                 :type elpa)
        ;(:name clojure-test-mode          :type elpa)
        (:name color-dired
           :type git
           :url "git://github.com/emacsmirror/color-dired.git")
        (:name dpastede
           :type git
           :url "git://github.com/emacsmirror/dpastede.git")
        ;(:name find-file-in-git-repo      :type elpa)
        ;(:name flymake-coffee             :type elpa)
        ;(:name flymake-haml               :type elpa)
        ;(:name flymake-jshint             :type elpa)
        (:name flymake-python
           :type git
           :url "git://github.com/akaihola/flymake-python.git")
        ;(:name flymake-sass               :type elpa)
        ;(:name flymake-shell              :type elpa)
        ;(:name furl                       :type elpa)
        ;(:name ghc                        :type elpa)
        ;(:name grin                       :type elpa)
        ;(:name idle-highlight             :type elpa)
        ;(:name iresize                    :type elpa)
        ;(:name javascript                 :type elpa)
        (:name js-beautify
           :type git
           :url "git://github.com/einars/js-beautify.git")
        (:name jshint-mode
           :type git
           :url "git://github.com/daleharvey/jshint-mode.git")
        ;(:name kill-ring-search           :type elpa)
        ;(:name nose                       :type elpa)
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
    '(;;auctex
      auto-complete
      auto-indent-mode
      autopair
      buffer-move
      ;calfw-gcal
      clojure-mode
      ;clojure-test-mode
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
      dpastede
      ;find-file-in-git-repo
      find-file-in-project
      ;flymake-coffee
      flymake-cursor
      ;flymake-haml
      ;flymake-jshint
      flymake-python
      ;flymake-sass
      ;flymake-shell
      ;furl
      ;ghc
      ;gist
      google-contacts
      google-maps
      google-weather
      grep+
      ;grin
      haml-mode
      haskell-mode
      highlight-parentheses
      highlight-symbol
      htmlize
      icomplete+
      ;idle-highlight
      ipython
      ;iresize
      ;javascript
      js-beautify
      jshint-mode
      keywiz
      ;kill-ring-search
      linum-off
      lua-mode
      lusty-explorer
      magit
      magithub
      markdown-mode
      maxframe
      nav
      ;nose
      oauth2
      org-mode
      paredit
      pony-mode
      pomodoro.el
      ;project
      python
      python-extras
      rainbow-delimiters
      rainbow-mode
      redo+
      sass-mode
      skype
      slime
      smart-tab
      smex
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

(load-cfg-files '(;"cfg_autopair"
                  ;"cfg_buffer-move"
                  ;"cfg_diredx"
                  ;"cfg_flymake-python"
                  ;"cfg_gnus"
                  ;"cfg_hideshow"
                  ;"cfg_html"
                  ;"cfg_jsbeautify"
                  ;"cfg_jshint"
                  ;"cfg_keybinding"
                  ;"cfg_maxframe"
                  ;"cfg_org"
                  ;"cfg_python"
                  ;"cfg_rst"
                  ;"cfg_tramp"
                  ;"cfg_twitter"
                  ;"cfg_uniquify"
                  "cfg_yasnippet"
                  "cfg_zenburn"))

;; Load up personalization files:
(setq system-config (concat user-emacs-directory system-name ".el"))
(setq user-config (concat user-emacs-directory user-login-name ".el"))
(setq user-dir (concat user-emacs-directory user-login-name))
;(when (file-exists-p user-config) (load user-config))
;(when (file-exists-p system-config) (load system-config))
;(when (file-exists-p user-dir) (mapc 'load (directory-files user-dir t "^[^#].*el$")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes (quote ("bf9d5728e674bde6a112979bd830cc90327850aaaf2e6f3cc4654f077146b406" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
