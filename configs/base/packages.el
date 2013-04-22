;; Define sources
(setq recipes
 '(el-get  ;; el-get is self-hosting
   auctex
   ace-jump-mode
   auto-complete
   auto-indent-mode
   autopair
   buffer-move
   calfw
   clojure-mode
   color-dired
   coffee-mode
   csv-mode
   dash
   dictionary
   dired+
   diredful
   dired-details
   dired-details+
   dired-isearch
   dired-single
   django-mode
   ;; django-html-mode
   dpastede
   emacs-ctable
   emacs-edbi
   emacs-epc
   emacs-deferred
   exec-path-from-shell
   expand-region
   find-file-in-git-repo
   find-file-in-project
   furl
   flycheck
   flymake-cursor
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
   kill-ring-search.el
   less-css-mode
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
   ;; project
   pylookup
   pymacs
   python
   python-extras
   rainbow-delimiters
   rainbow-mode
   redo+
   regex-tool
   rst-mode
   rope
   ropemacs
   ropemode
   ruby-mode
   ruby-end
   inf-ruby
   ;rvm
   s
   sass-mode
   ;; skype http://blog.mediaonfire.com/?p=60
   slime
   smart-tab
   smex
   space-chord
   swank-clojure
   synonyms
   tea-time
   tuareg-mode
   twittering-mode
   undo-tree
   virtualenv
   wgrep
   worklog
   wtf
   yaml-mode
   yasnippet
   zenburn
   zencoding-mode
   auto-complete))
(el-get 'sync recipes)
(el-get 'wait)
