;; Define sources
(setq recipes
 '(el-get  ;; el-get is self-hosting
   auctex
   ace-jump-mode
   auto-complete
   auto-indent-mode
   autopair
   buffer-move
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
   ;; django-html-mode
   exec-path-from-shell
   expand-region
   find-file-in-git-repo
   flycheck dash s
   ghc-mod
   gist
   google-contacts
   google-maps
   google-weather
   haskell-mode
   helm
   highlight-parentheses
   highlight-symbol
   idle-highlight-mode
   ido-ubiquitous
   js-beautify
   jshint-mode
   kill-ring-search.el
   less-css-mode
   linum-off
   magit
   ;; magithub
   markdown-mode
   maxframe
   multiple-cursors
   nav
   org-mode
   paredit
   pony-mode
   processing-mode
   ;; project
   pylookup
   rainbow-delimiters
   rainbow-mode
   ;; redo+
   rst-mode
   ;; skype http://blog.mediaonfire.com/?p=60
   ;; slime
   smart-tab
   ;; smex
   ;; space-chord
   ;; swank-clojure
   synonyms
   undo-tree
   virtualenv
   worklog
   ;; wtf
   yaml-mode
   yasnippet
   zenburn
   zencoding-mode))
(el-get 'sync recipes)
(el-get 'wait)
