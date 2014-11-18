;; Define sources
(setq recipes
 '(el-get  ;; el-get is self-hosting
   anaconda-mode
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
   exec-path-from-shell
   expand-region
   find-file-in-git-repo
   flycheck
   ghc-mod
   gist
   google-contacts
   google-maps
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
   magithub
   markdown-mode
   maxframe
   multiple-cursors
   nav
   ;; nose
   org-mode
   paredit
   pony-mode
   processing-mode
   project
   pyenv-mode
   pylookup
   rainbow-delimiters
   rainbow-mode
   redo+
   rst-mode
   ;; skype http://blog.mediaonfire.com/?p=60
   ;; slime
   smart-tab
   smex
   swank-clojure
   synonyms
   tuareg-mode
   undo-tree
   worklog
   wrap-region
   wtf
   yaml-mode
   yasnippet
   zenburn
   zencoding-mode))
(el-get 'sync recipes)
(el-get 'wait)
