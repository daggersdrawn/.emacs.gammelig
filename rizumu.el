;; Fonts
(set-face-attribute 'default nil :font "Inconsolata 12")

;; Color Theme 
(require 'color-theme) 
(color-theme-initialize)
(load-file "~/.emacs.d/src/color-theme-sanityinc/color-theme-sanityinc.el")
(load-file "~/.emacs.d/src/zenburn/zenburn.el")

;;(color-theme-sanityinc)
(color-theme-zenburn)