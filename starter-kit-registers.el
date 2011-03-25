
(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?s (file . ,(concat dotfiles-dir "starter-kit.org")))
             (?r (file . ,(concat dotfiles-dir "starter-kit-registers.org")))))
  (set-register (car r) (cadr r)))
