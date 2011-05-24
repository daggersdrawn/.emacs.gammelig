(add-hook 'html-mode-hook 'turn-off-auto-fill)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; start on any markup modes
