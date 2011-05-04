(require 'django-html-mode)
(require 'django-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))
(add-hook 'django-html-mode-hook 'turn-off-auto-fill)
