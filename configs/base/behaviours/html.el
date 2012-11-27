(add-hook 'html-mode-hook 'turn-off-auto-fill)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; start on any markup modes
(add-hook 'html-mode-hook
  (lambda ()
    ;; Default indentation is usually 2 spaces, changing to 4.
    (set (make-local-variable 'sgml-basic-offset) 4)))
