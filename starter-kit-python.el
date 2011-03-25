
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq org-babel-python-mode 'python-mode)

(defun python-use-ipython (cmd args)
  (setq ipython-command cmd)
  (setq py-python-command-args args)
  (require 'ipython)
  (setq ipython-completion-command-string
        "print(';'.join(__IP.Completer.all_completions('%s')))\n"))

(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))
