(require 'python)

;;; anaconda mode
(pyenv-mode)
(add-hook 'python-mode-hook 'anaconda-mode)

;;; indentation
(setq-default py-indent-offset 4)

;;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;; autopair
(add-hook 'python-mode-hook
           #'(lambda ()
               (setq autopair-handle-action-fns
                     (list #'autopair-default-handle-action
                           #'autopair-python-triple-quote-action))))

;;; ipython
(defcustom python-python-command "ipython"
  "Shell command to run Python interpreter."
  :group 'python
  :type 'string
)

;;; docstring formatting
(setq-default python-fill-docstring-style 'django)
