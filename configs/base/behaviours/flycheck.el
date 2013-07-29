;; enable Flycheck mode in all buffers in which it can be used
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable Flycheck for all files
(add-hook 'find-file-hook 'flycheck-mode)

(flycheck-declare-checker python-flake8
  "A Python syntax and style checker using the flake8 utility.
  See URL `http://pypi.python.org/pypi/flake8'."
  :command '("flake8" (config "--config" flycheck-flake8rc) source-inplace)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:[[:alpha:]]\\{2\\}.*\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E[0-9]+.*\\)$"
     error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:W[0-9]+.*\\)$"
     warning))
  :modes '(python-mode django-mode))
