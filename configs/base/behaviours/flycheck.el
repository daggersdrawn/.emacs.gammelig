;; Enable Flycheck for all files
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'flycheck-mode)


(setq flycheck-flake8-maximum-line-length 100)

(flycheck-declare-checker python-flake8
  "A Python syntax and style checker using the flake8 utility.

For best error reporting, use Flake8 2.0 or newer.

See URL `http://pypi.python.org/pypi/flake8'."
  :command '("flake8"
             (config-file "--config" flycheck-flake8rc)
             (option "--max-complexity"
                     flycheck-flake8-maximum-complexity
                     flycheck-option-int)
             (option "--max-line-length"
                     flycheck-flake8-maximum-line-length
                     flycheck-option-int)
             source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E[0-9]+.*\\)$"
     error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:F[0-9]+.*\\)$"
     warning) ; Flake8 >= 2.0
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:W[0-9]+.*\\)$"
     warning) ; Flake8 < 2.0
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:C[0-9]+.*\\)$"
     warning) ; McCabe complexity in Flake8 > 2.0
    ;; Syntax errors in Flake8 < 2.0, in Flake8 >= 2.0 syntax errors are caught
    ;; by the E.* pattern above
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes '(python-mode django-mode pony-mode))
