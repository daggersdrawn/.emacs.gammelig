
;;(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq org-babel-python-mode 'python-mode)

(defun python-use-ipython (cmd args)
  (setq ipython-command cmd)
  (setq py-python-command-args args)
  (require 'ipython)
  (setq ipython-completion-command-string
        "print(';'.join(__IP.Completer.all_completions('%s')))\n"))

;;(require 'cython-mode)
;;(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
;;(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
;;(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(require 'python)

; indentation
(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
  they line up with the line containing the corresponding opening bracket."
(save-excursion
  (beginning-of-line)
  (let ((syntax (syntax-ppss)))
    (if (and (not (eq 'string (syntax-ppss-context syntax)))
             (python-continuation-line-p)
             (cadr syntax)
             (skip-syntax-forward "-")
             (looking-at "\\s)"))
        (progn
          (forward-char 1)
          (ignore-errors (backward-sexp))
          (setq ad-return-value (current-indentation)))
      ad-do-it))))

(ad-activate 'python-calculate-indentation)
