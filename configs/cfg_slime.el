(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
        (cmucl ("/usr/local/bin/cmucl") :coding-system iso-latin-1-unix)))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
