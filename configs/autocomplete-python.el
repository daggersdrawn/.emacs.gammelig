(require 'auto-complete)

;; setup pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (pymacs-load "ropemacs" "rope-")
  
  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t))


(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))
(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

;(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)


(provide 'auto-complete-python)
