;; While visiting a Python file, run:
;; M-x python-check or
;; C-c C-v or
;; C-c C-w
(setq python-check-command "pyflakes")

;; Disable Emacs-Flymake for html mode.
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
