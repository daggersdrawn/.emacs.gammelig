;; https://github.com/ptrv/processing2-emacs
;; C-c C-r    Run a sketch.
;; C-c C-b    Compile a sketch into .class files.
;; C-c C-p    Run a sketch full screen.
;; C-c C-e    Export sketch.

(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(when macosx-p
  (setq processing-location "/usr/bin/processing-java"))
(when linux-p
  (setq processing-location "/usr/share/processing/processing-java"))
