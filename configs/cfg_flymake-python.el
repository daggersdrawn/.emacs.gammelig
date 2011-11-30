;; configure flymake-python
;; https://github.com/rassie/flymake-python/tree/04acbed4291595742363169ad2e34747119b40f3
(require 'flymake-cursor)

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers.sh" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; activate flymake automatically
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; disable flymake for html mode.
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
