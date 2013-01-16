;; ;; Enable Flycheck for all programming modes
;; (add-hook 'prog-mode-hook 'flycheck-mode)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (add-hook 'prog-mode-hook 'flycheck-mode-on)))
;; Enable Flycheck for all files
(add-hook 'find-file-hook 'flycheck-mode)
