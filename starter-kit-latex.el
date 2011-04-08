
(load "auctex-pkg.el" nil t t)

(load "preview.el" nil t t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; (org-babel-add-interpreter "latex")
;; (add-to-list 'org-babel-tangle-langs '("latex" "tex"))
;; ; (require 'org-babel-latex)

(add-to-list 'org-babel-noweb-error-langs "latex")
