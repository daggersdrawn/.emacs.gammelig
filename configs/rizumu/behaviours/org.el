;; provide the custom functions
(defun gtd ()
  (interactive)
  (find-file (concat org-directory "/gtd.org")))
(provide 'org-gtd)

(defun reference ()
  (interactive)
  (find-file (concat org-directory "/reference.org")))
(provide 'org-reference)

(defun someday ()
  (interactive)
  (find-file (concat org-directory "/someday.org")))
(provide 'org-someday)
