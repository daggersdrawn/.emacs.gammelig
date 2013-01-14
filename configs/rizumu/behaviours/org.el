;; provide the jump to file functions
(defun gtd-life ()
  (interactive)
  (find-file (concat org-directory "/gtd-life.org")))
(provide 'org-gtd-life)

(defun reference ()
  (interactive)
  (find-file (concat org-directory "/reference.org")))
(provide 'org-reference)

(defun someday ()
  (interactive)
  (find-file (concat org-directory "/someday.org")))
(provide 'org-someday)

;; These bits of captured information must eventually be reviewed and
;; filed somewhere (perhaps in gtd.org, or in a project-specific org
;; file.) The out-of-sight, out-of-mind rule applies here---if I don't
;; review these auxiliary org-files, I'll probably forget what's in them.
(setq org-capture-templates
      '(("t" "todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "note" entry (file+headline (concat org-directory "/notes.org") "Notes to review")
         "* %^{Title}\n  %i\n  %a")
        ("s" "someday" entry (file+headline (concat org-directory "/someday.org") "Ideas")
         "* %^{Title}\n  %i\n  %a")
        ("m" "morningpages" entry (file+datetree (concat org-directory "/morningpages.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))

