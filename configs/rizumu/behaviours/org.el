;; These bits of captured information must eventually be reviewed and
;; filed somewhere (perhaps in gtd.org, or in a project-specific org
;; file.) The out-of-sight, out-of-mind rule applies here---if I don't
;; review these auxiliary org-files, I'll probably forget what's in them.
(setq org-capture-templates
      '(("t" "todo" entry
         (file+headline (concat org-directory "/inbox.org")
                        "Tasks") "* TODO %?\n  %i\n  %a")
        ("n" "note" entry
         (file+headline (concat org-directory "/notes.org")
                        "Notes to review") "* %^{Title}\n  %i\n  %a")
        ("s" "someday" entry
         (file+headline (concat org-directory "/someday.org")
                        "Ideas") "* %^{Title}\n  %i\n  %a")
        ("m" "morningpages" entry
         (file+datetree (concat org-directory "/morningpages.org")
                        "Morning pages") "* %?\nEntered on %U\n  %i\n  %a")))

;; buy dl read watch
