;; These bits of captured information must eventually be reviewed and
;; filed somewhere (perhaps in gtd.org, or in a project-specific org
;; file.) The out-of-sight, out-of-mind rule applies here---if I don't
;; review these auxiliary org-files, I'll probably forget what's in them.
(setq org-capture-templates
      '(("t" "todo" entry
	 (file+headline (concat org-directory "/gtd-inbox.org")
			"Tasks") "* TODO %?\n  %i\n  %a")
	("n" "note" entry
	 (file+headline (concat org-directory "/notes.org")
			"Notes to review") "* %^{Title}\n  %i\n  %a")
	("m" "morningpages" entry
	 (file+datetree (concat org-directory "/morningpages.org")
			"Morning pages") "* %?\nEntered on %U\n  %i\n  %a")))

;; Use org's tag feature to implement contexts.
(setq org-tag-alist '((:startgroup)
		      ("@buyonline" . ?b)
		      ("@dl" . ?d)
		      ("@errand" . ?e)
		      ("@home" . ?h)
		      ("@listen" . ?l)
		      ("@mail" . ?m)
		      ("@phone" . ?p)
		      ("@read" . ?r)
		      ("@studio" . ?s)
		      ("@watch" . ?w)
		      (:endgroup)
		      ("DAILY" . ?D)
		      ("WEEKLY" . ?W)
		      ("MONTHLY" . ?M)
		      ("QUARTERLY" . ?Q)
		      ("YEARLY" . ?Y)
		      ("RESEARCH" . ?R)
		      ("PROJECT" . ?P)))


;; Pomodoro time tracking in org
;; http://orgmode.org/worg/org-gtd-etc.html
;; http://thread.gmane.org/gmane.emacs.orgmode/29347
;(defun my-after-load-org ()
;	(add-to-list 'org-modules 'org-timer))
;(eval-after-load "org" '(my-after-load-org))

(setq org-timer-default-timer 25)

(add-hook 'org-clock-in-hook
	  '(lambda () (if (not org-timer-current-timer)
		     (org-timer-set-timer '(16)))))
