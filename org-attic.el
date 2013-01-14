;; Dropbox + org-mode
;;
;; Example of GTD task:
;; ** TODO [#A] Buy milk :personal:
;;    DEADLINE: <2011-03-11 Fri>
;; Where:
;;    [#A] => Priority from 'A' to 'D'
;;    :personal: => Tag
;; See also:
;; http://thread.gmane.org/gmane.emacs.orgmode/4832/focus=4854

;; Use org's tag feature to implement contexts.
(setq org-tag-alist '(("STUDIO" . ?s)
                      ("COMPUTER" . ?c)
                      ("MAIL" . ?m)
                      ("HOME" . ?h)
                      ("FIELD" . ?f)
                      ("READING" . ?r)
                      ("DVD" . ?d)))

;; Create several files:
;;
;;   (concat org-directory "/gtd-life.org")       Where remembered TODO's are stored.
;;   (concat org-directory "/morningpages.org")   Timestamped morningpages entries.
;;   (concat org-directory "/notes.org")  All other notes
(setq org-reverse-note-order t)  ;; note at beginning of file by default.

;; Customizations: *work in progress*. The rest is less related to GTD, and more to my
;; particular setup. They are included here for completeness, and so
;; that new org users can see a complete example org-gtd
;; configuration.

;;
;; CUSTOM AGENDAS
;;
(setq org-agenda-custom-commands
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("n" "Notes" tags "NOTES" nil))))

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@Errand" . ?e)
                            ("@Work" . ?w)
                            ("@Home" . ?h)
                            ("@Phone" . ?p)
                            ("@Mind" . ?m)
                            ("@Studio" . ?s)
                            (:endgroup)
                            ("NEXT" . ?N)
                            ("PROJECT" . ?P)
                            ("WAITING" . ?W)
                            ("HOME" . ?H)
                            ("ORG" . ?O)
                            ("PLAY" . ?p)
                            ("R&D" . ?r)
                            ("MIND" . ?m)
                            ("STUDIO" . ?S)
                            ("CANCELLED" . ?C))))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

;; Remove Tasks With Dates From The Global Todo Lists
;;
;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)

(setq org-log-done t)
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t)
(setq org-fontify-done-headline t)
(setq org-agenda-include-all-todo nil)
(setq org-export-html-style "<link rel=stylesheet href=\"../e/freeshell2.css\" type=\"text/css\">")
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-adapt-indentation nil)

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")
