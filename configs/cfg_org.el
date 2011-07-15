;; MobileOrg + Dropbox + org-mode
;; Don't forget to create your ~/Dropbox/MobileOrg folder!
;;
;; Example of GTD task:
;; ** TODO [#A] Buy milk :personal:
;;    DEADLINE: <2011-03-11 Fri>
;; Where:
;;    [#A] => Priority from 'A' to 'D'
;;    :personal: => Tag
;; See also:
;; http://thread.gmane.org/gmane.emacs.orgmode/4832/focus=4854

;; keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c g") 'gtd)

;; Files for syncing
(setq org-agenda-files
    (list "~/org/gtd.org" "~/org/someday.org"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Custom agenda view
(setq org-mobile-force-id-on-agenda-items nil)

;; return activates a link
(setq org-return-follows-link t)

;; Set keywords and agenda commands
(setq org-todo-keywords
      '((type "TODO" "NEXT" "WAITING" "DONE")))
(setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil)
      ("n" todo "NEXT" nil)
      ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
)

;; Use org's tag feature to implement contexts.
(setq org-tag-alist '(("STUDIO" . ?s)
                      ("COMPUTER" . ?c)
                      ("MAIL" . ?m)
                      ("HOME" . ?h)
                      ("FIELD" . ?f)
                      ("READING" . ?r)
                      ("DVD" . ?d)))

;; Use color-coded task types.
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
        ("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("DONE" . (:foregorund "green" :weight bold))
        ("CANCEL" . (:foreground "blue" :weight bold))))

;; Put the archive in a separate file, because the gtd file will
;; probably already get pretty big just with current tasks.
(setq org-archive-location "%s_archive::")

;; Remember support. This creates several files:
;;
;;   ~/org/todo.org      Where remembered TODO's are stored.
;;   ~/org/journal.org   Timestamped journal entries.
;;   ~/org/remember.org  All other notes

;; and a keybinding of "C-c r" for making quick notes from any buffer.

;; These bits of Remembered information must eventually be reviewed
;; and filed somewhere (perhaps in gtd.org, or in a project-specific
;; org file.) The out-of-sight, out-of-mind rule applies here---if I
;; don't review these auxiliary org-files, I'll probably forget what's
;; in them.

(setq org-reverse-note-order t)  ;; note at beginning of file by default.
(setq org-default-notes-file "~/org/remember.org")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
      '((?t "* TODO %?\n  %i\n  %a" "~/todo.org")
        (?j "* %U %?\n\n  %i\n  %a" "~/journal.org")
        (?i "* %^{Title}\n  %i\n  %a" "~/remember.org" "New Ideas")))

;; My preferences. These are less related to GTD, and more to my
;; particular setup. They are included here for completeness, and so
;; that new org users can see a complete example org-gtd
;; configuration.

(setq org-return-follows-link t)
(setq org-hide-leading-stars t)
(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)

(setq org-log-done t)
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t)
(setq org-fontify-done-headline t)
(setq org-agenda-include-all-todo nil)
(setq org-directory "~/org")
(setq org-export-html-style "<link rel=stylesheet href=\"../e/freeshell2.css\" type=\"text/css\">")
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-adapt-indentation nil)

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")

;; provide the gtd function
(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org"))
(provide 'org-gtd)
;;; org-gtd.el ends here
