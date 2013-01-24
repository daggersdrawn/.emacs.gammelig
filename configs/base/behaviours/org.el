;; hide the initial stars. they're distracting
(setq org-hide-leading-stars t)

;; org folder (if you keep your org files in Dropbox: ln -s ~/Dropbox/org ~/org)
(setq org-directory "~/org")

;; Use a keybinding of "C-c c" for making quick inbox entries from any buffer.
(setq org-default-notes-file (concat org-directory "/gtd-inbox.org"))

;; note at beginning of file by default.
(setq org-reverse-note-order t)

;; save all org files every minute
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Follow links on enter
(setq org-return-follows-link t)

;; Set keywords and agenda commands
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(s!)")
        (sequence "|" "CANCELED(c@/!)")))
;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "red" :weight bold)
;;               ("NEXT" :foreground "blue" :weight bold)
;;               ("DONE" :foreground "forest green" :weight bold)
;;               ("WAITING" :foreground "yellow" :weight bold)
;;               ("SOMEDAY" :foreground "goldenrod" :weight bold)
;;               ("CANCELED" :foreground "orangered" :weight bold))))

;; Hide the leading stars so that we aren't seeing stars.
(setq org-hide-leading-stars t)

;; Do NOT put empty lines between collapsed trees
(setq org-cycle-separator-lines 0)

;; When should org leave a blank line before an item?
(setq org-blank-before-new-entry (quote ((heading)
  (plain-list-item))))

;; ask me for a note when I mark something as done
(setq org-log-done 'note)

;;  record a timestamp will each time the item is marked done.
(setq org-log-repeat "time")

;; work on the region if the region is active.
(transient-mark-mode 1)

;; Files for agenda and syncing?
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

;; smart goto beginning end of line
(setf org-special-ctrl-a/e t)

;; provide the jump-to-file functions
(defun gtd-visions ()
  (interactive)
  (find-file (concat org-directory "/gtd-visions.org")))
(provide 'org-gtd-visions)

(defun gtd-projects ()
  (interactive)
  (find-file (concat org-directory "/gtd-projects.org")))
(provide 'org-gtd-projects)

(defun gtd-inbox ()
  (interactive)
  (find-file (concat org-directory "/gtd-inbox.org")))
(provide 'org-gtd-inbox)

(defun reference ()
  (interactive)
  (find-file (concat org-directory "/reference.org")))
(provide 'org-reference)

(defun morningpages ()
  (interactive)
  (find-file (concat org-directory "/morningpages.org")))
(provide 'org-morningpages)

;; Speed commands are used when on the * of a given headline. If
;; these are forgotten, just press '?' as a speed-command to bring up
;; the cheat-sheet.
(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
("1" . delete-other-windows)
("2" . split-window-vertically)
("3" . split-window-horizontally)
("h" . hide-other)
("R" . org-reveal)
("s" . org-save-all-org-buffers)
("z" . org-add-note)
("N" . org-narrow-to-subtree)
("W" . widen))))

;; Manage the global tag list
(setq org-tag-alist '(("PROJECT" . ?p)))

;; Set the agenda deadline window
(setq org-deadline-warning-days 14)

;; Enable org-habit
(defun base-after-load-org ()
	(add-to-list 'org-modules 'org-habit))
(eval-after-load "org" '(base-after-load-org))

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))
