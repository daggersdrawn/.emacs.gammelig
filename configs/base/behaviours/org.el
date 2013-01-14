;; hide the initial stars. they're distracting
(setq org-hide-leading-stars t)

;; org folder
(setq org-directory "~/Dropbox/org")

;; Use a keybinding of "C-c c" for making quick notes from any buffer.
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; save all org files every minute
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Follow links on enter
(setq org-return-follows-link t)

;; Set keywords and agenda commands
(setq org-todo-keywords
      '((type "TODO" "NEXT" "WAITING" "DONE")))

;; ask me for a note when I mark something as done
(setq org-log-done 'note)

;; Files for agenda and syncing?
(setq org-agenda-files (file-expand-wildcards "~/org/gtd*.org"))
