;; MobileOrg + Dropbox + org-mode
;; Don't forget to create your ~/Dropbox/MobileOrg folder!

;; Set to the name of the file where unfiled actions will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Custom agenda view
(setq org-mobile-force-id-on-agenda-items nil)

;; Push and pull from MobileOrg on open/close of emacs
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

;; Push and pull from MobileOrg when away from computer
(defvar my-org-mobile-sync-timer nil)

(defvar my-org-mobile-sync-secs (* 60 20))

(defun my-org-mobile-sync-pull-and-push ()
  (org-mobile-pull)
  (org-mobile-push)
  (when (fboundp 'sauron-add-event)
    (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

(defun my-org-mobile-sync-start ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq my-org-mobile-sync-timer
        (run-with-idle-timer my-org-mobile-sync-secs t
                             'my-org-mobile-sync-pull-and-push)))

(defun my-org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer my-org-mobile-sync-timer))

(my-org-mobile-sync-start)
