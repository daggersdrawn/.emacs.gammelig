;; view keybindings cheatsheet
(defun keybindings ()
   (interactive)
   (find-file "~/.emacs.d/keybindings.org"))
(global-set-key (kbd "C-c k") 'keybindings)

;; fx keys
;(global-set-key  [f1]  (lambda () (interactive) (manual-entry (current-word))))
;(global-set-key  [f2]  (lambda () (interactive) (find-file "~/org/gtd-inbox.org")))
;(global-set-key "\M-\C-y" 'kill-ring-search)
(global-set-key (kbd "C-<f3>") (function
  (lambda nil (interactive)
  (joc-dired-magic-buffer default-directory))))
(global-set-key (kbd "S-<f3>") (function
  (lambda nil (interactive)
  (message "Current directory is: %s" default-directory))))
(global-set-key (kbd "S-<f3>") 'joc-dired-toggle-buffer-name)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c v") 'gtd-visions)
(global-set-key (kbd "C-c g") 'gtd-projects)
(global-set-key (kbd "C-c i") 'gtd-inbox)
(global-set-key (kbd "C-c m") 'morningpages)

;; lusty
;(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
;(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; Move between buffers
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'previous-buffer)

;; occur
(global-set-key (kbd "M-o") 'occur)

;; imenu
(global-set-key (kbd "M-i") 'imenu)

;; redo+
(global-set-key (kbd "C-?") 'redo)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; helm
(global-set-key (kbd "M-SPC") 'helm-mini)

;; buffer-move
(global-set-key (kbd "C-x 4") 'win-swap)

;; kill all other buffers
(global-set-key (kbd "C-x C-k") 'kill-other-buffers)

;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Margin indent
(global-set-key (kbd "M-]") 'increase-left-margin)
(global-set-key (kbd "M-[") 'decrease-left-margin)

;; HideShow code folding
(global-set-key (kbd "M-+") 'toggle-hiding-all)
(global-set-key (kbd "M-=") 'toggle-hiding-block)

;; Go to file
(global-set-key (kbd "C-c f") 'find-file-in-git-repo)

;; editing
(global-set-key (kbd "M-?") 'comment-or-uncomment-region)

;; Toggle soft word wrapping
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; Jump to the start/end of the document with C-PgUP/DN
(global-set-key [C-prior] (lambda () (interactive) (goto-char (point-min))))
(global-set-key [C-next]  (lambda () (interactive) (goto-char (point-max))))

;; Quicker access to go-to line
(global-set-key (kbd "M-g") 'goto-line)

;; Menu bar toggle, as in my vimperator setup
(global-set-key (kbd "<M-down>") 'menu-bar-mode)

;; Unfill paragraph at or after point.
(defun unfill-paragraph ()
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))
(global-set-key (kbd "M-Q") 'unfill-paragrah)

;; kill ring search
(global-set-key (kbd "C-c y") 'kill-ring-search)

;; multiple cursors
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
