;; lusty
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; Move between buffers
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'previous-buffer)

;; redo+
(global-set-key (kbd "C-?") 'redo)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; pyflakes
(global-set-key (kbd "M-m") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)

;; anything
;(global-set-key (kbd "M-SPC") 'anything)

;; buffer-move
(global-set-key (kbd "C-x 4") 'win-swap)

;; kill all other buffers
(global-set-key (kbd "C-x C-k") 'kill-other-buffers)

;; iresize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Margin indent
(global-set-key (kbd "M-]") 'increase-left-margin)
(global-set-key (kbd "M-[") 'decrease-left-margin)

;; HideShow
(global-set-key (kbd "M-+") 'toggle-hiding-all)
(global-set-key (kbd "M-=") 'toggle-hiding-block)

;; Go to file
(global-set-key (kbd "C-c f") 'find-file-in-git-repo)

;; editing
(global-set-key (kbd "M-?") 'comment-or-uncomment-region)
