;; Check for the existence of a function with mac- prefix
(when (symbolp 'mac-control-modifier)
  ;; Re-map modifiers
  (setq mac-control-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super)
  )


;; Bind "Emacs Mac port" keys the same as Emacs NS/Cocoa
(when (symbolp 'mac-control-modifier)

  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-x") 'cua-cut-region)
  (global-set-key (kbd "s-c") 'cua-copy-region)
  (global-set-key (kbd "s-v") 'cua-paste)
  (global-set-key (kbd "s-w") 'delete-frame)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-'") 'switch-window)

;;
;;  (global-set-key (kbd "<home>") 'beginning-of-buffer)
;;  (global-set-key (kbd "<end>") 'end-of-buffer)

  ;; Navigating around frames, windows & buffers
  (global-set-key (kbd "C-`") 'switch-window)
  (global-set-key (kbd "C-~") 'other-frame)

)
