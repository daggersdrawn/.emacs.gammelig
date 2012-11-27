(require 'buffer-move)
(defun win-swap () "Swap windows using buffer-move.el" (interactive)
  (if (null (windmove-find-other-window 'right)) (buf-move-left) (buf-move-right)))
