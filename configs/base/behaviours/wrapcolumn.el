(defvar wrap-column-width 100)

(defun wc/narrow-window ()
  (let (( new-right
          (max 0 (+ (or (cdr (window-margins)) 0)
                    (- (window-body-width) wrap-column-width)))))
    (set-window-margins nil nil new-right)
    (set-window-fringes nil (car (window-fringes)) 2)))

(define-minor-mode wrap-column-mode
  "Adjust the right margin, so that the editing area never exceeds\
 `wrap-column-width' characters."
  nil nil nil
  (if wrap-column-mode
      (progn
        (visual-line-mode 1)
        (add-hook 'window-configuration-change-hook
                  'wc/narraow-window nil t)
        (wc/narrow-window))
    (progn
      (remove-hook 'window-configuration-change-hook
                   'wc/narrow-window t)
      (set-window-margins nil nil nil)
      (set-window-fringes nil
                          (car (window-fringes))
                          (car (window-fringes))))))

(provide 'wrap-column)
