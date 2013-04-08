;;; yegge-rename.el -- Steve Yegge's rename buffer function
;;
;;; Commentary:
;;
;; Modification of Steve Yegge's function to rename a file that you're
;; editing along with its corresponding buffer.
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;
;;; Code:


(defun unique-name-for-buffer-p (new-name)
  "Make sure a buffer being renamed enjoys a unique NEW-NAME."
  (let* ((file-name (buffer-file-name))
         (dir-name (file-name-directory buffer-file-name))
         (new-complete-name (concat dir-name new-name)))
    (not (string-equal file-name new-complete-name))))


(defun yegge-rename (new-name)
  "Renames both current buffer and the file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name))
        (dir-name (file-name-directory (buffer-file-name))))
     (cond ((not filename)
            (message "Buffer '%s' is not visiting a file!" name))
           ((not (unique-name-for-buffer-p new-name))
            (message "A buffer named '%s' already exists in that location!" new-name))
           (t (rename-file (file-name-nondirectory filename) new-name 1)
              (kill-buffer)
              (set-buffer (find-file (concat dir-name new-name)))))))


(provide 'yegge-rename)

;;; yegge-rename.el ends here
