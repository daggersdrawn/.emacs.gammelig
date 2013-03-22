;; Modification of Steve Yegge's function to rename a file that you're
;; editing along with its corresponding buffer

(defun unique-name-for-buffer-p (new-name)
  (let* ((file-name (buffer-file-name))
         (dir-name (file-name-directory buffer-file-name))
         (new-complete-name (concat dir-name new-name)))
    (not (string-equal file-name new-complete-name))))

(defun yegge-rename (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
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
