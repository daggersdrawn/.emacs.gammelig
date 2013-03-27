;; http://ixmat.us/articles/2012-12-01_usable-org-drill.html

;; Format the interactive string with the index number of the dir in the list
(defun index-ident (item lst)
    (concat (format "[%s] " (number-to-string (position item lst))) item))

;; Get all dirs/files matching regex (should be expanded for more complicated naming schemes)
(defun get-subs (root path)
    (directory-files root path "[a-zA-Z0-9-_]"))

;; Build all of the formatted names into a string
(defun build-subjs (source)
    (let* ((subjs (get-subs source nil))
           (subfmt (mapconcat (function (lambda (x) (index-ident x subjs))) subjs "; ")))
      (concat subfmt ": ")))

;; Build an absolute path using the root dir, the item index, and the given dir/file list
(defun build-subj-dir (root item dirs)
    (concat root (nth (string-to-number item) dirs) "/"))

;; Return t if the given path contains *only* files; f if it has directories in it
(defun just-files (source)
    (defun check-list-files (ls acc)
        (cond ((not ls) acc)
              ((file-directory-p (car ls)) (check-list-files (cdr ls) acc))
              (t
                (check-list-files (cdr ls) t))))

    (check-list-files (get-subs source t) nil))

;; Heres the recursive and interactive subject selection function
(defun select-drill-subject (source)
    "Select a drill subject by index"
    (let* ((drs (get-subs source nil))
           (fmt (build-subjs source))
           (sel (read-string fmt))
           (val (build-subj-dir source sel drs)))

      (cond ((just-files val) (org-drill (get-subs val t)))
            (t
              (select-drill-subject val)))))

;; Use f12 to run org-drill
(global-set-key [f12] (lambda () (interactive) (select-drill-subject "~/org/drill/")))
