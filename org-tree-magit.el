;;; org-tree-magit.el --- create repositories in logical trees

;; Copyright (C) 2020 Nathanael Gentry <nathanael.gentrydb8@gmail.com>

;; Licensed under the same terms as Emacs and under the MIT license.

;;; Code:

(require 'org-tree)

(defun org-tree-magit-clone (&optional headline)
  "Create a new subtree and clone repository to its attachment
directory.

Unless optional argument HEADLINE is provided, set the subtree
headline to the name of the remote repository. The PROJECT
property is set to the name of the local directory chosen in
`magit-clone-read-args' to hold the repository."
  (interactive)
  (let* ((org-tree-info `(:headline ,(or headline "")))
         (default-directory (progn (org-meta-return '(16)) (org-attach-dir)))
         (args (magit-clone-read-args)))
    (unless headline (insert (file-name-nondirectory (car args))))
    (apply #'magit-clone-internal args)
    (org-tree-encode-subtree-project-directory
     (file-name-nondirectory (cadr args)))))

(defun org-tree-magit-init ()
  "Create a new subtree and initialize a repository in its
attachment directory. The repository subdirectory is given, by
default, by the subtree headline."
  (interactive)
    (let* ((default-directory (progn (org-meta-return '(16)) (org-attach-dir)))
           (buf (current-buffer))
           (headline (plist-get (org-tree-headline-parser) :raw-value))
           (project (file-name-as-directory
                     (expand-file-name
                      (read-directory-name
                       "Create repository in: "
                       (expand-file-name headline))))))
      (magit-init project)
      (with-current-buffer buf
        (org-tree-synchronize-headline-title)
        (org-tree-encode-subtree-project-directory project))))

(provide 'org-tree-magit)

;;; org-tree-magit.el ends here
