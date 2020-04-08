;;; org-tree-capture.el --- capture to logical trees

;; Copyright (C) 2020 Nathanael Gentry <nathanael.gentrydb8@gmail.com>

;; Licensed under the same terms as Emacs and under the MIT license.

;;; Code:

(require 'org-tree)

(push '(org-capture-set-target-location :around org-tree-capture-set-target-location)  org-tree-advices-map)

(defun org-tree-capture-set-target-location (func &optional target)
  "Add three additional org-capture target location types:

        (olp \"org-tree/outline/path\")

        (olp+attach base-target-type \"org-tree/outline/path\" \"path/to/attachment\")

        (olp+subtree \"org-tree/outline/path\" \"node headline\" \"subtree file name\")

For more information on target location types, see `org-capture-templates'."
  (let ((args (org-capture-get :target)))
    (pcase (or target args)
      (`(olp ,outline-path)
       (let ((m (car (org-tree-find-olp outline-path))))
         (setq args '(function (lambda ())))
         (set-buffer (marker-buffer m))
         (org-capture-put-target-region-and-position)
         (widen)
         (goto-char m)
         (set-marker m nil)))
      (`(olp+attach ,type ,outline-path ,attach)
       (setq args `(,type ,(org-tree-resolve-attachment-path outline-path attach))))
      (`(olp+subtree ,outline-path ,headline ,subtree)
       (flet ((org-capture-select-template (&optional keys)
              `(" " " " entry (olp ,outline-path) ,headline :immediate-finish t)))
         (let* ((org-capture-plist (let (org-capture-plist)
                  (org-capture)
                  org-capture-plist))
                (buf (org-capture-get :buffer))
                (loc (org-capture-get :insertion-point)))
           (with-current-buffer buf
             (goto-char loc)
             (setq args `(file ,(let ((org-tree-info `(:subtree ,subtree)))
                                  (org-meta-return '(16))))))))))
    (funcall func args)))

(provide 'org-tree-capture)

;;; org-tree-capture.el ends here
