;;; org-tree-refile.el --- refile to logical trees

;; Copyright (C) 2020 Nathanael Gentry <nathanael.gentrydb8@gmail.com>

;; Licensed under the same terms as Emacs and under the MIT license.

;;; Code:

(require 'org-tree)

(push '(org-refile-get-targets :around org-tree-refile-get-targets) org-tree-advices-map)
(push '(org-refile :around org-tree-refile) org-tree-advices-map)

(defun org-tree-refile (func &rest args)
  "Refiling for `org-tree' parses the tree as deeply as directed
by `org-tree-refile-max-level', and it does not parse subtrees
for which the TREE_SKIP property contains the string \"agenda
\"."
  (let ((heading-func (symbol-function 'org-back-to-heading))
        (subtree-func (symbol-function 'org-end-of-subtree)))
    (cl-letf (((symbol-function 'org-back-to-heading)
               (lambda (&optional invisible-ok)
                 (ignore-errors (funcall heading-func invisible-ok))))
             ((symbol-function 'org-end-of-subtree)
              (lambda (&optional invisible-ok to-heading)
                (ignore-errors (funcall subtree-func invisible-ok to-heading)))))
      (apply func args))))

(defun org-tree-refile-get-targets (func &rest args)
  (save-excursion
  (let ((org-refile-targets (list (cons nil (cons :maxlevel 3))))
       ;; (max-lisp-eval-depth org-tree-refile-max-level)
        (result nil))
    (apply #'org-tree-refile-get-targets-1 0 func args)
    (reverse result))))

(defun org-tree-refile-get-targets-1 (depth func &rest args)
  (mapc (lambda (elt)
     ;; must work around nasty quoting bug
     (let* ((olps (org-tree-find-olp (org-tree-path-string
             (with-current-buffer (org-get-agenda-file-buffer (cadr elt))
                                        (goto-char (car (last elt)))
                                        (org-get-outline-path t)))))
            (subtree (org-with-point-at (car olps)
                       (org-tree-resolve-subtree-file-name))))
       (if (and subtree (equal (file-name-extension subtree) "org"))
         ;; adjust the pointer here
         (progn (setf (cadr elt) subtree
               (cddr elt)
               (list ".*" (marker-position (or (cdr olps) (car olps)))))
         ;; inject the children here
         (add-to-list 'result elt)
         (unless (or (org-tree-entry-member-in-multivalued-property
                       nil "TREE_SKIP" "refile" :inherit)
                     (> depth org-tree-refile-max-level)
                     (> org-tree-refile-max-total-level (org-tree-outline-level)))
           (funcall #'org-tree-refile-get-targets-1
                    (1+ depth) func (org-get-agenda-file-buffer subtree))))
         (add-to-list 'result elt))))
        (apply func args)))

(provide 'org-tree-refile)

;;; org-tree-refile.el ends here
