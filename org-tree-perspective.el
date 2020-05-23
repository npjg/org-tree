;;; org-tree-perspective.el --- link window configuration to subtree

;; Copyright (C) 2020 Nathanael Gentry <nathanael.gentrydb8@gmail.com>

;; Licensed under the same terms as Emacs and under the MIT license.

;;; Code:

(require 'org-tree)
(require 'perspective)

(defvar org-tree-persp-subtree nil
  "The perspective-local variable that holds information on the
  currently active project.")

(defvar org-tree-active-persp-formatter #'org-format-outline-path
  "The function used to format active subtree perspectives in
`persp-switch' and `persp-kill'. It will be called with one
argument, the outline path to format in list form, and it should
return the formatted path string.")

(defface org-tree-persp-active-perspectives '((t
                                               :weight bold
                                               :underline t))
  "The face used for outline paths that have currently active perspectives.")

(set-face-attribute 'org-tree-persp-active-perspectives nil
                    :weight 'bold
                    :underline  t)

(defvar org-tree-persp-attachment-open-function #'org-attach-open
  "The function used to open attachments.")

(defvar org-tree-persp-path-list-sorted t
  "Whether the modified `persp-switch' prompt shows the active paths at the top.")

(defun org-tree-persp-switch-maybe (name &optional force)
  "Switch to perspective NAME if this perspective does not
already exist.

With FORCE, kill an existing perspective with the
same name and replace it with a new one."
  (when (gethash name (perspectives-hash))
    (when force (persp-kill name))
    (message "Switching to existing perspective")
    (persp-switch name)))

(defun org-tree-persp-raise-subtree-buffer ()
  "In the current window, open the subtree belonging to the
current org-perspective context, or throw an error if we aren't
in such a perspective."
  (interactive)
  (unless (bound-and-true-p org-tree-persp-subtree)
    (user-error "Not in an org-perspective context"))
  (let ((buf (org-find-base-buffer-visiting org-tree-persp-subtree)))
    (if buf (progn (persp-add-buffer buf) (switch-to-buffer buf)) (find-file
    org-tree-persp-subtree))))

(defun org-tree-persp-names ()
  "Return all possible perspectives, with currently active ones
marked with the deisgnated face."
  (funcall (if org-tree-persp-path-list-sorted
               ;; dumb sort by text properties
               (lambda (l) (sort l (lambda (e m)
                           (and (plist-get (text-properties-at 0 e) 'active)
                                (not (plist-get (text-properties-at 0 m) 'active))))))
             #'identity)
           (let ((real-persps (apply func nil)))
             (mapcar (lambda (l)
                       (if (member (cdr l) real-persps)
                           (propertize (funcall org-tree-active-persp-formatter
                                    (org-tree-path-list (cdr l))
                                    nil ""
                                    org-tree-path-separator)
                                    'active t)
                         (cdr l)))
                     org-tree-lookup-table))))

(defun org-tree-persp-names-fontify ()
  "Apply active formatting to all the paths in `persp-names'."
  (mapcar (lambda (l)
            (funcall org-tree-active-persp-formatter
                     (org-tree-path-list l) nil "" org-tree-path-separator))
          (apply func nil)))

(defun org-tree-persp-switch (func &rest args)
  "Switch to the perspective at outline path PATH, with ARGS
passed to `persp-switch'."
  (let* ((info (org-tree-reverse-lookup
                (or (car args)
                    (let ((func (symbol-function #'persp-names)))
                      (cl-letf (((symbol-function #'persp-names)
                                 (symbol-function #'org-tree-persp-names)))
                        (persp-prompt)))))))
    (unless (and (gethash (caadr info) (perspectives-hash))
                 (funcall func (caadr info)))
      (apply func (caadr info) args)
      (when (and (caar info) (file-exists-p (caar info)))
        ;; (message "%s" (caar info))
        (persp-make-variable-persp-local 'org-tree-persp-subtree)
        (setq org-tree-persp-subtree (caar info))
        ;; make the default scratch buffer point to the perspective root
        (with-current-buffer (car (persp-buffers (persp-curr)))
          (cd (expand-file-name (or (car (cdddar info)) "")
                                    (car (cddar info))))
        (org-tree-persp-raise-subtree-buffer)
        (caadr info))))))

(defun org-tree-persp-launch ()
	"Launch a perspective from an org mode buffer."
	(interactive)
	(let ((info (org-tree-resolve-subtree-file-name)))
		(if info
				(persp-switch (org-tree-path-string (org-get-outline-path t))))
		;; TODO: Finish recognizing other cases, like just opening attachments!
	))

(defun org-tree-persp-kill (funct &rest args)
  (let ((func (symbol-function #'persp-names)))
    (cl-letf (((symbol-function #'persp-names)
               (symbol-function #'org-tree-persp-names-fontify)))
      (call-interactively funct))))

(push '(persp-switch :around org-tree-persp-switch) org-tree-advices-map)
(push '(persp-kill :around org-tree-persp-kill) org-tree-advices-map)

(provide 'org-tree-perspective)

;;; org-tree-perspective.el ends here
