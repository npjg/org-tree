;;; org-tree.el --- make a semantic filesystem from your org files

;; Copyright (C) 2020 Nathanael Gentry <ngentry1@liberty.edu>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <ngentry1@liberty.edu>
;; URL: https://github.com/npjg/org-tree
;; Created: 19-01-2020
;; By: Nathanael Gentry <ngentry1@liberty.edu>
;; Keywords: semantic, workspace, org

;;; Commentary:

;; This package provides a middle road between keeping one huge org file and
;; many separate ones. org-tree.el, through its included `org-tree-mode',
;; overloads the term `subtree' to mean, in addition to its normal import, a
;; headline with an ID and a `SUBTREE' property. These uniquely identify an org
;; file whose contents appear to org-mode as a physical subtree under the
;; headline.

;;; Code:


(require 'org)
(require 'cl)
(require 'org-attach)

(defgroup org-tree 'nil
  "Customization group for org-tree"
  :group 'org)

(defconst org-tree-path-separator "/"
  "The separator string with which to delineate path components.")

(defconst org-tree-agenda-exclude-subtree-prop "AGENDA_EXCLUDE_SUBTREE"
  "The property value used to define the subtree excluder.")

(defvar org-tree-loose-text-post "\n\n"
  "The text inserted after loose text is injected into a subtree file.")

(defvar org-tree-refile-max-level 9
  "The total subtree outline level considered in generating refile targets.

Note that this restriction is supersed by the per-file maxlevel set in `org-refile-targets'.")

(defconst org-tree-format-spec '(?i id ?h headline)
  "An alist of the special formatting options available to
  org-tree subtree file names. Using
  `org-tree-dynamic-format-spec', each of the symbols referenced
  here will be dynamically substituted for the dynamically-scoped
  named symbol.")

(defcustom org-tree-default-subtree-file-name "%i.org"
  "A format string that specifies the default fulename for
subtree files, including the org extension.")

(defcustom org-tree-default-subtree-template "#+TITLE: %h"
  "A format string that specifies the default template for
  subtree files, including the org extension.")

(defcustom org-tree-root nil
  "The org file that holds root-level subtrees."
    :type '(file :must-match t)
    :group 'org-tree)

(defvar org-tree-lookup-table nil
  "An alist mapping subtree file names to logical paths")

(defsubst org-tree-safe-format (item spec)
  "Applies `format-spec' to ITEM if it is a string; otherwise,
return the argument as it is."
  (if (and spec (stringp item)) (format-spec item spec) item))

(defun org-tree-format-spec ()
  (apply #'format-spec-make (org-tree-format-spec-1 org-tree-format-spec)))

(defun org-tree-format-spec-1 (spec &optional new-spec)
  "Replace each symbol name in SPEC with its value in the current
dynamic scope, or nil otherwise."
  (if spec (progn (setq new-spec
    (append new-spec (list (car spec) (ignore-errors (symbol-value (cadr spec))))))
                  (org-tree-format-spec-1 (cddr spec) new-spec))
    new-spec))

(defun org-tree-format (item)
  "Format ITEM according to `org-tree-format-spec', substituting
  in dynamically bound variables."
  (when (stringp item) (format-spec item (org-tree-format-spec))))

(defun org-tree-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun org-tree-split-file-name (file &optional always-parse-as-file)
  "Take a file name FILE, and returns a list whose `car' is a
relative file name and whose `cdr' is a directory name.  If FILE
is actually a directory name, the `cdr' will be the empty string;
set `always-parse-as-file' true to convert FILE to a directory
file name before parsing it.  View this function as the inverse of
`expand-file-name'."
  (let* ((fixed
          (if always-parse-as-file
              file
            (directory-file-name file)))
         (parent (file-name-directory fixed)))
    (cons parent (directory-file-name (string-remove-prefix (or parent "") file)))))

(defun true-listp (object)
  "Return non-`nil' if OBJECT is a true list."
  (and (listp object)  (null (cdr (last object)))))

(defun org-tree-flatten (list)
  "Nondestructively return a flat version of LIST."
  (cond
   ((null list) nil)
   ((atom list) (list list))
   ((not (true-listp list)) (list list))
   (t (append (org-tree-flatten (car list)) (org-tree-flatten (cdr list))))))

(defun org-tree-outline-level (&optional offset)
  (+ (org-outline-level) (- (length (org-get-outline-path)) (or offset 0))))

(defun org-tree-path-list (path)
  "If PATH is not already a path list, convert it to a list by
splitting PATH as a string at each `org-tree-path-separator'. If PATH is
already a list, return it as it is."
  (if (listp path) path
    (split-string path (format "[%s]+" org-tree-path-separator) t)))

(defun org-tree-path-string (path &optional relative)
  "Concatenate the given PATH list into a string, beginning with
a path separator. If PATH is already a string, return it as it is.

With RELATIVE, do not start the path with an `org-tree-path-separator'."
  (if (stringp path) path
    (mapconcat 'identity (append (unless relative (list "")) path) org-tree-path-separator)))

(defun org-tree-resolve-subtree-file-name (&optional pom)
  "Provide the full file name for the org-tree subtree at POM, or
return nil if a subtree does not exist there. This function
does not verify that the subtree is actually an org document;
it can be any sort of document you wish, which allows easy
overloading for other terminal points of your org project
tree (say, a LaTeX document).

An org-tree subtree is defined by the presence of an ID from
`org-id-get', an attachment directory from `org-attach-dir', and
an extant file in the attachment directory with name specified by
the SUBTREE property or, lacking that, by the format string
`org-tree-default-subtree-file-name'."
  (org-with-point-at (or pom (point-marker))
    (let ((subtree (org-entry-get nil "SUBTREE" nil))
          (id (org-id-get))
          (ad (org-attach-dir)))
      (if (equal subtree "") subtree
        (when (and id ad) (ignore-errors (file-truename
         (expand-file-name (or subtree
          (org-tree-format org-tree-default-subtree-file-name)) ad))))))))

(defun org-tree-lookup-table-1 (path subtree
                                     &optional in-progress agenda-exclude-subtrees)
  "Recursively build the internal lookup alist with initial path PATH
for the subtree file SUBTREE.

All subtree files not explicitly excluded will be addded to
`org-agenda-files'.  To exclude a subtree, include property
`AGENDA_EXCLUDE_SUBTREE'."
  (with-current-buffer (org-get-agenda-file-buffer subtree)
    (unless agenda-exclude-subtrees
      (add-to-list 'org-agenda-files (buffer-file-name)))
    (remove 'nil (org-map-entries
      (lambda ()
        (let ((subtree (org-tree-resolve-subtree-file-name)))
          (when subtree
            (let* ((path (if (or path (equal (file-truename (buffer-file-name))
                                             (file-truename org-tree-root)))
                         (append path (unless (org-before-first-heading-p)
                                        (ad-with-originals 'org-get-outline-path
                                          (org-get-outline-path t))))
                       (org-tree-outline-path nil t)))
                   (spath (org-tree-path-string path))
                   (subtree-exists (and (not (equal subtree ""))
                                        (equal (file-name-extension subtree) "org")
                                        (ignore-errors (file-exists-p subtree))))
                   (app (when subtree-exists (cons (list subtree (org-id-get)) spath)))
                   (rec (when subtree-exists
                          (org-tree-lookup-table-1 path subtree in-progress
                           (or agenda-exclude-subtrees
                               (org-entry-get-with-inheritance
                                org-tree-agenda-exclude-subtree-prop))))))
              (if (and subtree-exists rec)
                  (append (list app) rec)
                app)))))
      t 'file))))

(defun org-tree-register-agenda-subtree (action &optional pom)
  "Remove the given subtree file and its children from
  `org-agenda-files', and prevent the subtree from being treated
  as an agenda subtree again by setting `AGENDA_EXCLUDE_SUBTREE'.

This does not parse the subtree for deeper exclusion."
  (interactive)
  (org-with-point-at pom
    (let ((subtree (org-tree-resolve-subtree-file-name nil)))
      (cond ((eq action 'deregister)
             (when subtree (setq org-agenda-files (delete subtree org-agenda-files)))
             (if (org-entry-get-multivalued-property nil
                   org-tree-agenda-exclude-subtree-prop)
                 (message "Agenda exclusion already set for this subtree")
               (org-entry-put nil org-tree-agenda-exclude-subtree-prop "t")))
            ((eq action 'register)
             (when subtree (add-to-list 'org-agenda-files subtree))
             (org-entry-delete nil org-tree-agenda-exclude-subtree-prop))))))

(defun org-tree-lookup-table (&optional force)
  "Return the entry path table, calculating it if necessary. If FORCE,
recalculate both tables anew before returning the requested
table.

The lookup table is stored as an alist, where each association
has `car' as a list whose `car' is the physical path to the
subtree file and chose `cdr' is the subtree's ID, and has as
`cdr' the logical outline path of the subtree."
  (unless org-tree-root (user-error "Tree index cannot be nil"))
  (unless (and org-tree-lookup-table (not force))
    (let (org-agenda-new-buffers org-mode-hook)
      (setq org-tree-lookup-table
            (org-tree-flatten (org-tree-lookup-table-1 nil org-tree-root)))
      (org-release-buffers org-agenda-new-buffers)))
      org-tree-lookup-table)

(defun org-tree-lookup (path)
  "Get the outline path corresponding to the subtree at path
PATH. This function does not interpolate paths from roots;
it directly maks from `org-tree-lookup-table'. Note that the root
path has an ID of nil, and its path is `org-tree-root'."
  (let ((path (org-tree-path-string path)))
    (cond ((equal path org-tree-root) "/")
          (t (cdr (assoc path (org-tree-lookup-table)
                         (lambda (o1 o2) (equal (car o1) o2))))))))

(defun org-tree-reverse-lookup (path &optional lax)
  "With logical subtree path PATH, return the entry of
`org-tree-lookup-table' that defines the exact subtree. With LAX,
return the highest subtree for which a match is found."
  (let ((rassoc (cl-rassoc (org-tree-path-string path) (org-tree-lookup-table)
                           :test (lambda (a b)
                                   (funcall (if lax #'string-prefix-p #'equal) b a)))))
    (cond  (rassoc (list (car rassoc)
            (append (list (cdr rassoc))
                    (or (ignore-errors
                     (substring (string-remove-prefix (cdr rassoc) path) 1)) ""))))
           (t (list (list org-tree-root nil)
                    (cons org-tree-path-separator (substring path 1)))))))

(defun org-tree-find-olp (path)
  "Return a marker to the place where PATH is defined."
  (let* ((info (org-tree-reverse-lookup (org-tree-path-string path) :lax))
        (path (org-tree-path-list (cdadr info))))
    (cond ((and (equal (cdadr info) "") path) (org-id-find (cadar info) t))
          (path (org-find-olp (append (list (caar info)) path)))
          (t (set-marker (make-marker) 0 (org-get-agenda-file-buffer (caar info)))))))

(defun org-tree-resolve-attachment-path (path attachment)
  "Return the full physical path to attachment ATTACHMENT of the
subtree at outline path PATH. A valid attachment directory,
returned by `org-attach-dir' is required to to properly expand
the file name Note that ATTACHMENT need not exist; it must just
be a file name."
  (let* ((ad (org-with-point-at
                 (org-id-find (or (cadar (org-tree-reverse-lookup
                                          (org-tree-path-string path)))
                                  (user-error "Subtree not found")) t)
                              (org-attach-dir))))
    (when ad (expand-file-name attachment ad))))

(defun org-tree-outline-path (&optional func with-self use-cache as-string)
  "Return the logical path of the physical headline at point.
FUNC is an unused argument for compatibility with the advice
framework.

When the optional argument WITH-SELF is non-nil, the path aldo
includes the current headline. See `org-get-outline-path' for
documentation of USE-CACHE."
   (ad-with-originals 'org-get-outline-path
    (let ((res (append
                (org-tree-path-list (or (org-tree-lookup
                                         (file-truename (buffer-file-name))) ""))
                (org-get-outline-path with-self use-cache))))
      (if as-string (org-tree-path-string res) res))))

(defun org-tree-push-lookup-table-maybe (subtree path id)
  "If SUBTREE references an org file, as determined from its
extension, add it into the `org-tree-lookup-table' variable."
  (when (equal (file-name-extension subtree) "org")
    (push (cons (list subtree id) (org-tree-path-string path)) org-tree-lookup-table)))

(defun org-tree-capture-set-target-location (func &optional target)
  "Add two additional org-capture target location types:

        (olp \"org-tree/outline/path\")

        (olp+attach base-target-type \"org-tree/outline/path\" \"path/to/attachment\"

        (olp+subtree \"org-tree/outline/path\" \"node headline\" \"subtree file name\")

For more information on target location types, see `org-capture-templates'."
  (let ((args (org-capture-get :target)))
    (pcase (or target args)
      (`(olp ,outline-path)
       (let ((m (org-tree-find-olp outline-path)))
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

(defun org-tree-meta-return-internal (&optional info)
  (let* ((headline (plist-get info :headline))
         (id (org-id-get-create))
         (ad (file-name-as-directory
              (progn (and (plist-get info :attach-dir)
                          (org-entry-put nil "ATTACH_DIR"
                                         (plist-get info :attach-dir)))
                     (org-attach-dir t))))
         (pd (plist-get info :project))
         (st (plist-get info :subtree))
         (subtree (org-tree-format
                   (if (not st)
                       (expand-file-name org-tree-default-subtree-file-name ad)
                     (if (equal st 'ask)
                         (setq st (read-file-name "Subtree: " ad)) st)
                     (when (equal (car (org-tree-split-file-name st)) ad)
                       (setq st (cdr (org-tree-split-file-name st))))
                     (unless (file-directory-p st)
                       (org-entry-put nil "SUBTREE" st))
                     (if (file-name-absolute-p st) st (expand-file-name st ad))))))
  (unless (file-exists-p subtree)
    (save-excursion
      (beginning-of-line)
      (setq headline
            (plist-get (org-tree-headline-parser) :raw-value))
      (find-file subtree)
      (insert (org-tree-format
               (or (plist-get info :template)
                   org-tree-default-subtree-template)))
      (save-buffer)
      (kill-buffer)))
  subtree))

(defun org-tree-meta-return (func &optional arg)
  "Insert a hew org-tree subtree. Calls `org-meta-return.'"
  (funcall func arg)
  (if (equal arg '(16))
      (let* ((info (when (boundp 'org-tree-info) org-tree-info))
             (headline (or (plist-get info :headline) (read-string "Headline: "))))
        (insert headline)
        (plist-put info :headline headline)
        (org-tree-meta-return-internal info))))

(define-minor-mode org-tree-mode
  "Logically combine many Orgdocuments into one. This minor mode
ues `org-tree-lookup-table' to find the full outline path of any
ement in the perspective tree."
  :lighter " OP"
  :global t
  (if org-tree-mode
      (progn
        (org-tree-lookup-table)
        (advice-add 'org-get-outline-path :around #'org-tree-outline-path)
        (advice-add 'org-meta-return :around #'org-tree-meta-return)
        (advice-add 'org-capture-set-target-location
                    :around #'org-tree-capture-set-target-location))
    (advice-remove 'org-get-outline-path #'org-tree-outline-path)
    (advice-remove 'org-meta-return #'org-tree-meta-return)
    (advice-remove 'org-capture-set-target-location
                   #'org-tree-capture-set-target-location)))

(defun org-tree-resolve-subtree-project-directory ()
  "Return the full path of a subtree's project directory."
  (let ((project (org-entry-get nil "PROJECT")))
    (if (file-name-absolute-p project)
        project
      (expand-file-name project (org-attach-dir)))))

(defun org-tree-encode-subtree-project-directory (project)
  "Truncate the project directory path if it lives inside the
subtree's attachment directory."
  (org-entry-put nil "PROJECT"
  (if (string-prefix-p (file-name-as-directory (org-attach-dir)) project)
      (file-name-nondirectory project)
    project)))

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
     (file-name-nondirectory (cadr agrs)))))

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

(defun org-tree-synchronize-headline-title ()
  "Set the subtree file's TITLE property to the physical headline
of the subtree."
  (let ((headline-logical)
        (org-agenda-new-buffers)
        (headline-physical (plist-get (org-tree-headline-parser) :raw-value)))
  (with-current-buffer
      (org-get-agenda-file-buffer (org-tree-resolve-subtree-file-name))
    (org-global-prop-set "TITLE" headline-physical))
  (org-release-buffers org-agenda-new-buffers)))

(defun org-tree-inject-subtree ()
  "Move the contents of the physical subtree at point to the
org-tree subtree, creating one if necessary."
  (save-excursion
    (org-back-to-heading)
    (let* ((info (org-tree-headline-parser))
           (headline (plist-get info :raw-value))
           (subtree (org-tree-meta-return-internal
                     (list :headline headline)))
           (end-metadata (save-excursion (org-end-of-meta-data) (point)))
           (first-child (save-excursion (when (org-goto-first-child) (point))))
           (end-subtree (save-excursion (org-end-of-subtree (point))))
           (buf (or (find-buffer-visiting subtree) (find-file-noselect subtree)))
           (org-refile-keep t))
      ;; first move loose text to the top (if we have any)
      (unless (= 0 (length (org-tree-trim-string
                     (buffer-substring end-metadata
                      (or first-child end-subtree)))))
        (kill-ring-save end-metadata (or first-child end-subtree))
        (with-current-buffer buf
          ;; jump before first headline
          (yank)
          (insert "\n\n")))
      ;; then move the subtrees
      (when first-child
        (goto-char first-child)
        (set-mark-command nil)
        (org-end-of-subtree)
        (org-refile nil nil (list headline subtree)))
      (kill-region end-metadata end-subtree))))

(defun org-tree-headline-parser ()
  (cadr (progn
          (beginning-of-line)
          (org-element-headline-parser (point-at-eol)))))

(provide 'org-tree)
