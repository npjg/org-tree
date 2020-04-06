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
(require 'org-global-props)

(defgroup org-tree 'nil
  "Customization group for org-tree"
  :group 'org)

(defconst org-tree-path-separator "/"
  "The separator string with which to delineate path components.")

(defconst org-keyword-prop-regexp "^[ \t]*#\\+[A-Z_]+:\\(\\s-*\\)\\S-+"
  "Again, it doesn't seem that org-mode supports these properties very well.")

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

(defun org-tree-outline-level ()
  (+ (org-outline-level) (save-excursion (goto-char (point-min))
                                         (length (org-get-outline-path)))))

(defun org-tree-path-list (path)
  "If PATH is not already a path list, convert it to a list by
splitting PATH as a string at each `org-tree-path-separator'. If PATH is
already a list, return it as it is."
  (if (listp path) path
    (split-string-and-unquote path org-tree-path-separator)))

(defun org-tree-path-string (path &optional relative)
  "Concatenate the given PATH list into a string, beginning with
a path separator. If PATH is already a string, return it as it is.

With RELATIVE, do not start the path with an `org-tree-path-separator'."
  (unless (stringp path)
    (setq path (combine-and-quote-strings path org-tree-path-separator)))
(concat (unless (equal (substring path 0 1) org-tree-path-separator)
	  org-tree-path-separator)
	path))

(defun org-tree-end-of-meta-data (func &rest args)
  "Apply `org-end-of-meta-data' unless we are before the first
headline, then jump immediately after the file-wide metadata,
which includes keyword properties but not plain text that appears
before the first headline.

In this alternate behavior, when argument FULL is non-nil, also
skip any text before the first headline."
  (condition-case nil (apply func args)
    ;; if we get an error expect that we are before the first headline
    (error
     (if (car args)
         (progn (re-search-forward org-complex-heading-regexp nil :noerror)
                (beginning-of-line)
                (goto-char (1- (point))))
       (goto-char (point-max))
       (re-search-backward org-keyword-prop-regexp nil :noerror)
       (end-of-line)
       (goto-char (1+ (point))))
     nil)))

(defun org-tree-first-heading-in-file ()
  "Goto the first heading in the current org file."
  (let ((p (point)))
    (goto-char (point-min))
    (unless (re-search-forward org-complex-heading-regexp nil :noerror)
      (goto-char p) nil)))

(defun org-tree-entry-member-in-multivalued-property (pom property value &optional inherit)
  "Is VALUE one of the words in the PROPERTY in entry at
point-or-marker POM? With INHERIT, also check up the subtree for
this property."
  (let* ((old (if inherit
                  (org-with-point-at (or pom (point-marker))
                    (org-entry-get-with-inheritance property))
                (org-entry-get pom property)))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (member value values)))

(defun org-tree-resolve-subtree-file-name ()
  "Provide the full file name for the logical subtree at point,
or return nil if a logical subtree does not exist there. This
function does not verify that the subtree is actually an org
document; it can be any sort of document you wish, which allows
easy overloading for other terminal points of your tree (say, a
LaTeX document).

A logical subtree is defined by the presence of an ID from
`org-id-get', an attachment directory from `org-attach-dir', and
an extant file in the attachment directory with name specified by
the SUBTREE property or, lacking this specification, the format string
`org-tree-default-subtree-file-name'."
  (let ((subtree (org-entry-get nil "SUBTREE" nil))
        (id (org-id-get))
        (ad (org-attach-dir)))
    (when (and id ad)
      (ignore-errors
        (file-truename
         (expand-file-name
          (or subtree
              (org-tree-format org-tree-default-subtree-file-name)) ad))))))

(defun org-tree-lookup-table (&optional reverse force)
  "Return the entry path table, calculating it if necessary. If FORCE,
recalculate both tables anew before returning the requested
table.

With REVERSE, return a flipped version of the table. This ensures
deepest subtrees appear above their parents, so
`org-tree-reverse-lookup' functions properly.

The lookup table is stored as an alist, where each association
has `car' as a list whose `car' is the physical path to the
subtree file and chose `cdr' is the subtree's ID, and has as
`cdr' the logical outline path of the subtree.

Parsing can be controlled via the TREE_SKIP property, which
admits three options that are inherited:

 agenda: do not include this subtree in `org-agenda-files';
 refile: do not include this subtree in refile targets;
 lookup: do not even parse this subtree for more logical subtrees.

The last option can be especially useful for large journal files
that would otherwise take a long time for `org-map-entries' to
iterate over, with no real utility."
  (unless org-tree-root (user-error "Tree index cannot be nil"))
  (setq org-tree-root (file-truename org-tree-root))
  (unless (and org-tree-lookup-table (not force))
    (message "Generating lookup table...")
    (let (org-agenda-new-buffers org-mode-hook results)
      (org-tree-lookup-table-1 nil org-tree-root)
      (message "Generating lookup table... done")
      (setq org-tree-lookup-table results)
      (setq org-tree-reversed-lookup-table
            (reverse org-tree-lookup-table))
      (org-release-buffers org-agenda-new-buffers)))
  (if reverse org-tree-reversed-lookup-table org-tree-lookup-table))

(defun org-tree-lookup-table-1 (path subtree &optional exclude)
  "Recursively build the internal lookup alist with initial outline path PATH
for the logical subtree in file SUBTREE.

All subtree files not explicitly excluded will be addded to
`org-agenda-files'. To exclude a subtree, include
string \"agenda\" in the TREE_SKIP property."
  (with-current-buffer (org-get-agenda-file-buffer subtree)
    (unless exclude
      (add-to-list 'org-agenda-files (buffer-file-name)))
    (org-map-entries
     (lambda () (let ((subtree (org-tree-resolve-subtree-file-name)))
                  (when (ignore-errors (and (equal (file-name-extension subtree) "org")
                                            (file-exists-p subtree)))
                    (let ((path (append path (ignore-errors
                                               (ad-with-originals 'org-get-outline-path
                                                 (org-get-outline-path t))))))
                      (push (cons (list subtree (org-id-get)) (org-tree-path-string path)) results)
                      (unless (org-tree-entry-member-in-multivalued-property nil "TREE_SKIP" "lookup")
                        (org-tree-lookup-table-1 path subtree (or exclude (org-tree-entry-member-in-multivalued-property
                                                                           nil "TREE_SKIP" "agenda" :inherit))))))))
     t 'file)))

(defun org-tree-lookup (path)
  "Get the outline path corresponding to the logical subtree in
file PATH. This function does not interpolate paths from roots;
it directly reads from `org-tree-lookup-table'. Note that the
root outline path has an ID of nil, and its filesystem path is
`org-tree-root'."
  (let ((path (org-tree-path-string path)))
    (cond ((equal path org-tree-root) org-tree-path-separator)
          (t (cdr (assoc path (org-tree-lookup-table)
                         (lambda (o1 o2) (equal (car o1) o2))))))))

(defun org-tree-reverse-lookup (path &optional lax)
  "With logical subtree path PATH, return the entry of
`org-tree-lookup-table' that defines the exact subtree. With LAX,
return the highest subtree for which a match is found."
  (when path
    (let* ((path (org-tree-path-string path))
           (rassoc (cl-rassoc path (org-tree-lookup-table :reverse)
                    :test (lambda (a b)
                            (funcall (if lax #'string-prefix-p #'equal) b a)))))
      (cond  (rassoc (list (car rassoc)
                           (append (list (cdr rassoc))
                            (or (ignore-errors
                                  (substring (string-remove-prefix (cdr rassoc) path) 1)) ""))))
             (t (list (list org-tree-root nil)
                      (cons org-tree-path-separator (substring path 1))))))))

(defun org-tree-find-olp (path)
  "Return a cons cell whose `car' is a marker to the physical
location of PATH and whose `cdr' is a marker to the logical
location of PATH.

Note that if PATH does not define an org-tree subtree, only the
`car' will be given, as the two paths are equal."
  (let* ((info (org-tree-reverse-lookup path :lax))
         (path (org-tree-path-list (cdadr info))))
    (cond ;; physical headline under logical subtree no subtree here: check to
          ;; make sure it's not actually in the physical location
     (path (cons (or (ignore-errors (org-find-olp (append (list (caar info)) path)))
                     (org-with-point-at
                         (unless (or (cadar info) (org-id-find (cadar info) :marker))
                           (user-error "Subtree ID expected but not found"))
                       (org-find-olp (append
                           (funcall (ad-get-orig-definition #'org-get-outline-path) t)
                           path) :this-buffer)))
                        nil))
          ;; subtree here
          (t (unless (cadar info) (user-error "Subtree ID expected but not found"))
             (cons (org-id-find (cadar info) :markerp)
                   (with-current-buffer (org-get-agenda-file-buffer (caar info))
                     (goto-char (point-min))
                     (org-end-of-meta-data)
             (set-marker (make-marker) (point))))))))

(defun org-tree-resolve-attachment-path (path attachment)
  "Return the full physical path to attachment ATTACHMENT of the
subtree at outline path PATH. A valid attachment directory,
returned by `org-attach-dir' is required to to properly expand
the file name Note that ATTACHMENT need not exist; it must just
be a file name."
  (let* ((ad (org-with-point-at
                 (org-id-find (or (cadar (org-tree-reverse-lookup path))
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
       (let ((m (cdr (org-tree-find-olp outline-path))))
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
                    :around #'org-tree-capture-set-target-location)
        (advice-add 'org-refile-get-targets
                    :around #'org-tree-refile-get-targets)
        (advice-add 'org-end-of-meta-data :around #'org-tree-end-of-meta-data)
        (advice-add 'org-refile :around #'org-tree-refile))
    (advice-remove 'org-get-outline-path #'org-tree-outline-path)
    (advice-remove 'org-meta-return #'org-tree-meta-return)
    (advice-remove 'org-capture-set-target-location
                   #'org-tree-capture-set-target-location)
    (advice-remove 'org-refile-get-targets #'org-tree-refile-get-targets)
    (advice-remove 'org-end-of-meta-data #'org-tree-end-of-meta-data)
    (advice-remove 'org-refile #'org-tree-refile)))

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

(defun org-tree-synchronize-headline-title ()
  "Set the subtree file's TITLE property to the physical headline
of the subtree."
  (let ((org-agenda-new-buffers)
        (headline-physical (plist-get (org-tree-headline-parser) :raw-value)))
  (with-current-buffer
      (org-get-agenda-file-buffer (org-tree-resolve-subtree-file-name))
    (org-global-prop-set "TITLE" headline-physical))
  (org-release-buffers org-agenda-new-buffers)))

(defun org-tree-inject-subtree (&optional no-kill)
  "Move the contents of the physical subtree at point to the
logical subtree, creating one if necessary. With NO-KILL, do not
remove the text from the physical subtree; just copy it."
  (save-excursion
    (org-back-to-heading)
    (let* ((subtree
            (or (org-tree-resolve-subtree-file-name)
                (org-tree-meta-return-internal
                 (list :headline (plist-get (org-tree-headline-parser):raw-value))))))
      (funcall (if no-kill #'kill-ring-save #'kill-region)
               (progn (org-end-of-meta-data) (point))
               (progn (org-end-of-subtree) (point)))
      (with-current-buffer (org-get-agenda-file-buffer subtree)
        (goto-char (point-max))
        (org-tree-paste-subtree #'org-paste-subtree)))))

(defun org-tree-extract-subtree (&optional kill)
  "Move the contents of the logical subtree at point to the
physical subtree. With KILL, kill the contents of the logical
subtree; otherwise, just copy them."
  (save-excursion
    (org-back-to-heading)
    (let* ((subtree (or (org-tree-resolve-subtree-file-name)
                        (user-error "No logical subtree to extract"))))
      (with-current-buffer (org-get-agenda-file-buffer subtree)
        (goto-char (point-min))
        (funcall (if kill #'kill-region #'kill-ring-save)
               (progn (org-end-of-meta-data) (point))
               (point-max)))
      (org-end-of-meta-data)
      (org-tree-paste-subtree #'org-paste-subtree (1+ (org-current-level))))))

(defun org-tree-paste-subtree (func &rest args)
  (flet ((org-kill-is-subtree-p (&optional txt) t))
    (apply func args)))

(defun org-tree-headline-parser ()
  (cadr (progn
          (beginning-of-line)
          (org-element-headline-parser (point-at-eol)))))

(defun org-tree-refile (func &rest args)
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
  (let ((org-refile-targets (list (cons nil (cons :maxlevel 3)))))
    (seq-partition (org-tree-flatten
                    (apply #'org-tree-refile-get-targets-1 func args)) 4)))

(defun org-tree-refile-get-targets-1 (func &rest args)
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
                      (> depth org-tree-refile-max-level))
           (setq depth (1+ depth))
           (funcall #'org-tree-refile-get-targets-1
                    func (org-get-agenda-file-buffer subtree))))
         (add-to-list 'result elt))))
        (apply func args)))

(defun org-tree-up-heading ()
  (unless (org-up-heading-safe)
    ;; get the parent location
    (let* ((info (org-tree-reverse-lookup (org-get-outline-path) :lax))
           (place (and (or (cadar info)
                           (user-error "Subtree ID expected but not found"))
                       (org-id-find (cadar info) :marker))))
      (switch-to-buffer (marker-buffer place))
      (goto-char place))))

(defun org-tree-goto-first-child ()
  (let ((subtree (org-tree-resolve-subtree-file-name)))
    (if subtree
        (progn
          (find-file subtree)
          (goto-char (point-min))
          (re-search-forward org-complex-heading-regexp nil :noerror))
      (org-goto-first-child))))

(provide 'org-tree)
