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
(require 'org-attach)

(defgroup org-tree 'nil
  "Customization group for org-tree"
  :group 'org)

(defconst org-tree-path-separator "/"
  "The separator string with which to delineate path components.")

(defconst org-tree-format-spec '(?i id)
  "The special formatting options available to org-tree subtree file names.")

(defcustom org-tree-default-subtree-file-name "%i.org"
  "A format string that specifies the default fulename for
subtree files, including the org extension.")

(defcustom org-tree-root nil
  "The org file that holds root-level subtrees."
    :type '(file :must-match t)
    :group 'org-tree)

(defvar org-tree-lookup-table nil
  "A plist containing alists which map file names to logical
  outline paths (`:physical') and logical outline paths to
  IDs (`:logical').")

(defun org-tree-flatten (list)
  "Nondestructively return a flat version of LIST."
  (cond
   ((null list) nil)
   ((atom list) (list list))
   (t (append (org-tree-flatten (car list)) (org-tree-flatten (cdr list))))))

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
  "Provide the full file name for the org-tree subtree at POM, or return nil if a subtree does not exist there.

An org-tree subtree is defined by the presence of an ID from
`org-id-get', an attachment directory from `org-attach-dir', and
an extant file in the attachment directory with name specified by
the SUBTREE property or, lacking that, by the format string
`org-tree-default-subtree-file-name'."
  (org-with-point-at (or pom (point-marker))
  (let ((id (org-id-get))
        (ad (org-attach-dir)))
    (when (and id ad)
      (ignore-errors (expand-file-name
       (or (org-entry-get nil "SUBTREE" nil)
           (format-spec org-tree-default-subtree-file-name (format-spec-make ?i id))) ad))))))

(defun org-tree-lookup-table-1 (path subtree type)
  "Recursively build the internal lookup plist of type TYPE with initial path PATH
for the subtree file SUBTREE.

When TYPE is :logical, link outline path strings to IDs for this
subtree.

When TYPE is :physical, link the physical file of this subtree to
its outline path entry point."
  (with-current-buffer (org-get-agenda-file-buffer subtree)
    (org-tree-flatten (remove 'nil (org-map-entries
      (lambda ()
        (let* ((subtree (org-tree-resolve-subtree-file-name)))
          (when subtree
            (let* ((path (if (or path (string= (buffer-file-name) org-tree-root))
                         (append path (unless (org-before-first-heading-p)
                                        (ad-with-originals 'org-get-outline-path
                                          (org-get-outline-path t))))
                       (org-tree-outline-path nil t)))
               (spath (org-tree-path-string path))
               (app (cond ((eq type :physical) (list subtree spath))
                          ((eq type :logical) (list spath (org-id-get)))
                          (t (user-error "Bad lookup plist type")))))
              (if (ignore-errors (file-exists-p subtree))
                  (append app (org-tree-lookup-table-1 path subtree type))
                app)))))
      t 'file)))))

(defun org-tree-lookup-table (&optional type force)
  "Return the entry path table of TYPE. If FORCE,
recalculate both tables anew before returning the requested table."
  (unless org-tree-root (user-error "Tree index cannot be nil"))
  (unless (and org-tree-lookup-table (not force))
    (let (org-agenda-new-buffers)
      (setq org-tree-lookup-table
            (list :physical (org-tree-lookup-table-1 nil org-tree-root :physical)
                  :logical (org-tree-lookup-table-1 nil org-tree-root :logical)))
      (org-release-buffers org-agenda-new-buffers)))
  (if type (plist-get org-tree-lookup-table type)
    org-tree-lookup-table))

(defun org-tree-lookup-1 (path type)
  "Get the value corresponding to PATH in lookup table TYPE. Note
that the root path has an ID of nil, and its path is `org-tree-root'."
  (let ((path (org-tree-path-string path)))
    (if (string= path "") ""
      (if (string= path org-tree-root) "/"
    (lax-plist-get (org-tree-lookup-table type) path)))))

(defun org-tree-outline-path (&optional func with-self use-cache as-string)
  "Return the logical path of the physical headline at point.
FUNC is an unused argument for compatibility with the advice
framework.

When the optional argument WITH-SELF is non-nil, the path aldo
includes the current headline. See `org-get-outline-path' for
documentation of USE-CACHE."
  (ad-with-originals 'org-get-outline-path
    (let ((res (append
                (org-tree-path-list (or (lax-plist-get (org-tree-lookup-table :physical) (buffer-file-name)) ""))
                (org-get-outline-path with-self use-cache))))
      (if as-string (org-tree-path-string res) res))))

(define-minor-mode org-tree-mode
  "Logically combine many Orgdocuments into one. This minor mode
uses `org-perspective-lookup-table' to find the full outline path of any
element in the perspective tree."
  :lighter " OP"
  :global t
  (if org-tree-mode
      (progn
        (advice-add 'org-get-outline-path :around #'org-tree-outline-path))
    (advice-remove 'org-get-outline-path #'org-tree-outline-path)))

(provide 'org-tree-mode)
