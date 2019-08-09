;;; treemacs-bookmark.el --- Show bookmarks in Treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2019 Jasper Mattsson

;; URL: https://github.com/jasu/treemacs-bookmark
;; Author: Jasper Mattsson <jasu@njomotys.info>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (treemacs "2.5") (dash "2.11.0") (s "1.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Treemacs-bookmark shows bookmarks as expandable nodes in Treemacs, at
;; root/dir/project level.

;;; Code:

(require 'dash)
(require 's)
(require 'treemacs)
(require 'treemacs-extensions)
(require 'bookmark)

(defgroup treemacs-bookmark nil "Treemacs Bookmark" :group 'treemacs)

(defface treemacs-bookmark-in-workspace '((t :inherit treemacs-file-face))
  "Face for bookmarks which are available in Treemacs.")

(defface treemacs-bookmark-not-in-workspace '((t :inherit treemacs-git-ignored-face))
  "Face for bookmarks which are not available in Treemacs.")

(defface treemacs-bookmark-top-level '((t :inherit treemacs-root-face))
  "Face for the top-level bookmarks button.")

(defface treemacs-bookmark-project '((t :inherit treemacs-directory-face))
  "Face for the per-project bookmarks button.")

(defface treemacs-bookmark-dir '((t :inherit treemacs-directory-face))
  "Face for the per-directory bookmarks button.")

(defface treemacs-bookmark-man-page '((t :inherit treemacs-bookmark-in-workspace))
  "Face for man page bookmarks.")

(defface treemacs-bookmark-info '((t :inherit treemacs-bookmark-in-workspace))
  "Face for Info page bookmarks.")

(defface treemacs-bookmark-non-existent '((t :inherit treemacs-git-conflict-face))
  "Face for bookmarks which do not exist on disk.")

(defface treemacs-bookmark-other '((t :inherit treemacs-git-conflict-face))
  "Face for bookmarks of unknown types.")

(defvar treemacs-bookmark--current-bookmarks nil
  "List of bookmark paths that belong to some Treemacs project.")

(defvar treemacs-bookmark--truename-cache (make-hash-table)
  "Cached `truename's of bookmarks.")

(defconst treemacs-bookmark--directory (file-name-directory load-file-name)
  "Directory where treemacs-bookmark.el resides.")

(defun treemacs-bookmark--normalize-directory (directory)
  "Make sure that DIRECTORY has a final slash."
  (let ((directory-name (directory-file-name directory)))
    (if (string= directory-name "/")
        directory-name
      (concat directory-name"/"))))

(defun treemacs-bookmark--bookmark-in-directory-p (bookmark directory)
  "Return non-nil if BOOKMARK is in DIRECTORY.

DIRECTORY is assumed to be the truename of the directory, with a trailing slash."
  (-when-let (path (bookmark-prop-get bookmark 'filename))
    (unless (file-remote-p path)
      (setq path (or (gethash path treemacs-bookmark--truename-cache)
                     (puthash path (file-truename path) treemacs-bookmark--truename-cache))))
    (and (string-prefix-p directory path)
         (not (string= path directory)))))

(defun treemacs-bookmark--get-bookmark-path ()
  "Get the path of the current bookmark node, NIL on failure.

On failure, error is pulsed."
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (or (-some-> (treemacs-button-get current-btn :bookmark)
                (bookmark-prop-get 'filename)
                (file-truename))
       (treemacs-pulse-on-failure "Not on a Treemacs Bookmark"))))

(defun treemacs-bookmark-goto-bookmark ()
  "Go to the bookmark of the current button in Treemacs.

This is used as the TAB action for bookmark nodes."
  (interactive)
  (-when-let* ((path (treemacs-bookmark--get-bookmark-path))
               (project
                (or (treemacs-is-path path :in-workspace)
                    (treemacs-pulse-on-failure
                        "Bookmark '%s' is not in the Treemacs workspace." path))))
    (cond ((not (treemacs-goto-file-node path project))
           ;; It is possible that the bookmarked file does not exist or is
           ;; ignored in Treemacs.
           (treemacs-pulse-on-failure "Bookmark '%s' was not found in the project." path))
          (t
           ;; When going to a directory, select the first file in it. If the
           ;; node is not expanded, expand it first.
           (when (file-directory-p path)
             (let ((button (treemacs-current-button)))
               (unless (treemacs-is-node-expanded? button)
                 (treemacs-toggle-node))
               (treemacs-next-line 1)
               ;; Empty directory - move back to the parent, keeping it expanded.
               (unless (= (treemacs-button-get (treemacs-current-button) :parent)
                          button)
                 (treemacs-previous-line 1))))
           (treemacs-pulse-on-success)))))

(defun treemacs-bookmark-delete ()
  "Delete the currently selected bookmark in Treemacs.

This is used as the delete action for bookmark nodes."
  (interactive)
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (treemacs-unless-let (bookmark (treemacs-button-get current-btn :bookmark))
       (treemacs-pulse-on-failure "Not on a bookmark node")
     (when (yes-or-no-p (concat "Delete bookmark " (bookmark-name-from-full-record bookmark) "? "))
       (bookmark-delete bookmark)))))

(defun treemacs-bookmark-rename ()
  "Rename the currently selected bookmark in Treemacs."
  (interactive)
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (treemacs-unless-let (bookmark (treemacs-button-get current-btn :bookmark))
       (treemacs-pulse-on-failure "Not on a bookmark node")
     (bookmark-rename bookmark))))

(defun treemacs-bookmark-visit (btn)
  "Jump to the bookmark node at BTN."
  (-some-> (treemacs-safe-button-get btn :bookmark)
           (bookmark-jump)))

(defun treemacs-bookmark-goto-or-visit (&optional arg)
  "Go to the current bookmark in Treemacs or visit the file if in workspace.

Stay in the current window with a prefix argument ARG."
  (interactive "P")
  (-when-let (path (treemacs-bookmark--get-bookmark-path))
    (if (treemacs-is-path path :in-workspace)
        (treemacs-bookmark-goto-bookmark)
      (treemacs-RET-action arg))))

(cl-macrolet
    ((def (name default doc)
          `(defcustom ,name ,default ,doc
             :type '(choice (const :tag "Bottom" bottom)
                            (const :tag "Top" top)
                            (const :tag "Disabled" nil)))))
  (def treemacs-bookmark-top-level-position 'top
       "Position of the top-level bookmarks node.")
  (def treemacs-bookmark-project-position 'top
       "Position of the per-project bookmarks node.")
  (def treemacs-bookmark-directory-position 'top
       "Position of the per-directory bookmarks node."))

(treemacs-define-leaf-node treemacs-bookmark-leaf 'dynamic-icon
                           :tab-action #'treemacs-bookmark-goto-or-visit
                           :visit-action #'treemacs-bookmark-visit
                           :mouse1-action #'treemacs-TAB-action
                           ;; :delete-action #'treemacs-bookmark-delete
                           ;; :rename-action #'treemacs-bookmark-rename
                           )

(defun treemacs-bookmark--top-level-bookmarks ()
                                        ; checkdoc-params: (checkdoc-symbol-words "top-level")
  "Get the list of bookmarks to show for the top-level bookmark list."
  bookmark-alist)

(defun treemacs-bookmark--top-level-predicate ()
                                        ; checkdoc-params: (checkdoc-symbol-words "top-level")
  "Return non-nil if top-level bookmarks should be visible."
  (and (bound-and-true-p treemacs-bookmark-mode)
       treemacs-bookmark-top-level-position
       bookmark-alist))

(defun treemacs-bookmark--project-predicate (project)
  "Return non-nil if any bookmark exists in PROJECT."
  (setq project (treemacs-bookmark--normalize-directory (treemacs-project->path project)))
  (and (bound-and-true-p treemacs-bookmark-mode)
       treemacs-bookmark-project-position
       (--any (treemacs-bookmark--bookmark-in-directory-p it project) bookmark-alist)))

(defun treemacs-bookmark--directory-predicate (directory)
  "Return non-nil if any bookmark exists in DIRECTORY."
  (setq directory (treemacs-bookmark--normalize-directory directory))
  (and (bound-and-true-p treemacs-bookmark-mode)
       treemacs-bookmark-directory-position
       (--any (treemacs-bookmark--bookmark-in-directory-p it directory) bookmark-alist)))

(defun treemacs-bookmark--project-bookmarks (btn)
  "Get the list of bookmarks to show for the current project.

BTN is the bookmark extension expandable button."
  (let ((path (-some-> (treemacs-project-of-node btn)
                       (treemacs-project->path)
                       (treemacs-bookmark--normalize-directory))))
    (--filter (treemacs-bookmark--bookmark-in-directory-p it path) bookmark-alist)))

(defun treemacs-bookmark--directory-bookmarks (btn)
  "Get the list of bookmarks to show for the current project.

BTN is the bookmark extension expandable button."
  ;; BTN refers to the expandable button, so its path is in form
  ;; '("/home/jasu/.emacs.d" Treemacs-Bookmark-Directory).
  ;; car-safe is used for robustness, not for any particular reason.
  (let ((directory (-some-> (treemacs-button-get btn :path)
                            (car-safe)
                            (treemacs-bookmark--normalize-directory))))
    (--filter (treemacs-bookmark--bookmark-in-directory-p it directory) bookmark-alist)))

(define-inline treemacs-bookmark--get-project-roots ()
  "Get the list of Treemacs project root paths."
  (inline-quote
   (->> (treemacs-current-workspace)
        (treemacs-workspace->projects)
        (mapcar #'treemacs-project->path)
        (mapcar #'treemacs-bookmark--normalize-directory))))

(define-inline treemacs-bookmark--get-project-by-root (root-path)
  "Get the Treemacs project by ROOT-PATH."
  (inline-letevals (root-path)
    (inline-quote
     (--> (treemacs-current-workspace)
          (treemacs-workspace->projects it)
          (--first (string= ,root-path (treemacs-project->path it)) it)))))

(defun treemacs-bookmark--remove-child-directories (paths project-roots)
  "Returns a copy of PATHS without directories that are within some other path.

E.g. '(\"/project1/a/b/c\" \"/project1/a/b\" \"/project2\") would return
'(\"/project1/a/b\" \"/project2\")."
  (let* (;; Remove paths that don't belong to any Treemacs project.
         (paths (--filter
                 (let ((path it))
                   (--some (s-starts-with-p it path) project-roots))
                 paths))
         ;; Sort the paths in descending order. This way, longer paths come
         ;; first, and thus child paths come before parents.
         (paths (sort paths #'string>)))
    (cl-loop for paths-tail on paths
             for current-path = (car paths-tail) then (car paths-tail)
             unless (--some (s-starts-with-p it current-path) (cdr paths-tail))
             collect current-path)))

(defun treemacs-bookmark--update-visible-node (path)
  "Update Treemacs node by PATH if it is visible."
  (treemacs-save-position
   (-when-let (btn (treemacs-find-visible-node path))
     (when (treemacs-is-node-expanded? btn)
       (goto-char btn)
       (treemacs-toggle-node)
       (treemacs-toggle-node)))))

(defun treemacs-bookmark--get-paths (path project-roots)
  "Gets PATH and its parents up to its Treemacs project root in PROJECT-ROOTS."
  (-when-let (project-root (--find (s-starts-with-p it path) project-roots))
    (let ((result (list path)))
      ;; Infinite looping is not possible, a type error will be signaled if
      ;; the root directory is met, since (file-name-directory "") returns nil.
      (while (not (string= path project-root))
        (setq path (file-name-directory (substring path 0 -1)))
        (push path result))
      result)))

(defun treemacs-bookmark--update (&rest _)
  "Update Treemacs after changes in `bookmark-alist'.

Also update `treemacs-bookmark--bookmark-alist-copy'.  Arguments are ignored, so
that this function can safely be used as advice."
  ;; Update the root-level bookmark node
  (with-current-buffer (treemacs-get-local-buffer)
    (treemacs-update-node '(:custom Treemacs-Bookmark-Top-Level-Variadic Treemacs-Bookmark-Top-Level))
    (let*
        ;; Get a list of bookmark paths that belong to a Treemacs project.
        ((project-roots (treemacs-bookmark--get-project-roots))
         (bookmarks (--filter (let ((bookmark it))
                                (--any (treemacs-bookmark--bookmark-in-directory-p bookmark it) project-roots))
                              bookmark-alist))
         ;; Compute the sets of directories which had nodes added, but did not have
         ;; any previously and that had nodes previously but no longer have any.
         ;; These directories need to be updated in Treemacs completely, since the
         ;; directory/project extension node predicate is evaluated only when
         ;; updating the parent directory.
         ;; Likewise, compute directories that had all their bookmarks removed.
         (old-directories (--mapcat (treemacs-bookmark--get-paths (file-name-directory (bookmark-prop-get it 'filename)) project-roots) treemacs-bookmark--current-bookmarks))
         (new-directories (--mapcat (treemacs-bookmark--get-paths (file-name-directory (bookmark-prop-get it 'filename)) project-roots) bookmarks))
         (fully-updated-directories (treemacs-bookmark--remove-child-directories
                                     (nconc (-difference new-directories old-directories)
                                            (-difference old-directories new-directories))
                                     project-roots)))

      ;; Update directories that now should or should not have the Bookmarks node at all.
      (--each fully-updated-directories
        ;; Remove the trailing slash unconditionally, as treemacs-bookmark--get-paths
        ;; will always include it, as will file-name-directory.
        (treemacs-update-node (substring it 0 -1)))

      ;; Update Bookmarks nodes with changes.
      (dolist (bookmark bookmarks)
        (let ((dir (file-name-directory (bookmark-prop-get bookmark 'filename))))
          ;; A parent directory above the modified bookmark was already updated.
          (unless (--some (s-starts-with-p it dir) fully-updated-directories)
            (if (member dir project-roots)
                (treemacs-update-node (list (treemacs-bookmark--get-project-by-root (s-chop-suffix "/" dir)) 'Treemacs-Bookmark-Project))
              (treemacs-update-node (list (s-chop-suffix "/" dir) 'Treemacs-Bookmark-Directory))))))

      (setq treemacs-bookmark--current-bookmarks bookmarks))))

(cl-macrolet
    ((def (name &rest extra)
          `(treemacs-define-expandable-node ,name
             ;; Icon form must be used, since the icon may change if the theme
             ;; is changed.
             :icon-open-form (treemacs-bookmark--icon)
             :icon-closed-form (treemacs-bookmark--icon)

             :root-label "Bookmarks"
             :ret-action #'treemacs-TAB-action

             :render-action
             (let* ((bookmark-path (bookmark-prop-get item 'filename))
                    (is-remote (when bookmark-path (file-remote-p bookmark-path)))
                    ;; Remote files are assumed to exist - no file operations are performed on Tramp endpoints.
                    (exists (and bookmark-path (or is-remote (file-exists-p bookmark-path))))
                    (is-dir (when exists (if is-remote
                                             ;; No error here - is-remote is always nil for empty strings.
                                             (eq ?/ (aref bookmark-path (1- (length bookmark-path))))
                                           (file-directory-p bookmark-path))))
                    (in-workspace (when exists (treemacs-is-path bookmark-path :in-workspace)))
                    (bookmark-type
                     (cond ((bookmark-prop-get item 'man-args) 'man-page)
                           ((bookmark-prop-get item 'info-node) 'info-page)
                           ((not exists) 'file-non-existent)
                           (is-dir (if in-workspace 'directory-in-workspace 'directory-not-in-workspace))
                           (bookmark-path (if in-workspace 'file-in-workspace 'file-not-in-workspace))
                           (t 'other)))
                    (bookmark-name (car item)))
               (treemacs-render-node
                :icon (pcase bookmark-type
                        ((or 'man-page 'info-page) (treemacs-get-icon-value 'info))
                        ('file-non-existent (treemacs-get-icon-value 'error))
                        ('directory-in-workspace (treemacs-get-icon-value 'dir-open))
                        ('directory-not-in-workspace (treemacs-get-icon-value 'dir-closed))
                        ('other  (treemacs-get-icon-value 'warning))
                        (_ (treemacs-icon-for-file bookmark-path)))
                :state treemacs-treemacs-bookmark-leaf-state
                :label-form bookmark-name
                :key-form bookmark-name
                :more-properties (:bookmark item)
                :face (pcase bookmark-type
                        ((or 'directory-in-workspace 'file-in-workspace) 'treemacs-bookmark-in-workspace)
                        ((or 'directory-not-in-workspace 'file-not-in-workspace) 'treemacs-bookmark-not-in-workspace)
                        ('file-non-existent 'treemacs-bookmark-non-existent)
                        ('man-page 'treemacs-bookmark-man-page)
                        ('info-page 'treemacs-bookmark-info)
                        ('other 'treemacs-bookmark-other))))
             ,@extra)))
  (def treemacs-bookmark-project
       :query-function (treemacs-bookmark--project-bookmarks node)
       :root-marker t
       :root-face 'treemacs-bookmark-project
       :root-key-form 'Treemacs-Bookmark-Project)

  (def treemacs-bookmark-directory
       :query-function (treemacs-bookmark--directory-bookmarks node)
       :root-marker t
       :root-face 'treemacs-bookmark-dir
       :root-key-form 'Treemacs-Bookmark-Directory)

  (def treemacs-bookmark-top-level
       :query-function (treemacs-bookmark--top-level-bookmarks)
       :top-level-marker t
       :root-face 'treemacs-bookmark-top-level
       :root-key-form 'Treemacs-Bookmark-Top-Level))


;; The top-level node is defined as a variadic node, since normal top-level
;; nodes cannot be displayed or hidden.
(treemacs-define-variadic-node treemacs-bookmark-top-level-variadic
  :root-key-form 'Treemacs-Bookmark-Top-Level-Variadic
  :query-function (when (treemacs-bookmark--top-level-predicate) '(t))
  :render-action (progn
                   (ignore item)
                   (treemacs-render-node
                    :icon (treemacs-bookmark--icon)
                    :state treemacs-treemacs-bookmark-top-level-closed-state
                    :label-form "Bookmarks"
                    :key-form 'Treemacs-Bookmark-Top-Level
                    :face 'treemacs-bookmark-top-level)))

(treemacs-define-top-level-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-TOP-LEVEL-VARIADIC-extension
 :position treemacs-bookmark-top-level-position)

(treemacs-define-project-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-PROJECT-extension
 :predicate #'treemacs-bookmark--project-predicate
 :position treemacs-bookmark-project-position)

(treemacs-define-directory-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-DIRECTORY-extension
 :predicate #'treemacs-bookmark--directory-predicate
 :position treemacs-bookmark-directory-position)

(defun treemacs-bookmark--icon ()
  "Get the icon for a bookmark node."
  (or (treemacs-get-icon-value 'bookmark)
      (progn
        (treemacs-create-icon
         :file "bookmark.png"
         :icons-dir treemacs-bookmark--directory
         :extensions (bookmark))
        (treemacs-get-icon-value 'bookmark))
      ""))

;;;###autoload
(define-minor-mode treemacs-bookmark-mode
  "Minor mode for displaying bookmarks in Treemacs.

`treemacs-bookmark-mode' can only be enabled for Treemacs buffers."
  :group 'treemacs-bookmark
  (cond (treemacs-bookmark-mode
         ;; Only check major mode when enabling - if the user disables Treemacs
         ;; mode before this, they should not be greeted with an error.
         (unless (eq major-mode 'treemacs-mode)
           (user-error "Cannot enable treemacs-bookmark-mode in a non-Treemacs buffer"))
         ;; Bookmark provides no hook for updating the bookmark list, so advice
         ;; the most ridiculously named function in Emacs instead and hope the
         ;; name won't change.
         (advice-add 'bookmark-bmenu-surreptitiously-rebuild-list :after #'treemacs-bookmark--update))
        (t
         (advice-remove 'bookmark-bmenu-surreptitiously-rebuild-list #'treemacs-bookmark--update))))

(provide 'treemacs-bookmark)

;;; treemacs-bookmark.el ends here
