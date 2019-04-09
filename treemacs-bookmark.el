;;; treemacs-bookmark.el --- Show bookmarks in Treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2019 Jasper Mattsson

;; URL: https://github.com/jasu/treemacs-bookmark
;; Author: Jasper Mattsson <jasu@njomotys.info>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (treemacs "2.5") (dash "2.11.0") (all-the-icons "3.2.0"))

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
(require 'treemacs)
(require 'treemacs-extensions)
(require 'all-the-icons)

(defgroup treemacs-bookmark nil "Treemacs Bookmark" :group 'treemacs)

(cl-macrolet
    ((def (name doc)
          `(defcustom ,name 'treemacs-visit-node-no-split ,doc
             :type '(choice
                     (const :tag "Visit in a horizontal split" treemacs-visit-node-horizontal-split)
                     (const :tag "Visit in a vertical split" treemacs-visit-node-vertical-split)
                     (const :tag "Visit without a split" treemacs-visit-node-no-split)
                     (const :tag "Visit with Ace" treemacs-visit-node-ace)
                     (const :tag "Visit with Ace in a horizontal split" treemacs-visit-node-ace-horizontal-split)
                     (const :tag "Visit with Ace in a vertical split" treemacs-visit-node-ace-vertical-split)))))
  (def treemacs-bookmark-default-file-visit-action
       "Default action to visit a bookmark pointing to a regular file.")
  (def treemacs-bookmark-default-directory-visit-action
       "Default action to visit a bookmark pointing to a directory."))

(defun treemacs-bookmark--get-bookmark-path ()
  "Get the path of the current bookmark node, NIL on failure.

On failure, error is pulsed."
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (or (treemacs-button-get current-btn :bookmark-location)
       (treemacs-pulse-on-failure "Not on a Treemacs Bookmark"))))

(defun treemacs-bookmark-goto-bookmark (&rest _)
  "Go to the bookmark of the current button in Treemacs."
  (interactive)
  (-when-let*
      ((path (treemacs-bookmark--get-bookmark-path))
       (project
        (or (treemacs-is-path path :in-workspace)
            (treemacs-pulse-on-failure
                "Bookmark '%s' is not in the Treemacs workspace." path))))
    (treemacs-goto-file-node path project)
    (when (file-directory-p path)
      (let ((button (treemacs-current-button)))
        (unless (treemacs-is-node-expanded? button)
          (treemacs-toggle-node))
        (treemacs-next-line 1)
        (unless (= (treemacs-button-get (treemacs-current-button) :parent)
                   button)
          (treemacs-previous-line 1))))
    (treemacs-pulse-on-success)))

(defvar treemacs-bookmark--visit-actions
  '(treemacs-visit-node-horizontal-split
    treemacs-visit-node-vertical-split
    treemacs-visit-node-no-split
    treemacs-visit-node-ace
    treemacs-visit-node-ace-horizontal-split
    treemacs-visit-node-ace-vertical-split)
  "List of actions to consider as visit actions when visiting a file.")

(defun treemacs-bookmark--find-button-visit-action (button)
  "Find the visit action defined for BUTTON in its current state."
  (-some--> button
            (treemacs-button-get it :state)
            (list (alist-get it treemacs-RET-actions-config)
                  (alist-get it treemacs-TAB-actions-config))
            (-intersection it treemacs-bookmark--visit-actions)
            (-first-item it)))

(defun treemacs-bookmark-visit-bookmark (&optional arg)
  "Visit the bookmark of the current button.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  ;; This function is a huge hack, and likely rather fragile - it temporarily
  ;; makes the current button a file/dir button, and runs the action.
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (-when-let (path (treemacs-bookmark--get-bookmark-path))
     (let* ((target-button (-some--> (treemacs-is-path path :in-workspace)
                                     (save-excursion (treemacs-find-file-node path it))))
            ;; Store previous values of our button
            (old-path (treemacs-button-get current-btn :path))
            (old-state (treemacs-button-get current-btn :state)))
       (treemacs-with-writable-buffer
        ;; HACK STARTS HERE
        (unwind-protect
            (let ((is-dir (file-directory-p path)))
              ;; Make the current bookmark button appear as a file/directory button.
              (treemacs-button-put current-btn :path path)
              (treemacs-button-put current-btn :state (if is-dir 'dir-node-closed 'file-node-closed))
              (funcall-interactively
               (cond
                ;; Primarily call the action of the button of the actual file/directory
                ((treemacs-bookmark--find-button-visit-action target-button))
                ;; If no button or the button does not have a visit action,
                ;; use the configured defaults.
                (is-dir treemacs-bookmark-default-directory-visit-action)
                (t treemacs-bookmark-default-file-visit-action))
               arg))
          ;; Always restore the values of current button, even on error.
          ;; Executing the action has likely focused another window - restoring
          ;; the button properties has to be done with another buffer.
          (treemacs-with-button-buffer current-btn
            (treemacs-button-put current-btn :path old-path)
            (treemacs-button-put current-btn :state old-state))))))))

(defun treemacs-bookmark-goto-or-visit-bookmark (&optional arg)
  "Go to the current bookmark Treemacs or visit the file if in workspace.
Stay in current window with a prefix argument ARG."
  (interactive "P")
  (-when-let (path (treemacs-bookmark--get-bookmark-path))
    (if (treemacs-is-path path :in-workspace)
        (treemacs-bookmark-goto-bookmark)
      (treemacs-bookmark-visit-bookmark arg))))

(cl-macrolet
    ((def (name default doc)
          `(defcustom ,name ,default ,doc
             :type '(choice (const :tag "Bottom" bottom)
                            (const :tag "Top" top)
                            (const :tag "Disabled" nil)))))
  (def treemacs-bookmark-top-level-position 'bottom
       "Position of the top-level bookmarks node.")
  (def treemacs-bookmark-project-position 'top
       "Position of the per-project bookmarks node.")
  (def treemacs-bookmark-directory-position 'top
       "Position of the per-directory bookmarks node."))

(cl-macrolet
    ((def (name default doc)
          `(defcustom ,name ,default ,doc
             :type '(choice (const :tag "Visit the bookmark" 'treemacs-bookmark-visit-bookmark)
                            (const :tag "Go to the bookmark in Treemacs" 'treemacs-bookmark-goto-bookmark)
                            (const :tag "Go to the bookmark in Treemacs or visit the file" 'treemacs-bookmark-goto-or-visit-bookmark)))))
  (def treemacs-bookmark-ret-action 'treemacs-bookmark-visit-bookmark
       "Action to perform when RETURN is pressed on a bookmark.")
  (def treemacs-bookmark-tab-action 'treemacs-bookmark-goto-bookmark
       "Action to perform when TAB is pressed on a bookmark.")
  (def treemacs-bookmark-mouse1-action 'treemacs-bookmark-goto-bookmark
       "Action to perform when a bookmark is clicked."))

(defface treemacs-bookmark-top-level-face
  '((t :inherit treemacs-root-face
       :foreground "goldenrod"))
  "Face for the top-level bookmarks button.")

(defface treemacs-bookmark-project-face
  '((t :inherit treemacs-dir-face
       :foreground "deep sky blue"))
  "Face for the per-project bookmarks button.")

(defface treemacs-bookmark-dir-face
  '((t :inherit treemacs-project-face))
  "Face for the per-directory bookmarks button.")

(defface treemacs-bookmark-in-workspace-face
  '((t :inherit treemacs-file-face))
  "Face for bookmarks which are available in Treemacs.")

(defface treemacs-bookmark-not-in-workspace-face
  '((t :inherit treemacs-bookmark-in-project-face
       :foreground "dark orange"))
  "Face for bookmarks which are not available in Treemacs.")

(defface treemacs-bookmark-non-existent-face
  '((t :inherit treemacs-bookmark-in-project-face
       :foreground "red"
       :weight bold))
  "Face for bookmarks which do not exist on disk.")

(treemacs-define-leaf-node treemacs-bookmark-leaf 'dynamic-icon
  :tab-action treemacs-bookmark-tab-action
  :ret-action treemacs-bookmark-ret-action
  :mouse1-action treemacs-bookmark-mouse1-action)

(defun treemacs-bookmark--top-level-bookmarks ()
  ; checkdoc-params: (checkdoc-symbol-words "top-level")
  "Get the list of bookmarks to show for the top-level bookmark list."
  bookmark-alist)

(defun treemacs-bookmark--top-level-predicate (_)
  ; checkdoc-params: (checkdoc-symbol-words "top-level")
  "Return non-nil if top-level bookmarks should be visible."
  (and treemacs-bookmark-top-level-position
       bookmark-alist))

(defun treemacs-bookmark--project-predicate (project)
  "Return non-nil if any bookmark exists in PROJECT."
  (and treemacs-bookmark-project-position
       (--any? (treemacs-is-path (bookmark-location it) :in-project project)
               bookmark-alist)))

(defun treemacs-bookmark--directory-predicate (directory)
  "Return non-nil if any bookmark exists in DIRECTORY."
  (and treemacs-bookmark-directory-position
       (--any? (treemacs-is-path directory :parent-of (bookmark-location it))
               bookmark-alist)))

(defun treemacs-bookmark--project-bookmarks (btn)
  "Get the list of bookmarks to show for the current project.

BTN is the bookmark button."
  (let ((project (treemacs-project-of-node btn)))
    (unless project
      (error "Tried to get Treemacs Bookmark project bookmarks outside a project"))
    (--filter (treemacs-is-path (bookmark-location it) :in-project project)
              bookmark-alist)))

(defun treemacs-bookmark--directory-bookmarks (btn)
  "Get the list of bookmarks to show for the current project.

BTN is the bookmark button."
  (let ((directory (car-safe (treemacs-safe-button-get btn :path))))
    (unless directory
      (error "Tried to get Treemacs Bookmark directory bookmarks outside a directory"))
    (--filter (treemacs-is-path directory :parent-of (bookmark-location it))
              bookmark-alist)))

(defvar treemacs-bookmark--icon-cache nil
  "Cache for Treemacs Bookmark icons.")

(defsubst treemacs-bookmark--octicon (icon-name face)
  "Get an Octicon by ICON-NAME as a Treemacs icon.  Use FACE to propertize the icon."
  (let* ((cache-key (list icon-name face))
         (cached (cdr (assoc cache-key treemacs-bookmark--icon-cache))))
    (or cached
        (let* ((face (list :family (all-the-icons-octicon-family) :inherit face))
               (icon (all-the-icons-octicon icon-name :face face))
               ;; Emacs does not provide a way to get string-width in pixels, except for
               ;; already rendered text. Get the width of the glyph and multiple it by
               ;; the :HEIGHT of the face.
               (glyph (aref (font-get-glyphs (font-at 0 nil icon) 0 1 icon) 0))
               (height (plist-get (get-char-property 0 'face icon) :height))
               (icon-width (* height (aref glyph 4)))
               (padding (* 0.5 (- treemacs--icon-size icon-width)))
               (space-left (propertize " " 'display `((space :width (,(floor padding))))))
               (space-right (propertize " " 'display `((space :width (,(ceiling padding))))))
               (result (treemacs-as-icon (concat space-left icon space-right " "))))
          (push (cons cache-key result) treemacs-bookmark--icon-cache)
          result))))

(cl-macrolet
    ((def (name &rest extra &key root-face &allow-other-keys)
          `(treemacs-define-expandable-node ,name
             :icon-open (treemacs-bookmark--octicon "bookmark" ,root-face)
             :icon-closed (treemacs-bookmark--octicon "bookmark" ,root-face)

             :root-label "Bookmarks"
             :ret-action #'treemacs-TAB-action

             :render-action
             (let* ((bookmark-path (bookmark-location item))
                    (exists (file-exists-p bookmark-path))
                    (is-dir (and exists (file-directory-p bookmark-path)))
                    (in-workspace (treemacs-is-path bookmark-path :in-workspace))
                    (bookmark-name (car item)))
               (treemacs-render-node
                :icon (cond
                       ((not exists) treemacs-icon-error)
                       ((and is-dir in-workspace) treemacs-icon-open)
                       (is-dir treemacs-icon-closed)
                       (t (treemacs-icon-for-file bookmark-path)))
                :state treemacs-treemacs-bookmark-leaf-state
                :label-form bookmark-name
                :key-form bookmark-name
                :more-properties (:bookmark-location bookmark-path)
                :face (cond
                       ((not exists)
                        'treemacs-bookmark-non-existent-face)
                       (in-workspace
                        'treemacs-bookmark-in-workspace-face)
                       (t
                        'treemacs-bookmark-not-in-workspace-face))))
             ,@extra)))
  (def treemacs-bookmark-top-level
       :query-function (treemacs-bookmark--top-level-bookmarks)
       :top-level-marker t
       :root-face 'treemacs-bookmark-top-level-face
       :root-key-form 'Treemacs-Bookmark-Top-Level)

  (def treemacs-bookmark-project
       :query-function (treemacs-bookmark--project-bookmarks btn)
       :root-marker t
       :root-face 'treemacs-bookmark-project-face
       :root-key-form 'Treemacs-Bookmark-Project)

  (def treemacs-bookmark-directory
       :query-function (treemacs-bookmark--directory-bookmarks btn)
       :root-marker t
       :root-face 'treemacs-bookmark-dir-face
       :root-key-form 'Treemacs-Bookmark-Directory))

(when treemacs-bookmark-top-level-position
  (treemacs-define-top-level-extension
   :extension #'treemacs-TREEMACS-BOOKMARK-TOP-LEVEL-extension
   :predicate #'treemacs-bookmark--top-level-predicate
   :position treemacs-bookmark-top-level-position))

(when treemacs-bookmark-project-position
  (treemacs-define-project-extension
   :extension #'treemacs-TREEMACS-BOOKMARK-PROJECT-extension
   :predicate #'treemacs-bookmark--project-predicate
   :position treemacs-bookmark-project-position))

(when treemacs-bookmark-directory-position
  (treemacs-define-directory-extension
   :extension #'treemacs-TREEMACS-BOOKMARK-DIRECTORY-extension
   :predicate #'treemacs-bookmark--directory-predicate
   :position treemacs-bookmark-directory-position))

(define-minor-mode treemacs-bookmark-mode
  "Global minor mode for displaying bookmarks in Treemacs"
  :global t
  :group 'treemacs-bookmark
  ;; TODO
  :init-value t
  (cond
   (treemacs-bookmark-mode)
   (t)))

(provide 'treemacs-bookmark)

;;; treemacs-bookmark.el ends here
