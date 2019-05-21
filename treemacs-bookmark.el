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

(defun treemacs-bookmark--get-bookmark-path ()
  "Get the path of the current bookmark node, NIL on failure.

On failure, error is pulsed."
  (treemacs-with-current-button
   "Not on a Treemacs button"
   (or (-some-> (treemacs-button-get current-btn :bookmark)
                (bookmark-prop-get 'filename)
                (file-truename))
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

(defun treemacs-bookmark-visit (btn)
  "Jump to the bookmark node at BTN."
  (bookmark-jump (treemacs-safe-button-get btn :bookmark)))

(defun treemacs-bookmark-goto-or-visit (&optional arg)
  "Go to the current bookmark in Treemacs or visit the file if in workspace.
Stay in current window with a prefix argument ARG."
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
  (def treemacs-bookmark-top-level-position 'bottom
       "Position of the top-level bookmarks node.")
  (def treemacs-bookmark-project-position 'top
       "Position of the per-project bookmarks node.")
  (def treemacs-bookmark-directory-position 'top
       "Position of the per-directory bookmarks node."))

(defface treemacs-bookmark-top-level
  '((t :inherit treemacs-root-face
       :foreground "deep sky blue"))
  "Face for the top-level bookmarks button.")

(defface treemacs-bookmark-project
  '((t :inherit treemacs-dir-face
       :foreground "deep sky blue"))
  "Face for the per-project bookmarks button.")

(defface treemacs-bookmark-dir
  '((t :inherit treemacs-dir-face
       :foreground "deep sky blue"))
  "Face for the per-directory bookmarks button.")

(defface treemacs-bookmark-man-page
  '((t :inherit treemacs-bookmark-in-workspace
       :foreground "cyan"))
  "Face for man page bookmarks.")

(defface treemacs-bookmark-info
  '((t :inherit treemacs-bookmark-in-workspace
       :foreground "green"))
  "Face for Info page bookmarks.")

(defface treemacs-bookmark-in-workspace
  '((t :inherit treemacs-file-face))
  "Face for bookmarks which are available in Treemacs.")

(defface treemacs-bookmark-not-in-workspace
  '((t :inherit treemacs-bookmark-in-workspace
       :foreground "dark orange"))
  "Face for bookmarks which are not available in Treemacs.")

(defface treemacs-bookmark-non-existent
  '((t :inherit treemacs-bookmark-in-workspace
       :foreground "red"
       :weight bold))
  "Face for bookmarks which do not exist on disk.")

(treemacs-define-leaf-node treemacs-bookmark-leaf 'dynamic-icon
                           :tab-action #'treemacs-bookmark-goto-or-visit
                           :visit-action #'treemacs-bookmark-visit
                           :mouse1-action #'treemacs-TAB-action)

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

(defun treemacs-bookmark--octicon (icon-name face)
  "Get an Octicon by ICON-NAME as a Treemacs icon.  Use FACE to propertize the icon."
  (if window-system
      (let* ((cache-key (list icon-name face))
             (cached (cdr (assoc cache-key treemacs-bookmark--icon-cache))))
        (or cached
            (let* ((family (all-the-icons-octicon-family))
                   (icon (cdr (assoc icon-name (all-the-icons-octicon-data))))
                   (font (font-at 0 nil (propertize icon 'face `(:family ,family))))
                   (font-height (-> font (font-info) (aref 3)))
                   (font-height-2 (-> font (font-info) (aref 2)))
                   (glyph-width (-> font (font-get-glyphs 0 1 icon) (aref 0) (aref 4)))
                   (max-height (/ (float treemacs--icon-size) (float font-height)))
                   (max-width  (/ (float treemacs--icon-size) (float glyph-width)))
                   (height (floor (/ (* 100.0 (float font-height) (min max-height max-width)) font-height-2)))
                   (padding (when (> max-width max-height)
                              (* 0.5 (- treemacs--icon-size (* max-height (float glyph-width))))))
                   (space-left (when padding (propertize " " 'display `(space :width (,(floor padding))))))
                   (space-right (when padding (propertize " " 'display `(space :width (,(ceiling padding))))))
                   (face `(:family ,family :height ,height :inherit ,face))
                   (result (treemacs-as-icon (concat space-left (propertize icon 'face face 'font-lock-face face 'rear-nonsticky t) space-right " "))))
              (push (cons cache-key result) treemacs-bookmark--icon-cache)
              result)))
    ""))

(defun treemacs-bookmark--handler (record)
  "Open Treemacs into a bookmark RECORD."
  (unless (window-live-p (treemacs-get-local-window))
    (treemacs))
  (select-window (treemacs-get-local-window))
  (treemacs-goto-node (bookmark-prop-get record 'treemacs-bookmark-path)))

(defun treemacs-bookmark-make-record (&rest _)
  "Make a bookmark record for the current Treemacs button."
  (treemacs-with-current-button
   "Not on a button."
   (let ((path (treemacs-button-get current-btn :path)))
     `((defaults . (,(concat "Treemacs - " (treemacs--get-label-of current-btn))))
       (treemacs-bookmark-path . ,path)
       (handler . treemacs-bookmark--handler)
       ,@(when (stringp path) `((filename . ,path)))))))

(defun treemacs-bookmark--install-bookmark-function ()
  "Install a `bookmark-make-record-function' in the current Treemacs buffer."
  (setq-local bookmark-make-record-function #'treemacs-bookmark-make-record))

(defun treemacs-bookmark--uninstall-bookmark-function ()
  "Remote the `bookmark-make-record-function' from the current Treemacs buffer."
  (kill-local-variable 'bookmark-make-record-function))

(cl-macrolet
    ((def (name &rest extra &key root-face &allow-other-keys)
          `(treemacs-define-expandable-node ,name
             :icon-open (treemacs-bookmark--octicon "bookmark" ,root-face)
             :icon-closed (treemacs-bookmark--octicon "bookmark" ,root-face)

             :root-label "Bookmarks"
             :ret-action #'treemacs-TAB-action

             :render-action
             (let* ((bookmark-path (bookmark-prop-get item 'filename))
                    (exists (when bookmark-path (file-exists-p bookmark-path)))
                    (is-dir (when exists (file-directory-p bookmark-path)))
                    (in-workspace (when exists (treemacs-is-path bookmark-path :in-workspace)))
                    (bookmark-type
                     (cond ((bookmark-prop-get item 'man-args) 'man-page)
                           ((bookmark-prop-get item 'info-node) 'info-page)
                           ((not exists) 'file-non-existent)
                           ((and is-dir in-workspace) 'directory-in-workspace)
                           (is-dir 'directory-not-in-workspace)
                           (in-workspace 'file-in-workspace)
                           (t 'file-not-in-workspace)))
                    (bookmark-name (car item)))
               (treemacs-render-node
                :icon (cl-case bookmark-type
                        (man-page treemacs-icon-info)
                        (info-page treemacs-icon-info)
                        (file-non-existent treemacs-icon-error)
                        (directory-in-workspace treemacs-icon-open)
                        (directory-not-in-workspace treemacs-icon-closed)
                        (t (treemacs-icon-for-file bookmark-path)))
                :state treemacs-treemacs-bookmark-leaf-state
                :label-form bookmark-name
                :key-form bookmark-name
                :more-properties (:bookmark item)
                :face (cl-case bookmark-type
                        (man-page 'treemacs-bookmark-man-page)
                        (info-page 'treemacs-bookmark-info)
                        (file-non-existent 'treemacs-bookmark-non-existent)
                        ((directory-in-workspace file-in-workspace) 'treemacs-bookmark-in-workspace)
                        ((directory-not-in-workspace file-not-in-workspace)
                         'treemacs-bookmark-not-in-workspace))))
             ,@extra)))
  (def treemacs-bookmark-top-level
       :query-function (treemacs-bookmark--top-level-bookmarks)
       :top-level-marker t
       :root-face 'treemacs-bookmark-top-level
       :root-key-form 'Treemacs-Bookmark-Top-Level)

  (def treemacs-bookmark-project
       :query-function (treemacs-bookmark--project-bookmarks btn)
       :root-marker t
       :root-face 'treemacs-bookmark-project
       :root-key-form 'Treemacs-Bookmark-Project)

  (def treemacs-bookmark-directory
       :query-function (treemacs-bookmark--directory-bookmarks btn)
       :root-marker t
       :root-face 'treemacs-bookmark-dir
       :root-key-form 'Treemacs-Bookmark-Directory))

(treemacs-define-top-level-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-TOP-LEVEL-extension
 :predicate #'treemacs-bookmark--top-level-predicate
 :position treemacs-bookmark-top-level-position)

(treemacs-define-project-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-PROJECT-extension
 :predicate #'treemacs-bookmark--project-predicate
 :position treemacs-bookmark-project-position)

(treemacs-define-directory-extension
 :extension #'treemacs-TREEMACS-BOOKMARK-DIRECTORY-extension
 :predicate #'treemacs-bookmark--directory-predicate
 :position treemacs-bookmark-directory-position)

(define-minor-mode treemacs-bookmark-mode
  "Global minor mode for displaying bookmarks in Treemacs"
  :group 'treemacs-bookmark
  (unless (eq major-mode 'treemacs-mode)
    (user-error "Cannot enable treemacs-bookmark-mode in a non-Treemacs buffer"))
  (if treemacs-bookmark-mode
      (treemacs-bookmark--install-bookmark-function)
    (treemacs-bookmark--uninstall-bookmark-function)))

(provide 'treemacs-bookmark)

;;; treemacs-bookmark.el ends here
