;;; unit-test.el --- Tests for treemacs-bookmark -*- lexical-binding: t -*-

;; Copyright (C) 2019 Jasper Mattsson

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

;;; Code:

;; Prevent warnings about missing color support when loading Treemacs.
(defvar treemacs-no-load-time-warnings t)

(require 'treemacs)
(require 'treemacs-bookmark)
(require 'buttercup)
(require 'bookmark)
(require 'dash)
(require 'cus-edit)

(defconst test-file load-file-name)
(defconst test-dir-with-slash (file-name-directory load-file-name))
(defconst test-dir-without-slash (substring test-dir-with-slash 0 -1))
(defconst test-parent-dir-with-slash (file-name-directory test-dir-without-slash))
(defconst test-parent-dir-without-slash (substring test-parent-dir-with-slash 0 -1))
(defconst test-file-nonexistent (concat test-dir-with-slash "nonexistent.txt"))

(defconst bookmark-other '("Bookmark" (some-special-bookmark-data . "foo")))
(defconst bookmark-file `("Treemacs Bookmark Test"
                          (filename . ,test-file)
                          (front-context-string . "Frooba")
                          (rear-context-string . "Asdf")
                          (position . 123)))
(defconst bookmark-file-nonexistent `("I don't exist"
                                       (filename . ,test-file-nonexistent)
                                       (front-context-string . "Frooba")
                                       (rear-context-string . "Asdf")
                                       (position . 123)))
(defconst bookmark-directory-without-slash `("Treemacs Bookmark Test Dir"
                                             (filename . ,test-dir-without-slash)))
(defconst bookmark-directory-with-slash `("Treemacs Bookmark Test Dir"
                                          (filename . ,test-dir-with-slash)))

(defconst default-bookmarks (list bookmark-file
                                  bookmark-file-nonexistent
                                  bookmark-directory-without-slash
                                  bookmark-directory-with-slash
                                  bookmark-other))

(describe "treemacs-bookmark--normalize-directory"
  (it "returns / for /"
    (expect (treemacs-bookmark--normalize-directory "/") :to-equal "/"))
  (it "returns /hello/ for /hello"
    (expect (treemacs-bookmark--normalize-directory "/hello") :to-equal "/hello/"))
  (it "returns /hello/ for /hello/"
    (expect (treemacs-bookmark--normalize-directory "/hello/") :to-equal "/hello/"))
  (it "returns /hello/world/ for /hello/world"
    (expect (treemacs-bookmark--normalize-directory "/hello/world") :to-equal "/hello/world/"))
  (it "returns /hello/world/ for /hello/world/"
    (expect (treemacs-bookmark--normalize-directory "/hello/world/") :to-equal "/hello/world/")))

(describe "treemacs-bookmark--bookmark-in-directory-p"
  (it "returns nil for pathless bookmarks"
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-other "/") :to-be nil)
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-other "") :to-be nil)
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-other "Bookmark") :to-be nil))
  (it "returns nil when the bookmark is the directory"
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-directory-without-slash test-dir-with-slash) :to-be nil)
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-directory-with-slash test-dir-with-slash) :to-be nil))
  (it "returns t for a file within the directory"
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-file test-dir-with-slash) :to-be t))
  (it "returns t for a non-existent file within the directory"
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-file-nonexistent test-dir-with-slash) :to-be t))
  (it "returns t for a file within a subdirectory"
    (expect (treemacs-bookmark--bookmark-in-directory-p bookmark-file test-dir-with-slash) :to-be t)))

(describe "treemacs-bookmark--top-level-bookmarks"
  (it "returns `bookmark-alist'"
    (let ((bookmark-alist "Frooba"))
      (expect (treemacs-bookmark--top-level-bookmarks) :to-be bookmark-alist))))

(defmacro with-default-settings (&rest body)
  "Evaluate BODY with default settings."
  (declare (indent 0))
  `(let ((treemacs-bookmark-mode t)
         (bookmark-alist default-bookmarks))
     ,@body))


(describe "treemacs-bookmark--top-level-predicate"
  (it "returns `bookmark-alist' when other conditions are true"
    (with-default-settings
      (expect (treemacs-bookmark--top-level-predicate nil) :to-be bookmark-alist)))

  (it "returns nil when bookmark-alist is nil"
    (with-default-settings
      (let ((bookmark-alist nil))
        (expect (treemacs-bookmark--top-level-predicate nil) :to-be nil))))

  (it "returns nil when `treemacs-bookmark-top-level-position' is nil"
    (with-default-settings
      (let ((treemacs-bookmark-top-level-position nil))
        (expect (treemacs-bookmark--top-level-predicate nil) :to-be nil))))

  (it "returns nil when `treemacs-bookmark-mode' is nil"
    (with-default-settings
      (let ((treemacs-bookmark-mode nil))
        (expect (treemacs-bookmark--top-level-predicate nil) :to-be nil)))))

(describe "treemacs-bookmark--directory-predicate"
  (it "returns non-nil for root directory"
    (with-default-settings
      (expect (treemacs-bookmark--directory-predicate "/") :to-be-truthy)))

  (it "returns non-nil for a direct parent directory"
    (with-default-settings
      (expect (treemacs-bookmark--directory-predicate test-dir-with-slash) :to-be-truthy)
      (expect (treemacs-bookmark--directory-predicate test-dir-without-slash) :to-be-truthy)))

  (it "returns non-nil for an upper parent directory"
    (with-default-settings
        (expect (treemacs-bookmark--directory-predicate test-parent-dir-with-slash) :to-be-truthy)
        (expect (treemacs-bookmark--directory-predicate test-parent-dir-without-slash) :to-be-truthy)))

  (it "returns nil for a sibling directory"
    (with-default-settings
        (expect (treemacs-bookmark--directory-predicate (concat test-parent-dir-with-slash "sibling")) :to-be nil)
        (expect (treemacs-bookmark--directory-predicate (concat test-parent-dir-with-slash "sibling/")) :to-be nil)))

  (it "returns nil for the directory itself"
    (with-default-settings
      (let ((bookmark-alist (list bookmark-directory-with-slash)))
        (expect (treemacs-bookmark--directory-predicate test-dir-with-slash) :to-be nil)
        (expect (treemacs-bookmark--directory-predicate test-dir-without-slash) :to-be nil))
      (let ((bookmark-alist (list bookmark-directory-without-slash)))
        (expect (treemacs-bookmark--directory-predicate test-dir-with-slash) :to-be nil)
        (expect (treemacs-bookmark--directory-predicate test-dir-without-slash) :to-be nil))))

  (it "returns non-nil for a non-existent file"
    (with-default-settings
      (let ((bookmark-alist (list bookmark-file-nonexistent)))
        (expect (treemacs-bookmark--directory-predicate test-dir-with-slash) :to-be-truthy)
        (expect (treemacs-bookmark--directory-predicate test-dir-without-slash) :to-be-truthy))))

  (it "returns nil when bookmark-alist is nil"
    (with-default-settings
      (let ((bookmark-alist nil))
        (expect (treemacs-bookmark--directory-predicate "/") :to-be nil))))

  (it "returns nil when `treemacs-bookmark-directory-position' is nil"
    (with-default-settings
      (let ((treemacs-bookmark-directory-position nil))
        (expect (treemacs-bookmark--directory-predicate "/") :to-be nil))))

  (it "returns nil when `treemacs-bookmark-mode' is nil"
    (with-default-settings
      (let ((treemacs-bookmark-mode nil))
        (expect (treemacs-bookmark--directory-predicate "/") :to-be nil)))))

(defmacro with-treemacs-project (path &rest body)
  "Evaluate BODY with `treemacs-project' in PATH in context."
  (declare (indent 1))
  `(let ((treemacs-project (make-treemacs-project :name "Project"
                                                  :path ,path
                                                  :path-status 'local-readable)))
     (with-default-settings
      ,@body)))

(describe "treemacs-bookmark--project-predicate"
  (it "returns non-nil for root directory"
    (with-treemacs-project "/"
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy)))

  (it "returns non-nil for a direct parent directory"
    (with-treemacs-project test-dir-with-slash
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy))
    (with-treemacs-project test-dir-without-slash
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy)))

  (it "returns non-nil for an upper parent directory"
    (with-treemacs-project test-parent-dir-with-slash
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy))
    (with-treemacs-project test-parent-dir-without-slash
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy)))

  (it "returns nil for a sibling directory"
    (with-treemacs-project (concat test-parent-dir-with-slash "sibling")
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))
    (with-treemacs-project (concat test-parent-dir-with-slash "sibling/")
      (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil)))

  (it "returns nil for the directory itself"
    (with-treemacs-project test-dir-with-slash
      (let ((bookmark-alist (list bookmark-directory-with-slash)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))
      (let ((bookmark-alist (list bookmark-directory-without-slash)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil)))
    (with-treemacs-project test-dir-without-slash
      (let ((bookmark-alist (list bookmark-directory-with-slash)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))
      (let ((bookmark-alist (list bookmark-directory-without-slash)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))))

  (it "returns non-nil for a non-existent file"
    (with-treemacs-project test-dir-with-slash
      (let ((bookmark-alist (list bookmark-file-nonexistent)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy)))
    (with-treemacs-project test-dir-without-slash
      (let ((bookmark-alist (list bookmark-file-nonexistent)))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be-truthy))))

  (it "returns nil when bookmark-alist is nil"
    (with-treemacs-project "/"
      (let ((bookmark-alist nil))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))))

  (it "returns nil when `treemacs-bookmark-project-position' is nil"
    (with-treemacs-project "/"
      (let ((treemacs-bookmark-project-position nil))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil))))

  (it "returns nil when `treemacs-bookmark-mode' is nil"
    (with-treemacs-project "/"
      (let ((treemacs-bookmark-mode nil))
        (expect (treemacs-bookmark--project-predicate treemacs-project) :to-be nil)))))

(describe "treemacs-bookmark--remove-child-directories"
  (it "returns nil for nil"
    (expect (treemacs-bookmark--remove-child-directories nil '("/")) :to-be nil))
  (it "returns /a/b/ for /a/b/"
    (expect (treemacs-bookmark--remove-child-directories '("/a/b/") '("/")) :to-equal '("/a/b/")))
  (it "removes entries not in project directories"
    (expect (treemacs-bookmark--remove-child-directories '("/a/b/" "/a/c/e/" "/a/c/d/" "/b/b/" "/") '("/a/c/" "/b/"))
            :to-have-same-items-as '("/a/c/e/" "/a/c/d/" "/b/b/")))
  (it "removes duplicates"
    (expect (treemacs-bookmark--remove-child-directories '("/a/b/" "/a/b/") '("/"))
            :to-equal '("/a/b/")))
  (it "removes subdirectories"
    (expect (treemacs-bookmark--remove-child-directories '("/a/b/" "/a/b/c/") '("/"))
            :to-equal '("/a/b/")))
  (it "removes subdirectories up to project directory"
    (expect (treemacs-bookmark--remove-child-directories '("/a/b/" "/a/b/c/" "/a/b/c/d/") '("/a/b/c/"))
            :to-equal '("/a/b/c/"))))

(describe "faces defined by treemacs-bookmark"
  (it "have defined parents"
    (dolist (face (custom-group-members 'treemacs-bookmark nil))
      (when (eq (cadr face) 'custom-face)
        (let ((inherit (face-attribute (car face) :inherit)))
          (expect inherit :to-be-truthy)
          (expect (facep inherit) :to-be-truthy))))))

(provide 'unit-test)

;;; unit-test.el ends here
