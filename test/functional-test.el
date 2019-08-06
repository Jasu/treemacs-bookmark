;;; functional-test.el --- Tests for treemacs-bookmark -*- lexical-binding: t -*-

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
(defvar treemacs-display-in-side-window nil)


(require 'treemacs)
(require 'treemacs-bookmark)
(require 'buttercup)
(require 'bookmark)
(require 'dash)

(defconst test-dir-with-slash (file-name-directory load-file-name))
(defconst test-project-dir (concat test-dir-with-slash "test-project"))
(defconst test-project2-dir (concat test-dir-with-slash "test-project2"))
(defvar test-project nil)
(defvar test-project2 nil)

(defmacro with-treemacs (&rest body)
  "Evaluate BODY with the Treemacs window selected."
  `(unwind-protect
       (progn
         ;; The default batch mode frame size is too small and does not change
         ;; with --geometry.
         (set-frame-size (selected-frame) 100 100)
         ;; Don't try to load workspaces
         (put 'treemacs :state-is-restored t)

         ;; Create the default workspace
         (setq test-project (make-treemacs-project
                             :name "Project 1"
                             :path test-project-dir
                             :path-status 'local-readable)
               test-project2 (make-treemacs-project
                              :name "Project 2"
                              :path test-project2-dir
                              :path-status 'local-readable))
         (setf (treemacs-current-workspace)
               (make-treemacs-workspace :name "Default"
                                        :projects (list test-project test-project2)))
         (treemacs)
         (treemacs-bookmark-mode 1)
         (goto-char (point-min))
         ,@body)
     (treemacs-kill-buffer)))

(describe "treemacs-bookmark-mode"
  (it "initializes cleanly"
    (with-treemacs))
  (it "advises bookmark-bmenu-surreptitiously-rebuild-list"
    (expect (advice-member-p #'treemacs-bookmark--update 'bookmark-bmenu-surreptitiously-rebuild-list) :to-be-truthy))
  (it "errors when invoked outside Treemacs"
    (expect (treemacs-bookmark-mode 1) :to-throw 'user-error)))

(provide 'functional-test)

;;; functional-test.el ends here
