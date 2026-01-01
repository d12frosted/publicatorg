;;; publicatorg-test.el --- Test publicatorg module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 17 Jul 2022
;;
;; URL: https://github.com/d12frosted/publicatorg
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'publicatorg)
(require 'buttercup)



(setq-default porg-log-level 'debug)



;; Unit tests for pure functions (no vulpea/org-roam setup required)

(describe "porg-slug"
  (it "converts simple title to lowercase"
    (expect (porg-slug "Hello World") :to-equal "hello-world"))

  (it "converts spaces and special chars to hyphens"
    (expect (porg-slug "Hello, World!") :to-equal "hello--world-"))

  (it "handles unicode characters"
    (expect (porg-slug "Café") :to-equal "cafe"))

  (it "handles accented characters"
    (expect (porg-slug "naïve résumé") :to-equal "naive-resume"))

  (it "removes diacritical marks"
    (expect (porg-slug "Zürich") :to-equal "zurich"))

  (it "handles empty string"
    (expect (porg-slug "") :to-equal "")))

(describe "porg-topological-sort"
  (it "sorts simple linear chain"
    (let ((graph '((a b) (b c) (c))))
      (expect (car (porg-topological-sort graph)) :to-equal '(c b a))))

  (it "sorts diamond dependency"
    ;; d depends on b and c, both depend on a
    (let* ((graph '((d b c) (b a) (c a) (a)))
           (result (car (porg-topological-sort graph))))
      ;; a must come before b and c, both must come before d
      (expect (seq-position result 'a) :to-be-less-than (seq-position result 'b))
      (expect (seq-position result 'a) :to-be-less-than (seq-position result 'c))
      (expect (seq-position result 'b) :to-be-less-than (seq-position result 'd))
      (expect (seq-position result 'c) :to-be-less-than (seq-position result 'd))))

  (it "handles nodes with no dependencies"
    (let ((graph '((a) (b) (c))))
      (expect (length (car (porg-topological-sort graph))) :to-equal 3)))

  (it "detects cycles and returns incomplete sort"
    (let* ((graph '((a b) (b c) (c a)))
           (result (porg-topological-sort graph)))
      ;; Second value should be nil (not all sorted)
      (expect (nth 1 result) :to-be nil)))

  (it "handles empty graph"
    (let ((result (porg-topological-sort nil)))
      (expect (car result) :to-equal nil)
      (expect (nth 1 result) :to-be-truthy)))

  (it "respects :test parameter for string keys"
    (let* ((graph '(("a" "b") ("b" "c") ("c")))
           (result (car (porg-topological-sort graph :test 'equal))))
      (expect result :to-equal '("c" "b" "a")))))

(describe "porg-topological-levels"
  (it "groups independent nodes into same level"
    (let* ((graph '((a) (b) (c)))
           (levels (porg-topological-levels graph)))
      ;; All should be in first level (no deps)
      (expect (length levels) :to-equal 1)
      (expect (length (car levels)) :to-equal 3)))

  (it "separates dependent nodes into different levels"
    (let* ((graph '((a b) (b c) (c)))
           (levels (porg-topological-levels graph)))
      (expect (length levels) :to-equal 3)))

  (it "handles diamond pattern correctly"
    ;; d depends on b,c; b,c depend on a
    (let* ((graph '((d b c) (b a) (c a) (a)))
           (levels (porg-topological-levels graph)))
      ;; Level 1: a, Level 2: b,c, Level 3: d
      (expect (length levels) :to-equal 3)
      (expect (car levels) :to-equal '(a))
      (expect (length (nth 1 levels)) :to-equal 2)
      (expect (car (last levels)) :to-equal '(d))))

  (it "ignores external dependencies not in graph"
    (let* ((graph '((a external-dep) (b)))
           (levels (porg-topological-levels graph)))
      ;; Both a and b should be in first level since external-dep is ignored
      (expect (length levels) :to-equal 1)
      (expect (length (car levels)) :to-equal 2))))

(describe "porg-sha1sum"
  (it "hashes lisp objects consistently"
    (let ((obj '(a b c)))
      (expect (porg-sha1sum obj) :to-equal (porg-sha1sum obj))))

  (it "produces different hashes for different objects"
    (expect (porg-sha1sum '(a b c)) :not :to-equal (porg-sha1sum '(a b d))))

  (it "handles nested structures"
    (let ((obj '((a . 1) (b . ((c . 2))))))
      (expect (porg-sha1sum obj) :to-be-truthy)
      (expect (length (porg-sha1sum obj)) :to-equal 40)))

  (it "handles strings"
    (expect (porg-sha1sum "hello") :to-be-truthy)
    (expect (length (porg-sha1sum "hello")) :to-equal 40)))

(describe "porg-string-from-number"
  (it "converts number to string"
    (expect (porg-string-from-number 42) :to-equal "42"))

  (it "pads with spaces by default"
    (expect (porg-string-from-number 5 :min-length 3) :to-equal "  5"))

  (it "pads with zeros when specified"
    (expect (porg-string-from-number 5 :min-length 3 :padding 'zero) :to-equal "005"))

  (it "pads based on padding-num"
    (expect (porg-string-from-number 5 :padding-num 100) :to-equal "  5")
    (expect (porg-string-from-number 5 :padding-num 1000) :to-equal "   5")))

(porg-define
 :name "porg-test"
 :root (file-name-as-directory (make-temp-file "porg-test" 'dir))
 :cache-file "build-cache"
 :describe
 (lambda (x)
   (cond
    ((vulpea-note-p x) (concat "(note) " (file-name-nondirectory (vulpea-note-path x))))
    ((porg-rule-output-p x) (concat "(output) " (porg-rule-output-file x)))
    ((porg-item-p x) (concat "(item) " (porg-item-target-abs x)))
    (t (format "%s" x))))

 :input
 (lambda ()
   (--filter
    (and (null (vulpea-note-primary-title it))
         (= 0 (vulpea-note-level it)))
    (vulpea-db-query)))

 :rules
 (list
  (porg-rule
   :name "note"
   :match #'identity
   :outputs
   (lambda (note)
     (list
      (porg-note-output
       note
       :file (or
              (vulpea-utils-with-note note
                (vulpea-buffer-prop-get "output-file"))
              (file-name-nondirectory (vulpea-note-path note))))))))

 :compilers
 (list
  (porg-compiler
   :name "note"
   :match #'identity
   :hash #'porg-sha1sum
   :build #'porg-test-build-item
   :clean #'porg-test-clean-item)))

(defun porg-test-build-item (item _items _cache)
  "Build an ITEM."
  (copy-file
   (vulpea-note-path (porg-item-item item))
   (porg-item-target-abs item)
   t))

(defun porg-test-clean-item (file)
  "Clean FILE."
  (delete-file file))



(defvar porg-test-directory (expand-file-name "test/notes")
  "Directory containing test notes.")

(defun porg-test-init ()
  "Initialize testing environment."
  (let ((original-dir porg-test-directory)
        (new-dir (expand-file-name (make-temp-name "notes") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (porg-test-init-in new-dir)))

(defun porg-test-init-in (dir)
  "Initialize testing environment in DIR."
  (setq org-roam-directory dir
        org-roam-db-location (expand-file-name "org-roam.db" dir))
  (vulpea-db-autosync-enable)
  (org-roam-db-autosync-enable))

(defun porg-test-teardown ()
  "Teardown testing environment."
  (vulpea-db-autosync-disable)
  (org-roam-db-autosync-disable)
  (delete-file org-roam-db-location))



(describe "publicatorg"
  (before-all (porg-test-init))
  (after-all (porg-test-teardown))

  (before-each
    (spy-on 'porg-test-build-item :and-call-through)
    (spy-on 'porg-test-clean-item :and-call-through))

  (it "should build every item on the first run"
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 3))

  (it "should not call any build function when project is unchanged"
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 0))

  (it "should call build functions only for changed items"
    (vulpea-utils-with-note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab")
      (vulpea-buffer-meta-set "title" "yes")
      (save-buffer))
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 1))

  (it "should call build function for transitively changed item"
    (vulpea-utils-with-note (vulpea-db-get-by-id "2801a0b7-53fe-4128-8ea3-0e6344c4c64c")
      (vulpea-buffer-meta-set "dependants" "yes")
      (save-buffer))
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 2))

  (it "should call build for newly created note"
    (vulpea-create "created note 1"
                   "created-note-1.org"
                   :immediate-finish t)
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 1))

  (it "should call cleanup for deleted note"
    (delete-file (expand-file-name "created-note-1.org" org-roam-directory))
    (porg-run "porg-test")
    (expect 'porg-test-clean-item :to-have-been-called-times 1))

  (it "should rebuild an item when dependency is deleted"
    (let ((note-1 (vulpea-create "created dep note 1"
                                 "created-dep-note-1.org"
                                 :immediate-finish t)))
      (vulpea-create "created dep note 2"
                     "created-dep-note-2.org"
                     :immediate-finish t
                     :body (concat "I depend on " (vulpea-utils-link-make-string note-1)))
      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 2)

      (spy-calls-reset 'porg-test-build-item)

      (delete-file (vulpea-note-path note-1))
      (porg-run "porg-test")
      (expect 'porg-test-clean-item :to-have-been-called-times 1)
      (expect 'porg-test-build-item :to-have-been-called-times 1)))

  (it "should rebuild an item when dependency is added"
    (vulpea-create "Note 4"
                   "note-4.org"
                   :id "fe2ba6c8-2af5-4bc7-a491-b5fadcb144e7"
                   :immediate-finish t)
    (porg-run "porg-test")
    ;; note-3 depends on previously non-existent fe2ba6c8-2af5-4bc7-a491-b5fadcb144e7
    (expect 'porg-test-build-item :to-have-been-called-times 2))

  (it "should delete and rebuild an item that changed output path"
    (let ((note (vulpea-create "Dancing note"
                               "dancing-note.org"
                               :immediate-finish t)))
      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 1)

      (spy-calls-reset 'porg-test-build-item)

      (vulpea-utils-with-note note
        (vulpea-buffer-prop-set "output-file" "moving-note.org")
        (save-buffer))

      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 1)
      (expect 'porg-test-clean-item :to-have-been-called-times 1))))



(provide 'publicatorg-test)
;;; publicatorg-test.el ends here
