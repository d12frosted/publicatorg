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



;; Tests for build-plan logic (mock-based, no vulpea required)

(describe "porg-build-plan"
  (describe "hard dependency build order"
    (it "orders items so hard deps are built first"
      (let* ((root (file-name-as-directory (make-temp-file "porg-plan-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        ;; Create items where "child" depends on "parent"
        (puthash "parent"
                 (porg-item-create
                  :id "parent"
                  :type "note"
                  :item "parent-content"
                  :hash "hash-parent"
                  :rule rule
                  :compiler compiler
                  :target-rel "parent.org"
                  :target-abs (expand-file-name "parent.org" root)
                  :hard-deps nil
                  :soft-deps nil)
                 items)
        (puthash "child"
                 (porg-item-create
                  :id "child"
                  :type "note"
                  :item "child-content"
                  :hash "hash-child"
                  :rule rule
                  :compiler compiler
                  :target-rel "child.org"
                  :target-abs (expand-file-name "child.org" root)
                  :hard-deps '("parent")
                  :soft-deps nil)
                 items)
        (let* ((project (porg-project-create
                         :name "plan-test"
                         :root root
                         :cache-file "cache"
                         :input (lambda () nil)
                         :rules (list rule)
                         :compilers (list compiler)))
               (plan (porg-build-plan project items cache))
               (build-order (plist-get plan :build)))
          ;; Parent must come before child in build order
          (expect (seq-position build-order "parent")
                  :to-be-less-than
                  (seq-position build-order "child")))))

    (it "handles diamond dependency pattern"
      (let* ((root (file-name-as-directory (make-temp-file "porg-diamond-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        ;; Diamond: D depends on B,C; B,C depend on A
        (puthash "A" (porg-item-create :id "A" :type "note" :item "a" :hash "a"
                                       :rule rule :compiler compiler
                                       :target-rel "a.org" :target-abs (expand-file-name "a.org" root)
                                       :hard-deps nil :soft-deps nil) items)
        (puthash "B" (porg-item-create :id "B" :type "note" :item "b" :hash "b"
                                       :rule rule :compiler compiler
                                       :target-rel "b.org" :target-abs (expand-file-name "b.org" root)
                                       :hard-deps '("A") :soft-deps nil) items)
        (puthash "C" (porg-item-create :id "C" :type "note" :item "c" :hash "c"
                                       :rule rule :compiler compiler
                                       :target-rel "c.org" :target-abs (expand-file-name "c.org" root)
                                       :hard-deps '("A") :soft-deps nil) items)
        (puthash "D" (porg-item-create :id "D" :type "note" :item "d" :hash "d"
                                       :rule rule :compiler compiler
                                       :target-rel "d.org" :target-abs (expand-file-name "d.org" root)
                                       :hard-deps '("B" "C") :soft-deps nil) items)
        (let* ((project (porg-project-create
                         :name "diamond-test"
                         :root root
                         :cache-file "cache"
                         :input (lambda () nil)
                         :rules (list rule)
                         :compilers (list compiler)))
               (plan (porg-build-plan project items cache))
               (build-order (plist-get plan :build)))
          ;; A before B and C; B and C before D
          (expect (seq-position build-order "A") :to-be-less-than (seq-position build-order "B"))
          (expect (seq-position build-order "A") :to-be-less-than (seq-position build-order "C"))
          (expect (seq-position build-order "B") :to-be-less-than (seq-position build-order "D"))
          (expect (seq-position build-order "C") :to-be-less-than (seq-position build-order "D"))))))

  (describe "soft dependency cache invalidation"
    (it "rebuilds item when soft dep changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-soft-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal))
             (rule-hash (porg-sha1sum rule))
             (compiler-hash (porg-sha1sum compiler)))
        ;; Parent item (soft dep target) - hash changed
        (puthash "parent"
                 (porg-item-create
                  :id "parent" :type "note" :item "p" :hash "new-hash"
                  :rule rule :compiler compiler
                  :target-rel "parent.org" :target-abs (expand-file-name "parent.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        ;; Child with soft dep on parent - hash unchanged
        (puthash "child"
                 (porg-item-create
                  :id "child" :type "note" :item "c" :hash "child-hash"
                  :rule rule :compiler compiler
                  :target-rel "child.org" :target-abs (expand-file-name "child.org" root)
                  :hard-deps nil :soft-deps '("parent"))
                 items)
        ;; Cache: parent has old hash, child is up-to-date
        (puthash "parent" (porg-cache-item-create :hash "old-hash" :output "parent.org"
                                                   :rule "test" :rule-hash rule-hash
                                                   :compiler "test" :compiler-hash compiler-hash)
                 cache)
        (puthash "child" (porg-cache-item-create :hash "child-hash" :output "child.org"
                                                  :rule "test" :rule-hash rule-hash
                                                  :compiler "test" :compiler-hash compiler-hash)
                 cache)
        (let* ((project (porg-project-create
                         :name "soft-test"
                         :root root
                         :cache-file "cache"
                         :input (lambda () nil)
                         :rules (list rule)
                         :compilers (list compiler)))
               (plan (porg-build-plan project items cache))
               (build-list (plist-get plan :build)))
          ;; Parent should rebuild because its hash changed
          (expect (member "parent" build-list) :to-be-truthy)
          ;; Child should rebuild because its soft dep (parent) changed
          (expect (member "child" build-list) :to-be-truthy))))

    (it "does not fail when soft dep is missing from items"
      (let* ((root (file-name-as-directory (make-temp-file "porg-soft-missing-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        ;; Only child exists, soft dep "missing" doesn't exist
        (puthash "child"
                 (porg-item-create
                  :id "child" :type "note" :item "c" :hash "child-hash"
                  :rule rule :compiler compiler
                  :target-rel "child.org" :target-abs (expand-file-name "child.org" root)
                  :hard-deps nil :soft-deps '("missing"))
                 items)
        (let* ((project (porg-project-create
                         :name "soft-missing-test"
                         :root root
                         :cache-file "cache"
                         :input (lambda () nil)
                         :rules (list rule)
                         :compilers (list compiler))))
          ;; Should not error
          (expect (porg-build-plan project items cache) :to-be-truthy)))))

  (describe "cache invalidation triggers"
    (it "rebuilds when item hash changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-hash-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (rule-hash (porg-sha1sum rule))
             (compiler-hash (porg-sha1sum compiler))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "new-hash"
                  :rule rule :compiler compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (puthash "item" (porg-cache-item-create :hash "old-hash" :output "item.org"
                                                 :rule "test" :rule-hash rule-hash
                                                 :compiler "test" :compiler-hash compiler-hash)
                 cache)
        (let* ((project (porg-project-create
                         :name "hash-test" :root root :cache-file "cache"
                         :input (lambda () nil) :rules (list rule) :compilers (list compiler)))
               (plan (porg-build-plan project items cache)))
          (expect (member "item" (plist-get plan :build)) :to-be-truthy))))

    (it "rebuilds when rule hash changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-rule-hash-test" 'dir)))
             (old-rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (new-rule (porg-rule :name "test" :match #'identity :outputs (lambda (x) (list x))))
             (compiler (porg-compiler :name "test" :match #'identity))
             (old-rule-hash (porg-sha1sum old-rule))
             (compiler-hash (porg-sha1sum compiler))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule new-rule :compiler compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (puthash "item" (porg-cache-item-create :hash "same-hash" :output "item.org"
                                                 :rule "test" :rule-hash old-rule-hash
                                                 :compiler "test" :compiler-hash compiler-hash)
                 cache)
        (let* ((project (porg-project-create
                         :name "rule-hash-test" :root root :cache-file "cache"
                         :input (lambda () nil) :rules (list new-rule) :compilers (list compiler)))
               (plan (porg-build-plan project items cache)))
          (expect (member "item" (plist-get plan :build)) :to-be-truthy))))

    (it "rebuilds when compiler hash changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-compiler-hash-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (old-compiler (porg-compiler :name "test" :match #'identity :build (lambda (x) x)))
             (new-compiler (porg-compiler :name "test" :match #'identity :build (lambda (x) (list x))))
             (rule-hash (porg-sha1sum rule))
             (old-compiler-hash (porg-sha1sum old-compiler))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule rule :compiler new-compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (puthash "item" (porg-cache-item-create :hash "same-hash" :output "item.org"
                                                 :rule "test" :rule-hash rule-hash
                                                 :compiler "test" :compiler-hash old-compiler-hash)
                 cache)
        (let* ((project (porg-project-create
                         :name "compiler-hash-test" :root root :cache-file "cache"
                         :input (lambda () nil) :rules (list rule) :compilers (list new-compiler)))
               (plan (porg-build-plan project items cache)))
          (expect (member "item" (plist-get plan :build)) :to-be-truthy))))

    (it "rebuilds when output path changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-path-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (rule-hash (porg-sha1sum rule))
             (compiler-hash (porg-sha1sum compiler))
             (items (make-hash-table :test 'equal))
             (cache (make-hash-table :test 'equal)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule rule :compiler compiler
                  :target-rel "new-path.org" :target-abs (expand-file-name "new-path.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (puthash "item" (porg-cache-item-create :hash "same-hash" :output "old-path.org"
                                                 :rule "test" :rule-hash rule-hash
                                                 :compiler "test" :compiler-hash compiler-hash)
                 cache)
        (let* ((project (porg-project-create
                         :name "path-test" :root root :cache-file "cache"
                         :input (lambda () nil) :rules (list rule) :compilers (list compiler)))
               (plan (porg-build-plan project items cache)))
          (expect (member "item" (plist-get plan :build)) :to-be-truthy)
          ;; Old path should be in delete list
          (expect (member "item" (plist-get plan :delete)) :to-be-truthy))))))

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
