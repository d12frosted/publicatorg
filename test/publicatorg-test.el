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


;; * Media type utilities tests

(describe "porg-supported-media-p"
  (it "returns t for supported images"
    (expect (porg-supported-media-p "photo.jpg") :to-be-truthy)
    (expect (porg-supported-media-p "photo.jpeg") :to-be-truthy)
    (expect (porg-supported-media-p "photo.png") :to-be-truthy)
    (expect (porg-supported-media-p "photo.webp") :to-be-truthy)
    (expect (porg-supported-media-p "photo.gif") :to-be-truthy)
    (expect (porg-supported-media-p "photo.svg") :to-be-truthy)
    (expect (porg-supported-media-p "photo.heic") :to-be-truthy))

  (it "returns t for supported videos"
    (expect (porg-supported-media-p "video.mp4") :to-be-truthy))

  (it "returns nil for unsupported files"
    (expect (porg-supported-media-p "document.pdf") :to-be nil)
    (expect (porg-supported-media-p "file.txt") :to-be nil)
    (expect (porg-supported-media-p "archive.zip") :to-be nil))

  (it "handles uppercase extensions"
    (expect (porg-supported-media-p "photo.JPG") :to-be-truthy)
    (expect (porg-supported-media-p "video.MP4") :to-be-truthy))

  (it "returns nil for files without extension"
    (expect (porg-supported-media-p "noextension") :to-be nil)))

(describe "porg-supported-video-p"
  (it "returns t for mp4"
    (expect (porg-supported-video-p "video.mp4") :to-be-truthy))

  (it "returns nil for images"
    (expect (porg-supported-video-p "photo.jpg") :to-be nil))

  (it "handles uppercase"
    (expect (porg-supported-video-p "VIDEO.MP4") :to-be-truthy)))

(describe "porg-supported-image-p"
  (it "returns t for all supported image formats"
    (expect (porg-supported-image-p "a.jpeg") :to-be-truthy)
    (expect (porg-supported-image-p "a.png") :to-be-truthy)
    (expect (porg-supported-image-p "a.jpg") :to-be-truthy)
    (expect (porg-supported-image-p "a.heic") :to-be-truthy)
    (expect (porg-supported-image-p "a.webp") :to-be-truthy)
    (expect (porg-supported-image-p "a.gif") :to-be-truthy)
    (expect (porg-supported-image-p "a.svg") :to-be-truthy))

  (it "returns nil for videos"
    (expect (porg-supported-image-p "video.mp4") :to-be nil)))

(describe "porg-convertible-image-p"
  (it "returns t for convertible formats"
    (expect (porg-convertible-image-p "a.jpeg") :to-be-truthy)
    (expect (porg-convertible-image-p "a.png") :to-be-truthy)
    (expect (porg-convertible-image-p "a.jpg") :to-be-truthy)
    (expect (porg-convertible-image-p "a.heic") :to-be-truthy)
    (expect (porg-convertible-image-p "a.webp") :to-be-truthy))

  (it "returns nil for non-convertible images"
    (expect (porg-convertible-image-p "a.gif") :to-be nil)
    (expect (porg-convertible-image-p "a.svg") :to-be nil))

  (it "returns nil for videos"
    (expect (porg-convertible-image-p "video.mp4") :to-be nil)))


;; * File name utilities tests

(describe "porg-file-name-sanitize"
  (it "replaces special chars with dashes"
    (expect (porg-file-name-sanitize "my file (1).jpg" "webp")
            :to-equal "my-file-1.webp"))

  (it "preserves alphanumeric chars"
    (expect (porg-file-name-sanitize "simple123.png" "webp")
            :to-equal "simple123.webp"))

  (it "preserves slashes in paths"
    (expect (porg-file-name-sanitize "path/to/file.jpg" "webp")
            :to-equal "path/to/file.webp"))

  (it "handles multiple dots"
    (expect (porg-file-name-sanitize "file.name.with.dots.jpg" "webp")
            :to-equal "file.name.with.dots.webp"))

  (it "collapses consecutive dashes"
    (expect (porg-file-name-sanitize "hello   world.jpg" "webp")
            :to-equal "hello-world.webp")
    (expect (porg-file-name-sanitize "a!!!b.jpg" "webp")
            :to-equal "a-b.webp"))

  (it "removes leading and trailing dashes from basename"
    (expect (porg-file-name-sanitize "(test).jpg" "webp")
            :to-equal "test.webp")
    (expect (porg-file-name-sanitize "path/to/(test).jpg" "webp")
            :to-equal "path/to/test.webp")))

(describe "porg-file-name-for-web"
  (it "converts convertible images to webp"
    (expect (porg-file-name-for-web "photo.jpg") :to-equal "photo.webp")
    (expect (porg-file-name-for-web "photo.png") :to-equal "photo.webp")
    (expect (porg-file-name-for-web "photo.heic") :to-equal "photo.webp"))

  (it "leaves non-convertible images unchanged"
    (expect (porg-file-name-for-web "icon.svg") :to-equal "icon.svg")
    (expect (porg-file-name-for-web "animation.gif") :to-equal "animation.gif"))

  (it "leaves videos unchanged"
    (expect (porg-file-name-for-web "video.mp4") :to-equal "video.mp4"))

  (it "sanitizes file names while converting"
    (expect (porg-file-name-for-web "My Photo (1).jpg")
            :to-equal "My-Photo-1.webp"))

  (it "sanitizes non-convertible files too"
    (expect (porg-file-name-for-web "My Animation (1).gif")
            :to-equal "My-Animation-1.gif")
    (expect (porg-file-name-for-web "icon file.svg")
            :to-equal "icon-file.svg")
    (expect (porg-file-name-for-web "my video (final).mp4")
            :to-equal "my-video-final.mp4")))

(describe "porg-file-name-replace-ext"
  (it "replaces extension"
    (expect (porg-file-name-replace-ext "file.md" "json")
            :to-equal "file.json"))

  (it "handles paths"
    (expect (porg-file-name-replace-ext "path/to/file.org" "md")
            :to-equal "path/to/file.md"))

  (it "handles multiple dots in filename"
    (expect (porg-file-name-replace-ext "file.backup.org" "md")
            :to-equal "file.backup.md")))


;; * Delete utilities tests

(describe "porg-delete-with-metadata"
  (it "deletes file and its metadata file"
    (let* ((temp-dir (make-temp-file "porg-delete-test" t))
           (file (expand-file-name "test.md" temp-dir))
           (meta (concat file ".metadata")))
      (with-temp-file file (insert "content"))
      (with-temp-file meta (insert "metadata"))
      (expect (file-exists-p file) :to-be-truthy)
      (expect (file-exists-p meta) :to-be-truthy)
      (porg-delete-with-metadata file)
      (expect (file-exists-p file) :to-be nil)
      (expect (file-exists-p meta) :to-be nil)
      (delete-directory temp-dir t)))

  (it "handles missing metadata file gracefully"
    (let* ((temp-dir (make-temp-file "porg-delete-test" t))
           (file (expand-file-name "test.md" temp-dir)))
      (with-temp-file file (insert "content"))
      (expect (file-exists-p file) :to-be-truthy)
      (porg-delete-with-metadata file)
      (expect (file-exists-p file) :to-be nil)
      (delete-directory temp-dir t)))

  (it "handles missing main file gracefully"
    (let* ((temp-dir (make-temp-file "porg-delete-test" t))
           (file (expand-file-name "nonexistent.md" temp-dir)))
      ;; Should not error
      (expect (porg-delete-with-metadata file) :not :to-throw)
      (delete-directory temp-dir t))))


;; Tests for build-plan logic (mock-based, no vulpea required)

;; Test helper to create a cache from a hash-table
(defun porg-test-make-cache (&optional ht)
  "Create a porg-cache from hash-table HT for testing."
  (porg-cache--create :backend 'file :file nil :data (or ht (make-hash-table :test 'equal))))

(describe "porg-build-plan"
  (describe "hard dependency build order"
    (it "orders items so hard deps are built first"
      (let* ((root (file-name-as-directory (make-temp-file "porg-plan-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (porg-test-make-cache)))
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
             (cache (porg-test-make-cache)))
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
             (cache (porg-test-make-cache))
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
        (porg-cache-put cache "parent" (porg-cache-item-create :hash "old-hash" :output "parent.org"
                                                                :rule "test" :rule-hash rule-hash
                                                                :compiler "test" :compiler-hash compiler-hash))
        (porg-cache-put cache "child" (porg-cache-item-create :hash "child-hash" :output "child.org"
                                                               :rule "test" :rule-hash rule-hash
                                                               :compiler "test" :compiler-hash compiler-hash))
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
             (cache (porg-test-make-cache)))
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
          (expect (porg-build-plan project items cache) :to-be-truthy))))

    (it "rebuilds item when hard dep changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-hard-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (items (make-hash-table :test 'equal))
             (cache (porg-test-make-cache))
             (rule-hash (porg-sha1sum rule))
             (compiler-hash (porg-sha1sum compiler)))
        ;; Attachment (hard dep) - hash changed
        (puthash "attachment"
                 (porg-item-create
                  :id "attachment" :type "attachment" :item "a" :hash "new-hash"
                  :rule rule :compiler compiler
                  :target-rel "attachment.jpg" :target-abs (expand-file-name "attachment.jpg" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        ;; Note with hard dep on attachment - hash unchanged
        (puthash "note"
                 (porg-item-create
                  :id "note" :type "note" :item "n" :hash "note-hash"
                  :rule rule :compiler compiler
                  :target-rel "note.org" :target-abs (expand-file-name "note.org" root)
                  :hard-deps '("attachment") :soft-deps nil)
                 items)
        ;; Cache: attachment has old hash, note is up-to-date
        (porg-cache-put cache "attachment" (porg-cache-item-create :hash "old-hash" :output "attachment.jpg"
                                                                    :rule "test" :rule-hash rule-hash
                                                                    :compiler "test" :compiler-hash compiler-hash))
        (porg-cache-put cache "note" (porg-cache-item-create :hash "note-hash" :output "note.org"
                                                              :rule "test" :rule-hash rule-hash
                                                              :compiler "test" :compiler-hash compiler-hash))
        (let* ((project (porg-project-create
                         :name "hard-test"
                         :root root
                         :cache-file "cache"
                         :input (lambda () nil)
                         :rules (list rule)
                         :compilers (list compiler)))
               (plan (porg-build-plan project items cache))
               (build-list (plist-get plan :build)))
          ;; Attachment should rebuild because its hash changed
          (expect (member "attachment" build-list) :to-be-truthy)
          ;; Note should rebuild because its hard dep (attachment) changed
          (expect (member "note" build-list) :to-be-truthy)
          ;; Attachment must come before note in build order (due to hard-dep)
          (expect (seq-position build-list "attachment")
                  :to-be-less-than (seq-position build-list "note"))))))

  (describe "cache invalidation triggers"
    (it "rebuilds when item hash changes"
      (let* ((root (file-name-as-directory (make-temp-file "porg-hash-test" 'dir)))
             (rule (porg-rule :name "test" :match #'identity :outputs #'identity))
             (compiler (porg-compiler :name "test" :match #'identity))
             (rule-hash (porg-sha1sum rule))
             (compiler-hash (porg-sha1sum compiler))
             (items (make-hash-table :test 'equal))
             (cache (porg-test-make-cache)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "new-hash"
                  :rule rule :compiler compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (porg-cache-put cache "item" (porg-cache-item-create :hash "old-hash" :output "item.org"
                                                              :rule "test" :rule-hash rule-hash
                                                              :compiler "test" :compiler-hash compiler-hash))
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
             (cache (porg-test-make-cache)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule new-rule :compiler compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (porg-cache-put cache "item" (porg-cache-item-create :hash "same-hash" :output "item.org"
                                                              :rule "test" :rule-hash old-rule-hash
                                                              :compiler "test" :compiler-hash compiler-hash))
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
             (cache (porg-test-make-cache)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule rule :compiler new-compiler
                  :target-rel "item.org" :target-abs (expand-file-name "item.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (porg-cache-put cache "item" (porg-cache-item-create :hash "same-hash" :output "item.org"
                                                              :rule "test" :rule-hash rule-hash
                                                              :compiler "test" :compiler-hash old-compiler-hash))
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
             (cache (porg-test-make-cache)))
        (puthash "item"
                 (porg-item-create
                  :id "item" :type "note" :item "content" :hash "same-hash"
                  :rule rule :compiler compiler
                  :target-rel "new-path.org" :target-abs (expand-file-name "new-path.org" root)
                  :hard-deps nil :soft-deps nil)
                 items)
        (porg-cache-put cache "item" (porg-cache-item-create :hash "same-hash" :output "old-path.org"
                                                              :rule "test" :rule-hash rule-hash
                                                              :compiler "test" :compiler-hash compiler-hash))
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
              (file-name-nondirectory (vulpea-note-path note)))
       :soft-deps (mapcar (lambda (l) (plist-get l :dest))
                         (vulpea-note-links note)))))))

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
        vulpea-db-location (expand-file-name "vulpea.db" dir)
        vulpea-db-sync-directories (list dir)
        vulpea-default-notes-directory dir)
  (vulpea-db-autosync-mode +1)
  ;; Force a blocking sync to populate the database
  (vulpea-db-sync-update-directory dir t))

(defun porg-test-teardown ()
  "Teardown testing environment."
  (vulpea-db-autosync-mode -1)
  (when (file-exists-p vulpea-db-location)
    (delete-file vulpea-db-location)))



(describe "publicatorg"
  (before-all (porg-test-init))
  (after-all (porg-test-teardown))

  (before-each
    (spy-on 'porg-test-build-item :and-call-through)
    (spy-on 'porg-test-clean-item :and-call-through))

  (it "should build every item on the first run"
    (porg-run "porg-test")
    ;; 6 notes: note-1, note-2, note-3, hard-dep-source, hard-dep-target, note-with-meta
    (expect 'porg-test-build-item :to-have-been-called-times 6))

  (it "should not call any build function when project is unchanged"
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 0))

  (it "should call build functions only for changed items"
    (let ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab")))
      (vulpea-utils-with-note note
        (vulpea-buffer-meta-set "title" "yes")
        (save-buffer))
      (vulpea-db-update-file (vulpea-note-path note)))
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 1))

  (it "should call build function for transitively changed item"
    (let ((note (vulpea-db-get-by-id "2801a0b7-53fe-4128-8ea3-0e6344c4c64c")))
      (vulpea-utils-with-note note
        (vulpea-buffer-meta-set "dependants" "yes")
        (save-buffer))
      (vulpea-db-update-file (vulpea-note-path note)))
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 2))

  (it "should call build for newly created note"
    (vulpea-create "created note 1"
                   "created-note-1.org"
                   )
    (porg-run "porg-test")
    (expect 'porg-test-build-item :to-have-been-called-times 1))

  (it "should call cleanup for deleted note"
    (delete-file (expand-file-name "created-note-1.org" org-roam-directory))
    (vulpea-db-sync--cleanup-deleted-files)
    (porg-run "porg-test")
    (expect 'porg-test-clean-item :to-have-been-called-times 1))

  (it "should rebuild an item when dependency is deleted"
    (let ((note-1 (vulpea-create "created dep note 1"
                                 "created-dep-note-1.org")))
      (vulpea-create "created dep note 2"
                     "created-dep-note-2.org"
                     :body (concat "I depend on " (vulpea-utils-link-make-string note-1)))
      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 2)

      (spy-calls-reset 'porg-test-build-item)

      (delete-file (vulpea-note-path note-1))
      (vulpea-db-sync--cleanup-deleted-files)
      (porg-run "porg-test")
      (expect 'porg-test-clean-item :to-have-been-called-times 1)
      (expect 'porg-test-build-item :to-have-been-called-times 1)))

  (it "should rebuild an item when dependency is added"
    (vulpea-create "Note 4"
                   "note-4.org"
                   :id "fe2ba6c8-2af5-4bc7-a491-b5fadcb144e7"
                   )
    (porg-run "porg-test")
    ;; note-3 depends on previously non-existent fe2ba6c8-2af5-4bc7-a491-b5fadcb144e7
    (expect 'porg-test-build-item :to-have-been-called-times 2))

  (it "should delete and rebuild an item that changed output path"
    (let ((note (vulpea-create "Dancing note"
                               "dancing-note.org"
                               )))
      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 1)

      (spy-calls-reset 'porg-test-build-item)

      (vulpea-utils-with-note note
        (vulpea-buffer-prop-set "output-file" "moving-note.org")
        (save-buffer))

      (porg-run "porg-test")
      (expect 'porg-test-build-item :to-have-been-called-times 1)
      (expect 'porg-test-clean-item :to-have-been-called-times 1)))

  (describe "porg-make-outputs"
    (it "returns note output as the last item"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (outputs-fn (porg-make-outputs
                          :file (lambda (n) (concat "out/" (vulpea-note-id n) ".md"))))
             (outputs (funcall outputs-fn note)))
        ;; Note output should be last
        (expect (porg-rule-output-type (car (last outputs))) :to-equal "note")
        (expect (porg-rule-output-id (car (last outputs))) :to-equal (vulpea-note-id note))))

    (it "includes extra outputs before note output"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (outputs-fn (porg-make-outputs
                          :file (lambda (n) (concat "out/" (vulpea-note-id n) ".md"))
                          :outputs-extra
                          (lambda (note-output)
                            (list
                             (porg-rule-output
                              :id (concat (porg-rule-output-id note-output) ".json")
                              :type "json"
                              :item (porg-rule-output-item note-output)
                              :file (porg-file-name-replace-ext
                                     (porg-rule-output-file note-output) "json"))))))
             (outputs (funcall outputs-fn note))
             (json-output (seq-find (lambda (o) (string-equal (porg-rule-output-type o) "json"))
                                    outputs)))
        ;; Should have json output
        (expect json-output :to-be-truthy)
        (expect (porg-rule-output-file json-output) :to-match "\\.json$")))

    (it "applies soft-deps function to note"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (soft-dep-id "test-soft-dep-id")
             (outputs-fn (porg-make-outputs
                          :file (lambda (n) (concat "out/" (vulpea-note-id n) ".md"))
                          :soft-deps (lambda (n) (list soft-dep-id))))
             (outputs (funcall outputs-fn note))
             (note-output (car (last outputs))))
        (expect (member soft-dep-id (porg-rule-output-soft-deps note-output)) :to-be-truthy)))

    (it "applies hard-deps function to note"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (hard-dep-id "test-hard-dep-id")
             (outputs-fn (porg-make-outputs
                          :file (lambda (n) (concat "out/" (vulpea-note-id n) ".md"))
                          :hard-deps (lambda (n) (list hard-dep-id))))
             (outputs (funcall outputs-fn note))
             (note-output (car (last outputs))))
        (expect (member hard-dep-id (porg-rule-output-hard-deps note-output)) :to-be-truthy)))

    (it "adds extra attachment outputs to note hard-deps"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (extra-attachment-id "extra-attachment-id")
             (outputs-fn (porg-make-outputs
                          :file (lambda (n) (concat "out/" (vulpea-note-id n) ".md"))
                          :outputs-extra
                          (lambda (note-output)
                            (list
                             (porg-rule-output
                              :id extra-attachment-id
                              :type "attachment"
                              :item "/path/to/file.jpg"
                              :file "images/file.webp")))))
             (outputs (funcall outputs-fn note))
             (note-output (car (last outputs))))
        ;; Note should hard-depend on the extra attachment
        (expect (member extra-attachment-id (porg-rule-output-hard-deps note-output))
                :to-be-truthy))))

  (describe "porg-extract-content"
    (it "extracts content from note without meta section"
      (let* ((note (vulpea-db-get-by-id "f3a5264e-963f-4059-b497-934d6e7df1ab"))
             (content (porg-extract-content note)))
        ;; Should contain actual note content
        (expect content :to-match "Luckily, I depend on")
        ;; Should not contain properties or title
        (expect content :not :to-match ":PROPERTIES:")))

    (it "skips vulpea-meta section when present"
      (let* ((note (vulpea-db-get-by-id "a1b2c3d4-e5f6-7890-abcd-ef1234567890"))
             (content (porg-extract-content note)))
        ;; Should contain actual content
        (expect content :to-match "Content Section")
        (expect content :to-match "This is the actual content")
        ;; Should not contain metadata
        (expect content :not :to-match "category :: test")
        (expect content :not :to-match "priority :: high")))

    (it "returns empty string for note with only metadata"
      (let* ((note (vulpea-db-get-by-id "2801a0b7-53fe-4128-8ea3-0e6344c4c64c"))
             (content (porg-extract-content note)))
        ;; Note 2 has only PROPERTIES and title
        (expect (string-trim content) :to-equal "")))))



;; * Org to Markdown publishing tests

(describe "porg-clean-org-for-pandoc"
  (it "removes table captions before #+results:"
    (with-temp-buffer
      (insert "#+caption: Table title\n#+results:\n| a | b |\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :to-equal "#+results:\n| a | b |\n")))

  (it "keeps captions not followed by #+results:"
    (with-temp-buffer
      (insert "#+caption: Image caption\n[[file:image.png]]\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :to-match "\\+caption: Image caption")))

  (it "adds attr_html before file links with captions"
    (with-temp-buffer
      (insert "#+caption: Nice image\n[[file:photo.jpg]]\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :to-match "\\+attr_html: :class image")))

  (it "does not add attr_html for non-file links"
    (with-temp-buffer
      (insert "#+caption: A link\n[[https://example.com]]\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :not :to-match "\\+attr_html")))

  (it "converts verse blocks to HTML blockquotes"
    (with-temp-buffer
      (insert "#+begin_verse\nFirst line\nSecond line\n#+end_verse\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :to-match "\\+begin_export html")
      (expect (buffer-string) :to-match "<blockquote>")
      (expect (buffer-string) :to-match "</blockquote>")
      (expect (buffer-string) :to-match "<br/>")))

  (it "replaces --- with en-dash in verses"
    (with-temp-buffer
      (insert "#+begin_verse\n---\n#+end_verse\n")
      (porg-clean-org-for-pandoc)
      (expect (buffer-string) :to-match "–"))))

(describe "porg-clean-markdown-for-web"
  (it "removes file:// prefixes from links"
    (with-temp-buffer
      (insert "![](file:///images/photo.webp)\n")
      (porg-clean-markdown-for-web)
      (expect (buffer-string) :to-equal "![](/images/photo.webp)\n")))

  (it "converts video text links to image syntax"
    (with-temp-buffer
      (insert "[file:/content/test/video.mp4](/content/test/video.mp4)\n")
      (porg-clean-markdown-for-web)
      (expect (buffer-string) :to-equal "![](/content/test/video.mp4)\n")))

  (it "does not convert non-video links"
    (with-temp-buffer
      (insert "[file:/content/test/image.jpg](/content/test/image.jpg)\n")
      (porg-clean-markdown-for-web)
      ;; Should remain unchanged since jpg is not a video
      (expect (buffer-string) :to-match "\\[file:/content")))

  (it "uses custom video check function when provided"
    (with-temp-buffer
      (insert "[file:/content/test/movie.webm](/content/test/movie.webm)\n")
      (porg-clean-markdown-for-web (lambda (p) (string-suffix-p ".webm" p)))
      (expect (buffer-string) :to-equal "![](/content/test/movie.webm)\n"))))

(describe "porg-image-width"
  (before-each
    (porg-images-cache-clear))

  (it "returns 0 for non-existent file"
    (expect (porg-image-width "/non/existent/file.jpg") :to-equal 0))

  (it "caches result and avoids repeated shell calls"
    (let* ((temp-file (make-temp-file "porg-img-test" nil ".png"))
           (call-count 0))
      ;; Create a minimal valid PNG (1x1 pixel)
      (with-temp-file temp-file
        (insert (unibyte-string
                 #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a  ; PNG signature
                 #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52  ; IHDR chunk
                 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01  ; 1x1
                 #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53  ; 8-bit RGB
                 #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41  ; IDAT chunk
                 #x54 #x08 #xd7 #x63 #xf8 #xff #xff #x3f
                 #x00 #x05 #xfe #x02 #xfe #xdc #xcc #x59
                 #xe7 #x00 #x00 #x00 #x00 #x49 #x45 #x4e  ; IEND chunk
                 #x44 #xae #x42 #x60 #x82)))
      (unwind-protect
          (progn
            (spy-on 'shell-command-to-string :and-call-fake
                    (lambda (cmd)
                      (setq call-count (1+ call-count))
                      "100"))
            ;; First call should invoke shell
            (expect (porg-image-width temp-file) :to-equal 100)
            (expect call-count :to-equal 1)
            ;; Second call should use cache
            (expect (porg-image-width temp-file) :to-equal 100)
            (expect call-count :to-equal 1))
        (delete-file temp-file))))

  (it "invalidates cache when file is modified"
    (let* ((temp-file (make-temp-file "porg-img-test" nil ".png"))
           (call-count 0))
      (with-temp-file temp-file (insert "fake-image-1"))
      (unwind-protect
          (progn
            (spy-on 'shell-command-to-string :and-call-fake
                    (lambda (cmd)
                      (setq call-count (1+ call-count))
                      (format "%d" (* call-count 100))))
            ;; First call
            (expect (porg-image-width temp-file) :to-equal 100)
            (expect call-count :to-equal 1)
            ;; Modify file (changes mtime and size)
            (sleep-for 0.1)
            (with-temp-file temp-file (insert "fake-image-2-longer"))
            ;; Should call shell again due to changed mtime/size
            (expect (porg-image-width temp-file) :to-equal 200)
            (expect call-count :to-equal 2))
        (delete-file temp-file))))

  (it "clears cache with porg-images-cache-clear"
    (let* ((temp-file (make-temp-file "porg-img-test" nil ".png"))
           (call-count 0))
      (with-temp-file temp-file (insert "fake-image"))
      (unwind-protect
          (progn
            (spy-on 'shell-command-to-string :and-call-fake
                    (lambda (cmd)
                      (setq call-count (1+ call-count))
                      "100"))
            ;; First call
            (porg-image-width temp-file)
            (expect call-count :to-equal 1)
            ;; Clear cache
            (porg-images-cache-clear)
            ;; Should call shell again
            (porg-image-width temp-file)
            (expect call-count :to-equal 2))
        (delete-file temp-file)))))

(describe "porg-make-publish"
  (it "returns a function"
    (let ((publish-fn (porg-make-publish
                       :copy-fn #'ignore
                       :image-dir-fn (lambda (_) "test")
                       :sanitize-id-fn (lambda (_) #'identity))))
      (expect (functionp publish-fn) :to-be-truthy)))

  (it "uses default path-for-web-fn"
    ;; Just verify it doesn't error with defaults
    (let ((publish-fn (porg-make-publish
                       :copy-fn #'ignore
                       :image-dir-fn (lambda (_) "test")
                       :sanitize-id-fn (lambda (_) #'identity))))
      (expect publish-fn :to-be-truthy)))

  (it "uses default video-check-fn"
    ;; Just verify it doesn't error with defaults
    (let ((publish-fn (porg-make-publish
                       :copy-fn #'ignore
                       :image-dir-fn (lambda (_) "test")
                       :sanitize-id-fn (lambda (_) #'identity))))
      (expect publish-fn :to-be-truthy))))


(provide 'publicatorg-test)
;;; publicatorg-test.el ends here
