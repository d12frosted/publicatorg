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
           :file (file-name-nondirectory (vulpea-note-path note)))))))

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
   (porg-item-target-abs item)))

(defun porg-test-clean-item (cache-item root)
  "Clean CACHE-ITEM from ROOT."
  (delete-file
   (expand-file-name (porg-cache-item-output cache-item) root)))



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
        org-roam-db-location (expand-file-name "org-roam.db" dir)
        org-roam-database-connector 'sqlite-builtin)
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
    (spy-on 'porg-test-build-item)
    (spy-on 'porg-test-clean-item))

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
      (expect 'porg-test-build-item :to-have-been-called-times 1))))



(provide 'publicatorg-test)
;;; publicatorg-test.el ends here
