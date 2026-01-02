;;; publicatorg.el --- Make your vulpea notes public -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (emacsql "3.0.0") (vulpea "2.0.0") (org-ml "5.8"))
;;
;; Created: 08 Jul 2022
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
;; Documentation is to be done.
;;
;;; Code:

(require 'org-ml)
(require 'vulpea)



(defvar porg--projects nil)
(defvar porg-enable-cleanup t)
(defvar porg-detect-output-clashes nil)
(defvar porg-dry-run nil
  "When non-nil, show what would be built/cleaned but don't execute.
In dry-run mode, no files are created, modified, or deleted, and
the cache is not updated.")

(defvar porg-parallel nil
  "Control parallel builds.
When nil, builds are sequential (default).
When a positive integer, specifies the maximum number of parallel workers.
Items that don't depend on each other can be built in parallel.
Note: Requires Emacs 26+ for thread support.")

(cl-defgeneric porg-describe (thing)
  "Describe THING.")

(cl-defmethod porg-describe ((thing string))
  "Describe THING."
  thing)


(defvar porg-debug-input '("cbee3dd3-b997-4bec-96d3-99f483276e74"
                           "a2d55f77-3e0d-44b6-9253-dacf46184497"
                           "1a42758f-150a-4280-a980-f5897d69b54b"))
(defun porg-debug-input-p (item)
  "Return non-nil if ITEM is in the debug input list."
  (cond
   ((vulpea-note-p item) (seq-contains-p porg-debug-input (vulpea-note-id item)))
   ((porg-item-p item) (porg-debug-input-p (porg-item-item item)))))



(cl-defstruct (porg-project (:constructor porg-project-create)
                            (:copier porg-project-copy))
  (name nil :read-only t :type string)
  (root nil :read-only t :type string)
  (cache-file nil :read-only t :type string)
  (input nil :read-only t :type function)
  (describe #'porg-describe :read-only t :type function)
  (rules nil :type list)
  (compilers nil :type list))

(cl-defun porg-define (&rest args)
  "Smart constructor for `porg-project'.

ARGS are used to construct project."
  (let* ((project (apply #'porg-project-create args))
         (name (porg-project-name project)))
    (unless name (user-error "Missing `name'"))
    (unless (porg-project-root project) (user-error "Missing `root'"))
    (unless (porg-project-cache-file project) (user-error "Missing `cache-file'"))
    (unless (porg-project-input project) (user-error "Missing `input'"))
    (unless (porg-project-rules project) (user-error "Missing `rules'"))
    (unless (porg-project-compilers project) (user-error "Missing `compilers'"))
    (setf (porg-project-compilers project)
          (-snoc
           (porg-project-compilers project)
           (porg-compiler
            :name "$$void$$"
            :match (-rpartial #'porg-rule-output-that :type "$$void$$"))))
    (if-let* ((val (assoc name porg--projects)))
        (setf (cdr val) project)
      (setf porg--projects (cons (cons name project) porg--projects)))
    project))

(cl-defmethod porg-project-hash (project &optional ignore-rules)
  "Calculate hash of the PROJECT.

When IGNORE-RULES is non-nil, rules do not depend on resulting hash."
  (let ((project (porg-project-copy project)))
    (when ignore-rules
      (setf (porg-project-rules project) nil)
      (setf (porg-project-compilers project) nil))
    (porg-sha1sum project)))

(cl-defmethod porg-project-resolve-rules ((project porg-project) note)
  "Resolve rule for NOTE from PROJECT."
  (->> (porg-project-rules project)
       (-filter #'porg-rule-p)
       (--filter (when-let* ((match (porg-rule-match it)))
                   (funcall match note)))))

(cl-defmethod porg-project-resolve-compiler ((project porg-project) output)
  "Resolve compiler for OUTPUT from PROJECT."
  (-find
   (lambda (compiler)
     (when-let* ((match (porg-compiler-match compiler)))
       (funcall match output)))
   (-filter #'porg-compiler-p (porg-project-compilers project))))



(cl-defstruct (porg-compiler (:constructor porg-compiler)
                             (:copier nil))
  "Define a compiler for `porg-rule-output'.

MATCH is a predicate on `porg-rule-output' that control which
items are build using this rule."
  (name nil :read-only t :type string)
  (match nil :read-only t :type function)
  (build nil :read-only t :type function)
  (clean nil :read-only t :type function)
  (hash nil :read-only t :type function))

(cl-defstruct (porg-rule (:constructor porg-rule)
                         (:copier nil))
  "Define a rule.

NAME is a string, it must be unique in the scope of a single project.

MATCH is a predicate on `vulpea-note' that controls which notes
are built using this rule.

OUTPUTS is a function that takes single matched note and returns
list of `porg-rule-output'."
  (name nil :read-only t :type string)
  (match nil :read-only t :type function)
  (outputs nil :read-only t :type function))



(cl-defstruct (porg-rule-output (:constructor porg-rule-output)
                                (:copier nil))
  "Define a `porg-rule' output.

ID is a string identifier of the output item.

TYPE is a string representing type of the ITEM. Publicatorg uses
the TYPE to find suitable compiler. For example, TYPE can be
note, attachment, etc.

ITEM is an object that needs to be built.

FILE is the relative location of the output.

HARD-DEPS and SOFT-DEPS are lists of dependencies. When
dependency change, matched is considered as modified. Hard
dependency is strictly required for rule to succeed. Use
SOFT-DEPS when you simply want to make sure that this rule needs
to run again whenever soft dependency change. Use HARD-DEPS when
you want compilation to fail if dependency is missing.

Difference between hard and soft dependencies is that hard
dependencies declare what notes are required to build this rule."
  (id nil :read-only t :type string)
  (type nil :read-only t :type string)
  (item nil :read-only t)
  (file nil :read-only t :type string)
  (hard-deps nil :read-only t :type function)
  (soft-deps nil :read-only t :type function)
  (extra-args nil :read-only t))

(cl-defun porg-note-output (note &key file soft-deps hard-deps)
  "Make an output for NOTE.

Creates a `porg-rule-output' with:
- ID from the note's vulpea ID
- TYPE set to \"note\"
- ITEM set to NOTE itself

FILE is the relative output path.
SOFT-DEPS is a list of IDs that trigger rebuild when changed.
HARD-DEPS is a list of IDs that must be built before this note.

Note: Linked notes are NOT automatically added as soft-deps.
Callers should use `vulpea-note-links' if they want to depend on
linked notes."
  (porg-rule-output
   :id (vulpea-note-id note)
   :type "note"
   :item note
   :file file
   :hard-deps hard-deps
   :soft-deps soft-deps))

(cl-defun porg-attachments-output (note &key dir file-mod filter owner variants)
  "Make an list of attachments output for NOTE.

DIR can be either a string or a function that takes
attachment name and returns a string. For example, this can be
used to copy attachments to different destinations based on their
type.

FILE-MOD allows to modify output file name. It can be either a
function or a list of functions. In the latter case one
attachment can have multiple outputs.

FILTER controls which attachments get copied, it's a function that
takes attachment name and returns non-nil if attachment should be
copied. When FILTER-FN is not provided, all attachments are copied.

OWNER allows to steal attachments of one NOTE to another OWNER.

VARIANTS is a list of output alternatives. By each attachment has
one default output (with no extra args) and extra output for each
variant, which is passed as extra args in form of a
plist (:variant)."
  (->> note
       (vulpea-note-path)
       (emacsql (vulpea-db)
                [:select id
                 :from notes
                 :where (= path $s1)])
       (-map #'car)
       (vulpea-db-query-by-ids)
       (-mapcat
        (lambda (a)
          (->> a
               (vulpea-note-links)
               (--filter (and (string-equal (plist-get it :type) "attachment")
                              (or (not filter) (funcall filter (plist-get it :dest)))))
               (--map (plist-get it :dest))
               (--mapcat
                (let* ((dir (if (functionp dir) (funcall dir it) dir))
                       (newname (concat (file-name-as-directory dir) it))
                       (newnames (cond
                                  ((null file-mod) (list newname))
                                  ((functionp file-mod) (list (funcall file-mod newname)))
                                  ((listp file-mod) (--map (funcall it newname) file-mod)))))
                  (-map
                   (lambda (newname)
                     (cons
                      (porg-rule-output
                       :id (concat (vulpea-note-id (or owner note)) ":" (file-name-nondirectory newname))
                       :type "attachment"
                       :item (expand-file-name it (vulpea-note-attach-dir a))
                       :file newname)
                      (-map
                       (lambda (variant)
                         (porg-rule-output
                          :id (concat (vulpea-note-id (or owner note)) ":"
                                      (file-name-nondirectory newname)
                                      "@" (number-to-string variant))
                          :type "attachment"
                          :item (expand-file-name it (vulpea-note-attach-dir a))
                          :file (porg-file-name-set-variant newname variant)
                          :extra-args `(:variant ,variant)))
                       variants)))
                   newnames)))
               (-flatten))))
       (funcall
        (lambda (list)
          (let ((-compare-fn (lambda (a b)
                               (string-equal (porg-rule-output-id a)
                                             (porg-rule-output-id b)))))
            (-distinct list))))))

(cl-defun porg-void-output (note)
  "Make a void output for NOTE."
  (porg-rule-output
   :id (vulpea-note-id note)
   :type "$$void$$"
   :item note))

(cl-defmethod porg-rule-output-that ((output porg-rule-output) &key type predicate)
  "Check that OUTPUT has TYPE and satisfies PREDICATE (optional)."
  (and (string-equal (porg-rule-output-type output) type)
       (or (not predicate) (funcall predicate (porg-rule-output-item output)))))


;; * Output generators

(cl-defun porg-make-outputs (&key file
                                   attach-dir
                                   attach-filter
                                   attach-file-mod
                                   soft-deps
                                   hard-deps
                                   outputs-extra)
  "Create an outputs function that combines note and attachment outputs.

This is a convenience wrapper around `porg-note-output' and
`porg-attachments-output' that handles common patterns:
- Creates note output at FILE path
- Creates attachment outputs with optional filtering
- Creates extra outputs (e.g., JSON metadata)
- Sets up hard dependencies so note depends on its attachments

Arguments:
FILE is a function (note -> string) returning relative output path.

ATTACH-DIR is a function (note -> string) returning relative
directory for attachments. When nil, attachments are skipped.

ATTACH-FILTER is a predicate on attachment file name. Controls
which attachments are included. Defaults to `porg-supported-media-p'.

ATTACH-FILE-MOD is a function or list of functions to modify
attachment file names. Common use: `porg-file-name-for-web'.

SOFT-DEPS is a function (note -> list) returning soft dependencies.

HARD-DEPS is a function (note -> list) returning hard dependencies.

OUTPUTS-EXTRA is a function (note-output -> list) that takes a
preliminary note output (with ID and file, but no deps) and returns
additional outputs (e.g., JSON files). These extra outputs can
declare their own dependencies.

Returns a function suitable for use as :outputs in `porg-rule'.

Dependency behavior:
- The note output hard-depends on all attachment IDs
- Extra outputs with type \"attachment\" are also added as hard deps
- This ensures attachments are built before the note that references them"
  (lambda (note)
    (let* ((file-path (funcall file note))
           ;; Create preliminary note-output for outputs-extra callback
           (note-output-preview (porg-note-output note :file file-path))
           (attachment-outputs
            (when attach-dir
              (porg-attachments-output
               note
               :dir (lambda (_attachment)
                      (funcall attach-dir note))
               :file-mod (or attach-file-mod (list #'porg-file-name-for-web))
               :filter (or attach-filter #'porg-supported-media-p))))
           (extra-outputs (when outputs-extra
                            (funcall outputs-extra note-output-preview)))
           (extra-attachment-ids
            (mapcar #'porg-rule-output-id
                    (seq-filter
                     (lambda (it)
                       (string-equal (porg-rule-output-type it) "attachment"))
                     extra-outputs))))
      (append
       attachment-outputs
       extra-outputs
       (list
        (porg-note-output
         note
         :file file-path
         :soft-deps (when soft-deps (funcall soft-deps note))
         :hard-deps (append
                     (when hard-deps (funcall hard-deps note))
                     (mapcar #'porg-rule-output-id attachment-outputs)
                     extra-attachment-ids)))))))


(cl-defstruct (porg-batch-rule (:constructor porg-batch-rule)
                               (:copier nil))
  "Define a batch rule with NAME.

NAME is a string, it must be unique in the scope of a single project.

FILTER is a predicate on `vulpea-note' that controls which notes
are passed to PUBLISH function.

TARGET is a relative file path, describes output of this rule. It
can be a function that takes list of matched notes.

PUBLISH is a function the defines how publishing happens. It
takes 4 arguments: notes (that were filtered from project input
based on FILTER), target file, input (as calculated by
`porg-build-input') and cache."
  name
  filter
  target
  publish)



(defvar porg-cache-backend 'file
  "Cache backend to use.

Can be one of:
- `file' - serialize to a file using `porg-cache-file-method'
- `sqlite' - use SQLite database for incremental updates")

;; Legacy alias for backward compatibility - must be before defvar
(define-obsolete-variable-alias 'porg-cache-method 'porg-cache-file-method "0.1")

(defvar porg-cache-file-method 'pp
  "Serialization method for file-based cache.

Can be one of:
- `pp' - pretty print as Lisp object (human and git-friendly, but slow)
- `prin1' - compact Lisp object (fast, but single line)")

(cl-defstruct (porg-cache-item (:constructor porg-cache-item-create))
  (hash nil :type string)
  (output nil :type string)
  (project-hash nil :type string)
  (rule nil :type string)
  (rule-hash nil :type string)
  (compiler nil :type string)
  (compiler-hash nil :type string))

;; Cache handle structure
(cl-defstruct (porg-cache (:constructor porg-cache--create))
  "Cache handle for abstracting storage backend."
  (backend nil :type symbol)
  (file nil :type string)
  (data nil))

;; Generic cache operations
(cl-defgeneric porg-cache-open (backend file)
  "Open cache FILE using BACKEND.")

(cl-defgeneric porg-cache-get (cache id)
  "Get value for ID from CACHE.")

(cl-defgeneric porg-cache-put (cache id value)
  "Put VALUE for ID into CACHE.")

(cl-defgeneric porg-cache-remove (cache id)
  "Remove ID from CACHE.")

(cl-defgeneric porg-cache-keys (cache)
  "Return all keys in CACHE.")

(cl-defgeneric porg-cache-sync (cache)
  "Sync CACHE to persistent storage.")

(cl-defgeneric porg-cache-close (cache)
  "Close CACHE and release resources.")

;; File backend implementation
(cl-defmethod porg-cache-open ((_backend (eql file)) file)
  "Open file-based cache from FILE."
  (let ((data (if (file-exists-p file)
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))
                    (error
                     (message "Could not read cache from %s, starting fresh" file)
                     (make-hash-table :test 'equal)))
                (make-hash-table :test 'equal))))
    (porg-cache--create :backend 'file :file file :data data)))

;; SQLite backend implementation
(cl-defmethod porg-cache-open ((_backend (eql sqlite)) file)
  "Open SQLite-based cache from FILE."
  (let* ((db-file (concat file ".db"))
         (db (emacsql-sqlite-open db-file)))
    ;; Create table if not exists
    (emacsql db [:create-table-if-not-exists cache
                 ([(id text :primary-key)
                   (hash text)
                   (output text)
                   (project_hash text)
                   (rule text)
                   (rule_hash text)
                   (compiler text)
                   (compiler_hash text)])])
    (porg-cache--create :backend 'sqlite :file db-file :data db)))

;; Unified cache operations (dispatch based on backend)
(cl-defmethod porg-cache-get ((cache porg-cache) id)
  "Get value for ID from CACHE."
  (pcase (porg-cache-backend cache)
    ('file (gethash id (porg-cache-data cache)))
    ('sqlite
     (let ((row (car (emacsql (porg-cache-data cache)
                              [:select [hash output project_hash rule rule_hash compiler compiler_hash]
                               :from cache
                               :where (= id $s1)]
                              id))))
       (when row
         (porg-cache-item-create
          :hash (nth 0 row)
          :output (nth 1 row)
          :project-hash (nth 2 row)
          :rule (nth 3 row)
          :rule-hash (nth 4 row)
          :compiler (nth 5 row)
          :compiler-hash (nth 6 row)))))))

(cl-defmethod porg-cache-put ((cache porg-cache) id value)
  "Put VALUE for ID into CACHE."
  (pcase (porg-cache-backend cache)
    ('file (puthash id value (porg-cache-data cache)))
    ('sqlite
     (emacsql (porg-cache-data cache)
              [:insert-or-replace :into cache
               :values $v1]
              (vector id
                      (porg-cache-item-hash value)
                      (porg-cache-item-output value)
                      (porg-cache-item-project-hash value)
                      (porg-cache-item-rule value)
                      (porg-cache-item-rule-hash value)
                      (porg-cache-item-compiler value)
                      (porg-cache-item-compiler-hash value))))))

(cl-defmethod porg-cache-remove ((cache porg-cache) id)
  "Remove ID from CACHE."
  (pcase (porg-cache-backend cache)
    ('file (remhash id (porg-cache-data cache)))
    ('sqlite
     (emacsql (porg-cache-data cache)
              [:delete :from cache :where (= id $s1)]
              id))))

(cl-defmethod porg-cache-keys ((cache porg-cache))
  "Return all keys in CACHE."
  (pcase (porg-cache-backend cache)
    ('file (hash-table-keys (porg-cache-data cache)))
    ('sqlite
     (mapcar #'car (emacsql (porg-cache-data cache)
                            [:select id :from cache])))))

(cl-defmethod porg-cache-sync ((cache porg-cache))
  "Sync CACHE to persistent storage."
  (pcase (porg-cache-backend cache)
    ('file
     (when (porg-cache-file cache)
       (with-temp-file (porg-cache-file cache)
         (let ((print-level nil)
               (print-length nil))
           (pcase porg-cache-file-method
             (`pp (pp (porg-cache-data cache) (current-buffer)))
             (`prin1 (prin1 (porg-cache-data cache) (current-buffer)))
             (_ (prin1 (porg-cache-data cache) (current-buffer))))))))
    ('sqlite
     ;; SQLite writes are immediate, nothing to do
     nil)))

(cl-defmethod porg-cache-close ((cache porg-cache))
  "Close CACHE and release resources."
  (pcase (porg-cache-backend cache)
    ('file (porg-cache-sync cache))
    ('sqlite (emacsql-close (porg-cache-data cache)))))

;; Convenience function for opening cache
(defun porg-cache-open-default (file)
  "Open cache at FILE using the configured backend."
  (porg-cache-open porg-cache-backend file))

;; Legacy compatibility functions
(cl-defun porg-cache-query (cache id access)
  "Query CACHE for ID by ACCESS.
CACHE can be a hash-table (legacy) or porg-cache struct."
  (let ((item (if (porg-cache-p cache)
                  (porg-cache-get cache id)
                (gethash id cache))))
    (when item
      (funcall access item))))

(cl-defun porg-cache-load (file)
  "Load build cache from FILE.
Return a porg-cache handle."
  (porg-cache-open-default file))

(cl-defun porg-cache-write (file cache)
  "Write build CACHE to FILE.
CACHE can be a hash-table (legacy) or porg-cache struct."
  (if (porg-cache-p cache)
      (porg-cache-sync cache)
    ;; Legacy: raw hash-table
    (with-temp-file file
      (let ((print-level nil)
            (print-length nil))
        (pcase porg-cache-file-method
          (`pp (pp cache (current-buffer)))
          (_ (prin1 cache (current-buffer))))))))

(defun porg-cache-needs-sync-p (cache)
  "Return non-nil if CACHE requires explicit sync to persist.
File backend needs periodic sync; SQLite persists on each put."
  (and (porg-cache-p cache)
       (eq (porg-cache-backend cache) 'file)))



(defmacro porg-benchmark-run (n fn &optional to-str &rest args)
  "Benchmark FN by running it N times with ARGS.

It also prints some possibly useful information. Result of the
last FN evaluation is converted to string using provided TO-STR
function.

Return a list of the last FN evaluation result, the total elapsed
time for execution, the number of garbage collections that ran,
and the time taken by garbage collection. See also
`benchmark-run-compiled'."
  (declare (indent 1) (debug t))
  `(let* ((v)
          (result)
          (name (if (symbolp ,fn)
                    (symbol-name ,fn)
                  "*lambda*")))
    (porg-log "[%s] begin benchmark with %s invocations"
     name ,n)
    (setq result
     (benchmark-run ,n
      (setq v (funcall ,fn ,@args))))
    (porg-log "[%s] benchmark result is %s after %s invocations%s"
     name result ,n
     (if ,to-str
         (concat " => " (string-from (funcall ,to-str v)))
       ""))
    v))



;;;###autoload
(defun porg-run (name)
  "Export project with NAME."
  (let ((project (assoc-default name porg--projects)))
    (unless project
      (user-error "Could not find project named '%s'" name))
    (porg-log-s "calculating build plan")
    (let ((default-directory (porg-project-root project)))
      (porg-log "loading cache")
      (let* ((cache-file (expand-file-name (porg-project-cache-file project)
                                           (porg-project-root project)))
             (cache (porg-benchmark-run 1 #'porg-cache-load nil cache-file))
             (describe (porg-project-describe project))
             (items (porg-benchmark-run 1 #'porg-build-items nil project))
             (plan (porg-benchmark-run 1 #'porg-build-plan nil project items cache))
             (build-size (seq-length (plist-get plan :build)))
             (delete-size (seq-length (plist-get plan :delete)))
             (batch-rules (-filter #'porg-batch-rule-p
                                   (porg-project-rules project)))
             (project-hash (porg-project-hash project 'ignore-rules)))

        (porg-log-s (if porg-dry-run "cleanup (dry-run)" "cleanup"))
        (unless (plist-get plan :delete)
          (porg-log "Nothing to delete, everything is used."))
        (when porg-enable-cleanup
          (condition-case err
              (progn
                (--each-indexed (plist-get plan :delete)
                  (when-let* ((cached (porg-cache-get cache it))
                              (compiler-name (porg-cache-item-compiler cached))
                              (compiler (--find (string-equal compiler-name (porg-compiler-name it))
                                                (porg-project-compilers project))))
                    (porg-log
                     "[%s/%s]%s cleaning %s using %s rule from %s"
                     (porg-string-from-number (+ 1 it-index) :padding-num delete-size)
                     delete-size
                     (if porg-dry-run " [dry-run]" "")
                     it
                     compiler-name
                     (porg-cache-item-output cached))
                    (unless porg-dry-run
                      (when (porg-compiler-clean compiler)
                        (funcall (porg-compiler-clean compiler)
                                 (expand-file-name
                                  (porg-cache-item-output cached)
                                  (porg-project-root project))))
                      (porg-cache-remove cache it))))
                ;; update cache after we cleaned everything
                (when (and (not porg-dry-run) (porg-cache-needs-sync-p cache))
                  (porg-cache-write cache-file cache)))
            (error
             ;; on any error, still write out what we have so far
             (when (and (not porg-dry-run) (porg-cache-needs-sync-p cache))
               (message "Cleanup failed, saving partial cache...")
               (porg-cache-write cache-file cache))
             (signal (car err) (cdr err)))))

        (porg-log-s (cond
                     (porg-dry-run "build (dry-run)")
                     (porg-parallel (format "build (parallel, %d workers)" porg-parallel))
                     (t "build")))
        (unless (plist-get plan :build)
          (porg-log "Nothing to build, everything is up to date."))
        (condition-case err
            (if (and porg-parallel (not porg-dry-run) (plist-get plan :build))
                ;; Parallel build mode
                (let* ((build-ids (plist-get plan :build))
                       (build-graph (--map (cons it (porg-item-hard-deps (gethash it items)))
                                           build-ids))
                       (levels (porg-topological-levels build-graph))
                       (total-built 0)
                       (all-errors nil))
                  (porg-log "Building in %d parallel levels" (length levels))
                  (dolist (level-ids levels)
                    (let* ((level-items (--map (gethash it items) level-ids)))
                      ;; Log items in this level
                      (dolist (item level-items)
                        (cl-incf total-built)
                        (porg-log
                         "[%s/%s] (%s:%s) building %s"
                         (porg-string-from-number total-built :padding-num build-size)
                         build-size
                         (porg-rule-name (porg-item-rule item))
                         (porg-compiler-name (porg-item-compiler item))
                         (funcall describe item)))
                      ;; Build level in parallel
                      (let ((errors (porg--build-level-parallel
                                     level-items items cache describe
                                     project-hash porg-parallel)))
                        (when errors
                          (setq all-errors (append errors all-errors))))
                      ;; Write cache after each level (file backend only)
                      (when (porg-cache-needs-sync-p cache)
                        (porg-cache-write cache-file cache))))
                  ;; Report any errors
                  (when all-errors
                    (porg-log "Build completed with %d errors:" (length all-errors))
                    (dolist (err all-errors)
                      (porg-log "  %s: %s" (car err) (cdr err)))))
              ;; Sequential build mode (original behavior)
              (progn
                (--each-indexed (plist-get plan :build)
                  (let* ((item (gethash it items))
                         (rule (porg-item-rule item))
                         (compiler (porg-item-compiler item)))
                    (porg-log
                     "[%s/%s]%s (%s:%s) building %s"
                     (porg-string-from-number (+ 1 it-index) :padding-num build-size)
                     build-size
                     (if porg-dry-run " [dry-run]" "")
                     (porg-rule-name rule)
                     (porg-compiler-name compiler)
                     (funcall describe item))
                    (unless porg-dry-run
                      (when-let* ((build (porg-compiler-build compiler)))
                        (funcall build item items cache))
                      (porg-cache-put cache
                                      (porg-item-id item)
                                      (porg-cache-item-create
                                       :hash (porg-item-hash item)
                                       :output (porg-item-target-rel item)
                                       :project-hash project-hash
                                       :rule (porg-rule-name rule)
                                       :rule-hash (porg-sha1sum rule)
                                       :compiler (porg-compiler-name compiler)
                                       :compiler-hash (porg-sha1sum compiler)))
                      (when (and (porg-cache-needs-sync-p cache)
                                 (> it-index 0)
                                 (= (% it-index 100) 0))
                        (porg-cache-write cache-file cache)))))
                (when (and (not porg-dry-run)
                           (porg-cache-needs-sync-p cache)
                           (plist-get plan :build))
                  (porg-cache-write cache-file cache))))
          (error
           ;; on any error, still write out what we have so far
           (when (and (not porg-dry-run) (porg-cache-needs-sync-p cache))
             (message "Build failed, saving partial cache...")
             (porg-cache-write cache-file cache))
           (signal (car err) (cdr err))))

        (porg-log-s (if porg-dry-run "run batch actions (dry-run)" "run batch actions"))
        (unless batch-rules
          (porg-log "No batch actions to run"))
        (--each-indexed batch-rules
          (let* ((filter (porg-batch-rule-filter it))
                 (items-selected (hash-table-values items))
                 (items-selected (if filter (funcall #'-filter filter items-selected) items-selected))
                 (size (seq-length items-selected))
                 (items-selected (let ((tbl (make-hash-table :test 'equal :size size)))
                                   (--each items-selected (puthash (porg-item-id it) it tbl))
                                   tbl))
                 (target (porg-batch-rule-target it))
                 (target (if (functionp target) (funcall target items) target)))
            (porg-log
             "[%s/%s]%s running %s batch action on the set of %s notes"
             (porg-string-from-number (+ 1 it-index) :padding-num (seq-length batch-rules))
             (seq-length batch-rules)
             (if porg-dry-run " [dry-run]" "")
             (porg-batch-rule-name it)
             size)
            (unless porg-dry-run
              (funcall (porg-batch-rule-publish it) target items-selected items cache))))

        (unless porg-dry-run
          (porg-cache-write cache-file cache))
        (porg-cache-close cache)
        (if porg-dry-run
            (porg-log "Dry run complete! No changes were made.")
          (porg-log "The work is done! Enjoy your published vulpea notes!")
          (porg-log "        ٩(^ᴗ^)۶"))))))

;;;###autoload
(defun porg-run-dry (name)
  "Export project with NAME in dry-run mode.
Shows what would be built/cleaned without actually executing anything.
This is equivalent to (let ((porg-dry-run t)) (porg-run NAME))."
  (let ((porg-dry-run t))
    (porg-run name)))

(defvar porg--build-mutex nil
  "Mutex for thread-safe cache updates during parallel builds.")

(defun porg--build-item (item items cache _describe project-hash cache-mutex)
  "Build a single ITEM with thread safety.
ITEMS is the hash table of all items.
CACHE is the build cache.
PROJECT-HASH is the project hash.
CACHE-MUTEX is the mutex for cache updates (nil for sequential builds)."
  (let* ((rule (porg-item-rule item))
         (compiler (porg-item-compiler item)))
    (condition-case err
        (progn
          (when-let* ((build (porg-compiler-build compiler)))
            (funcall build item items cache))
          (let ((cache-entry (porg-cache-item-create
                              :hash (porg-item-hash item)
                              :output (porg-item-target-rel item)
                              :project-hash project-hash
                              :rule (porg-rule-name rule)
                              :rule-hash (porg-sha1sum rule)
                              :compiler (porg-compiler-name compiler)
                              :compiler-hash (porg-sha1sum compiler))))
            (if cache-mutex
                (with-mutex cache-mutex
                  (porg-cache-put cache (porg-item-id item) cache-entry))
              (porg-cache-put cache (porg-item-id item) cache-entry)))
          nil) ; return nil for success
      (error
       (cons (porg-item-id item) err))))) ; return error info

(defun porg--build-level-parallel (level items cache describe project-hash max-workers)
  "Build ITEMS in LEVEL in parallel with at most MAX-WORKERS threads.
Returns list of errors (each is (id . error))."
  (let* ((mutex (make-mutex "porg-build"))
         (threads nil)
         (errors nil)
         (errors-mutex (make-mutex "porg-errors"))
         (semaphore (make-mutex "porg-semaphore"))
         (running 0))
    ;; Start threads for each item, respecting max-workers
    (dolist (item level)
      ;; Wait if we're at max capacity
      (while (>= running max-workers)
        (thread-yield)
        (sleep-for 0.01))
      ;; Start a new thread
      (with-mutex semaphore
        (cl-incf running))
      (push
       (make-thread
        (lambda ()
          (unwind-protect
              (let ((err (porg--build-item item items cache describe project-hash mutex)))
                (when err
                  (with-mutex errors-mutex
                    (push err errors))))
            (with-mutex semaphore
              (cl-decf running)))))
       threads))
    ;; Wait for all threads to complete
    (dolist (thread threads)
      (thread-join thread))
    errors))



(cl-defstruct (porg-item (:constructor porg-item-create)
                         (:copier nil))
  id
  type
  item
  hash
  rule
  compiler
  target-abs
  target-rel
  soft-deps
  hard-deps
  extra-args)

(cl-defmethod porg-item-deps ((item porg-item))
  "Return dependencies of ITEM."
  (let ((hard-deps (porg-item-hard-deps item))
        (soft-deps (porg-item-soft-deps item))
        (-compare-fn #'string-equal))
    (-distinct (-concat hard-deps soft-deps))))

(cl-defmethod porg-item-that ((item porg-item) &key type predicate)
  "Check that ITEM has TYPE and satisfies PREDICATE (optional)."
  (and (string-equal (porg-item-type item) type)
       (or (not predicate) (funcall predicate (porg-item-item item)))))


(cl-defmethod porg-describe ((thing porg-item))
  "Describe THING."
  (format "(item %s) %s" (porg-item-type thing) (porg-item-id thing)))

(cl-defmethod porg-describe ((thing porg-rule-output))
  "Describe THING."
  (format "(output %s) %s" (porg-rule-output-type thing) (porg-rule-output-id thing)))



(defun porg-build-items (project)
  "Calculate PROJECT items to build.

Result is a table, where key is note id and the value is `porg-item'.

Throws a user error if any of the input has no matching rule."
  (porg-log "querying notes to build")
  (let* ((describe (porg-project-describe project))
         (input (porg-project-input project))
         (input (if (functionp input) (funcall input) input))
         (size (seq-length input))
         (without-rule nil)
         (without-compiler nil)
         (tbl (make-hash-table :test 'equal :size size))
         (logf #'porg-debug))

    (porg-log "found %s notes to resolve." size)

    (--each input
      (if (porg-debug-input-p it)
          (setq logf #'porg-log)
        (setq logf #'porg-debug))
      (funcall logf "- outputs of %s:" (funcall describe it))
      (if-let* ((rules (porg-project-resolve-rules project it)))
          (-each rules
            (lambda (rule)
              (--each (when-let* ((outputs-fn (porg-rule-outputs rule)))
                        (funcall outputs-fn it))
                (funcall logf "  - [%s] %s" (porg-rule-name rule) (funcall describe it))
                (if-let* ((compiler (porg-project-resolve-compiler project it)))
                    (let* ((target-rel (porg-rule-output-file it))
                           (target-abs (when target-rel
                                         (expand-file-name target-rel (porg-project-root project)))))
                      (when porg-detect-output-clashes
                        (when-let* ((old (gethash (porg-rule-output-id it) tbl)))
                          (user-error "Clash of output items by id '%s'" (porg-rule-output-id it))))
                      (funcall logf "    - rel: %s" target-rel)
                      (funcall logf "    - abs: %s" target-abs)
                      (funcall logf "    - hard deps:")
                      (--each (porg-rule-output-hard-deps it)
                        (funcall logf "      - %s" (funcall describe it)))
                      (funcall logf "    - soft deps:")
                      (--each (porg-rule-output-soft-deps it)
                        (funcall logf "      - %s" (funcall describe it)))
                      (puthash
                       (porg-rule-output-id it)
                       (porg-item-create
                        :id (porg-rule-output-id it)
                        :type (porg-rule-output-type it)
                        :item (porg-rule-output-item it)
                        :hash (funcall (or (porg-compiler-hash compiler)
                                           #'porg-sha1sum)
                                       it)
                        :rule rule
                        :compiler compiler
                        :target-abs target-abs
                        :target-rel target-rel
                        :hard-deps (--map (if (vulpea-note-p it) (vulpea-note-id it) it)
                                          (porg-rule-output-hard-deps it))
                        :soft-deps (--map (if (vulpea-note-p it) (vulpea-note-id it) it)
                                          (porg-rule-output-soft-deps it))
                        :extra-args (porg-rule-output-extra-args it))
                       tbl))
                  (setf without-compiler (cons it without-compiler))))))
        (setf without-rule (cons it without-rule))))

    (porg-log "found %s items to resolve" (seq-length (hash-table-keys tbl)))

    ;; quit if not all input can be handled by this project rules or compilers
    (when without-rule
      (porg-log "could not find rule for %s notes:" (seq-length without-rule))
      (--each without-rule
        (porg-log "- %s" (funcall describe it)))
      (user-error "Not all input notes have matching rules, see above"))
    (when without-compiler
      (porg-log "could not find compiler for %s items:" (seq-length without-compiler))
      (--each without-compiler
        (porg-log "- %s" (funcall describe it)))
      (user-error "Not all items have matching compilers, see above"))

    (when-let* ((missing (--filter
                          (--remove (gethash it tbl) (porg-item-hard-deps it))
                          (hash-table-values tbl))))
      (porg-log "could not find dependencies for %s items" (seq-length missing))
      (-each missing
        (lambda (item)
          (porg-log "missing hard dependencies of %s:" (funcall describe item))
          (--each (--remove (gethash it tbl) (porg-item-hard-deps item))
            (porg-log "- %s" (funcall describe it)))))
      (user-error "Missing some hard dependencies, see above"))

    tbl))



(defun porg-build-plan (project items cache)
  "Calculate build plan of ITEMS for PROJECT with CACHE.

Result is a property list (:compile :delete)."
  (let* ((describe (porg-project-describe project))
         ;; project-hash currently unused - was for project-level cache invalidation
         ;; (_project-hash (porg-project-hash project 'ignore-rules))
         (build-map (let ((tbl (make-hash-table :test 'equal
                                                :size (hash-table-size items))))
                      (--each (hash-table-keys items)
                        (let* ((item (gethash it items))
                               (cache-item (porg-cache-get cache it))
                               (rule (porg-item-rule item))
                               (compiler (porg-item-compiler item)))
                          (puthash
                           it
                           (or
                            ;; no cache information
                            (not cache-item)

                            ;; project changed
                            ;; (let ((res (not (string-equal project-hash (porg-cache-item-project-hash cache-item)))))
                            ;;   (when res (porg-debug "%s: project changed" (funcall describe item)))
                            ;;   res)

                            ;; rule changed
                            (let ((res (not (string-equal (porg-sha1sum rule)
                                                          (porg-cache-item-rule-hash cache-item)))))
                              (when res (porg-debug "%s: rule %s changed"
                                                    (funcall describe item)
                                                    (porg-rule-name rule)))
                              res)
                            ;; compiler changed
                            (let ((res (not (string-equal (porg-sha1sum compiler)
                                                          (porg-cache-item-compiler-hash cache-item)))))
                              (when res (porg-debug "%s: compiler %s changed"
                                                    (funcall describe item)
                                                    (porg-compiler-name compiler)))
                              res)
                            ;; item itself is changed
                            (let ((res (not (string-equal (porg-item-hash item)
                                                          (porg-cache-item-hash cache-item)))))
                              (when res (porg-debug "%s: content changed" (funcall describe item)))
                              res)

                            ;; item has moved (might happen without change in the item itself)
                            (let ((res (not
                                        (string-equal
                                         (porg-item-target-rel item)
                                         (porg-cache-query cache it #'porg-cache-item-output)))))
                              (when res (porg-debug "%s: target changed" (funcall describe item)))
                              res)

                            ;; one of the deps is changed
                            (-any-p
                             (lambda (a-id)
                               (let ((a (gethash a-id items)))
                                 (let ((res (if a
                                                ;; changed
                                                (not (string-equal (porg-item-hash a)
                                                                   (porg-cache-query cache a-id #'porg-cache-item-hash)))
                                              ;; removed
                                              (porg-cache-query cache a-id #'porg-cache-item-hash))))
                                   (when res
                                     (porg-debug "%s: dependency %s changed"
                                                 (funcall describe item)
                                                 (if a (funcall describe a) a-id)))
                                   res)))
                             (porg-item-deps item)))
                           tbl)))
                      tbl))
         (build-raw (-filter (lambda (it) (gethash it build-map))
                             (hash-table-keys build-map)))
         (build-sorted (porg-topological-sort
                        (--map
                         (cons it (porg-item-hard-deps (gethash it items)))
                         build-raw)
                        :test 'equal))
         (build (if (nth 1 build-sorted)
                    (-filter (lambda (it) (gethash it build-map)) (car build-sorted))
                  (porg-log "Can't sort topologically by hard dependencies due to cycles.")
                  build-raw))
         (delete (--remove
                  (when-let* ((item (gethash it items)))
                    (if-let* ((target-old (porg-cache-query cache (porg-item-id item) #'porg-cache-item-output)))
                        (string-equal target-old (porg-item-target-rel item))
                      item))
                  (porg-cache-keys cache))))

    (porg-log "found %s items to compile." (seq-length build))
    (porg-log "found %s items to delete." (seq-length delete))

    (list
     :build build
     :delete delete)))



;; Publish utilities. Use these functions in your rule definition.

(cl-defun porg-clean-noexport-headings (file)
  "Remove headings tagged as noexport from FILE."
  (with-current-buffer (find-file-noselect file)
    (seq-each
     (lambda (hl)
       (when (seq-contains-p (org-ml-get-property :tags hl) "noexport")
         (delete-region (org-ml-get-property :begin hl)
                        (org-ml-get-property :end hl))))
     (seq-reverse (org-element-map (org-element-parse-buffer) 'headline #'identity)))
    (save-buffer)))

(cl-defun porg-clean-links-in-buffer (&key sanitize-id-fn
                                           sanitize-attachment-fn
                                           sanitize-file-fn)
  "Clean links in current buffer.

Each link in the buffer is modified based on its type.

If it's ID link, then it's kept as link unconditionally.
SANITIZE-ID-FN is called with link to allow custom modifications.

If it's ATTACHMENT link, then it's kept as link unconditionally.
SANITIZE-ATTACHMENT-FN is called with link to allow custom
modifications.

If it's FILE link, then it's kept as link unconditionally.
SANITIZE-FILE-FN is called with link to allow custom modifications.

If it's HTTPS or MAILTO link, then it's kept as is without modifications.

All other links are transformed to plain text."
  (-> (seq-reverse (org-element-map (org-element-parse-buffer) 'link #'identity))
      (--each
          (org-ml-update
           (lambda (link)
             (let ((type (org-ml-get-property :type link)))
               (cond
                ((seq-contains-p '("https") type) link)
                ((seq-contains-p '("mailto") type) link)

                ((string-equal type "attachment")
                 (if sanitize-attachment-fn (funcall sanitize-attachment-fn link) link))

                ((string-equal type "file")
                 (if sanitize-file-fn (funcall sanitize-file-fn link) link))

                ((string-equal type "id")
                 (if sanitize-id-fn (funcall sanitize-id-fn link) link))

                (t (org-ml-from-string
                    'plain-text
                    (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))))
           it))))



;; Logging utilities

(defvar porg-log-level 'info)

(defun porg-log-s (format-string &rest args)
  "Log FORMAT-STRING with ARGS as section.

The output width is limited to 80 characters."
  (message (porg-section (apply #'format format-string args))))

(defun porg-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS.

The output width is limited to 80 characters."
  (message (s-truncate 80 (apply #'format format-string args))))

(defun porg-debug (format-string &rest args)
  "Debug log FORMAT-STRING with ARGS.

Noops depending on `porg-log-level'."
  (when (equal porg-log-level 'debug)
    (message (apply #'format format-string args))))

(defun porg-section (str)
  "Convert STR into nice section."
  (let* ((s (concat ">>> " str))
         (l 76))
    (concat
     "┌──────────────────────────────────────────────────────────────────────────────┐\n"
     "│ " (s-truncate l (s-pad-right l " " s)) " │\n"
     "└──────────────────────────────────────────────────────────────────────────────┘")))



;;  Other utilities

(cl-defun porg-slug (title)
  "Convert TITLE to slug."
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768  ; U+0300 COMBINING GRAVE ACCENT
                           769  ; U+0301 COMBINING ACUTE ACCENT
                           770  ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771  ; U+0303 COMBINING TILDE
                           772  ; U+0304 COMBINING MACRON
                           774  ; U+0306 COMBINING BREVE
                           775  ; U+0307 COMBINING DOT ABOVE
                           776  ; U+0308 COMBINING DIAERESIS
                           777  ; U+0309 COMBINING HOOK ABOVE
                           778  ; U+030A COMBINING RING ABOVE
                           779  ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780  ; U+030C COMBINING CARON
                           795  ; U+031B COMBINING HORN
                           803  ; U+0323 COMBINING DOT BELOW
                           804  ; U+0324 COMBINING DIAERESIS BELOW
                           805  ; U+0325 COMBINING RING BELOW
                           807  ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                      ("__*" . "_") ;; remove sequential underscores
                      ("^_" . "")   ;; remove starting underscore
                      ("_$" . ""))) ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defun porg-sha1sum (obj)
  "Calculate SHA1 sum of OBJ.

OBJ can be either a note, a file or a Lisp object."
  (cond
   ((vulpea-note-p obj)
    (caar
     (emacsql (vulpea-db)
              [:select hash
               :from files
               :where (= path $s1)]
              (vulpea-note-path obj))))

   ((porg-rule-output-p obj)
    (porg-sha1sum (porg-rule-output-item obj)))

   ((and
     (stringp obj)
     (file-exists-p obj))
    (with-temp-buffer
      (insert-file-contents-literally obj)
      (secure-hash 'sha1 (current-buffer))))

   (t (with-temp-buffer
        (let ((print-level nil)
	            (print-length nil))
	        (print obj (current-buffer))
          (secure-hash 'sha1 (current-buffer)))))))



(cl-defun porg-string-from-number (num
                                   &key
                                   min-length
                                   padding
                                   padding-num)
  "Return the decimal representation of NUM as string.

When MIN-LENGTH is specified, the result is padded on the left
with PADDING, which can be either \\'zero or \\'soace (default).

Padding also happens when PADDING-NUM is specified, in that case
 MIN-LENGTH equals to the length of decimal representation of
 PADDING-NUM as string.

This function might be considered an overkill, but it's used in
 short-lived scripts so often that I am tired of writing this
 kind of formatting every time."
  (if (or min-length padding-num)
      (let ((l (number-to-string
                (or min-length
                    (length (number-to-string padding-num))))))
        (format (if (eq padding 'zero)
                    (concat "%." l "d")
                  (concat "%" l ".d"))
                num))
    (number-to-string num)))



;;
;; topological sort, see
;; https://rosettacode.org/wiki/Topological_sort#Common_Lisp
;;
(cl-defun porg-topological-sort (graph &key (test 'eql))
  "Return a list of topologically ordered elements of GRAPH.

GRAPH is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
TEST is used to compare elements, and should be a suitable test for
hash-tables.

Topological-sort returns two values. The first is a
list of objects sorted toplogically. The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let* ((entries (make-hash-table :test test))
         ;; avoid obsolete `flet' & backward-incompatible `cl-flet'
         (entry (lambda (v)
                  "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
                  (or (gethash v entries)
                      (puthash v (cons 0 '()) entries)))))
    ;; populate entries initially
    (dolist (gvertex graph)
      (cl-destructuring-bind (vertex &rest dependencies) gvertex
        (let ((ventry (funcall entry vertex)))
          (dolist (dependency dependencies)
            (let ((dentry (funcall entry dependency)))
              (unless (funcall test dependency vertex)
                (cl-incf (car ventry))
                (push vertex (cdr dentry))))))))
    ;; L is the list of sorted elements, and S the set of vertices
    ;; with no outstanding dependencies.
    (let ((L '())
          (S (cl-loop for entry being each hash-value of entries
                      using (hash-key vertex)
                      when (zerop (car entry)) collect vertex)))
      ;; Until there are no vertices with no outstanding dependencies,
      ;; process vertices from S, adding them to L.
      (cl-do* () ((cl-endp S))
        (let* ((v (pop S)) (ventry (funcall entry v)))
          (remhash v entries)
          (dolist (dependant (cdr ventry) (push v L))
            (when (zerop (cl-decf (car (funcall entry dependant))))
              (push dependant S)))))
      ;; return (1) the list of sorted items, (2) whether all items
      ;; were sorted, and (3) if there were unsorted vertices, the
      ;; hash table mapping these vertices to their dependants
      (let ((all-sorted-p (zerop (hash-table-count entries))))
        (cl-values (nreverse L)
                   all-sorted-p
                   (unless all-sorted-p
                     entries))))))

(defun porg-topological-levels (graph &optional test)
  "Return parallel-safe levels for GRAPH, ignoring external deps.

GRAPH is an alist of (node . deps). We only build the node's themselves;
any dep not present as a node is treated as already done. TEST is the
hash-table test, defaulting to `equal`."
  (let* ((test     (or test 'equal))
         ;; 1. collect just the nodes we want to build
         (nodes    (mapcar #'car graph))
         (node-set (let ((h (make-hash-table :test test)))
                     (dolist (n nodes) (puthash n t h))
                     h))
         ;; 2. init in-degree and adjacency tables
         (in-deg   (make-hash-table :test test))
         (adj      (make-hash-table :test test))
         levels queue next)

    ;; initialise every node with zero in-degree and empty adj list
    (dolist (n nodes)
      (puthash n 0 in-deg)
      (puthash n nil adj))

    ;; 3. count only deps that are themselves in ‘nodes’
    (dolist (pair graph)
      (let ((node (car pair))
            (deps (cdr pair)))
        (dolist (d deps)
          (when (gethash d node-set)
            ;; increment in-degree for this node
            (puthash node (1+ (gethash node in-deg)) in-deg)
            ;; note that ‘node’ depends on ‘d’
            (push node (gethash d adj))))))

    ;; 4. seed initial queue of in-degree zero
    (maphash (lambda (n deg)
               (when (zerop deg)
                 (push n queue)))
             in-deg)

    ;; 5. Kahn’s algorithm, collecting one level at a time
    (while queue
      (push queue levels)
      (setq next nil)
      (dolist (n queue)
        ;; for each dependant m of n, decrement its in-degree
        (dolist (m (gethash n adj))
          (let ((new-deg (1- (gethash m in-deg))))
            (puthash m new-deg in-deg)
            (when (zerop new-deg)
              (push m next)))))
      (setq queue (nreverse next)))
    (nreverse levels)))



(defun porg-file-name-set-variant (file variant)
  "Add VARIANT to FILE."
  (let ((variant (cond
                  ((numberp variant) (number-to-string variant))
                  ((stringp variant) variant)
                  (t (user-error "Unsupported variant type: %s" variant)))))
    (concat (file-name-directory file)
            (file-name-base file)
            "@" variant
            "." (file-name-extension file))))


;; * Media type utilities

(defvar porg-supported-video-extensions '("mp4")
  "List of supported video file extensions.")

(defvar porg-supported-image-extensions '("jpeg" "png" "jpg" "heic" "webp" "gif" "svg")
  "List of supported image file extensions.")

(defvar porg-convertible-image-extensions '("jpeg" "png" "jpg" "heic" "webp")
  "List of image extensions that can be converted to webp.")

(defvar porg-supported-media-extensions
  (append porg-supported-image-extensions porg-supported-video-extensions)
  "Combined list of all supported media extensions (images + videos).
Cached for performance - avoids creating new list on each call.")

(defun porg-supported-media-p (file)
  "Return non-nil if FILE is a supported media (image or video)."
  (when-let* ((ext (file-name-extension file)))
    (seq-contains-p porg-supported-media-extensions (downcase ext))))

(defun porg-supported-video-p (file)
  "Return non-nil if FILE is a supported video."
  (when-let* ((ext (file-name-extension file)))
    (seq-contains-p porg-supported-video-extensions (downcase ext))))

(defun porg-supported-image-p (file)
  "Return non-nil if FILE is a supported image."
  (when-let* ((ext (file-name-extension file)))
    (seq-contains-p porg-supported-image-extensions (downcase ext))))

(defun porg-convertible-image-p (file)
  "Return non-nil if FILE is an image that can be converted to webp."
  (when-let* ((ext (file-name-extension file)))
    (seq-contains-p porg-convertible-image-extensions (downcase ext))))


;; * File name utilities

(defun porg-file-name-sanitize (file-name new-ext)
  "Sanitize FILE-NAME by replacing non-alphanumeric chars and setting NEW-EXT.
Non-alphanumeric characters (except / and .) are replaced with dashes.
Consecutive dashes are collapsed, and leading/trailing dashes are removed
from the basename."
  (let* ((ext-old (file-name-extension file-name))
         (without-ext (string-remove-suffix (concat "." ext-old) file-name))
         (dir (file-name-directory without-ext))
         (base (file-name-nondirectory without-ext))
         ;; Replace special chars with dashes
         (base-sanitized (replace-regexp-in-string "[^a-zA-Z0-9.]" "-" base))
         ;; Collapse consecutive dashes
         (base-collapsed (replace-regexp-in-string "-+" "-" base-sanitized))
         ;; Remove leading/trailing dashes
         (base-trimmed (replace-regexp-in-string "^-\\|-$" "" base-collapsed)))
    (concat dir base-trimmed "." new-ext)))

(defun porg-file-name-for-web (file-name)
  "Convert FILE-NAME to web-friendly format.
For convertible images, changes extension to webp and sanitizes.
For other supported media, sanitizes but keeps original extension."
  (cond
   ((porg-convertible-image-p file-name)
    (porg-file-name-sanitize file-name "webp"))
   ((porg-supported-media-p file-name)
    (porg-file-name-sanitize file-name (file-name-extension file-name)))
   (t file-name)))

(defun porg-file-name-replace-ext (file-name new-ext)
  "Replace FILE-NAME extension with NEW-EXT."
  (let ((ext-old (file-name-extension file-name)))
    (concat (string-remove-suffix (concat "." ext-old) file-name) "." new-ext)))


;; * Delete utilities

(defun porg-delete-with-metadata (file)
  "Delete FILE and its associated metadata file if it exists."
  (when (file-exists-p file)
    (delete-file file))
  (let ((meta (concat file ".metadata")))
    (when (file-exists-p meta)
      (delete-file meta))))


;; * Content extraction

(defun porg-extract-content (note)
  "Extract content from NOTE, skipping metadata section.
Returns the content as a string, excluding:
- PROPERTIES drawer
- Buffer keywords (#+title, etc.)
- vulpea-meta section (first description list if present)
- Leading whitespace after the above"
  (vulpea-utils-with-note note
    (let* ((meta (vulpea-buffer-meta))
           (pl (plist-get meta :pl))
           (start (if pl
                      (org-element-property :end pl)
                    (save-excursion
                      (goto-char (point-min))
                      ;; Skip PROPERTIES drawer
                      (while (looking-at org-property-re)
                        (forward-line 1))
                      ;; Skip buffer keywords (#+title, etc.)
                      (while (looking-at "^#\\+.+$")
                        (forward-line 1))
                      ;; Skip empty lines
                      (while (and (looking-at "^ *$")
                                  (not (eobp)))
                        (forward-line 1))
                      (point)))))
      (buffer-substring-no-properties start (point-max)))))

(defun porg-copy-content-to-file (target-file item)
  "Copy content from ITEM's note to TARGET-FILE.
Uses `porg-extract-content' to skip metadata section."
  (with-temp-file target-file
    (insert (porg-extract-content (porg-item-item item)))))


;; * JSON support

(defun porg-struct-to-alist (struct)
  "Convert STRUCT to an alist."
  (let ((type (type-of struct)))
    (append `((type . ,type))
            (->> (cl-struct-slot-info type)
                 (--map (cons (nth 0 it) (intern (format "%s-%s" type (nth 0 it)))))
                 (--filter (functionp (cdr it)))
                 (--map (cons (car it) (funcall (cdr it) struct)))
                 (--remove (null (cdr it)))))))

(defun porg-alist-to-struct (alist)
  "Convert ALIST to a struct."
  (let ((type (cdr (assoc 'type alist))))
    ;; not sure how to get a constructor in generic way;
    ;; but all porg structs use x-create convention
    (apply (intern (format "%s-create" type))
           (apply 'append
                  (mapcar (lambda (pair)
                            (unless (eq (car pair) 'type)
                              (list (intern (format ":%s" (car pair))) (cdr pair))))
                          alist)))))

(defun porg-json-print-wrapper (orig-fun object)
  "Wrapper for `json--print' that supports structs.

If OBJECT is not a struct, it calls ORIG-FUN."
  (if (cl-struct-p object)
      (funcall orig-fun (porg-struct-to-alist object))
    (funcall orig-fun object)))

(advice-add 'json--print :around #'porg-json-print-wrapper)



(provide 'publicatorg)
;;; publicatorg.el ends here
