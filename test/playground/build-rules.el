;;; build-rules.el --- Build rules for playground -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Simple build rules for benchmarking publicatorg.
;; This uses a minimal setup without vulpea/org-roam to isolate
;; the build system performance.
;;
;;; Code:

(require 'publicatorg)

;;; Configuration variables for benchmarking
(defvar playground-cache-backend 'file
  "Cache backend to use: 'file or 'sqlite.")

(defvar playground-parallel nil
  "Number of parallel workers, or nil for sequential.")

;;; Note structure (simple replacement for vulpea-note)
(cl-defstruct (playground-note (:constructor playground-note-create))
  "A simple note structure for playground."
  id
  title
  path
  links)   ; list of IDs this note links to

;;; Parse notes from directory
(defun playground-parse-note (file)
  "Parse a note from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((id nil)
          (title nil)
          (links nil))
      ;; Parse ID
      (goto-char (point-min))
      (when (re-search-forward "^:ID: +\\(.+\\)$" nil t)
        (setq id (match-string 1)))
      ;; Parse title
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title: +\\(.+\\)$" nil t)
        (setq title (match-string 1)))
      ;; Parse links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
        (push (match-string 1) links))
      (playground-note-create
       :id id
       :title title
       :path file
       :links (nreverse links)))))

(defun playground-load-notes (dir)
  "Load all notes from DIR."
  (let ((files (directory-files dir t "\\.org$")))
    (mapcar #'playground-parse-note files)))

;;; Describe methods
(cl-defmethod porg-describe ((note playground-note))
  "Describe NOTE."
  (format "(note) %s" (playground-note-title note)))

(cl-defmethod porg-describe ((item porg-item))
  "Describe ITEM."
  (if (playground-note-p (porg-item-item item))
      (porg-describe (porg-item-item item))
    (format "(item) %s" (porg-item-id item))))

;;; Compiler - simple file copy with transformation
(defun playground-compile-note (item _items _cache)
  "Compile ITEM by copying and transforming."
  (let ((source (playground-note-path (porg-item-item item)))
        (target (porg-item-target-abs item)))
    (make-directory (file-name-directory target) t)
    ;; Simple transformation: copy and add a build timestamp
    (with-temp-file target
      (insert-file-contents source)
      (goto-char (point-max))
      (insert (format "\n\n# Built: %s\n" (current-time-string))))))

;;; Hash function for notes
(defun playground-hash-note (output)
  "Calculate hash for note OUTPUT."
  (let ((note (porg-rule-output-item output)))
    (porg-sha1sum (playground-note-path note))))

;;; Output generator for notes
(defun playground-make-outputs (note)
  "Create outputs for NOTE."
  (list
   (porg-rule-output
    :id (playground-note-id note)
    :type "note"
    :item note
    :file (format "%s.txt" (playground-note-id note))
    :soft-deps (playground-note-links note))))

;;; Project definition
(defun playground-define-project ()
  "Define the playground project."
  (let* ((playground-dir (file-name-directory (or load-file-name buffer-file-name)))
         (notes-dir (expand-file-name "notes" playground-dir))
         (output-dir (expand-file-name "output" playground-dir)))

    ;; Apply configuration
    (setq porg-cache-backend playground-cache-backend)
    (setq porg-parallel playground-parallel)
    (setq porg-log-level 'info)

    (porg-define
     :name "playground"
     :root output-dir
     :cache-file "cache"

     :input
     (lambda ()
       (playground-load-notes notes-dir))

     :rules
     (list
      (porg-rule
       :name "notes"
       :match #'playground-note-p
       :outputs #'playground-make-outputs))

     :compilers
     (list
      (porg-compiler
       :name "notes"
       :match (lambda (output) (string-equal (porg-rule-output-type output) "note"))
       :hash #'playground-hash-note
       :build #'playground-compile-note
       :clean #'delete-file)))))

;; Define the project when loaded
(playground-define-project)

(provide 'build-rules)
;;; build-rules.el ends here
