;;; build-rules.el --- Build rules for playground -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Build rules for benchmarking publicatorg with attachments.
;; Uses extracted utility functions from publicatorg.
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
  attach-dir      ; directory containing attachments (relative to notes dir)
  attachments     ; list of attachment file names
  links)          ; list of IDs this note links to

;;; Parse notes from directory
(defun playground-parse-note (file)
  "Parse a note from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((id nil)
          (title nil)
          (attach-dir nil)
          (attachments nil)
          (links nil))
      ;; Parse ID
      (goto-char (point-min))
      (when (re-search-forward "^:ID: +\\(.+\\)$" nil t)
        (setq id (match-string 1)))
      ;; Parse DIR (attachment directory)
      (goto-char (point-min))
      (when (re-search-forward "^:DIR: +\\(.+\\)$" nil t)
        (setq attach-dir (match-string 1)))
      ;; Parse title
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title: +\\(.+\\)$" nil t)
        (setq title (match-string 1)))
      ;; Parse attachment links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[attachment:\\([^]]+\\)\\]" nil t)
        (push (match-string 1) attachments))
      ;; Parse ID links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
        (push (match-string 1) links))
      (playground-note-create
       :id id
       :title title
       :path file
       :attach-dir attach-dir
       :attachments (nreverse attachments)
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
  (pcase (porg-item-type item)
    ("note" (if (playground-note-p (porg-item-item item))
                (porg-describe (porg-item-item item))
              (format "(note) %s" (porg-item-id item))))
    ("attachment"
     (format "(%s) %s"
             (if (porg-supported-image-p (porg-item-target-abs item)) "image" "file")
             (file-name-nondirectory (porg-item-target-abs item))))
    (_ (format "(%s) %s" (porg-item-type item) (porg-item-id item)))))

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

;;; Compiler for attachments (simple copy, using extracted utilities)
(defun playground-compile-attachment (item _items _cache)
  "Compile attachment ITEM by copying."
  (let ((source (porg-item-item item))
        (target (porg-item-target-abs item)))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)))

;;; Hash function for notes
(defun playground-hash-note (output)
  "Calculate hash for note OUTPUT."
  (let ((note (porg-rule-output-item output)))
    (porg-sha1sum (playground-note-path note))))

;;; Hash function for attachments
(defun playground-hash-attachment (output)
  "Calculate hash for attachment OUTPUT."
  (porg-sha1sum (porg-rule-output-item output)))

;;; Output generator for notes with attachments
(defun playground-make-outputs (note)
  "Create outputs for NOTE including attachments.
Uses extracted utility functions: porg-supported-media-p, porg-file-name-for-web."
  (let* ((note-id (playground-note-id note))
         (attach-dir (playground-note-attach-dir note))
         (notes-dir (file-name-directory (playground-note-path note)))
         ;; Create attachment outputs
         (attachment-outputs
          (when (and attach-dir (playground-note-attachments note))
            (mapcar
             (lambda (attach-name)
               (when (porg-supported-media-p attach-name)
                 (let* ((source-path (expand-file-name
                                      (concat attach-dir "/" attach-name)
                                      notes-dir))
                        (output-name (porg-file-name-for-web attach-name)))
                   (porg-rule-output
                    :id (format "%s:%s" note-id attach-name)
                    :type "attachment"
                    :item source-path
                    :file (format "images/%s/%s" note-id output-name)))))
             (playground-note-attachments note))))
         (attachment-ids (mapcar #'porg-rule-output-id
                                 (seq-filter #'identity attachment-outputs))))
    ;; Return attachments first, then note (note hard-depends on attachments)
    (append
     (seq-filter #'identity attachment-outputs)
     (list
      (porg-rule-output
       :id note-id
       :type "note"
       :item note
       :file (format "notes/%s.txt" note-id)
       :soft-deps (playground-note-links note)
       :hard-deps attachment-ids)))))

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
      ;; Compiler for notes
      (porg-compiler
       :name "notes"
       :match (lambda (output) (string-equal (porg-rule-output-type output) "note"))
       :hash #'playground-hash-note
       :build #'playground-compile-note
       :clean #'porg-delete-with-metadata)
      ;; Compiler for attachments (using extracted utilities)
      (porg-compiler
       :name "attachments"
       :match (lambda (output) (string-equal (porg-rule-output-type output) "attachment"))
       :hash #'playground-hash-attachment
       :build #'playground-compile-attachment
       :clean #'porg-delete-with-metadata)))))

;; Define the project when loaded
(playground-define-project)

(provide 'build-rules)
;;; build-rules.el ends here
