;;; benchmark.el --- Benchmark utilities for playground -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Utilities for benchmarking publicatorg with different configurations.
;;
;; Usage:
;;   eldev exec '(playground-benchmark-full)'
;;   eldev exec '(playground-benchmark-incremental)'
;;   eldev exec '(playground-benchmark-compare-caches)'
;;
;;; Code:

(require 'cl-lib)

(defvar playground-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Playground directory.")

(defvar playground-results nil
  "List of benchmark results.")

;;; Helpers

(defun playground-clean-output ()
  "Remove all output files and cache."
  (let ((output-dir (expand-file-name "output" playground-dir)))
    (when (file-directory-p output-dir)
      (delete-directory output-dir t))
    (make-directory output-dir t)))

(defun playground-clean-cache ()
  "Remove cache files only."
  (let ((output-dir (expand-file-name "output" playground-dir)))
    (dolist (file (directory-files output-dir t "^cache"))
      (delete-file file))))

(defun playground-count-output-files ()
  "Count output files."
  (let ((output-dir (expand-file-name "output" playground-dir)))
    (length (directory-files output-dir nil "\\.txt$"))))

(defun playground-modify-random-note ()
  "Modify a random note to trigger rebuild."
  (let* ((notes-dir (expand-file-name "notes" playground-dir))
         (files (directory-files notes-dir t "\\.org$"))
         (file (nth (random (length files)) files)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (insert (format "\n# Modified: %s\n" (current-time-string)))
      (save-buffer)
      (kill-buffer))
    file))

(defun playground-run-build (name backend parallel)
  "Run a build with NAME, BACKEND cache and PARALLEL workers.
Returns time in seconds."
  (let ((start-time (float-time)))
    ;; Set configuration
    (setq playground-cache-backend backend)
    (setq playground-parallel parallel)
    ;; Reload project definition
    (load (expand-file-name "build-rules.el" playground-dir))
    ;; Run build
    (porg-run "playground")
    (let ((elapsed (- (float-time) start-time)))
      (message ">>> %s: %.2fs (backend=%s, parallel=%s, files=%d)"
               name elapsed backend parallel (playground-count-output-files))
      elapsed)))

;;; Benchmarks

(defun playground-benchmark-full ()
  "Benchmark full rebuild with different configurations."
  (interactive)
  (message "\n=== Full Rebuild Benchmark ===\n")
  (setq playground-results nil)

  ;; File cache, sequential
  (playground-clean-output)
  (push (cons "file-seq" (playground-run-build "File cache (sequential)"
                                                'file nil))
        playground-results)

  ;; File cache, sequential (second run - should be fast, nothing to build)
  (push (cons "file-seq-cached" (playground-run-build "File cache (cached)"
                                                       'file nil))
        playground-results)

  ;; SQLite cache, sequential
  (playground-clean-output)
  (push (cons "sqlite-seq" (playground-run-build "SQLite cache (sequential)"
                                                  'sqlite nil))
        playground-results)

  ;; SQLite cache, sequential (cached)
  (push (cons "sqlite-seq-cached" (playground-run-build "SQLite cache (cached)"
                                                         'sqlite nil))
        playground-results)

  ;; Summary
  (message "\n=== Summary ===")
  (dolist (result (reverse playground-results))
    (message "  %s: %.2fs" (car result) (cdr result)))

  playground-results)

(defun playground-benchmark-incremental ()
  "Benchmark incremental builds (single note change)."
  (interactive)
  (message "\n=== Incremental Build Benchmark ===\n")
  (setq playground-results nil)

  ;; Build everything first with file cache
  (playground-clean-output)
  (playground-run-build "Initial build (file)" 'file nil)

  ;; Modify one note and rebuild
  (let ((modified (playground-modify-random-note)))
    (message "Modified: %s" (file-name-nondirectory modified)))
  (push (cons "file-incremental" (playground-run-build "File cache (incremental)"
                                                        'file nil))
        playground-results)

  ;; Now with SQLite
  (playground-clean-output)
  (playground-run-build "Initial build (sqlite)" 'sqlite nil)

  (let ((modified (playground-modify-random-note)))
    (message "Modified: %s" (file-name-nondirectory modified)))
  (push (cons "sqlite-incremental" (playground-run-build "SQLite cache (incremental)"
                                                          'sqlite nil))
        playground-results)

  ;; Summary
  (message "\n=== Summary ===")
  (dolist (result (reverse playground-results))
    (message "  %s: %.2fs" (car result) (cdr result)))

  playground-results)

(defun playground-benchmark-compare-caches ()
  "Compare file vs SQLite cache performance across scenarios."
  (interactive)
  (message "\n=== Cache Comparison Benchmark ===\n")
  (setq playground-results nil)

  (let ((scenarios '(("Cold build" . playground-clean-output)
                     ("Rebuild (no changes)" . ignore)
                     ("After modify" . playground-modify-random-note))))

    ;; File cache
    (message "\n--- File Cache ---")
    (playground-clean-output)
    (dolist (scenario scenarios)
      (funcall (cdr scenario))
      (push (cons (format "file: %s" (car scenario))
                  (playground-run-build (car scenario) 'file nil))
            playground-results))

    ;; SQLite cache
    (message "\n--- SQLite Cache ---")
    (playground-clean-output)
    (dolist (scenario scenarios)
      (funcall (cdr scenario))
      (push (cons (format "sqlite: %s" (car scenario))
                  (playground-run-build (car scenario) 'sqlite nil))
            playground-results)))

  ;; Summary
  (message "\n=== Summary ===")
  (dolist (result (reverse playground-results))
    (message "  %s: %.2fs" (car result) (cdr result)))

  playground-results)

(defun playground-benchmark-scaling ()
  "Test how cache performance scales with more notes.
Generates additional notes and measures rebuild time."
  (interactive)
  (message "\n=== Scaling Benchmark ===\n")
  (setq playground-results nil)

  ;; Test with current 101 notes
  (playground-clean-output)
  (let ((time (playground-run-build "101 notes (file)" 'file nil)))
    (push (cons "101-file" time) playground-results))

  (playground-clean-output)
  (let ((time (playground-run-build "101 notes (sqlite)" 'sqlite nil)))
    (push (cons "101-sqlite" time) playground-results))

  ;; Summary
  (message "\n=== Summary ===")
  (message "  Notes: 101")
  (dolist (result (reverse playground-results))
    (message "  %s: %.2fs" (car result) (cdr result)))

  playground-results)

(provide 'benchmark)
;;; benchmark.el ends here
