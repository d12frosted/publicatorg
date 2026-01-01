;;; generate-notes.el --- Generate synthetic notes for playground -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Generates synthetic org notes for benchmarking publicatorg.
;; Run with: eldev exec -f playground-generate-notes
;;
;;; Code:

(require 'org-id)

(defvar playground-notes-dir
  (expand-file-name "notes" (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory for generated notes.")

(defvar playground-note-count 1000
  "Target number of notes to generate (approximate).")

(defvar playground-scale 10
  "Scale factor for note generation.
1 = ~100 notes, 10 = ~1000 notes, 100 = ~10000 notes.")

(defun playground-generate-content (size)
  "Generate random content of approximate SIZE words."
  (let ((words '("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog"
                 "lorem" "ipsum" "dolor" "sit" "amet" "consectetur"
                 "adipiscing" "elit" "sed" "do" "eiusmod" "tempor"
                 "incididunt" "ut" "labore" "et" "dolore" "magna" "aliqua"
                 "enim" "ad" "minim" "veniam" "quis" "nostrud" "exercitation"
                 "ullamco" "laboris" "nisi" "aliquip" "ex" "ea" "commodo")))
    (mapconcat (lambda (_) (nth (random (length words)) words))
               (number-sequence 1 size) " ")))

(defun playground-generate-paragraphs (count)
  "Generate COUNT paragraphs of content."
  (mapconcat (lambda (_)
               (concat (playground-generate-content (+ 50 (random 100))) "\n"))
             (number-sequence 1 count) "\n"))

(defun playground-write-note (id title &optional links content-size)
  "Write a note with ID, TITLE, optional LINKS and CONTENT-SIZE."
  (let ((file (expand-file-name (format "%s.org" id) playground-notes-dir))
        (content-size (or content-size (+ 2 (random 5)))))
    (with-temp-file file
      (insert (format ":PROPERTIES:
:ID:       %s
:END:
#+title: %s
" id title))
      ;; Add links section if we have dependencies
      (when links
        (insert "\n* References\n")
        (dolist (link links)
          (insert (format "- [[id:%s][%s]]\n" (car link) (cdr link)))))
      ;; Add content
      (insert "\n* Content\n\n")
      (insert (playground-generate-paragraphs content-size)))))

(defvar playground--notes nil "List of generated notes (id . title).")
(defvar playground--id-counter 0 "Counter for note IDs.")

(defun playground--make-note (prefix &optional links size)
  "Create a note with PREFIX, optional LINKS and SIZE."
  (let* ((id (format "%08d-0000-0000-0000-%012d"
                     (/ playground--id-counter 1000)
                     (mod playground--id-counter 1000)))
         (title (format "%s %d" prefix playground--id-counter)))
    (cl-incf playground--id-counter)
    (playground-write-note id title links size)
    (push (cons id title) playground--notes)
    (cons id title)))

(defun playground-generate-notes (&optional scale)
  "Generate all synthetic notes.
SCALE controls the number of notes:
  1 = ~100 notes, 10 = ~1000 notes, 100 = ~10000 notes."
  (interactive)
  (let ((scale (or scale playground-scale)))
    ;; Ensure directory exists and is clean
    (setq playground-notes-dir (expand-file-name "test/playground/notes" default-directory))
    (make-directory playground-notes-dir t)
    (dolist (file (directory-files playground-notes-dir t "\\.org$"))
      (delete-file file))

    (setq playground--notes nil)
    (setq playground--id-counter 0)

    ;; Calculate counts based on scale
    ;; Base: 40 independent, 5 chains×6, 5 diamonds×4, 1 hub×11 = 101 notes
    (let* ((independent-count (* 40 scale))
           (chain-count (* 5 scale))
           (chain-length 6)
           (diamond-count (* 5 scale))
           (hub-count scale)
           (hub-spokes 10))

      ;; 1. Independent notes (no dependencies)
      (message "Generating %d independent notes..." independent-count)
      (dotimes (_ independent-count)
        (playground--make-note "Independent"))

      ;; 2. Linear chains
      (message "Generating %d chains of %d notes..." chain-count chain-length)
      (dotimes (chain chain-count)
        (let ((prev nil))
          (dotimes (_ chain-length)
            (let ((note (playground--make-note (format "Chain%d-Step" chain)
                                               (when prev (list prev)))))
              (setq prev note)))))

      ;; 3. Diamond patterns (A <- B, A <- C, B <- D, C <- D)
      (message "Generating %d diamond patterns..." diamond-count)
      (dotimes (d diamond-count)
        (let* ((a (playground--make-note (format "Diamond%d-Base" d)))
               (b (playground--make-note (format "Diamond%d-Left" d) (list a)))
               (c (playground--make-note (format "Diamond%d-Right" d) (list a)))
               (_top (playground--make-note (format "Diamond%d-Top" d) (list b c))))))

      ;; 4. Hub patterns (1 hub with N dependents)
      (message "Generating %d hub patterns with %d spokes each..." hub-count hub-spokes)
      (dotimes (h hub-count)
        (let ((hub (playground--make-note (format "Hub%d-Center" h) nil 10)))
          (dotimes (_ hub-spokes)
            (playground--make-note (format "Hub%d-Spoke" h) (list hub))))))

    (message "Generated %d notes in %s" (length playground--notes) playground-notes-dir)
    playground--notes))

(provide 'generate-notes)
;;; generate-notes.el ends here
