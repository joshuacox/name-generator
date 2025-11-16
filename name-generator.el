#!/usr/bin/env -S emacs -Q --script

;; Configuration - environment overrides with sensible defaults
(defcustom noun-folder (expand-file-name "nouns" default-directory)
  "Directory containing noun files"
  :type 'string
  :group 'name-generator)

(defcustom adj-folder (expand-file-name "adjectives" default-directory)
  "Directory containing adjective files"
  :type 'string
  :group 'name-generator)

(defcustom separator "-"
  "Separator between adjective and noun"
  :type 'string
  :group 'name-generator)

;; Helper functions
(defun command-exists-p (cmd)
  "Check if CMD is available in the system."
  (let ((executable (executable-find cmd)))
    (and executable (file-executable-p executable))))

(defun strip-string (s)
  "Trim leading and trailing whitespace from string S."
  (if (fboundp 'string-trim)
      (string-trim s)
    ;; Fallback for older Emacs versions
    (replace-regexp-in-string
     "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" s)))

(defun random-file-from (directory)
  "Pick a random regular file from DIRECTORY."
  (let* ((all-files (directory-files directory t))
         (files (seq-filter #'file-regular-p all-files)))
    (when files
      (nth (random (length files)) files))))

(defun read-nonempty-lines (file)
  "Read FILE and return non-empty lines."
  (with-temp-buffer
    (insert-file-contents file nil nil nil t)
    (mapcar #'strip-string
            (split-string (buffer-string) "\n" t))))

;; Random selection
(defun random-adjective ()
  "Pick a random adjective, preserving case."
  (let ((file (or (getenv "ADJ_FILE")
                  (random-file-from adj-folder))))
    (let ((lines (read-nonempty-lines file)))
      (when lines
        (nth (random (length lines)) lines)))))

(defun random-noun ()
  "Pick a random noun and convert to lowercase."
  (let* ((file (or (getenv "NOUN_FILE")
                   (random-file-from noun-folder)))
         (lines (read-nonempty-lines file)))
    (when lines
      (downcase (nth (random (length lines)) lines)))))

;; Debugging
(defun debug-print (&optional adjective noun _noun-file _adj-file _noun-folder _adj-folder)
  "Print debug information when DEBUG is set."
  (when (equal (getenv "DEBUG") "true")
    (princ "Debug:\n")
    (princ (format "Adjective: %s\n" adjective))
    (princ (format "Noun: %s\n" noun))
    (princ (format "ADJ_FILE: %s\n" _adj-file))
    (princ (format "ADJ_FOLDER: %s\n" _adj-folder))
    (princ (format "NOUN_FILE: %s\n" _noun-file))
    (princ (format "NOUN_FOLDER: %s\n" _noun-folder))))

;; Main generation
(defun generate-name ()
  "Generate and return a random name."
  (let ((adjective (random-adjective))
        (noun (random-noun)))
    (debug-print adjective noun
                (or (getenv "NOUN_FILE") "")
                (or (getenv "ADJ_FILE") "")
                noun-folder
                adj-folder)
    (concat adjective separator noun)))

(defun generate-names (count)
  "Generate COUNT names and print them."
  (let ((terminal-lines (if (command-exists-p "tput")
                           (string-to-number (shell-command-to-string "tput lines"))
                         24)))
    (dotimes (_ count terminal-lines)
      (princ (generate-name))
      (princ "\n"))))

;; Entry point
(let ((count (if (getenv "counto")
                 (string-to-number (getenv "counto"))
               24)))
  (if (member "--batch" command-line-args)
      (progn
        (generate-names count)
        (kill-emacs))
    (generate-names count)))
