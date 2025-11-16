#!/usr/bin/env emacs --script
;; name-generator.el
;; Emacs‑Lisp implementation of the behaviour of name-generator.sh
;; It respects the environment variables:
;;   SEPARATOR   – string placed between adjective and noun (default "-")
;;   NOUN_FILE   – path to a noun list file (if unset a random file from NOUN_FOLDER is used)
;;   ADJ_FILE    – path to an adjective list file (if unset a random file from ADJ_FOLDER is used)
;;   NOUN_FOLDER – folder containing noun files (default "$PWD/nouns")
;;   ADJ_FOLDER  – folder containing adjective files (default "$PWD/adjectives")
;;   counto      – number of lines to emit (default: value of `tput lines` or 24)
;;   DEBUG       – if set to "true", prints debugging information to stderr.

(require 'cl-lib)
(require 'subr-x)   ;; for `string-trim`

;; ----------------------------------------------------------------------
;; Helper utilities
;; ----------------------------------------------------------------------
(defun env-or-default (var default)
  "Return the value of environment variable VAR or DEFAULT if not set."
  (or (getenv var) default))

(defun env-or-nil (var)
  "Return the value of environment variable VAR or nil if not set."
  (let ((val (getenv var)))
    (unless (or (null val) (string= val ""))
      val)))

(defun call-tput-lines ()
  "Return the number of terminal lines reported by `tput lines`, or nil on failure."
  (ignore-errors
    (let ((output (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "tput" nil t nil "lines"))))
      (when (string-match-p "^[0-9]+$" (string-trim output))
        (string-to-number (string-trim output))))))

(defun get-counto ()
  "Determine how many names to generate."
  (or (let ((env (env-or-nil "counto")))
        (when env (string-to-number env)))
      (call-tput-lines)
      24))

(defun list-regular-files (folder)
  "Return a list of absolute paths of regular files inside FOLDER."
  (let ((files '()))
    (dolist (entry (directory-files folder t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
      (when (file-regular-p entry)
        (push entry files)))
    (nreverse files)))

(defun pick-random-file (folder)
  "Pick a random regular file from FOLDER. Signal an error if none exist."
  (let ((files (list-regular-files folder)))
    (unless files
      (error "Folder %s does not contain any regular files" folder))
    (nth (random (length files)) files)))

(defun resolve-file (env-var folder)
  "Return a file path for ENV-VAR or a random file from FOLDER."
  (let ((val (env-or-nil env-var)))
    (if val
        (let ((abs (expand-file-name val)))
          (unless (file-regular-p abs)
            (error "Environment variable %s points to a non‑regular file: %s" env-var abs))
          abs)
      (pick-random-file folder))))

(defun read-nonempty-lines (file)
  "Read FILE and return a list of non‑empty, trimmed lines."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (cl-remove-if (lambda (s) (string= s "")) (mapcar #'string-trim lines)))))

(defun debug-print (iteration noun-file adj-file noun-folder adj-folder noun adj)
  "Print debugging information when DEBUG=true."
  (when (string= (or (getenv "DEBUG") "") "true")
    (message "DEBUG iteration %d:" iteration)
    (message "  NOUN_FILE   = %s" noun-file)
    (message "  ADJ_FILE    = %s" adj-file)
    (message "  NOUN_FOLDER = %s" noun-folder)
    (message "  ADJ_FOLDER  = %s" adj-folder)
    (message "  noun        = %s" noun)
    (message "  adjective   = %s" adj)))

;; ----------------------------------------------------------------------
;; Main generation logic
;; ----------------------------------------------------------------------
(defun name-generator-run ()
  "Generate names according to the same rules as name-generator.sh."
  (let* ((separator (env-or-default "SEPARATOR" "-"))
         (here (expand-file-name default-directory))
         (noun-folder (env-or-default "NOUN_FOLDER"
                                      (expand-file-name "nouns" here)))
         (adj-folder  (env-or-default "ADJ_FOLDER"
                                      (expand-file-name "adjectives" here)))
    ;; Resolve files (env var overrides, otherwise random file from folder)
    (let* ((noun-file (resolve-file "NOUN_FILE" noun-folder))
           (adj-file  (resolve-file "ADJ_FILE"  adj-folder))
           (noun-lines (read-nonempty-lines noun-file))
           (adj-lines  (read-nonempty-lines adj-file))
           (counto (get-counto)))
      (unless noun-lines
        (error "Noun list %s is empty" noun-file))
      (unless adj-lines
        (error "Adjective list %s is empty" adj-file))
      (dotimes (i counto)
        (let* ((raw-noun (nth (random (length noun-lines)) noun-lines))
               (noun (downcase raw-noun))
               (adj  (nth (random (length adj-lines)) adj-lines)))
          (debug-print (1+ i) noun-file adj-file noun-folder adj-folder noun adj)
          (princ (format "%s%s%s\n" adj separator noun)))))))

;; Execute when run as a script (non‑interactive Emacs)
(when (not noninteractive)
  ;; In interactive sessions we do nothing.
  nil)

(when noninteractive
  (name-generator-run))
