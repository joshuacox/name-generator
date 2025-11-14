#lang racket

;; ------------------------------------------------------------
;; Name Generator – Racket implementation
;; Behaves like the original shell script `name-generator.sh`.
;; ------------------------------------------------------------

(require racket/file
         racket/list
         racket/string
         racket/system
         racket/port
         racket/format
         racket/os)

;; ---------- Helper utilities ----------
;; Return the value of an environment variable or #f if not set.
(define (env-var name)
  (let ([v (getenv name)])
    (if (and v (not (string-blank? v))) v #f)))

;; Convert a string to a number, returning #f on failure.
(define (string->maybe-number s)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (string->number (string-trim s))))

;; Pick a random element from a non‑empty list.
(define (random-choice lst)
  (list-ref lst (random (length lst))))

;; Read a file and return a list of its non‑empty, trimmed lines.
(define (read-nonempty-lines path)
  (define raw (file->lines path))
  (filter (λ (l) (not (string-blank? l)))
          (map string-trim raw)))

;; Debug output – prints to stderr when DEBUG=true.
(define (debugger adjective noun noun-file adj-file noun-folder adj-folder countzero counto)
  (when (string=? (getenv "DEBUG") "true")
    (fprintf (current-error-port) "DEBUG:\n")
    (fprintf (current-error-port) "  adjective : ~a\n" adjective)
    (fprintf (current-error-port) "  noun      : ~a\n" noun)
    (fprintf (current-error-port) "  ADJ_FILE  : ~a\n" adj-file)
    (fprintf (current-error-port) "  ADJ_FOLDER: ~a\n" adj-folder)
    (fprintf (current-error-port) "  NOUN_FILE : ~a\n" noun-file)
    (fprintf (current-error-port) "  NOUN_FOLDER: ~a\n" noun-folder)
    (fprintf (current-error-port) "  ~a > ~a\n" countzero counto)))

;; ---------- Configuration ----------
(define HERE (current-directory-path))

;; Separator (default "-")
(define SEPARATOR (or (env-var "SEPARATOR") "-"))

;; Count of lines to emit (default: tput lines or 24)
(define COUNTO
  (or (string->maybe-number (env-var "counto"))
      (let* ([tput-output (with-handlers ([exn:fail? (λ (_) #f)])
                            (with-output-to-string
                              (λ () (system* "tput" "lines"))))])
        (and tput-output (string->maybe-number (string-trim tput-output))))
      24))

;; Folders (default to sub‑directories of the current directory)
(define NOUN_FOLDER (or (env-var "NOUN_FOLDER")
                        (build-path HERE "nouns")))
(define ADJ_FOLDER  (or (env-var "ADJ_FOLDER")
                        (build-path HERE "adjectives")))

;; Helper: pick a random regular file from a folder.
(define (random-file-from folder)
  (define files
    (for/list ([p (in-directory folder)]
               #:when (and (file-exists? p) (not (directory-exists? p))))
      p))
  (when (null? files)
    (error "Folder has no regular files:" folder))
  (random-choice files))

;; Files (env var overrides, otherwise random file from the folder)
(define NOUN_FILE
  (or (env-var "NOUN_FILE")
      (path->string (random-file-from NOUN_FOLDER))))
(define ADJ_FILE
  (or (env-var "ADJ_FILE")
      (path->string (random-file-from ADJ_FOLDER))))

;; ---------- Load data ----------
(define noun-lines (read-nonempty-lines NOUN_FILE))
(define adj-lines  (read-nonempty-lines ADJ_FILE))

;; ---------- Main generation loop ----------
(for ([countzero (in-range COUNTO)])
  (define raw-noun (random-choice noun-lines))
  (define noun (string-downcase raw-noun))
  (define adjective (random-choice adj-lines))

  ;; Debug output if requested
  (debugger adjective noun NOUN_FILE ADJ_FILE NOUN_FOLDER ADJ_FOLDER countzero COUNTO)

  ;; Print the generated name
  (printf "~a~a~a\n" adjective SEPARATOR noun))
