
#!/usr/bin/env emacs --script

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

;;
;; Helper functions
;;

(defun command-exists-p (cmd)
  "Check if CMD is available in the system."
  (let ((executable (executable-find cmd)))
    (and executable (file-executable-p executable))))

(defun random-file-from (directory)
  "Pick a random regular file from DIRECTORY."
  (let (files)
    (dired-do-find-files-and-directories directory t nil (lambda (file) 
      (when (file-regular-p file)
        (push (expand-file-name file) files))))
    (and files
         (nth (random (length files)) files))))

(defun read-nonempty-lines (file)
  "Read FILE and return non-empty lines."
  (with-temp-buffer
    (insert-file-contents file nil nil nil t)
    (split-string-and-strip (buffer-string) "\n" t)))

;; Random selection
(defun random-adjective ()
  "Pick a random adjective, preserving case."
  (let ((file (or (getenv "ADJ_FILE")
                  (random-file-from adj-folder))))
    (nth (random (length (read-nonempty-lines file))) 
         (read-nonempty-lines file))))

(defun random-noun ()
  "Pick a random noun and convert to lowercase."
  (let ((file (or (getenv "NOUN_FILE") 
                  (random-file-from noun-folder))))
    (downcase (nth (random (length (read-nonempty-lines file)))
                   (read-nonempty-lines file)))))

;; Debugging
(defun debug-print (&optional adjective noun _noun-file _adj-file _noun-folder _adj-folder)
  "Print debug information when DEBUG is set."
  (when (equal (getenv "DEBUG") "true")
    (message "Debug:")
    (message "Adjective: %s" adjective)
    (message "Noun: %s" noun)
    (message "ADJ_FILE: %s" _adj-file)
    (message "ADJ_FOLDER: %s" _adj-folder)
    (message "NOUN_FILE: %s" _noun-file)
    (message "NOUN_FOLDER: %s" _noun-folder)))

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
      (message "%s" (generate-name)))))

;; Entry point
(unless (batch-mode)
  (generate-names 
   (if-let ((counto (getenv "counto")))
     (string-to-number counto)
   24)))
