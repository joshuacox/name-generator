#!/usr/bin/env -S clojure -M ./name-generator.clj
(ns name-generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]])
  (:import [java.nio.file Files Paths Path LinkOption]
           [java.util.stream Stream]))

;; --------------------------------
;; Helpers
;; --------------------------------
(defn env
  "Return the value of environment variable `k` or `default` if not set or blank."
  [k default]
  (let [v (System/getenv k)]
    (if (and v (not (str/blank? v))) v default)))

(defn int-or
  "Parse `s` as an integer, returning `default` on failure."
  [s default]
  (try
    (Integer/parseInt (str/trim s))
    (catch Exception _ default)))

(defn terminal-lines
  "Try to obtain the terminal height via `tput lines`; fall back to 24."
  []
  (let [{:keys [out exit]} (sh "tput" "lines")]
    (if (zero? exit)
      (int-or out 24)
      24)))

(defn list-regular-files
  "Return a vector of absolute paths (as strings) for regular files in `dir`."
  [dir]
  (with-open [stream (Files/list (Paths/get dir (into-array String [])))]
    (->> (iterator-seq (.iterator ^Stream stream))
         (filter #(Files/isRegularFile ^Path % (into-array LinkOption [])))
         (map #(.toString ^Path %))
         vec)))

(defn random-file
  "Pick a random regular file from `folder`. Throws if none are found."
  [folder]
  (let [files (list-regular-files folder)]
    (if (empty? files)
      (throw (ex-info (str "Folder " folder " does not contain any regular files") {}))
      (rand-nth files))))

(defn read-nonempty-lines
  "Read a file and return a vector of non‑empty, trimmed lines."
  [path]
  (->> (io/reader path)
       line-seq
       (map str/trim)
       (remove str/blank?)
       vec))

;; --------------------------------
;; Configuration (mirrors name-generator.sh)
;; --------------------------------
(def separator (env "SEPARATOR" "-"))

;; Number of lines to emit – use `tput lines` when available, otherwise 24.
(def counto
  (or (some-> (System/getenv "counto") (int-or nil))
      (terminal-lines)))

(def here (System/getProperty "user.dir"))

(def noun-folder (env "NOUN_FOLDER" (str here "/nouns")))
(def adj-folder  (env "ADJ_FOLDER"  (str here "/adjectives")))

(def noun-file (env "NOUN_FILE" (random-file noun-folder)))
(def adj-file  (env "ADJ_FILE"  (random-file adj-folder)))

;; --------------------------------
;; Load word lists
;; --------------------------------
(def noun-lines (read-nonempty-lines noun-file))
(def adj-lines  (read-nonempty-lines adj-file))

(when (empty? noun-lines)
  (throw (ex-info (str "Noun list is empty in " noun-file) {})))
(when (empty? adj-lines)
  (throw (ex-info (str "Adjective list is empty in " adj-file) {})))

;; --------------------------------
;; Debug helper (mirrors the shell script's debugger)
;; --------------------------------
(def debug? (= (env "DEBUG" "") "true"))

(defn debug-print
  "Print debugging information to stderr when DEBUG=true."
  [adjective noun countzero]
  (when debug?
    (binding [*out* *err*]
      (println adjective)
      (println noun)
      (println adj-file)
      (println adj-folder)
      (println noun-file)
      (println noun-folder)
      (println (format "%s > %s" countzero counto)))))

;; --------------------------------
;; Main generation loop
;; --------------------------------
(defn -main
  "Generate `counto` names, printing each to stdout."
  [& _args]
  (dotimes [i counto]
    (let [noun      (-> (rand-nth noun-lines) str/lower-case)
          adjective (rand-nth adj-lines)]
      (debug-print adjective noun i)
      (println (str adjective separator noun)))))

;; Allow the script to be run directly with `clj -M -m name-generator.core`
