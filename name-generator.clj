#!/usr/bin/env clojure
;; --------------------------------------------------------
;; Clojure implementation of the original `name-generator.sh`
;; --------------------------------------------------------
;; Behaviour:
;;   * SEPARATOR   – defaults to "-"
;;   * NOUN_FILE   – env var overrides, otherwise a random regular file
;;                   from $NOUN_FOLDER (default "./nouns")
;;   * ADJ_FILE    – same logic, using $ADJ_FOLDER (default "./adjectives")
;;   * counto      – number of lines to emit.
;;                   1️⃣ env var `counto` (preferred)
;;                   2️⃣ `tput lines` (if available)
;;                   3️⃣ fallback = 24
;;   * Each output line = <adjective><SEPARATOR><noun>
;;       – adjective keeps its original case
;;       – noun is lower‑cased
;; ---------------------------------------------------------

(ns name-generator.core
  (:require [clojure.java.io :as io]
            [clojure.string   :as str]
            [clojure.java.shell :refer [sh]]))

;; ---------------------------------------------------------
;; Helper – read an environment variable, return nil when not set
;; ---------------------------------------------------------
(defn env
  [k]
  (let [v (System/getenv (str k))]
    (when (and v (not (str/blank? v))) v)))

;; ---------------------------------------------------------
;; Resolve a folder path (defaults to cwd/<subdir>)
;; ---------------------------------------------------------
(defn resolve-folder
  [env-var default-subdir]
  (let [env-path (env env-var)]
    (if env-path
      (io/file env-path)
      (io/file (.getAbsolutePath (io/file ".")) default-subdir))))

;; ---------------------------------------------------------
;; Pick a random regular file from a folder
;; ---------------------------------------------------------
(defn random-file-from
  [folder]
  (let [files (->> (.listFiles ^java.io.File folder)
                   (filter #(.isFile ^java.io.File %)))]
    (when (empty? files)
      (throw (ex-info (str "Folder " (.getPath folder) " contains no regular files")
                      {:folder folder})))
    (rand-nth files)))

;; ---------------------------------------------------------
;; Resolve a file: env var overrides, otherwise random file from folder
;; ---------------------------------------------------------
(defn resolve-file
  [env-var folder]
  (if-let [p (env env-var)]
    (let [f (io/file p)]
      (when (or (not (.exists f)) (not (.isFile f)))
        (throw (ex-info (str "Environment variable " env-var
                             " points to a non‑regular file: " p)
                        {:env-var env-var :path p})))
      f)
    (random-file-from folder)))

;; ---------------------------------------------------------
;; Read non‑empty, trimmed lines from a file
;; ---------------------------------------------------------
(defn read-nonempty-lines
  [file]
  (with-open [r (io/reader file)]
    (->> (line-seq r)
         (map str/trim)
         (remove str/blank?))))

;; ---------------------------------------------------------
;; Determine how many names to generate (COUNT_O)
;; ---------------------------------------------------------
(defn count-o
  []
  (cond
    ;; 1️⃣ env var `counto`
    (env "counto")
    (try
      (Integer/parseInt (env "counto"))
      (catch Exception _ 24))

    ;; 2️⃣ `tput lines`
    :else
    (let [{:keys [out err exit]} (sh "tput" "lines")]
      (if (zero? exit)
        (try
          (Integer/parseInt (str/trim out))
          (catch Exception _ 24))
        24))))

;; ---------------------------------------------------------
;; Main generation loop
;; ---------------------------------------------------------
(defn -main
  [& _args]
  (let [separator   (or (env "SEPARATOR") "-")
        noun-folder (resolve-folder "NOUN_FOLDER" "nouns")
        adj-folder  (resolve-folder "ADJ_FOLDER"  "adjectives")
        noun-file   (resolve-file "NOUN_FILE" noun-folder)
        adj-file    (resolve-file "ADJ_FILE"  adj-folder)
        nouns       (vec (read-nonempty-lines noun-file))
        adjs        (vec (read-nonempty-lines adj-file))
        total       (count-o)]

    (when (or (empty? nouns) (empty? adjs))
      (throw (ex-info "One of the source files contains no usable lines"
                      {:noun-file noun-file
                       :adj-file  adj-file})))

    (dotimes [_ total]
      (let [noun (-> (rand-nth nouns) str/lower-case)
            adj  (rand-nth adjs)]
        (println (str adj separator noun))))))

;; Allow the script to be run directly (`clojure -M -m name-generator.core`)
;; The `:gen-class` directive is not needed because we use `-main` as an entry point.
