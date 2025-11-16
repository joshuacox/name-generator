#!/usr/bin/env clojure

;; Environment variables and configuration
(def ^:dynamic *separator* (or (System/getenv "SEPARATOR") "-"))
(def ^:dynamic *noun-folder* (or (System/getenv "NOUN_FOLDER") "./nouns"))
(def ^:dynamic *adj-folder* (or (System/getenv "ADJ_FOLDER") "./adjectives"))

;; Randomness and file handling
(defn command-exists? [cmd]
  (try 
    (Runtime/getRuntime (str "which " cmd))
    true
    (catch java.io.IOException _ false)))

(defn realpath-fallback [path-str]
  (if (command-exists? "realpath")
    (-> (.getPath (java.nio.file.Paths/get path-str)) .toAbsolutePath .toString)
    (if (command-exists? "readlink") 
      (-> (.exec (Runtime/getRuntime) (str "readlink -f " path-str)) .waitFor() (.getInputStream) (slurp))
      path-str)))

(defn random-file-from [folder]
  (let [files (->> (clojure.java.io/file folder)
                   .listFiles 
                   (filter #(.isFile %)))]
    (if (command-exists? "shuf")
      (-> (.exec (Runtime/getRuntime) (str "find " folder " -type f | shuf -n 1")) 
          .waitFor() 
          (.getInputStream) 
          (slurp))
      (rand-nth files))))

;; Debugging
(def ^:dynamic *debug* (System/getenv "DEBUG"))

(defn debugger [adjective noun adj-file adj-folder noun-file noun-folder countzero counto]
  (when (= *debug* "true")
    (println "DEBUG:")
    (println (str "  adjective: " adjective))
    (println (str "  noun:      " noun))
    (println (str "  ADJ_FILE:  " adj-file))
    (println (str "  ADJ_FOLDER:" adj-folder))
    (println (str "  NOUN_FILE: " noun-file))
    (println (str "  NOUN_FOLDER:" noun-folder))
    (println (str countzero " > " counto))))

;; Main logic
(let [counto (if-let [tput-lines (try 
                                  (-> (.exec (Runtime/getRuntime) "tput lines") 
                                      .waitFor() 
                                      .getInputStream 
                                      (slurp) 
                                      Integer/parseInt)
                                  (catch Exception _ nil)] 
                            tput-lines 24)]
  (def ^:dynamic *counto* counto))

(when-not (System/getenv "NOUN_FILE")
  (set! (var-get (find-var 'user/*noun-file*)) 
        (realpath-fallback (random-file-from *noun-folder*))))

(when-not (System/getenv "ADJ_FILE")
  (set! (var-get (find-var 'user/*adj-file*)) 
        (realpath-fallback (random-file-from *adj-folder*))))

(doseq [i (range *counto*)]
  (let [noun-line (-> (slurp *noun-file*) 
                     clojure.string/split-lines 
                     rand-nth 
                     .toLowerCase)
        adj-line (-> (slurp *adj-file*) 
                    clojure.string/split-lines 
                    rand-nth)]
    (debugger adj-line noun-line *adj-file* *adj-folder* *noun-file* *noun-folder* i *counto*)
    (println (str adj-line *separator* noun-line))))
