(ns lcov-merge
  (:require [clojure.string :as str]))

(defn- parse-record-line [{:keys [file] :as state} line]
  (cond
    (str/starts-with? line "SF:") (assoc state :file (subs line 3))
    (str/starts-with? line "DA:")
    (let [[line-no hits] (map parse-long (str/split (subs line 3) #","))]
      (update-in state [:hits file line-no] (fnil + 0) hits))
    :else state))

(defn- add-report [hits text]
  (:hits (reduce parse-record-line {:file nil :hits hits} (str/split-lines text))))

(defn- record-lines [file line-hits]
  (concat [(str "SF:" file)]
          (map (fn [[line-no hits]] (str "DA:" line-no "," hits)) line-hits)
          [(str "LF:" (count line-hits))
           (str "LH:" (count (filter pos? (vals line-hits))))
           "end_of_record"]))

(defn merge-lcov [texts]
  (let [hits (reduce add-report (sorted-map) texts)]
    (if (empty? hits)
      ""
      (str (str/join "\n" (cons "TN:" (mapcat (fn [[file line-hits]]
                                                (record-lines file (into (sorted-map) line-hits)))
                                              hits)))
           "\n"))))

(defn merge-files! [out-path in-paths]
  (spit out-path (merge-lcov (map slurp in-paths))))

(defn -main [out-path & in-paths]
  (merge-files! out-path in-paths))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
