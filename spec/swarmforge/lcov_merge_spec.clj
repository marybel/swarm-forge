(ns swarmforge.lcov-merge-spec
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [speclj.core :refer :all]))

(when-not (find-ns 'lcov-merge)
  (load-file "swarmforge/scripts/lcov_merge.bb"))

(defn merge-lcov [texts]
  ((ns-resolve 'lcov-merge 'merge-lcov) texts))

(defn merge-files! [out-path in-paths]
  ((ns-resolve 'lcov-merge 'merge-files!) out-path in-paths))

(defn record [file & line-hits]
  (str "TN:\nSF:" file "\n"
       (apply str (map (fn [[line hits]] (str "DA:" line "," hits "\n")) line-hits))
       "end_of_record\n"))

(defn lines [text]
  (str/split-lines text))

(describe "merge-lcov"
  (it "keeps a single record's line hits"
    (should= (lines (str "TN:\nSF:a.bb\nDA:1,2\nDA:3,0\nLF:2\nLH:1\nend_of_record\n"))
             (lines (merge-lcov [(record "a.bb" [1 2] [3 0])]))))

  (it "sums hits for the same file and line across reports"
    (should-contain "DA:1,5"
                    (lines (merge-lcov [(record "a.bb" [1 2]) (record "a.bb" [1 3])]))))

  (it "keeps a line covered by only one report covered"
    (let [merged (lines (merge-lcov [(record "a.bb" [1 0] [2 0])
                                     (record "a.bb" [1 4] [2 0])]))]
      (should-contain "DA:1,4" merged)
      (should-contain "DA:2,0" merged)
      (should-contain "LF:2" merged)
      (should-contain "LH:1" merged)))

  (it "unions files and lines that appear in only some reports"
    (let [merged (lines (merge-lcov [(record "a.bb" [1 1])
                                     (record "b.bb" [7 2])
                                     (record "a.bb" [9 0])]))]
      (should= ["SF:a.bb" "SF:b.bb"] (filter #(str/starts-with? % "SF:") merged))
      (should-contain "DA:9,0" merged)
      (should-contain "DA:7,2" merged)))

  (it "lists lines in numeric order"
    (should= ["DA:2,1" "DA:10,1"]
             (filter #(str/starts-with? % "DA:")
                     (lines (merge-lcov [(record "a.bb" [10 1] [2 1])])))))

  (it "closes every file with end_of_record"
    (should= 2 (count (filter #{"end_of_record"}
                              (lines (merge-lcov [(record "a.bb" [1 1])
                                                  (record "b.bb" [1 1])]))))))

  (it "merges no reports into an empty report"
    (should= "" (merge-lcov []))))

(describe "merge-files!"
  (it "writes the merged report of the input files to the output path"
    (let [dir (str (fs/create-temp-dir))
          in-a (str dir "/a.info")
          in-b (str dir "/b.info")
          out (str dir "/merged.info")]
      (spit in-a (record "a.bb" [1 1]))
      (spit in-b (record "a.bb" [1 2]))
      (merge-files! out [in-a in-b])
      (should-contain "DA:1,3" (lines (slurp out))))))

(run-specs)
