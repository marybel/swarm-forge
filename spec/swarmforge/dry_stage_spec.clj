(ns swarmforge.dry-stage-spec
  (:require [babashka.fs :as fs]
            [speclj.core :refer :all]))

(when-not (find-ns 'dry-stage)
  (load-file "swarmforge/scripts/dry_stage.bb"))

(defn stage-copies! [src-root dest-root]
  ((ns-resolve 'dry-stage 'stage-copies!) src-root dest-root))

(defn exit-code [candidates]
  ((ns-resolve 'dry-stage 'exit-code) candidates))

(defn write-file! [path text]
  (fs/create-dirs (fs/parent path))
  (spit (str path) text))

(defn relative-files [root]
  (set (map #(str (fs/relativize root %))
            (filter fs/regular-file? (file-seq (fs/file root))))))

(describe "stage-copies!"
  (with-all dir (str (fs/create-temp-dir)))

  (it "copies every .bb file as .clj, preserving relative paths"
    (let [src (fs/path @dir "src1")
          dest (fs/path @dir "dest1")]
      (write-file! (fs/path src "a.bb") "(ns a)")
      (write-file! (fs/path src "nested" "b.bb") "(ns b)")
      (stage-copies! (str src) (str dest))
      (should= #{"a.clj" "nested/b.clj"} (relative-files dest))
      (should= "(ns b)" (slurp (str (fs/path dest "nested" "b.clj"))))))

  (it "skips files that are not .bb"
    (let [src (fs/path @dir "src2")
          dest (fs/path @dir "dest2")]
      (write-file! (fs/path src "a.bb") "(ns a)")
      (write-file! (fs/path src "run.sh") "echo hi")
      (write-file! (fs/path src "notes.bb.txt") "x")
      (stage-copies! (str src) (str dest))
      (should= #{"a.clj"} (relative-files dest))))

  (it "removes stale copies left by an earlier run"
    (let [src (fs/path @dir "src3")
          dest (fs/path @dir "dest3")]
      (write-file! (fs/path src "a.bb") "(ns a)")
      (write-file! (fs/path dest "gone.clj") "(ns gone)")
      (stage-copies! (str src) (str dest))
      (should= #{"a.clj"} (relative-files dest)))))

(describe "exit-code"
  (it "is zero when dry4clj reports no candidates"
    (should= 0 (exit-code [])))

  (it "is non-zero when dry4clj reports any candidate"
    (should= 1 (exit-code [{:score 0.9}]))))

(run-specs)
