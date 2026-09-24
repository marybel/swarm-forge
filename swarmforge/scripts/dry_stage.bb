(ns dry-stage
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- clj-name [rel-path]
  (str/replace (str rel-path) #"\.bb$" ".clj"))

(defn stage-copies! [src-root dest-root]
  (fs/delete-tree dest-root)
  (doseq [src (fs/glob src-root "**.bb")
          :let [dest (fs/path dest-root (clj-name (fs/relativize src-root src)))]]
    (fs/create-dirs (fs/parent dest))
    (fs/copy src dest)))

(defn exit-code [{:keys [candidates]}]
  (if (seq candidates) 1 0))
