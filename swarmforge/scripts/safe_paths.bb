(ns safe-paths
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn invalid! [label value]
  (throw (ex-info (str "Invalid " label ": " (pr-str value))
                  {:http-status 400 :error "invalid-identifier"})))

(defn project-name? [value]
  (boolean
   (and (string? value)
        (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" value)
        (not (#{"." ".."} value)))))

(defn task-name? [value]
  (boolean
   (and (string? value)
        (not (str/blank? value))
        (= value (str/trim value))
        (not (#{"." ".."} value))
        (not (re-find #"[\\/\p{Cntrl}]" value)))))

(defn internal-id? [value]
  (boolean
   (and (string? value)
        (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" value)
        (not (#{"." ".."} value)))))

(defn state-key? [value]
  (or (internal-id? value) (task-name? value)))

(defn require-project-name! [value]
  (when-not (project-name? value)
    (invalid! "project name" value))
  value)

(defn require-task-name! [value]
  (when-not (task-name? value)
    (invalid! "task name" value))
  value)

(defn require-internal-id! [value]
  (when-not (internal-id? value)
    (invalid! "identifier" value))
  value)

(defn definition-root [project]
  (let [definition (fs/path project ".swarmforge" "definition")]
    (if (fs/directory? definition) definition project)))

(defn normalized [path]
  (.normalize (.toAbsolutePath (fs/path path))))

(defn canonical-or-normalized [path]
  (try
    (fs/canonicalize path)
    (catch Exception _
      (normalized path))))

(defn nearest-existing [path]
  (loop [candidate (fs/path path)]
    (cond
      (nil? candidate) nil
      (fs/exists? candidate) candidate
      :else (recur (fs/parent candidate)))))

(defn linked-child? [base target]
  (loop [candidate (fs/path target)]
    (cond
      (nil? candidate) false
      (= (normalized candidate) (normalized base)) false
      (fs/sym-link? candidate) true
      :else (recur (fs/parent candidate)))))

(defn contained-path! [dir target label value]
  (let [base (normalized dir)
        target (normalized target)]
    (when-not (and (not= base target) (fs/starts-with? target base))
      (invalid! label value))
    (when (linked-child? base target)
      (invalid! label value))
    (when-let [target-anchor (nearest-existing target)]
      (let [base-anchor (nearest-existing base)
            real-base-anchor (canonical-or-normalized base-anchor)
            real-target-anchor (canonical-or-normalized target-anchor)]
        (when-not (fs/starts-with? real-target-anchor real-base-anchor)
          (invalid! label value))))
    target))

(defn child-path! [dir component suffix validator label]
  (when-not (validator component)
    (invalid! label component))
  (let [base (normalized dir)
        target (normalized (fs/path base (str component suffix)))]
    (contained-path! base target label component)))

(defn project-path! [dir name]
  (child-path! dir name "" project-name? "project name"))

(defn task-path! [dir name suffix]
  (child-path! dir name suffix task-name? "task name"))

(defn id-path! [dir id suffix]
  (child-path! dir id suffix internal-id? "identifier"))

(defn state-key-path! [dir value suffix]
  (child-path! dir value suffix state-key? "state key"))
