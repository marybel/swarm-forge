#!/usr/bin/env bb

(ns ready-for-next-guard
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def script-dir (fs/parent *file*))
(try
  (require 'card-type)
  (catch Exception _
    (load-file (str (fs/path script-dir "card_type.bb")))))
(try
  (require 'safe-paths)
  (catch Exception _
    (load-file (str (fs/path script-dir "safe_paths.bb")))))

(defn command [& args]
  (apply sh/sh args))

(defn git-root []
  (let [result (command "git" "rev-parse" "--show-toplevel")]
    (when (zero? (:exit result))
      (str/trim (:out result)))))

(defn git-common-dir []
  (let [result (command "git" "rev-parse" "--git-common-dir")]
    (when (zero? (:exit result))
      (let [path (str/trim (:out result))]
        (if (fs/absolute? path)
          path
          (str (fs/absolutize path)))))))

(defn roles-at? [root]
  (and root (fs/exists? (fs/path root ".swarmforge" "roles.tsv"))))

(defn project-root []
  (or (let [parent (some-> (git-common-dir) fs/parent str)]
        (when (roles-at? parent) parent))
      (when (roles-at? (git-root)) (git-root))
      (when (roles-at? (fs/cwd)) (str (fs/cwd)))))

(defn same-path? [a b]
  (try
    (= (str (fs/canonicalize a)) (str (fs/canonicalize b)))
    (catch Exception _
      (= (str a) (str b)))))

(defn roles-file []
  (when-let [root (project-root)]
    (fs/path root ".swarmforge" "roles.tsv")))

(defn role-rows []
  (if-let [file (roles-file)]
    (if (fs/exists? file)
      (->> (str/split-lines (slurp (str file)))
           (remove str/blank?)
           (map #(str/split % #"\t" -1))
           vec)
      [])
    []))

(defn infer-role-from-worktree []
  (let [here (or (git-root) (str (fs/absolutize ".")))]
    (some (fn [cols]
            (let [role-name (first cols)
                  wt (nth cols 2 nil)]
              (when (and (not-empty role-name) (not-empty wt) (same-path? wt here))
                role-name)))
          (role-rows))))

(defn current-role []
  (or (not-empty (System/getenv "SWARMFORGE_ROLE"))
      (infer-role-from-worktree)))

(declare header-map)

(defn task-document-relative-path [task-name]
  (when (safe-paths/task-name? task-name)
    (str "tasks/" task-name ".md")))

(defn committed-file [root relative-path]
  (command "git" "-C" root "show" (str "HEAD:" relative-path)))

(defn task-document-committed? [root relative-path file]
  (let [result (committed-file root relative-path)]
    (and (zero? (:exit result))
         (= (:out result) (slurp (str file))))))

(defn task-document-excluded? [root relative-path]
  (zero? (:exit (command "git" "-C" root "check-ignore" "-q" "--no-index" "--" relative-path))))

(defn task-document-fail! [message]
  (binding [*out* *err*]
    (println message))
  (System/exit 1))

(defn require-git-success! [result]
  (when-not (zero? (:exit result))
    (task-document-fail! (str/trim (str (:err result) "\n" (:out result))))))

(defn commit-task-document! [root relative-path]
  (require-git-success! (command "git" "-C" root "add" "--" relative-path))
  (require-git-success! (command "git" "-C" root "commit" "--only"
                                 "-m" "Record task document" "--" relative-path)))

(defn ensure-task-document-committed! [handoff-file]
  (when-let [relative-path (task-document-relative-path
                            (get (header-map handoff-file) "task"))]
    (when-let [project (project-root)]
      (let [source (fs/path project relative-path)
            worktree (git-root)
            destination (when worktree (fs/path worktree relative-path))]
        (when (and worktree (fs/regular-file? source))
          (when-not (same-path? source destination)
            (fs/create-dirs (fs/parent destination))
            (fs/copy source destination {:replace-existing true}))
          (when-not (or (task-document-excluded? worktree relative-path)
                        (task-document-committed? worktree relative-path destination))
            (commit-task-document! worktree relative-path)))))))

(defn header-map [file]
  (into {}
        (for [line (take-while (complement str/blank?) (str/split-lines (slurp (str file))))
              :let [[k v] (str/split line #": " 2)]
              :when (and k v)]
          [k v])))

(defn recursive-handoff-files [dir]
  (if (fs/directory? dir)
    (->> (concat (fs/glob dir "*.handoff")
                 (fs/glob dir "**/*.handoff"))
         (filter fs/regular-file?)
         distinct
         vec)
    []))

(defn pending-approval-dir []
  (when-let [root (project-root)]
    (fs/path root ".swarmforge" "handoffs" "pending_approval")))

(defn active-outbox-dirs []
  (concat
   (when-let [root (project-root)]
     [(fs/path root ".swarmforge" "handoffs" "outbox")])
   (for [cols (role-rows)
         :let [wt (nth cols 2 nil)]
         :when (not (str/blank? wt))]
     (fs/path wt ".swarmforge" "handoffs" "outbox"))))

(defn outbound-git-from-role? [role file]
  (let [headers (header-map file)]
    (and (= "git_handoff" (get headers "type"))
         (= role (get headers "from")))))

(defn active-outbound-git-files [role]
  (if (str/blank? role)
    []
    (let [pending (if-let [dir (pending-approval-dir)]
                    (recursive-handoff-files dir)
                    [])
          outbox-active (mapcat recursive-handoff-files (active-outbox-dirs))]
      (->> (concat pending outbox-active)
           (filter #(outbound-git-from-role? role %))
           distinct
           vec))))

(defn wait-message [active]
  ["WAITING_FOR_APPROVAL: current git handoff is still active"
   (str/join "\n" (map #(str "- " %) active))])

(defn reverse-git-handoff? [headers]
  (and (= "git_handoff" (get headers "type"))
       (or (= "true" (get headers "non-forwarding"))
           (= "00" (get headers "priority")))))

(defn reverse-git-file? [file]
  (reverse-git-handoff? (header-map file)))

(defn merge-from-role [task-name]
  (when (safe-paths/task-name? task-name)
    (when-let [root (project-root)]
      (let [file (safe-paths/task-path! (fs/path root "tasks") task-name ".md")]
        (when (fs/regular-file? file)
          (some (fn [line]
                  (when-let [[_ role] (re-matches #"Merge-from:\s*(\S+)" line)]
                    role))
                (str/split-lines (slurp (str file)))))))))

(defn role-worktree [role]
  (some (fn [cols]
          (when (= role (first cols))
            (not-empty (nth cols 2 nil))))
        (role-rows)))

(defn role-head [role]
  (when-let [wt (role-worktree role)]
    (let [result (command "git" "-C" wt "rev-parse" "HEAD")]
      (when (zero? (:exit result))
        (not-empty (str/trim (:out result)))))))

(defn board-card-type [task-name]
  (when-let [root (project-root)]
    (let [file (fs/path root ".swarmforge" "board" "tasks.tsv")]
      (when (and task-name (fs/regular-file? file))
        (some (fn [line]
                (let [row (card-type/parse-row root line)]
                  (when (= task-name (:name row))
                    (:type row))))
              (str/split-lines (slurp (str file))))))))

(defn card-type-of [file]
  (let [headers (header-map file)]
    (or (not-empty (get headers "card_type"))
        (board-card-type (get headers "task")))))

(defn print-card-briefing! [file]
  (when-let [card-type (card-type-of file)]
    (println "CARD_TYPE:" card-type)
    (when-let [role (current-role)]
      (if (card-type/last-on-card? (project-root) card-type role)
        (println (str "THIS_CARD: last; terminal to: "
                      (str/join "," (card-type/terminal-upstream
                                     (project-root)
                                     (mapv first (role-rows))
                                     card-type))))
        (when-let [nxt (card-type/next-role (project-root) card-type role)]
          (println "THIS_CARD: next" nxt))))))
