(ns swarmforge.handoff-test-support
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def repo-root (fs/cwd))
(def scripts-dir (fs/path repo-root "swarmforge" "scripts"))
(def ^:dynamic *temp-dirs* nil)

(defn once-fixture [tests]
  (binding [*temp-dirs* (atom [])]
    (try
      (tests)
      (finally
        (doseq [dir @*temp-dirs*]
          (fs/delete-tree dir))))))

(defn script [name]
  (str (fs/path scripts-dir name)))
(defn tmp-dir []
  (let [dir (fs/create-temp-dir {:prefix "swarmforge-handoff-test."})]
    (when *temp-dirs*
      (swap! *temp-dirs* conj dir))
    dir))
(defn run
  [{:keys [dir env ok?]} & args]
  (let [result (apply sh/sh (concat args [:dir (str dir)
                                          :env (merge {"PATH" (System/getenv "PATH")
                                                       "GIT_CONFIG_NOSYSTEM" "1"}
                                                      env)]))]
    (when (and (not (false? ok?)) (not= 0 (:exit result)))
      (throw (ex-info (str "Command failed: " (str/join " " args))
                      (assoc result :args args))))
    result))
(defn write-file [path text]
  (fs/create-dirs (fs/parent path))
  (spit (str path) text))
(defn read-file [path]
  (slurp (str path)))
(defn read-argv [path]
  (when (fs/exists? path)
    (->> (str/split-lines (slurp (str path)))
         (remove str/blank?)
         (mapv read-string))))
(defn inject-target [argv-vec]
  (second (drop-while #(not= "-t" %) argv-vec)))
(defn inject-literal [argv-vec]
  (second (drop-while #(not= "-l" %) argv-vec)))
(defn submitted-texts
  ([argv] (submitted-texts argv nil))
  ([argv target]
   (loop [calls argv pending nil acc []]
     (if-let [call (first calls)]
       (let [tgt (inject-target call)
             lit (inject-literal call)
             match? (or (nil? target) (= target tgt))]
         (cond
           (not match?) (recur (next calls) pending acc)
           lit (recur (next calls) lit acc)
           pending (recur (next calls) nil (conj acc pending))
           :else (recur (next calls) nil acc)))
       acc))))
(defn init-repo! [root]
  (run {:dir root} "git" "init" "-q")
  (run {:dir root} "git" "config" "user.email" "test@example.com")
  (run {:dir root} "git" "config" "user.name" "Test User")
  (write-file (fs/path root "README.md") "initial\n")
  (run {:dir root} "git" "add" "README.md")
  (run {:dir root} "git" "commit" "-q" "-m" "Initial commit")
  (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD"))))
(defn role-spec-rows [roles]
  (if (map? roles)
    (mapv (fn [[role mode]] [role (or mode "task") ""]) roles)
    (mapv (fn [entry]
            (let [role (first entry)
                  mode (or (second entry) "task")
                  prop (or (nth entry 2 nil) "")]
              [role mode prop]))
          roles)))
(defn setup-project!
  ([root] (setup-project! root {"sender" "task" "receiver" "task"}))
  ([root roles]
   (doseq [dir [".swarmforge/handoffs/outbox/tmp"
                ".swarmforge/handoffs/sent"
                ".swarmforge/handoffs/failed"
                ".swarmforge/handoffs/inbox/new"
                ".swarmforge/handoffs/inbox/in_process"
                ".swarmforge/handoffs/inbox/completed"]]
     (fs/create-dirs (fs/path root dir)))
   (let [rows (role-spec-rows roles)
         role-names (mapv first rows)]
     (write-file
    (fs/path root ".swarmforge/roles.tsv")
    (apply str
           (for [[role mode prop] rows]
             (format "%s\tmaster\t%s\tsession\t%s\tcodex\t%s\t%s\n"
                     role root (str/capitalize role) mode prop))))
     (let [preferred-route (fn [preferred]
                             (let [found (vec (filter (set role-names) preferred))]
                               (if (seq found) found role-names)))
           component (vec (remove #{"QA"} role-names))]
       (write-file
        (fs/path root ".swarmforge/routes.tsv")
        (apply str
               (for [[type route] [["utility" (preferred-route ["coder" "cleaner"])]
                                   ["component" (if (seq component) component role-names)]
                                   ["QA" role-names]
                                   ["review" (preferred-route ["cleaner" "architect" "hardender" "QA"])]]]
                 (str type "\t" (str/join "," route) "\n"))))))))
(defn handoff
  [{:keys [id from to recipient priority type task-id task commit body
           task-base-commit enqueued-at dequeued-at completed-at
           card-type delivery-kind non-forwarding batch-task-ids]}]
  (let [id (or id (str "test-" (System/nanoTime)))]
    (str "id: " id "\n"
       "from: " from "\n"
       "to: " to "\n"
       (when recipient (str "recipient: " recipient "\n"))
       "priority: " priority "\n"
       "type: " type "\n"
       (when task-id (str "task_id: " task-id "\n"))
       (when task (str "task: " task "\n"))
       (when batch-task-ids (str "batch_task_ids: " (pr-str batch-task-ids) "\n"))
       (when commit (str "commit: " commit "\n"))
       (when card-type (str "card_type: " card-type "\n"))
       (when delivery-kind (str "delivery_kind: " delivery-kind "\n"))
       (when non-forwarding (str "non-forwarding: true\n"))
       (when task-base-commit (str "task_base_commit: " task-base-commit "\n"))
       (when enqueued-at (str "enqueued_at: " enqueued-at "\n"))
       (when dequeued-at (str "dequeued_at: " dequeued-at "\n"))
       (when completed-at (str "completed_at: " completed-at "\n"))
       "\n"
         (or body (str "payload for " id)) "\n")))
(defn handoff-path [root state filename]
  (fs/path root ".swarmforge" "handoffs" "inbox" state filename))
(defn put-handoff! [root state filename attrs]
  (let [path (handoff-path root state filename)]
    (write-file path (handoff attrs))
    path))
(defn header [path field]
  (some->> (str/split-lines (read-file path))
           (take-while seq)
           (some (fn [line]
                   (let [prefix (str field ": ")]
                     (when (str/starts-with? line prefix)
                       (subs line (count prefix))))))))
(defn head-sha [root]
  (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD"))))
(defn board-audit-count [root task-name]
  (let [file (fs/path root ".swarmforge/board/tasks.tsv")]
    (when (fs/regular-file? file)
      (some (fn [line]
              (let [[name _lane _created _updated _task-id audit-count]
                    (str/split line #"\t" -1)]
                (when (= task-name name)
                  (Long/parseLong (or (not-empty audit-count) "0")))))
            (str/split-lines (read-file file))))))
(defn audit-pending-dir [root]
  (fs/path root ".swarmforge/handoffs/audit_pending"))
(defn audit-sender-dirs [root]
  (let [dir (audit-pending-dir root)]
    (if (fs/directory? dir)
      (->> (fs/list-dir dir)
           (filter fs/directory?)
           vec)
      [])))
(defn empty-audit-sender-dirs [root]
  (->> (audit-sender-dirs root)
       (filter (fn [d]
                 (empty? (filter fs/regular-file? (fs/list-dir d)))))
       vec))
(defn audit-edn-files [root]
  (let [dir (audit-pending-dir root)]
    (if (fs/directory? dir)
      (vec (fs/glob dir "**/*.edn"))
      [])))
(defn queued-path [out]
  (some->> (str/split-lines out)
           (some (fn [line]
                   (when (str/starts-with? line "HANDOFF QUEUED: ")
                     (subs line (count "HANDOFF QUEUED: ")))))))
(defn outbox-handoffs [root]
  (let [dir (fs/path root ".swarmforge" "handoffs" "outbox")]
    (if (fs/directory? dir)
      (->> (fs/list-dir dir)
           (filter #(and (fs/regular-file? %) (str/ends-with? (fs/file-name %) ".handoff")))
           (sort-by #(fs/file-name %))
           vec)
      [])))
(defn outbox-to [root role]
  (some #(when (str/ends-with? (fs/file-name %) (str "_to_" role ".handoff")) %)
        (outbox-handoffs root)))
(defn handoff-body [path]
  (or (second (str/split (read-file path) #"\n\n" 2)) ""))
(defn audit-and-submit-git-handoff [opts draft]
  (let [first-call (run (assoc opts :ok? false)
                        (script "swarm_handoff.sh") (str draft))]
    (if (and (zero? (:exit first-call))
             (str/includes? (:out first-call) "AUDIT_REQUIRED"))
      (run opts (script "swarm_handoff.sh") (str draft))
      first-call)))
(defn make-queued-handoff!
  ([root filename attrs]
   (let [sha (or (:commit attrs) (head-sha root))]
     (put-handoff! root "new" filename
                   (merge {:from "sender"
                           :to "receiver"
                           :recipient "receiver"
                           :priority "50"
                           :type "git_handoff"
                           :task "task-one"
                           :commit sha
                           :body (str "merge_and_process sender " sha)}
                          attrs)))))
(defn add-worktree! [root name]
  (let [wt (fs/path root ".worktrees" name)]
    (fs/create-dirs (fs/parent wt))
    (run {:dir root} "git" "worktree" "add" "-q" (str wt) "HEAD")
    wt))
(defn seed-operator-task-document! [root receiver document]
  (setup-project! root {"receiver" "task"})
  (write-file (fs/path root ".swarmforge/roles.tsv")
              (format "receiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                      receiver))
  (write-file (fs/path root "tasks/Utility task.md") document)
  (put-handoff! receiver "new" "50_utility.handoff"
                {:id "utility"
                 :from "(New Task)"
                 :to "receiver"
                 :recipient "receiver"
                 :priority "50"
                 :type "note"
                 :task-id "utility-id"
                 :task "Utility task"
                 :body "Build the shim."}))
(defn pack-board
  ([root ok? & args]
   (apply run {:dir root :ok? ok?} (script "pack_board.sh") args)))
(defn commit-work! [root]
  (write-file (fs/path root "slice.md") (str "work " (System/nanoTime) "\n"))
  (run {:dir root} "git" "add" "slice.md")
  (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
  (head-sha root))
(defn queue-git-from! [root role to task]
  (let [draft (fs/path root "tmp" (str role "-" task ".handoff"))]
    (write-file draft (str "type: git_handoff\nto: " to "\npriority: 50\ntask: " task
                           "\n\nPlease also rewrite the layout.\n"))
    (audit-and-submit-git-handoff
     {:dir root :env {"SWARMFORGE_ROLE" role} :ok? false} draft)))
(def four-pack-role-rows
  [["specifier" "task" "forward-only"]
   ["coder" "task" "forward-only"]
   ["refactorer" "task" "back-one"]
   ["architect" "batch" "back-all"]])
(def six-pack-role-rows
  [["specifier" "task" "forward-only"]
   ["coder" "task" "forward-only"]
   ["cleaner" "task" "back-one"]
   ["architect" "batch" "back-all"]
   ["hardender" "task" "forward-only"]
   ["QA" "task" "back-all"]])
