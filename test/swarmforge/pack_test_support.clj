(ns swarmforge.pack-test-support
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def six-pack-roles ["specifier" "coder" "cleaner" "architect" "hardender" "QA"])

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
  (let [dir (fs/create-temp-dir {:prefix "swarmforge-pack-ui-test."})]
    (when *temp-dirs*
      (swap! *temp-dirs* conj dir))
    dir))
(defn tmp-tmux-socket []
  (str "/tmp/swarmforge-test-" (System/nanoTime) ".sock"))
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
(defn write-codex-session! [codex-root cwd session-id events]
  (let [path (fs/path codex-root "sessions/2026/09/07"
                      (str "rollout-2026-09-07T12-00-00-" session-id ".jsonl"))
        meta {:timestamp "2026-09-07T12:00:00Z"
              :type "session_meta"
              :payload {:id session-id
                        :session_id session-id
                        :timestamp "2026-09-07T12:00:00Z"
                        :cwd (str cwd)
                        :originator "codex-tui"}}]
    (write-file path
                (str (str/join "\n" (map json/generate-string (cons meta events)))
                     "\n"))
    path))
(defn pack-worktree [root roles role]
  (if (= role (first roles))
    (str root)
    (str (fs/path root ".worktrees" role))))
(defn setup-pack!
  ([root] (setup-pack! root ["specifier"]))
  ([root roles] (setup-pack! root roles {}))
  ([root roles propagation]
   (write-file
    (fs/path root ".swarmforge/roles.tsv")
    (apply str
           (map-indexed
            (fn [i role]
              (format "%s\t%s\t%s\t%s\t%s\tcodex\ttask\t%s\n"
                      role
                      (if (zero? i) "master" role)
                      (pack-worktree root roles role)
                      role
                      (str/capitalize role)
                      (get propagation role "forward-only")))
            roles)))
   (let [route (fn [preferred]
                 (let [found (vec (filter (set roles) preferred))]
                   (if (seq found) found (vec roles))))]
     (write-file
      (fs/path root ".swarmforge/routes.tsv")
      (apply str
             (for [[type preferred] [["utility" ["coder" "cleaner"]]
                                     ["component" ["specifier" "coder" "cleaner" "architect" "hardender"]]
                                     ["QA" ["specifier" "coder" "cleaner" "architect" "hardender" "QA"]]
                                     ["review" ["cleaner" "architect" "hardender" "QA"]]]]
               (str type "\t" (str/join "," (route preferred)) "\n")))))
   (doseq [role roles
           dir [".swarmforge/handoffs/outbox"
                ".swarmforge/handoffs/sent"
                ".swarmforge/handoffs/failed"
                ".swarmforge/handoffs/inbox/new"]]
     (fs/create-dirs (fs/path (pack-worktree root roles role) dir)))
   (fs/create-dirs (fs/path root ".swarmforge/handoffs/pending_approval"))))
(defn pack-board
  ([root ok? & args]
   (apply run {:dir root :ok? ok?} (script "pack_board.sh") args)))
(defn pack-web
  ([root ok? & args]
   (apply run {:dir root :ok? ok?} (script "pack_web.sh") args)))
(defn pack-web-env
  [root env & args]
  (apply run {:dir root :env env} (script "pack_web.sh") args))
(defn set-backend!
  [root backend]
  (let [file (fs/path root ".swarmforge/roles.tsv")]
    (spit (str file)
          (str/replace (slurp (str file)) #"\tcodex\t" (str "\t" backend "\t")))))
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
(defn list-tasks [root]
  (pack-board root true "list" "--root" (str root)))
(defn task-row [listed name]
  (some #(when (str/starts-with? % (str name "\t")) %)
        (str/split-lines listed)))
(defn task-lane [root name]
  (let [cols (str/split (or (task-row (:out (list-tasks root)) name) "") #"\t")]
    (nth cols 1 nil)))
(defn drop-new-task-notes! [root]
  (let [dir (fs/path root ".swarmforge/handoffs/outbox")]
    (when (fs/directory? dir)
      (doseq [file (fs/list-dir dir)]
        (when (str/includes? (fs/file-name file) "New_Task")
          (fs/delete-if-exists file))))))
(defn create-task
  ([root name lane] (create-task root name lane true))
  ([root name lane ok?]
   (let [card-type (case lane
                     "coder" "utility"
                     "cleaner" "utility"
                     "QA" "QA"
                     "component")
         created (pack-board root ok?
                             "create"
                             "--root" (str root)
                             "--name" name
                             "--type" card-type
                             "--text" "Integrate HTW stories")]
     (when (and ok? (zero? (:exit created)))
       (drop-new-task-notes! root)
       (let [now (task-lane root name)]
         (when (and now (not= now lane))
           (pack-board root true
                       "move"
                       "--root" (str root)
                       "--name" name
                       "--lane" lane
                       "--caller" "handoffd"))))
     created)))
(defn increment-audit! [root task-id]
  (pack-board root true "increment-audit" "--root" (str root)
              "--task-id" task-id "--caller" "handoffd"))
(defn queue-handoff! [root {:keys [from to task task-id batch-id batch-task-ids artifacts
                                   delivery-kind non-forwarding priority body]}]
  (let [priority (or priority "50")
        id (str "test-" (System/nanoTime))]
    (write-file
     (fs/path root ".swarmforge/handoffs/outbox"
              (str priority "_from_" from "_to_" (str/replace to #"," "_") ".handoff"))
     (str "id: " id "\n"
          "from: " from "\n"
          "to: " to "\n"
          "priority: " priority "\n"
          "type: git_handoff\n"
          (when task-id (str "task_id: " task-id "\n"))
          "task: " task "\n"
          (when batch-id (str "batch_id: " batch-id "\n"))
          (when batch-task-ids (str "batch_task_ids: " (pr-str batch-task-ids) "\n"))
          (when artifacts (str "artifacts: " artifacts "\n"))
          (when delivery-kind (str "delivery_kind: " delivery-kind "\n"))
          (when non-forwarding "non-forwarding: true\n")
          "\n"
          (or body "payload") "\n"))))
(defn handoff-names [dir]
  (if (fs/directory? dir)
    (->> (fs/list-dir dir)
         (filter #(str/ends-with? (fs/file-name %) ".handoff"))
         (mapv #(fs/file-name %)))
    []))
(defn pending-names [root]
  (handoff-names (fs/path root ".swarmforge/handoffs/pending_approval")))
(defn write-pending-audit! [root task-id]
  (write-file
   (fs/path root ".swarmforge/handoffs/audit_pending/sender" (str task-id ".edn"))
   (str (pr-str {:candidate {:version 1
                             :sender "specifier"
                             :task-id task-id
                             :type "git_handoff"}})
        "\n")))
(defn pending-audits [root]
  (let [dir (fs/path root ".swarmforge/handoffs/audit_pending")]
    (if (fs/directory? dir)
      (vec (fs/glob dir "**/*.edn"))
      [])))
(defn pending-audit-task-ids [root]
  (->> (pending-audits root)
       (map #(get-in (edn/read-string (slurp (str %))) [:candidate :task-id]))
       set))
(defn inbox-names [root roles role]
  (handoff-names (fs/path (pack-worktree root roles role)
                          ".swarmforge/handoffs/inbox/new")))
(defn in-process-dir [root roles role]
  (fs/path (pack-worktree root roles role)
           ".swarmforge/handoffs/inbox/in_process"))
(defn put-in-process! [root roles role {:keys [from task task-id batch-task-ids filename]}]
  (write-file
   (fs/path (in-process-dir root roles role)
            (or filename (str "50_from_" from "_to_" role ".handoff")))
   (str "from: " from "\n"
        "to: " role "\n"
        "priority: 50\n"
        "type: git_handoff\n"
        (when task-id (str "task_id: " task-id "\n"))
        "task: " task "\n"
        (when batch-task-ids (str "batch_task_ids: " (pr-str batch-task-ids) "\n"))
        "\n"
        "payload\n")))
(defn web-state [root]
  (json/parse-string (:out (pack-web root true "--test-state" (str root))) true))
(defn task-card [root name]
  (some #(when (= name (:name %)) %) (:tasks (web-state root))))
(defn start-tmux!
  ([root sessions] (start-tmux! root sessions (tmp-tmux-socket)))
  ([root sessions sock]
   (write-file (fs/path root ".swarmforge/tmux-socket") (str sock "\n"))
   (doseq [session sessions]
     (run {:dir root} "tmux" "-S" sock "new-session" "-d" "-s" session "sleep" "120"))
   sock))
(defn stop-tmux! [sock]
  (run {:dir "." :ok? false} "tmux" "-S" sock "kill-server"))
(defn handoffd-once
  ([root] (handoffd-once root nil))
  ([root env]
   (run {:dir root :env env} "bb" (script "handoffd.bb") "--once" (str root))))
(defn pane-path [root role task]
  (fs/path root ".swarmforge/sessions" role task "pane.txt"))
(defn role-pane-path [root role]
  (fs/path root ".swarmforge/sessions" role "pane.txt"))
(def four-pack-roles ["specifier" "coder" "refactorer" "architect"])
(def reverse-structure-body
  (str "Re-read your role and constitution.\n\n"
       "merge_and_process.sh refactorer abcdef1234\n\n"
       "The inbound tree is the structure. Replay this role's current task onto that shape."))
(def example-task-text
  "Integrate the stories in ~/junk/htw-stories into one console application.")
(def example-task-payload
  (str "Task: htw-console-app\n\n" example-task-text))
(defn wait-file [path timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (fs/exists? path) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 50) (recur))))))
(defn raw-state [root]
  (json/parse-string (:out (pack-web root true "--test-state" (str root)))))
(defn write-pending-approval! [root {:keys [id task task-id artifacts body]}]
  (write-file
   (fs/path root ".swarmforge/handoffs/pending_approval" (str id ".handoff"))
   (str "id: " id "\n"
        "from: specifier\n"
        "to: coder\n"
        "priority: 50\n"
        "type: git_handoff\n"
        "task_id: " (or task-id task) "\n"
        "task: " task "\n"
        (when artifacts (str "artifacts: " artifacts "\n"))
        "\n"
        (or body "payload\n"))))
(defn seed-mini-forge! [root]
  (fs/create-dirs (fs/path root "projects"))
  (write-file (fs/path root "swarmforge/scripts/keep.sh") "#!/bin/sh\n")
  (write-file (fs/path root "swarmforge/constitution/articles/engineering.prompt") "eng\n")
  (write-file (fs/path root "swarmforge/constitution/articles/workflow.prompt") "wf\n")
  (write-file (fs/path root "swarmforge/constitution/articles/handoffs.prompt") "ho\n")
  (write-file (fs/path root ".swarmforge/project-pack/swarmforge/swarmforge.conf")
              "window specifier grok master\nwindow coder grok coder\n")
  (write-file (fs/path root ".swarmforge/project-pack/swarmforge/constitution.prompt") "pack-const\n")
  (write-file (fs/path root ".swarmforge/project-pack/swarmforge/roles/specifier.prompt") "spec\n")
  (write-file (fs/path root ".swarmforge/project-pack/swarmforge/roles/coder.prompt") "coder\n"))
(defn head-sha [dir]
  (str/trim (:out (run {:dir dir} "git" "rev-parse" "HEAD"))))
(defn seed-definition-project! [root]
  (let [dest (fs/path root "projects/cave")
        env {"SWARMFORGE_SKIP_START" "1"}]
    (pack-web-env root env "--test-new-project" (str root) "cave" "lieutenant" "m")
    (pack-web-env root env "--test-close-project" (str root) "cave")
    (write-file (fs/path dest "swarmforge/obsolete.txt") "tracked\n")
    (run {:dir dest} "git" "add" "-A")
    (run {:dir dest} "git" "commit" "-q" "-m" "Track obsolete file")
    (write-file (fs/path dest ".swarmforge/definition/swarmforge/swarmforge.conf")
                "window specifier grok master extra-flag\n")
    (write-file (fs/path dest ".swarmforge/definition/swarmforge/obsolete.txt") "old\n")
    dest))
(defn plant-live-card-for-halt! [root roles]
  (setup-pack! root roles)
  (run {:dir root} "git" "init" "-q")
  (run {:dir root} "git" "config" "user.email" "test@example.com")
  (run {:dir root} "git" "config" "user.name" "Test")
  (write-file (fs/path root "README.md") "base\n")
  (run {:dir root} "git" "add" "README.md")
  (run {:dir root} "git" "commit" "-q" "-m" "base")
  (create-task root "HTW" "specifier")
  (let [task-id (:id (task-card root "HTW"))
        base (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD")))
        in-process (fs/path (in-process-dir root roles "specifier") "50_htw.handoff")
        outbox (fs/path root ".swarmforge/handoffs/outbox/50_from_New_Task_to_specifier.handoff")
        inbox (fs/path root ".swarmforge/handoffs/inbox/new/50_htw.handoff")]
    (write-file in-process
                (str "from: (New Task)\nto: specifier\npriority: 50\ntype: note\n"
                     "task: HTW\ntask_id: " task-id "\ntask_base_commit: " base "\n\nGo\n"))
    (write-file outbox
                (str "from: (New Task)\nto: specifier\npriority: 50\ntype: note\n"
                     "task: HTW\ntask_id: " task-id "\n\nGo\n"))
    (write-file inbox
                (str "from: (New Task)\nto: specifier\npriority: 50\ntype: note\n"
                     "task: HTW\ntask_id: " task-id "\n\nGo\n"))
    (write-pending-approval! root {:id "50_htw" :task "HTW" :task-id task-id})
    (write-pending-approval! root {:id "50_other" :task "Other" :task-id "other-id"})
    (write-pending-audit! root task-id)
    (write-pending-audit! root "unrelated-id")
    (write-file (fs/path root "extra.md") "card work\n")
    (run {:dir root} "git" "add" "extra.md")
    (run {:dir root} "git" "commit" "-q" "-m" "card")
    {:task-id task-id
     :in-process in-process
     :outbox outbox
     :inbox inbox
     :pending (fs/path root ".swarmforge/handoffs/pending_approval/50_htw.handoff")
     :other-pending (fs/path root ".swarmforge/handoffs/pending_approval/50_other.handoff")}))
(defn assert-card-halted! [root roles {:keys [in-process outbox inbox pending other-pending]}]
  (is (= "waiting" (task-lane root "HTW")))
  (is (not (fs/exists? (fs/path root "extra.md"))))
  (is (not (fs/exists? in-process)))
  (is (not (fs/exists? outbox)))
  (is (not (fs/exists? inbox)))
  (is (not (fs/exists? pending)))
  (is (fs/exists? other-pending))
  (is (= #{"unrelated-id"} (pending-audit-task-ids root)))
  (is (empty? (handoff-names (in-process-dir root roles "specifier")))))
