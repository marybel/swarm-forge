#!/usr/bin/env bb

(ns handoffd
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def poll-ms 1000)
(def wake-message
  "You have new handoff mail. If idle, run ready_for_next.sh.")

(defn usage []
  (binding [*out* *err*]
    (println "Usage: handoffd.bb [--once] <project-root>"))
  (System/exit 1))

(def once? false)
(def project-root nil)
(def script-dir (fs/parent *file*))
(try
  (require 'card-type)
  (catch Exception _
    (load-file (str (fs/path script-dir "card_type.bb")))))
(try
  (require 'safe-paths)
  (catch Exception _
    (load-file (str (fs/path script-dir "safe_paths.bb")))))
(try
  (require '[handoff-state :as handoff-state])
  (catch Exception _
    (load-file (str (fs/path script-dir "handoff_state.bb")))
    (require '[handoff-state :as handoff-state])))
(def state-dir nil)
(def daemon-dir nil)
(def roles-file nil)
(def socket-file nil)
(def pid-file nil)
(def stop-file nil)
(def log-file nil)
(def stopping-flag (atom false))

(defn configure!
  ([] (configure! *command-line-args*))
  ([args]
   (let [once-flag (boolean (some #(= "--once" %) args))
         root (first (remove #(= "--once" %) args))]
     (when-not root
       (usage))
     (let [state (fs/path root ".swarmforge")
           daemon (fs/path state "daemon")]
       (alter-var-root #'once? (constantly once-flag))
       (alter-var-root #'project-root (constantly root))
       (alter-var-root #'state-dir (constantly state))
       (alter-var-root #'daemon-dir (constantly daemon))
       (alter-var-root #'roles-file (constantly (fs/path state "roles.tsv")))
       (alter-var-root #'socket-file (constantly (fs/path state "tmux-socket")))
       (alter-var-root #'pid-file (constantly (fs/path daemon "handoffd.pid")))
       (alter-var-root #'stop-file (constantly (fs/path daemon "stop")))
       (alter-var-root #'log-file (constantly (fs/path daemon "handoffd.log")))))))

(defn now []
  (.format (java.time.format.DateTimeFormatter/ISO_INSTANT)
           (java.time.Instant/now)))

(defn log! [& parts]
  (fs/create-dirs daemon-dir)
  (spit (str log-file)
        (str (now) " " (str/join " " parts) "\n")
        :append true))

(defn safe-log! [& parts]
  (try
    (apply log! parts)
    (catch Exception _ nil)))

(defn read-lines [path]
  (when (fs/exists? path)
    (str/split-lines (slurp (str path)))))

(defn load-roles []
  (into {}
        (for [line (read-lines roles-file)
              :when (not (str/blank? line))
              :let [[role worktree-name worktree-path session display agent receive-mode]
                    (str/split line #"\t")]]
          [role {:role role
                 :worktree-name worktree-name
                 :worktree-path worktree-path
                 :session session
                 :display display
                 :agent agent
                 :receive-mode (or receive-mode "task")}])))

(defn parse-message [path]
  (let [content (slurp (str path))
        [header body] (str/split content #"\n\n" 2)
        headers (into {}
                      (for [line (str/split-lines header)
                            :let [[k v] (str/split line #": " 2)]
                            :when (and k v)]
                        [k v]))]
    {:headers headers
     :body (or body "")
     :content content}))

(defn render-message [headers body]
  (let [preferred ["id" "from" "to" "recipient" "priority" "type" "role" "task_id" "task" "commit"
                   "artifacts" "batch_id" "batch_task_ids" "card_type" "delivery_kind"
                   "task_base_commit" "message" "created_at" "enqueued_at" "dequeued_at" "completed_at"]
        remaining (->> (keys headers)
                       (remove (set preferred))
                       sort)
        ordered (concat preferred remaining)]
    (str (str/join "\n"
                   (for [k ordered
                         :let [v (get headers k)]
                         :when v]
                     (str k ": " v)))
         "\n\n"
         body)))

(defn add-delivery-headers [message recipient]
  (-> message
      (assoc-in [:headers "recipient"] recipient)
      (assoc-in [:headers "enqueued_at"] (now))))

(defn target-path [role-info filename]
  (fs/path (:worktree-path role-info)
           ".swarmforge" "handoffs" "inbox" "new" filename))

(defn notify! [socket session & [message]]
  (let [text (or message wake-message)
        send-text (sh "tmux" "-S" socket "send-keys" "-t" session "-l" text)
        _ (Thread/sleep 150)
        send-carriage-return (sh "tmux" "-S" socket "send-keys" "-t" session "C-m")
        _ (Thread/sleep 50)
        send-line-feed (sh "tmux" "-S" socket "send-keys" "-t" session "C-j")]
    (when-not (zero? (:exit send-text))
      (throw (ex-info "tmux send text failed" send-text)))
    (when-not (zero? (:exit send-carriage-return))
      (throw (ex-info "tmux send carriage return failed" send-carriage-return)))
    (when-not (zero? (:exit send-line-feed))
      (throw (ex-info "tmux send line feed failed" send-line-feed)))))

(defn move-with-collision [source target-dir]
  (fs/create-dirs target-dir)
  (let [base (fs/file-name source)
        target (fs/path target-dir base)]
    (if (fs/exists? target)
      (fs/move source
               (fs/path target-dir (str (now) "_" base))
               {:replace-existing false})
      (fs/move source target {:replace-existing false}))))

(declare clear-retry-state!)

(defn fail! [path reason]
  (let [headers (:headers (parse-message path))
        failed-dir (fs/path (fs/parent (fs/parent path)) "failed")]
    (log! "failed" (str path) reason)
    (clear-retry-state! headers path)
    (let [moved (move-with-collision path failed-dir)]
      (spit (str (fs/path failed-dir (str (fs/file-name moved) ".error")))
            (str reason "\n")))))

(defn recipient-list [headers]
  (some->> (get headers "to")
           (#(str/split % #","))
           (map str/trim)
           (remove str/blank?)
           seq))

(defn board-file []
  (fs/path project-root ".swarmforge" "board" "tasks.tsv"))

(defn pack-board! [& args]
  (let [script (str (fs/path script-dir "pack_board.sh"))
        result (apply sh (concat [script] args ["--caller" "handoffd" "--root" (str project-root)]))]
    (when-not (zero? (:exit result))
      (log! "pack-board-failed" args (:err result) (:out result))
      (throw (ex-info (str/trim (str (:err result) "\n" (:out result))) result)))))

(defn archive-sender! [headers]
  (let [from (get headers "from")]
    (when (and (not (str/blank? from))
               (not (re-matches #"\(.+\)" from)))
      (pack-board! "archive" "--archive" from))))

(defn master-role-name [roles]
  (some (fn [[role info]]
          (when (= "master" (:worktree-name info))
            role))
        roles))

(defn specifier-pack? [roles]
  (contains? roles "specifier"))

(defn from-master? [roles headers]
  (= (get headers "from") (master-role-name roles)))

(defn non-forwarding? [headers]
  (boolean
   (or (= "true" (get headers "non-forwarding"))
       (#{"reverse" "terminal"} (get headers "delivery_kind")))))

(defn reverse-git-mail? [headers]
  (and (= "git_handoff" (get headers "type"))
       (or (= "reverse" (get headers "delivery_kind"))
           (= "terminal" (get headers "delivery_kind"))
           (non-forwarding? headers)
           (= "00" (get headers "priority")))))

(defn pack-role-names []
  (->> (read-lines roles-file)
       (remove str/blank?)
       (mapv #(first (str/split % #"\t")))))

(defn last-pack-role? [role]
  (= role (last (pack-role-names))))

(defn listed-handoffs [dir]
  (if (fs/directory? dir)
    (->> (fs/list-dir dir)
         (filter #(and (fs/regular-file? %)
                       (str/ends-with? (fs/file-name %) ".handoff")))
         vec)
    []))

(defn listed-batches [dir]
  (if (fs/directory? dir)
    (->> (fs/list-dir dir)
         (filter #(and (fs/directory? %)
                       (str/starts-with? (fs/file-name %) "batch_")))
         vec)
    []))

(defn inbox-handoffs [role-info state]
  (let [dir (fs/path (:worktree-path role-info)
                     ".swarmforge" "handoffs" "inbox" state)]
    (into (listed-handoffs dir)
          (mapcat listed-handoffs (listed-batches dir)))))

(defn role-has-inbox-state? [role-info state]
  (boolean (seq (inbox-handoffs role-info state))))

(defn task-key [headers]
  (or (not-empty (get headers "task_id"))
      (get headers "task")))

(defn parsed-batch-task-ids [headers]
  (let [value (get headers "batch_task_ids")]
    (if (str/blank? value)
      []
      (try
        (let [parsed (edn/read-string value)]
          (when (and (vector? parsed)
                     (every? #(and (string? %) (not (str/blank? %))) parsed))
            parsed))
        (catch Exception _ nil)))))

(defn valid-batch-task-ids? [headers]
  (let [value (get headers "batch_task_ids")
        parsed (parsed-batch-task-ids headers)]
    (or (str/blank? value)
        (and (seq parsed)
             (= parsed (vec (distinct parsed)))
             (= (task-key headers) (first parsed))))))

(defn valid-batch-id? [headers]
  (let [batch-id (get headers "batch_id")]
    (or (str/blank? batch-id)
        (and (safe-paths/state-key? batch-id)
             (seq (parsed-batch-task-ids headers))))))

(defn batch-task-keys [headers]
  (let [parsed (parsed-batch-task-ids headers)]
    (if (seq parsed) parsed [(task-key headers)])))

(defn board-row-for-key [key]
  (some (fn [line]
          (let [row (card-type/parse-row project-root line)]
            (when (or (= key (:id row))
                      (= (str/lower-case (or key ""))
                         (str/lower-case (or (:name row) ""))))
              row)))
        (or (read-lines (board-file)) [])))

(defn board-row-for-headers [headers]
  (board-row-for-key (task-key headers)))

(def delivery-kinds #{"forward" "reverse" "terminal"})

(defn expected-terminal-recipients [headers]
  (let [from (get headers "from")
        row (board-row-for-headers headers)]
    (cond
      (and row (card-type/last-on-card? project-root (:type row) from))
      (card-type/terminal-upstream project-root (pack-role-names) (:type row))

      (and (nil? row) (last-pack-role? from))
      (vec (butlast (pack-role-names)))

      :else nil)))

(defn exact-recipient-set? [actual expected]
  (and (some? expected)
       (= (count actual) (count expected))
       (= (set actual) (set expected))))

(defn exact-terminal-recipients? [headers]
  (exact-recipient-set? (vec (recipient-list headers))
                        (expected-terminal-recipients headers)))

(defn effective-delivery-kind [headers]
  (or (get headers "delivery_kind")
      (cond
        (exact-terminal-recipients? headers) "terminal"
        (or (= "true" (get headers "non-forwarding"))
            (= "00" (get headers "priority"))) "reverse"
        :else "forward")))

(defn terminal-handoff? [_roles headers]
  (and (= "git_handoff" (get headers "type"))
       (= "terminal" (effective-delivery-kind headers))
       (exact-terminal-recipients? headers)))

(defn board-row-key [line]
  (let [[name _lane _created _updated task-id] (str/split line #"\t" -1)]
    (or (not-empty task-id) name)))

(defn board-row-name [line]
  (first (str/split line #"\t" -1)))

(defn board-name-for-key [task-key]
  (some (fn [line]
          (let [name (board-row-name line)]
            (when (or (= task-key (board-row-key line))
                      (= task-key name))
              name)))
        (read-lines (board-file))))

(defn forge-root []
  (let [parent (fs/parent project-root)
        grand (when parent (fs/parent parent))]
    (when (and parent grand
               (= "projects" (fs/file-name parent))
               (fs/directory? (fs/path grand "projects")))
      (str grand))))

(defn notify-event [headers]
  (if (terminal-handoff? nil headers)
    "card-done"
    (str (or (not-empty (get headers "from")) "unknown") "-handoff")))

(defn notify-lieutenant! [headers]
  (when-let [forge (forge-root)]
    (let [event (notify-event headers)
          dir (fs/path forge ".swarmforge" "notify")
          stamp (str/replace (now) #"[^0-9A-Za-z]" "")
          file (fs/path dir (str stamp "-" event ".notify"))
          socket-path (fs/path forge ".swarmforge" "tmux-socket")
          socket (when (fs/exists? socket-path)
                   (not-empty (str/trim (slurp (str socket-path)))))]
      (fs/create-dirs dir)
      (spit (str file)
            (str "project: " (fs/file-name project-root) "\n"
                 "event: " event "\n"
                 "from: " (get headers "from") "\n"
                 "task: " (or (get headers "task") "") "\n"))
      (when socket
        (try
          (notify! socket "swarmforge-lieutenant" (str "Notify: " event))
          (catch Exception e
            (log! "lieutenant-notify-failed" (.getMessage e))))))))

(defn update-board! [roles headers]
  (when (and (fs/exists? (board-file))
             (= "git_handoff" (get headers "type"))
             (seq (recipient-list headers)))
    (cond
      (terminal-handoff? roles headers)
      (let [keys (batch-task-keys headers)]
        (if (next keys)
          (pack-board! "transition-batch" "--task-ids" (pr-str keys) "--lane" "done")
          (let [name (or (board-name-for-key (first keys)) (get headers "task"))]
            (when-not (str/blank? name)
              (pack-board! "done" "--name" name)))))

      (non-forwarding? headers)
      nil

      :else
      (let [keys (batch-task-keys headers)
            lane (first (recipient-list headers))]
        (if (next keys)
          (pack-board! "transition-batch" "--task-ids" (pr-str keys) "--lane" lane)
          (let [task (or (board-name-for-key (first keys)) (get headers "task"))]
            (when-not (str/blank? task)
              (pack-board! "move" "--name" task "--lane" lane))))))))

(defn single-recipient? [headers]
  (let [recipients (recipient-list headers)]
    (boolean (and recipients (nil? (next recipients))))))

(defn already-approved? [headers]
  (not (str/blank? (get headers "approved"))))

(defn should-hold? [roles headers]
  (and (= "git_handoff" (get headers "type"))
       (specifier-pack? roles)
       (from-master? roles headers)
       (single-recipient? headers)
       (not (already-approved? headers))))

(defn pending-dir []
  (fs/path state-dir "handoffs" "pending_approval"))

(defn hold! [path]
  (move-with-collision path (pending-dir))
  (log! "held" (str path)))

(defn phantom-sender? [from]
  (boolean (re-matches #"\(.+\)" (or from ""))))

(defn sent-dir [roles sender-role]
  (if (phantom-sender? sender-role)
    (fs/path project-root ".swarmforge" "handoffs" "sent")
    (fs/path (get-in roles [sender-role :worktree-path])
             ".swarmforge" "handoffs" "sent")))

(declare outbox-files queue-wakeup!)

(defn approved-git-handoff? [headers]
  (and (= "git_handoff" (get headers "type"))
       (not (str/blank? (get headers "approved")))))

(defn outbound-git-from-role? [role file]
  (let [headers (:headers (parse-message file))]
    (and (= "git_handoff" (get headers "type"))
         (= role (get headers "from")))))

(defn active-outbound-git-files [roles sender-role]
  (if (str/blank? sender-role)
    []
    (let [pending (listed-handoffs (pending-dir))
          outbox (->> (concat (mapcat #(or (outbox-files %) []) (vals roles))
                              (or (outbox-files {:worktree-path project-root}) []))
                      distinct)]
      (->> (concat pending outbox)
           (filter #(outbound-git-from-role? sender-role %))
           vec))))

(defn sender-ready-work? [roles sender-role]
  (when-let [role-info (get roles sender-role)]
    (and (role-has-inbox-state? role-info "new")
         (not (role-has-inbox-state? role-info "in_process"))
         (empty? (active-outbound-git-files roles sender-role)))))

(defn maybe-notify-unblocked-sender! [roles socket headers sender-role]
  (when (and (approved-git-handoff? headers)
             (sender-ready-work? roles sender-role)
             (not (contains? (set (recipient-list headers)) sender-role)))
    (try
      (notify! socket (get-in roles [sender-role :session]))
      (safe-log! "notified-unblocked-sender" sender-role)
      (catch Exception e
        (try
          (queue-wakeup! headers sender-role (.getMessage e))
          (catch Exception queue-error
            (safe-log! "sender-wake-queue-failed" sender-role
                       (.getMessage queue-error))))
        (safe-log! "sender-wake-failed" sender-role (.getMessage e))))))

(defn retry-file [path]
  (fs/path (str path ".retry.edn")))

(defn read-edn-file [path]
  (when (fs/regular-file? path)
    (try (edn/read-string (slurp (str path)))
         (catch Exception _ nil))))

(defn epoch-ms []
  (.toEpochMilli (java.time.Instant/now)))

(defn retry-delay-ms [attempt]
  (min 60000 (* 1000 (long (Math/pow 2 (min 6 (max 0 (dec attempt))))))))

(defn write-edn-atomic! [path value]
  (fs/create-dirs (fs/parent path))
  (let [tmp (fs/create-temp-file {:dir (fs/parent path) :prefix ".state."})]
    (spit (str tmp) (str (pr-str value) "\n"))
    (fs/move tmp path {:replace-existing true :atomic-move true})))

(defn attention-dir []
  (fs/path state-dir "handoffs" "delivery_attention"))

(defn safe-stem [value]
  (str/replace (or value "handoff") #"[^A-Za-z0-9._-]+" "_"))

(defn reverse-cycle-file []
  (fs/path daemon-dir "reverse-cycle.edn"))

(defn new-reverse-cycle []
  {:id (str (safe-stem (now)) "-" (java.util.UUID/randomUUID))
   :started-at (now)})

(defn reverse-cleared-event-file [forge cycle]
  (fs/path forge ".swarmforge" "notify"
           (str (safe-stem (:id cycle)) "-reverse-cleared.notify")))

(defn write-reverse-cleared-event! [forge cycle]
  (let [file (reverse-cleared-event-file forge cycle)]
    (if (fs/exists? file)
      false
      (let [dir (fs/parent file)
            tmp (do
                  (fs/create-dirs dir)
                  (fs/create-temp-file {:dir dir :prefix ".reverse-cleared."}))]
        (try
          (spit (str tmp)
                (str "project: " (fs/file-name project-root) "\n"
                     "event: reverse-cleared\n"
                     "cycle: " (:id cycle) "\n"))
          (try
            (fs/move tmp file {:replace-existing false :atomic-move true})
            true
            (catch java.nio.file.FileAlreadyExistsException _
              false))
          (finally
            (fs/delete-if-exists tmp)))))))

(defn notify-reverse-cleared! [cycle]
  (when-let [forge (forge-root)]
    (when (write-reverse-cleared-event! forge cycle)
      (let [socket-path (fs/path forge ".swarmforge" "tmux-socket")
            socket (when (fs/regular-file? socket-path)
                     (not-empty (str/trim (slurp (str socket-path)))))]
        (when socket
          (try
            (notify! socket "swarmforge-lieutenant" "Notify: reverse-cleared")
            (catch Exception e
              (safe-log! "lieutenant-notify-failed" (.getMessage e)))))))))

(defn read-reverse-cycle []
  (let [file (reverse-cycle-file)]
    (when (fs/regular-file? file)
      (or (read-edn-file file)
          {:id (safe-stem (str/trim (slurp (str file))))}))))

(defn reconcile-reverse-cycle! []
  (let [file (reverse-cycle-file)
        active? (handoff-state/synchronization-active? project-root)
        cycle (read-reverse-cycle)]
    (cond
      (and active? (nil? cycle))
      (write-edn-atomic! file (new-reverse-cycle))

      (and (not active?) cycle)
      (do
        (notify-reverse-cleared! cycle)
        (fs/delete-if-exists file)))))

(defn attention-file [headers path]
  (fs/path (attention-dir)
           (str (safe-stem (or (get headers "id") (fs/file-name path))) ".edn")))

(defn clear-retry-state! [headers path]
  (fs/delete-if-exists (retry-file path))
  (fs/delete-if-exists (attention-file headers path)))

(defn record-retry! [path error]
  (let [file (retry-file path)
        prior (or (read-edn-file file) {})
        attempt (inc (long (or (:attempt prior) 0)))
        state {:attempt attempt
               :error error
               :updated-at (now)
               :next-at (+ (epoch-ms) (retry-delay-ms attempt))}
        headers (:headers (parse-message path))]
    (write-edn-atomic! file state)
    (when (>= attempt 3)
      (write-edn-atomic! (attention-file headers path)
                         (assoc state
                                :id (get headers "id")
                                :task (get headers "task")
                                :from (get headers "from"))))
    (log! "retry" (str path) (str "attempt=" attempt) error)))

(defn retry-due? [path]
  (let [state (read-edn-file (retry-file path))]
    (or (nil? state) (<= (long (or (:next-at state) 0)) (epoch-ms)))))

(defn permanent-error [message]
  (ex-info message {:permanent true}))

(defn raw-recipients [headers]
  (mapv str/trim (str/split (or (get headers "to") "") #"," -1)))

(defn validate-delivery-kind! [headers recipients]
  (let [type (get headers "type")
        declared (get headers "delivery_kind")
        kind (effective-delivery-kind headers)
        expected-terminal (expected-terminal-recipients headers)]
    (when (and declared (not (delivery-kinds declared)))
      (throw (permanent-error "invalid delivery_kind header")))
    (when (and declared (not= "git_handoff" type))
      (throw (permanent-error "delivery_kind is only valid for git_handoff")))
    (when (= "git_handoff" type)
      (case kind
        "terminal"
        (do
          (when (and declared (not= "true" (get headers "non-forwarding")))
            (throw (permanent-error "terminal delivery must be non-forwarding")))
          (when-not (exact-recipient-set? recipients expected-terminal)
            (throw (permanent-error
                    (str "terminal recipient set must be exactly "
                         (str/join "," (or expected-terminal [])))))))

        "reverse"
        (when (and declared (not= "true" (get headers "non-forwarding")))
          (throw (permanent-error "reverse delivery must be non-forwarding")))

        "forward"
        (when expected-terminal
          (throw (permanent-error
                  (str "last role must send one terminal handoff to "
                       (str/join "," expected-terminal)))))

        (throw (permanent-error "invalid delivery_kind header"))))))

(defn same-delivery? [source-headers target recipient]
  (let [target-headers (:headers (parse-message target))]
    (and (= (get source-headers "id") (get target-headers "id"))
         (= recipient (get target-headers "recipient")))))

(defn validate-header-fields!
  "Pure header-shape checks: no filesystem or roles access."
  [headers sender-role]
  (when (str/blank? (get headers "id"))
    (throw (permanent-error "missing id header")))
  (try
    (safe-paths/require-internal-id! (get headers "id"))
    (catch Exception _
      (throw (permanent-error "invalid id header"))))
  (when (str/blank? sender-role)
    (throw (permanent-error "missing from header")))
  (when-not (#{"git_handoff" "note"} (get headers "type"))
    (throw (permanent-error "missing or invalid type header")))
  (when-not (re-matches #"[0-9][0-9]" (or (get headers "priority") ""))
    (throw (permanent-error "missing or invalid priority header")))
  (when (and (= "git_handoff" (get headers "type"))
             (str/blank? (task-key headers)))
    (throw (permanent-error "missing task header")))
  (when (and (= "git_handoff" (get headers "type"))
             (not (valid-batch-task-ids? headers)))
    (throw (permanent-error "invalid batch_task_ids header")))
  (when (and (= "git_handoff" (get headers "type"))
             (not (valid-batch-id? headers)))
    (throw (permanent-error "invalid batch_id header"))))

(defn validate-board-task-keys! [headers]
  (when (and (= "git_handoff" (get headers "type"))
             (fs/regular-file? (board-file)))
    (doseq [key (batch-task-keys headers)]
      (when-not (safe-paths/state-key? key)
        (throw (permanent-error "invalid board task key")))
      (when-not (board-row-for-key key)
        (throw (permanent-error (str "unknown board task " key)))))))

(defn validate-recipient-set!
  "Pure recipient-shape checks: no filesystem or roles access."
  [recipients]
  (when (or (empty? recipients) (some str/blank? recipients))
    (throw (permanent-error "missing or empty recipient")))
  (when-not (= (count recipients) (count (distinct recipients)))
    (throw (permanent-error "duplicate recipient"))))

(defn validate-recipients! [roles recipients headers filename]
  (doseq [recipient recipients]
    (let [role-info (get roles recipient)]
      (when-not role-info
        (throw (permanent-error (str "unknown recipient " recipient))))
      (when-not (fs/directory? (:worktree-path role-info))
        (throw (ex-info (str "recipient worktree unavailable: " recipient) {})))
      (let [target (target-path role-info filename)]
        (when (and (fs/exists? target)
                   (not (same-delivery? headers target recipient)))
          (throw (permanent-error (str "conflicting recipient file " target))))))))

(defn preflight! [roles sender-role path message]
  (let [headers (:headers message)
        recipients (raw-recipients headers)
        filename (fs/file-name path)]
    (validate-header-fields! headers sender-role)
    (validate-board-task-keys! headers)
    (validate-recipient-set! recipients)
    (validate-delivery-kind! headers recipients)
    (when (and (not (phantom-sender? sender-role)) (nil? (get roles sender-role)))
      (throw (permanent-error (str "unknown sender " sender-role))))
    (validate-recipients! roles recipients headers filename)
    recipients))

(defn store-recipient! [message role-info recipient filename]
  (let [target (target-path role-info filename)]
    (when-not (fs/exists? target)
      (let [delivered (add-delivery-headers message recipient)
            dir (fs/parent target)
            tmp (do (fs/create-dirs dir)
                    (fs/create-temp-file {:dir dir :prefix ".delivery."}))]
        (try
          (spit (str tmp) (render-message (:headers delivered) (:body delivered)))
          (fs/move tmp target {:replace-existing false :atomic-move true})
          (finally
            (fs/delete-if-exists tmp)))))))

(defn wakeup-dir []
  (fs/path daemon-dir "wakeups"))

(defn wakeup-file [handoff-id recipient]
  (fs/path (wakeup-dir) (str (safe-stem handoff-id) "--" (safe-stem recipient) ".edn")))

(defn queue-wakeup! [headers recipient error]
  (write-edn-atomic! (wakeup-file (get headers "id") recipient)
                     {:id (get headers "id")
                      :recipient recipient
                      :attempt 1
                      :next-at (+ (epoch-ms) 1000)
                      :error error}))

(defn notify-or-queue! [roles socket headers recipient]
  (try
    (notify! socket (get-in roles [recipient :session]))
    (catch Exception e
      (try
        (queue-wakeup! headers recipient (.getMessage e))
        (safe-log! "wake-queued" recipient (.getMessage e))
        (catch Exception queue-error
          (safe-log! "wake-queue-failed" recipient
                     (.getMessage queue-error)))))))

(defn process-wakeups! [roles socket]
  (when (fs/directory? (wakeup-dir))
    (doseq [file (fs/list-dir (wakeup-dir))
            :when (fs/regular-file? file)
            :let [state (read-edn-file file)]
            :when (and state (<= (long (or (:next-at state) 0)) (epoch-ms)))]
      (let [recipient (:recipient state)
            info (get roles recipient)]
        (if-not info
          (fs/delete-if-exists file)
          (try
            (notify! socket (:session info))
            (fs/delete-if-exists file)
            (catch Exception e
              (let [attempt (inc (long (or (:attempt state) 0)))]
                (write-edn-atomic! file (assoc state
                                               :attempt attempt
                                               :next-at (+ (epoch-ms) (retry-delay-ms attempt))
                                               :error (.getMessage e)))))))))))

(defn deliver! [roles socket sender-role path]
  (let [filename (fs/file-name path)
        message (parse-message path)
        headers (:headers message)
        recipients (preflight! roles sender-role path message)]
    (doseq [recipient recipients]
      (store-recipient! message (get roles recipient) recipient filename))
    (update-board! roles headers)
    (archive-sender! headers)
    (move-with-collision path (sent-dir roles sender-role))
    ;; Delivery is committed once the source reaches sent/. Nothing after this
    ;; point is allowed to turn it back into a delivery failure.
    (try
      (clear-retry-state! headers path)
      (catch Exception e
        (safe-log! "retry-cleanup-failed" (str path) (.getMessage e))))
    (doseq [recipient recipients]
      (notify-or-queue! roles socket headers recipient))
    (try
      (maybe-notify-unblocked-sender! roles socket headers sender-role)
      (catch Exception e
        (safe-log! "sender-wake-processing-failed" sender-role (.getMessage e))))
    (try
      (notify-lieutenant! headers)
      (catch Exception e
        (safe-log! "lieutenant-event-failed" (.getMessage e))))
    (safe-log! "delivered" (str path))))

(defn outbox-files [role-info]
  (let [outbox (fs/path (:worktree-path role-info) ".swarmforge" "handoffs" "outbox")]
    (when (fs/exists? outbox)
      (->> (fs/list-dir outbox)
           (filter #(and (fs/regular-file? %)
                         (str/ends-with? (fs/file-name %) ".handoff")))
           (sort-by #(fs/file-name %))))))

(defn should-stop? []
  (or @stopping-flag (fs/exists? stop-file)))

(defn sleep-poll! [ms]
  (loop [remaining ms]
    (when (and (pos? remaining) (not (should-stop?)))
      (let [step (min remaining 100)]
        (Thread/sleep step)
        (recur (- remaining step))))))

(defn process-outbox-file! [roles socket path]
  (let [headers (:headers (parse-message path))
        from (get headers "from")]
    (if (should-hold? roles headers)
      (do
        (hold! (fs/path path))
        (try (notify-lieutenant! headers)
             (catch Exception e (log! "lieutenant-event-failed" (.getMessage e)))))
      (deliver! roles socket (or from "") (fs/path path)))))

(defn poll-once! []
  (when-not (should-stop?)
    (let [roles (load-roles)
          socket (str/trim (slurp (str socket-file)))
          paths (->> (concat (mapcat #(or (outbox-files %) []) (vals roles))
                             (or (outbox-files {:worktree-path project-root}) []))
                     (map str)
                     distinct
                     vec)]
      (doseq [path paths
              :while (not (should-stop?))
              :when (retry-due? path)]
        (try
          (process-outbox-file! roles socket path)
          (catch Exception e
            (log! "error" path (.getMessage e))
            (if (:permanent (ex-data e))
              (try
                (fail! (fs/path path) (.getMessage e))
                (catch Exception nested
                  (log! "failed-to-archive" path (.getMessage nested))))
              (try
                (record-retry! (fs/path path) (.getMessage e))
                (catch Exception nested
                  (log! "failed-to-record-retry" path (.getMessage nested))))))))
      (process-wakeups! roles socket)
      (try
        (reconcile-reverse-cycle!)
        (catch Exception e
          (safe-log! "reverse-cycle-failed" (.getMessage e)))))))

(defn shutdown! []
  (reset! stopping-flag true)
  (try
    (fs/delete-if-exists pid-file)
    (log! "stopped")
    (catch Exception _ nil)))

(defn run-daemon! []
  (fs/create-dirs daemon-dir)
  (fs/delete-if-exists stop-file)
  (spit (str pid-file) (str (.pid (java.lang.ProcessHandle/current)) "\n"))
  (.addShutdownHook (Runtime/getRuntime) (Thread. shutdown!))
  (log! "started")
  (try
    (while (not (should-stop?))
      (poll-once!)
      (sleep-poll! poll-ms))
    (finally
      (fs/delete-if-exists pid-file)
      (log! "stopped"))))

(defn -main [& args]
  (configure! (if (seq args) args *command-line-args*))
  (if once?
    (poll-once!)
    (run-daemon!)))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
