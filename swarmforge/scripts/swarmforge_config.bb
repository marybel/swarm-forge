;; Config parse, workspace, and worktree sync. Loaded into swarmforge.

(defn config-fail! [message]
  (fail! (str red "Error:" reset " " message)))

(defn skip-config-line? [line]
  (or (str/blank? line) (str/starts-with? line "#")))

(defn special-worktree? [worktree]
  (#{"none" "master"} worktree))

(defn visible-window? [directive line-no]
  (case directive
    "window" true
    "window-invisible" false
    (config-fail! (str "Unknown config directive on line " line-no ": " directive))))

(def receive-modes #{"task" "batch"})
(def propagation-modes #{"forward-only" "back-one" "back-all"})
(def known-agents #{"claude" "codex" "copilot" "grok" "deepseek"})
(def worktree-state-files
  {:sessions-file "sessions.tsv"
   :roles-file "roles.tsv"
   :routes-file "routes.tsv"
   :tmux-socket-file "tmux-socket"
   :tmux-env-file "tmux-env"})

(defn role-name? [value]
  (boolean (re-matches #"[A-Za-z][A-Za-z0-9-]*" (or value ""))))

(defn worktree-name? [value]
  (or (special-worktree? value)
      (boolean (and (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" (or value ""))
                    (not (#{"." ".."} value))))))

(defn card-type-name? [value]
  (boolean (re-matches #"[A-Za-z][A-Za-z0-9_-]*" (or value ""))))

(defn receive-fields [trailing]
  (let [[receive-mode after-receive]
        (if (receive-modes (first trailing))
          [(first trailing) (rest trailing)]
          ["task" trailing])
        [propagation extra]
        (if (propagation-modes (first after-receive))
          [(first after-receive) (rest after-receive)]
          ["forward-only" after-receive])]
    [receive-mode propagation extra]))

(defn extra-args-str [tokens]
  (when (seq tokens)
    (str/join " " tokens)))

(defn reject-if [pred message]
  (when pred (config-fail! message)))

(defn validate-window! [ctx line-no role agent worktree receive-mode roles worktrees]
  (reject-if (not (role-name? role))
             (str "Invalid role '" role "' on line " line-no
                  ": use letters, digits, and hyphens, beginning with a letter"))
  (reject-if (contains? roles role)
             (str "Duplicate role '" role "' in " (:config-file ctx)))
  (reject-if (and (not (special-worktree? worktree)) (contains? worktrees worktree))
             (str "Duplicate worktree '" worktree "' in " (:config-file ctx)))
  (reject-if (not (worktree-name? worktree))
             (str "Invalid worktree '" worktree "' for role '" role "'"))
  (reject-if (not (known-agents agent))
             (str "Unsupported agent '" agent "' for role '" role "'"))
  (reject-if (not (#{"task" "batch"} receive-mode))
             (str "Invalid receive mode '" receive-mode "' for role '" role "' on line " line-no ": expected task or batch"))
  (reject-if (not (fs/exists? (fs/path (:roles-dir ctx) (str role ".prompt"))))
             (str "Missing role prompt " (fs/path (:roles-dir ctx) (str role ".prompt")))))

(defn window-row [ctx role agent worktree receive-mode propagation extra-args visible?]
  {:role role
   :agent agent
   :session (session-name-for-role role)
   :display-name (display-name-for-role role)
   :worktree-name worktree
   :worktree-path (if (special-worktree? worktree)
                    (:working-dir ctx)
                    (worktree-path-for-name (:worktrees-dir ctx) worktree))
   :receive-mode receive-mode
   :propagation propagation
   :extra-args extra-args
   :visible? visible?})

(defn parse-window-line [ctx line-no line roles worktrees]
  (let [fields (str/split line #"\s+")]
    (reject-if (< (count fields) 4)
               (str "Invalid config line " line-no ": " line))
    (let [[directive role agent worktree & trailing] fields
          agent (str/lower-case agent)
          [receive-mode propagation extra-tokens] (receive-fields trailing)
          visible? (visible-window? directive line-no)]
      (validate-window! ctx line-no role agent worktree receive-mode roles worktrees)
      (window-row ctx role agent worktree receive-mode propagation (extra-args-str extra-tokens) visible?))))

(defn require-master-worktree! [rows]
  (let [masters (filterv #(= "master" (:worktree-name %)) rows)]
    (reject-if (not= 1 (count masters))
               "Config must name exactly one master worktree")))

(defn parse-card-line [ctx line-no line]
  (let [[_ card-type & route] (str/split line #"\s+")]
    (reject-if (not (card-type-name? card-type))
               (str "Invalid card type on line " line-no ": " (or card-type "")))
    (reject-if (empty? route)
               (str "Card route '" card-type "' is empty on line " line-no))
    (reject-if (not= (count route) (count (distinct route)))
               (str "Card route '" card-type "' repeats a role on line " line-no))
    {:type card-type :roles (vec route) :line-no line-no}))

(defn validate-routes! [ctx rows routes]
  (let [known (set (map :role rows))
        positions (into {} (map-indexed (fn [index row] [(:role row) index]) rows))
        types (map :type routes)]
    (reject-if (not= (count types) (count (distinct types)))
               (str "Duplicate card type in " (:config-file ctx)))
    (doseq [{:keys [type roles line-no]} routes]
      (doseq [role roles]
        (reject-if (not (contains? known role))
                   (str "Unknown role '" role "' in card route '" type
                        "' on line " line-no)))
      (let [indices (mapv positions roles)]
        (reject-if (not= indices (vec (sort indices)))
                   (str "Card route '" type "' does not follow configured window order"))))))

(defn parse-config [ctx]
  (when-not (fs/exists? (:config-file ctx))
    (config-fail! (str "Config not found at " (:config-file ctx))))
  (when-not (fs/exists? (:constitution-file ctx))
    (config-fail! (str "Constitution prompt not found at " (:constitution-file ctx))))
  (loop [lines (map-indexed vector (str/split-lines (slurp (str (:config-file ctx)))))
         rows []
         routes []
         roles #{}
         worktrees #{}]
    (if-let [[line-index raw-line] (first lines)]
      (let [line-no (inc line-index)
            line (str/trim raw-line)]
        (if (skip-config-line? line)
          (recur (next lines) rows routes roles worktrees)
          (if (str/starts-with? line "card ")
            (recur (next lines) rows (conj routes (parse-card-line ctx line-no line)) roles worktrees)
            (let [row (parse-window-line ctx line-no line roles worktrees)
                  worktree (:worktree-name row)]
              (recur (next lines)
                     (conj rows row)
                     routes
                     (conj roles (:role row))
                     (cond-> worktrees (not (special-worktree? worktree)) (conj worktree)))))))
      (do
        (reject-if (empty? rows)
                   (str "No windows defined in " (:config-file ctx)))
        (require-master-worktree! rows)
        (validate-routes! ctx rows routes)
        (assoc ctx :roles rows :routes routes)))))

(defn write-sessions-file! [ctx]
  (spit (str (:sessions-file ctx))
        (apply str
               (map-indexed
                (fn [index row]
                  (format "%d\t%s\t%s\t%s\t%s\n"
                          (inc index) (:role row) (:session row) (:display-name row) (:agent row)))
                (:roles ctx)))))

(defn write-roles-file! [ctx]
  (spit (str (:roles-file ctx))
        (apply str
               (for [row (:roles ctx)]
                 (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
                         (:role row)
                         (:worktree-name row)
                         (:worktree-path row)
                         (:session row)
                         (:display-name row)
                         (:agent row)
                         (:receive-mode row)
                         (:propagation row))))))

(defn write-routes-file! [ctx]
  (spit (str (:routes-file ctx))
        (apply str
               (for [{:keys [type roles]} (:routes ctx)]
                 (str type "\t" (str/join "," roles) "\n")))))

(def required-helpers
  ["handoff_lib.bb" "swarm_handoff.sh" "swarm_handoff.bb"
   "swarm_tool.sh" "swarm_tool.bb"
   "commit-msg-hook.sh" "commit_msg_hook.bb"
   "merge_and_process.sh" "merge_and_process.bb"
   "ready_for_next.sh" "ready_for_next.bb"
   "ready_for_next_guard.bb"
   "done_with_current.sh" "done_with_current.bb"
   "ready_for_next_task.sh" "ready_for_next_task.bb"
   "done_with_current_task.sh" "done_with_current_task.bb"
   "ready_for_next_batch.sh" "ready_for_next_batch.bb"
   "done_with_current_batch.sh" "done_with_current_batch.bb"
   "handoffd.bb" "stop_handoff_daemon.bb" "stop_handoff_daemon.sh"
   "swarm-cleanup.sh" "swarm-window-watchdog.sh" "swarm_window_watchdog.bb"
   "swarm-terminal-adapter.sh" "swarmforge.sh" "swarmforge.bb"
   "pack_board.sh" "pack_board.bb"
   "pack_web.sh" "pack_web.bb"
   "pack_dashboard_request.sh" "pack_dashboard_request.bb"])

(def terminal-helpers
  ["terminal-app.sh" "iterm2.sh" "ghostty.sh" "windows-terminal.sh" "none.sh"])

(def required-libraries ["card_type.bb" "safe_paths.bb" "handoff_state.bb"])

(defn check-helper-scripts! [ctx]
  (doseq [helper required-helpers]
    (let [path (fs/path (:script-dir ctx) helper)]
      (when-not (and (fs/exists? path) (fs/executable? path))
        (fail! (str red "Error:" reset " Required helper script not found or not executable: " path)))))
  (doseq [helper terminal-helpers]
    (let [path (fs/path (:script-dir ctx) "terminal-adapters" helper)]
      (when-not (and (fs/exists? path) (fs/executable? path))
        (fail! (str red "Error:" reset " Required terminal adapter not found or not executable: " path)))))
  (doseq [library required-libraries]
    (let [path (fs/path (:script-dir ctx) library)]
      (when-not (fs/regular-file? path)
        (fail! (str red "Error:" reset " Required script library not found: " path))))))

(defn git-hooks-dir [ctx]
  (let [path (sh-out "git" "-C" (str (:working-dir ctx)) "rev-parse" "--git-path" "hooks")
        dir (fs/path path)]
    (if (fs/absolute? dir)
      dir
      (fs/path (:working-dir ctx) dir))))

(defn install-commit-msg-hook! [ctx]
  (let [dir (git-hooks-dir ctx)
        hook (fs/path dir "commit-msg")
        saved (fs/path dir "commit-msg.before-swarmforge")
        bb (str (fs/absolutize (fs/path (:script-dir ctx) "commit_msg_hook.bb")))]
    (fs/create-dirs dir)
    (let [managed? (and (fs/regular-file? hook)
                        (try
                          (str/includes? (slurp (str hook)) "SWARMFORGE COMBINED COMMIT-MSG HOOK")
                          (catch Exception _ false)))
          hook-present? (or (fs/exists? hook) (fs/sym-link? hook))
          saved-present? (or (fs/exists? saved) (fs/sym-link? saved))]
      (when (and hook-present? (not managed?))
        (when saved-present?
          (config-fail! (str "Cannot install commit-msg hook: " saved " already exists")))
        (fs/move hook saved))
      (spit (str hook)
            (str "#!/usr/bin/env zsh\n"
                 "# SWARMFORGE COMBINED COMMIT-MSG HOOK v1\n"
                 "set -u\n"
                 "bb " (sq bb) " \"$@\" || exit $?\n"
                 "saved=" (sq (str saved)) "\n"
                 "if [[ -x \"$saved\" ]]; then\n"
                 "  \"$saved\" \"$@\"\n"
                 "fi\n")))
    (fs/set-posix-file-permissions hook "rwxr-xr-x")))

(defn remove-commit-msg-hook! [ctx]
  (let [dir (git-hooks-dir ctx)
        hook (fs/path dir "commit-msg")
        saved (fs/path dir "commit-msg.before-swarmforge")
        managed? (and (fs/regular-file? hook)
                      (try
                        (str/includes? (slurp (str hook)) "SWARMFORGE COMBINED COMMIT-MSG HOOK")
                        (catch Exception _ false)))]
    (when (and (or (fs/exists? hook) (fs/sym-link? hook)) (not managed?))
      (config-fail! "Cannot remove SwarmForge hook: commit-msg was changed"))
    (when managed?
      (fs/delete-if-exists hook))
    (when (or (fs/exists? saved) (fs/sym-link? saved))
      (fs/move saved hook))))

(defn prepare-workspace! [ctx]
  (doseq [dir [(:state-dir ctx) (:notify-dir ctx) (:prompts-dir ctx)
               (:worktrees-dir ctx) (:tmux-socket-dir ctx) (:daemon-dir ctx)]]
    (fs/create-dirs dir))
  (spit (str (:tmux-socket-file ctx)) (str (:tmux-socket ctx) "\n"))
  (check-helper-scripts! ctx)
  (write-sessions-file! ctx)
  (write-roles-file! ctx)
  (write-routes-file! ctx))

(defn prepare-worktrees! [ctx]
  (doseq [row (:roles ctx)
          :let [worktree-name (:worktree-name row)
                worktree-path (:worktree-path row)
                branch-name (str "swarmforge-" worktree-name)]
          :when (not (#{"none" "master"} worktree-name))]
    (when-not (or (fs/exists? (fs/path worktree-path ".git"))
                  (fs/directory? (fs/path worktree-path ".git")))
      (sh "git" "-C" (str (:working-dir ctx)) "worktree" "add" "--force" "-B" branch-name (str worktree-path) "HEAD"))))

(defn prepare-handoff-dirs! [ctx]
  (doseq [row (:roles ctx)
          dir ["outbox/tmp" "sent" "failed" "inbox/new" "inbox/in_process" "inbox/completed"]]
    (fs/create-dirs (fs/path (:worktree-path row) ".swarmforge" "handoffs" dir))))

(defn write-tmux-env-file! [ctx]
  (spit (str (:tmux-env-file ctx))
        (str (sh-out "tmux" "-S" (:tmux-socket ctx) "display-message" "-p" "#{socket_path},#{pid},#{pane_id}") "\n")))

(defn mirror-tree! [src dest]
  (when (fs/exists? dest)
    (fs/delete-tree dest))
  (when (fs/directory? src)
    (fs/create-dirs (fs/parent dest))
    (fs/copy-tree src dest)))

(defn sync-worktree-roles! [ctx worktree-path]
  (mirror-tree! (:roles-dir ctx) (fs/path worktree-path "swarmforge" "roles"))
  (mirror-tree! (fs/path (:swarm-forge-dir ctx) "constitution")
                (fs/path worktree-path "swarmforge" "constitution"))
  (when (fs/exists? (:constitution-file ctx))
    (fs/create-dirs (fs/path worktree-path "swarmforge"))
    (fs/copy (:constitution-file ctx)
             (fs/path worktree-path "swarmforge" "constitution.prompt")
             {:replace-existing true})))

(defn sync-worktree-state! [ctx worktree-path]
  (let [role-state-dir (fs/path worktree-path ".swarmforge")]
    (fs/create-dirs (fs/path role-state-dir "notify"))
    (doseq [[ctx-key file-name] worktree-state-files]
      (fs/copy (ctx-key ctx) (fs/path role-state-dir file-name) {:replace-existing true}))))

(defn sync-worktree-scripts! [ctx]
  (doseq [row (:roles ctx)
          :let [worktree-path (:worktree-path row)]
          :when (not= (str worktree-path) (str (:working-dir ctx)))]
    (when-not (:definition-project? ctx)
      (mirror-tree! (:script-dir ctx) (fs/path worktree-path "swarmforge" "scripts"))
      (sync-worktree-roles! ctx worktree-path))
    (sync-worktree-state! ctx worktree-path)))
