(ns swarmforge.coverage-in-process-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [commit-msg-hook]
            [handoff-lib]
            [handoffd]
            [merge-and-process]
            [pack-board]
            [pack-dashboard-request]
            [pack-web]
            [ready-for-next-guard]
            [stop-handoff-daemon]
            [swarm-handoff]
            [swarm-tool]
            [swarm-window-watchdog]
            [swarmforge]))

(defn- tmp-dir []
  (fs/create-temp-dir {:prefix "swarmforge-in-process-test."}))

(deftest commit-msg-hook-builds-bylines
  (is (= "By specifier." (commit-msg-hook/byline "specifier")))
  (is (= "hello\n\nBy coder.\n" (commit-msg-hook/append-byline "hello" "coder"))))

(deftest watchdog-rewrites-window-state-in-process
  (let [root (tmp-dir)
        state-file (fs/path root "windows.tsv")
        ids-file (fs/path root "window-ids")]
    (try
      (spit (str state-file)
            (str "1\told-a\tswarmforge-coder\tSwarmForge Coder\n"
                 "2\told-b\tswarmforge-cleaner\tSwarmForge Cleaner\n"))
      (spit (str ids-file) "old-a\nold-b\n")
      (swarm-window-watchdog/rewrite-window-id! state-file ids-file "2" "new-b")
      (is (re-find #"2\tnew-b\tswarmforge-cleaner\tSwarmForge Cleaner" (slurp (str state-file))))
      (is (= "old-a\nnew-b\n" (slurp (str ids-file))))
      (finally
        (fs/delete-tree root)))))

(deftest handoff-lib-validates-priority-and-headers
  (is (handoff-lib/valid-priority? "10"))
  (is (not (handoff-lib/valid-priority? "5")))
  (let [root (tmp-dir)
        file (fs/path root "task.handoff")]
    (try
      (spit (str file) "task: alpha\nfrom: coder\n\nbody\n")
      (is (= "alpha" (handoff-lib/header-field file "task")))
      (is (= "body\n" (handoff-lib/body file)))
      (finally
        (fs/delete-tree root)))))

(deftest swarm-handoff-parses-note-draft
  (let [root (tmp-dir)
        draft (fs/path root "note.handoff")]
    (try
      (spit (str draft) "type: note\nto: cleaner\npriority: 50\nmessage: hello\n")
      (let [{:keys [headers errors]} (swarm-handoff/parse-draft draft)]
        (is (empty? errors))
        (is (= "note" (get headers "type")))
        (is (= "cleaner" (get headers "to")))
        (is (= "hello" (get headers "message"))))
      (finally
        (fs/delete-tree root)))))

(deftest pack-web-reads-query-values
  (is (= "HTW" (pack-web/query-value "/api/task?name=HTW" "name")))
  (is (nil? (pack-web/query-value "/api/task" "name"))))

(deftest pack-web-caches-board-list-for-one-state-request
  (let [root (tmp-dir)
        list-calls (atom 0)
        listed "HTW\tspecifier\tcreated\tupdated\ttask-1\t0\tQA\n"]
    (try
      (with-redefs [pack-web/pack-board
                    (fn [_ & args]
                      (when (= ["list"] args)
                        (swap! list-calls inc))
                      listed)
                    pack-web/dashboard-state
                    (fn [request-root]
                      [(pack-web/board-tasks request-root)
                       (pack-web/board-tasks request-root)])]
        (let [[first-read second-read] (pack-web/api-state root)]
          (is (= first-read second-read))
          (is (= 1 @list-calls))))
      (finally
        (fs/delete-tree root)))))

(deftest pack-board-parses-flags
  (is (= {:positional ["list"] :root "/tmp/root"}
         (pack-board/parse-args ["list" "--root" "/tmp/root"]))))

(deftest swarmforge-classifies-worktrees-and-windows
  (is (swarmforge/special-worktree? "master"))
  (is (swarmforge/special-worktree? "none"))
  (is (not (swarmforge/special-worktree? "coder")))
  (is (true? (swarmforge/visible-window? "window" 1)))
  (is (false? (swarmforge/visible-window? "window-invisible" 2))))

(deftest handoffd-parses-recipients-and-messages
  (is (= ["coder" "cleaner"] (handoffd/recipient-list {"to" "coder, cleaner"})))
  (is (true? (handoffd/non-forwarding? {"non-forwarding" "true"})))
  (is (true? (handoffd/phantom-sender? "(New Task)")))
  (is (false? (handoffd/phantom-sender? "coder")))
  (let [root (tmp-dir)
        file (fs/path root "mail.handoff")]
    (try
      (spit (str file) "from: coder\nto: cleaner\ntype: note\n\npayload\n")
      (let [message (handoffd/parse-message file)]
        (is (= "coder" (get-in message [:headers "from"])))
        (is (= "payload\n" (:body message)))
        (is (re-find #"from: coder" (handoffd/render-message (:headers message) (:body message)))))
      (finally
        (fs/delete-tree root)))))

(deftest merge-and-process-usage-text-names-the-script
  (is (re-find #"merge_and_process" merge-and-process/usage-text)))

(deftest ready-for-next-guard-formats-wait-message
  (let [lines (ready-for-next-guard/wait-message ["/tmp/a.handoff"])]
    (is (re-find #"WAITING_FOR_APPROVAL" (first lines)))
    (is (re-find #"/tmp/a.handoff" (second lines)))))

(deftest handoff-lib-rewrites-headers
  (is (= ["task: beta" "" "body"]
         (vec (handoff-lib/set-header-lines ["task: alpha" "" "body"] "task" "beta"))))
  (is (= ["task: alpha" "from: coder"]
         (vec (handoff-lib/append-header ["task: alpha"] "from: " "coder"))))
  (let [root (tmp-dir)
        file (fs/path root "item.handoff")]
    (try
      (spit (str file) "task: alpha\n\nbody\n")
      (handoff-lib/set-header! file "task" "beta")
      (is (re-find #"task: beta" (slurp (str file))))
      (handoff-lib/print-task file)
      (is (not (handoff-lib/roles-at? nil)))
      (is (handoff-lib/same-path? "/tmp" "/tmp"))
      (finally
        (fs/delete-tree root)))))

(defn- git-command [disambiguate-out object-type short-out]
  (fn [_dir & args]
    (cond
      (some #(str/starts-with? % "--disambiguate=") args)
      {:exit 0 :out disambiguate-out}
      (= ["git" "cat-file" "-t"] (take 3 args))
      {:exit 0 :out object-type}
      (some #(= "--short=10" %) args)
      {:exit 0 :out short-out}
      :else {:exit 1 :out "" :err "unexpected git"})))

(defn- with-validate-mocks [{:keys [known? command]} f]
  (with-redefs [swarm-handoff/git-cwd (constantly ".")
                swarm-handoff/role-known? (or known? (constantly true))
                swarm-handoff/command (or command (git-command "abcdef1234\n" "commit\n" "abcdef1234\n"))]
    (f)))

(defn- has-error? [result re]
  (boolean (some #(re-find re %) (:errors result))))

(deftest swarm-handoff-validates-headers
  (is (= [[] []] (swarm-handoff/validate-recipients "")))
  (is (= [] (swarm-handoff/current-work-state-errors {"type" "note"})))
  (is (= [] (swarm-handoff/task-state-errors {"type" "note"} "coder")))
  (let [root (tmp-dir)
        draft (fs/path root "bad.handoff")]
    (try
      (spit (str draft) "type: note\ntype: note\npriority: 50\nto: x\nmessage: hi\n")
      (is (seq (:errors (swarm-handoff/parse-draft draft))))
      (finally
        (fs/delete-tree root))))
  (with-validate-mocks {}
    (fn []
      (let [ok (swarm-handoff/validate
                {"type" "note" "to" "receiver" "priority" "50" "message" "hello"}
                ["type" "to" "priority" "message"])]
        (is (empty? (:errors ok)))
        (is (= ["receiver"] (:recipients ok)))
        (is (nil? (:canonical-commit ok))))
      (let [missing (swarm-handoff/validate {} [])]
        (is (has-error? missing #"Missing required header 'type'"))
        (is (has-error? missing #"Missing required header 'to'"))
        (is (has-error? missing #"Missing required header 'priority'")))
      (let [bad-type (swarm-handoff/validate
                      {"type" "fax" "to" "receiver" "priority" "50"}
                      ["type" "to" "priority"])]
        (is (has-error? bad-type #"must be one of git_handoff or note")))
      (let [bad-priority (swarm-handoff/validate
                          {"type" "note" "to" "receiver" "priority" "zz" "message" "hi"}
                          ["type" "to" "priority" "message"])]
        (is (has-error? bad-priority #"two digits from 00 to 99")))
      (let [illegal (swarm-handoff/validate
                     {"type" "note" "to" "receiver" "priority" "50" "message" "hi" "commit" "abcdef1234" "task" "nope"}
                     ["type" "to" "priority" "message" "commit" "task"])]
        (is (has-error? illegal #"Header 'commit' is not allowed for type 'note'"))
        (is (has-error? illegal #"Header 'task' is not allowed for type 'note'"))
        (is (has-error? illegal #"Header 'commit' is only allowed for git_handoff"))
        (is (has-error? illegal #"Header 'task' is only allowed for git_handoff")))
      (let [note-msg (swarm-handoff/validate
                      {"type" "note" "to" "receiver" "priority" "50"}
                      ["type" "to" "priority"])]
        (is (has-error? note-msg #"Missing required header 'message'")))
      (let [long-note (swarm-handoff/validate
                       {"type" "note" "to" "receiver" "priority" "50"
                        "message" (apply str (repeat 81 "x"))}
                       ["type" "to" "priority" "message"])]
        (is (has-error? long-note #"Header 'message' must be no longer than 80")))
      (let [git-msg (swarm-handoff/validate
                     {"type" "git_handoff" "to" "receiver" "priority" "50"
                      "task_id" "t1" "task" "t1" "commit" "abcdef1234" "message" "nope"}
                     ["type" "to" "priority" "task_id" "task" "commit" "message"])]
        (is (has-error? git-msg #"Header 'message' is not allowed for type 'git_handoff'"))
        (is (has-error? git-msg #"Header 'message' is only allowed for note")))
      (let [git-missing (swarm-handoff/validate
                         {"type" "git_handoff" "to" "receiver" "priority" "50"}
                         ["type" "to" "priority"])]
        (is (has-error? git-missing #"Missing required header 'commit'"))
        (is (has-error? git-missing #"Missing required header 'task_id'"))
        (is (has-error? git-missing #"Missing required header 'task'")))
      (let [bad-sha (swarm-handoff/validate
                     {"type" "git_handoff" "to" "receiver" "priority" "50"
                      "task_id" "t1" "task" "t1" "commit" "not-a-sha!"}
                     ["type" "to" "priority" "task_id" "task" "commit"])]
        (is (has-error? bad-sha #"exactly 10 hexadecimal characters")))
      (let [long-task (swarm-handoff/validate
                       {"type" "git_handoff" "to" "receiver" "priority" "50"
                        "task_id" "t1" "task" (apply str (repeat 81 "t")) "commit" "abcdef1234"}
                       ["type" "to" "priority" "task_id" "task" "commit"])]
        (is (has-error? long-task #"Header 'task' must be no longer than 80")))
      (let [ok-git (swarm-handoff/validate
                    {"type" "git_handoff" "to" "receiver" "priority" "50"
                     "task_id" "t1" "task" "t1" "commit" "abcdef1234"}
                    ["type" "to" "priority" "task_id" "task" "commit"])]
        (is (empty? (:errors ok-git)))
        (is (= "abcdef1234" (:canonical-commit ok-git))))))
  (with-validate-mocks {:command (git-command "aaa\nbbb\n" "commit\n" "aaa\n")}
    (fn []
      (let [result (swarm-handoff/validate
                    {"type" "git_handoff" "to" "receiver" "priority" "50"
                     "task_id" "t1" "task" "t1" "commit" "abcdef1234"}
                    ["type" "to" "priority" "task_id" "task" "commit"])]
        (is (has-error? result #"must resolve to exactly one Git object")))))
  (with-validate-mocks {:command (git-command "abcdef1234\n" "blob\n" "abcdef1234\n")}
    (fn []
      (let [result (swarm-handoff/validate
                    {"type" "git_handoff" "to" "receiver" "priority" "50"
                     "task_id" "t1" "task" "t1" "commit" "abcdef1234"}
                    ["type" "to" "priority" "task_id" "task" "commit"])]
        (is (has-error? result #"must resolve to a commit")))))
  (with-validate-mocks {:known? (constantly false)}
    (fn []
      (let [result (swarm-handoff/validate
                    {"type" "note" "to" "ghost" "priority" "50" "message" "hi"}
                    ["type" "to" "priority" "message"])]
        (is (has-error? result #"Unknown recipient role 'ghost'")))))
  (let [[_ errors] (with-validate-mocks {:known? (constantly true)}
                     (fn [] (swarm-handoff/validate-recipients "receiver,,receiver,bad_role")))]
    (is (some #(re-find #"empty recipient" %) errors))
    (is (some #(re-find #"underscores" %) errors))
    (is (some #(re-find #"Duplicate recipient 'receiver'" %) errors))))

(deftest pack-web-routes-and-parsing
  (is (= 404 (:status (pack-web/handle-get nil "/missing"))))
  (is (= 404 (:status (pack-web/handle-post nil "/nope" ""))))
  (is (seq (pack-web/codex-bullets "• one\n  continued\n• two\n")))
  (is (true? (pack-web/confirm-teardown? "TEARDOWN")))
  (is (false? (pack-web/confirm-teardown? "no")))
  (is (= "&lt;x&gt;" (pack-web/html-escape "<x>")))
  (let [entry (pack-web/task-entry nil "HTW\tspecifier\tnow\tnow\tid1\t2\tcomponent")]
    (is (= "HTW" (:name entry)))
    (is (= 2 (:audit_count entry))))
  (is (seq (pack-web/parse-unified-diff "--- a\n+++ b\n@@ -1 +1 @@\n-old\n+new\n")))
  (let [missing (pack-web/handle-request nil {:method "HEAD" :uri "/missing" :body nil})]
    (is (= 404 (:status missing)))))

(deftest swarmforge-launch-helpers
  (is (= "iterm2" (swarmforge/normalize-terminal-backend "iTerm")))
  (is (= "terminal-app" (swarmforge/normalize-terminal-backend "terminal")))
  (is (= "windows-terminal" (swarmforge/normalize-terminal-backend "wt")))
  (is (= "none" (swarmforge/normalize-terminal-backend "none")))
  (is (= "ghostty" (swarmforge/normalize-terminal-backend "ghostty")))
  (is (= "--yolo " (swarmforge/yolo-flag "codex" {:extra-args ""})))
  (is (= "" (swarmforge/yolo-flag "codex" {:extra-args "--yolo"})))
  (is (= "--permission-mode bypassPermissions "
         (swarmforge/yolo-flag "claude" {:extra-args ""})))
  (is (= "" (swarmforge/yolo-flag "unknown" {:extra-args ""})))
  (is (swarmforge/skip-config-line? "# hi"))
  (is (swarmforge/skip-config-line? ""))
  (is (not (swarmforge/special-worktree? "coder")))
  ;; nil is the correct result on a host with no sleep-inhibitor tool
  ;; available (e.g. a container with no systemd running); the "explicitly
  ;; disabled" case is covered by swarmforge-sleep-prevention-can-be-disabled.
  (let [prefix (swarmforge/sleep-inhibitor-prefix)]
    (is (or (nil? prefix) (vector? prefix)))))

(deftest swarmforge-resolves-the-command-each-backend-needs
  (is (= "codex" (swarmforge/required-command "deepseek")))
  (is (= "claude" (swarmforge/required-command "claude")))
  (is (swarmforge/codex-backed? "deepseek"))
  (is (swarmforge/codex-backed? "codex"))
  (is (not (swarmforge/codex-backed? "claude"))))

(deftest swarmforge-checks-the-command-behind-each-role-backend
  (let [checked (atom [])]
    (with-redefs [swarmforge/check-dependency! #(swap! checked conj %)]
      (swarmforge/check-backend-dependencies!
       {:roles [{:agent "deepseek"} {:agent "claude"}]}))
    (is (= ["codex" "claude"] @checked))))

(deftest swarmforge-role-session-pipes-pane-output-to-its-log
  (let [root (tmp-dir)
        log-file (fs/path root "logs" "coder.log")
        calls (atom [])]
    (try
      (with-redefs [swarmforge/sh (fn [& args] (swap! calls conj (vec args)))]
        (swarmforge/create-role-session! {:tmux-socket "sock"} "swarmforge-coder" "Coder" log-file))
      (is (fs/directory? (fs/parent log-file)))
      (is (= ["tmux" "-S" "sock" "new-session" "-d" "-s" "swarmforge-coder" "-n" swarmforge/agent-window]
             (first @calls)))
      (is (= ["tmux" "-S" "sock" "pipe-pane" "-o" "-t" "swarmforge-coder"
              (str "cat >> '" log-file "'")]
             (last @calls)))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-boot-creates-a-logged-session-per-role
  (let [created (atom [])
        env-written (atom false)
        ctx {:logs-dir (fs/path "state" "logs")
             :roles [{:role "coder" :session "swarmforge-coder" :display-name "Coder"}
                     {:role "cleaner" :session "swarmforge-cleaner" :display-name "Cleaner"}]}]
    (is (= (fs/path "state" "logs" "coder.log") (swarmforge/pane-log-file-for ctx "coder")))
    (with-redefs [swarmforge/create-role-session! (fn [_ & args] (swap! created conj (vec args)))
                  swarmforge/write-tmux-env-file! (fn [_] (reset! env-written true))]
      (with-out-str (swarmforge/boot-sessions! ctx)))
    (is (= [["swarmforge-coder" "Coder" (fs/path "state" "logs" "coder.log")]
            ["swarmforge-cleaner" "Cleaner" (fs/path "state" "logs" "cleaner.log")]]
           @created))
    (is (true? @env-written))))

(defn- sync-fixture [root]
  (let [state (fs/path root ".swarmforge")
        scripts (fs/path root "scripts")
        worktree (fs/path root ".worktrees" "coder")]
    (fs/create-dirs state)
    (fs/create-dirs scripts)
    (spit (str (fs/path scripts "helper.sh")) "helper\n")
    (doseq [[_ file-name] swarmforge/worktree-state-files]
      (spit (str (fs/path state file-name)) (str file-name "\n")))
    {:worktree worktree
     :ctx {:working-dir root
           :script-dir scripts
           :roles [{:worktree-path worktree}]
           :sessions-file (fs/path state "sessions.tsv")
           :roles-file (fs/path state "roles.tsv")
           :routes-file (fs/path state "routes.tsv")
           :tmux-socket-file (fs/path state "tmux-socket")
           :tmux-env-file (fs/path state "tmux-env")}}))

(deftest swarmforge-sync-copies-state-files-into-a-role-worktree
  (let [root (tmp-dir)
        {:keys [ctx worktree]} (sync-fixture root)]
    (try
      (swarmforge/sync-worktree-state! ctx worktree)
      (is (fs/directory? (fs/path worktree ".swarmforge" "notify")))
      (doseq [[_ file-name] swarmforge/worktree-state-files]
        (is (= (str file-name "\n") (slurp (str (fs/path worktree ".swarmforge" file-name))))))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-sync-mirrors-scripts-unless-the-project-is-a-definition
  (let [root (tmp-dir)
        {:keys [ctx worktree]} (sync-fixture root)
        mirrored (fs/path worktree "swarmforge" "scripts" "helper.sh")]
    (try
      (swarmforge/sync-worktree-scripts! (assoc ctx :definition-project? true))
      (is (not (fs/exists? mirrored)))
      (is (fs/exists? (fs/path worktree ".swarmforge" "roles.tsv")))
      (swarmforge/sync-worktree-scripts! (assoc ctx :definition-project? false
                                                :roles-dir (fs/path root "roles")
                                                :swarm-forge-dir (fs/path root "swarmforge")))
      (is (= "helper\n" (slurp (str mirrored))))
      (finally
        (fs/delete-tree root)))))

(defn- launch-ctx [root]
  (let [state (fs/path root ".swarmforge")]
    (fs/create-dirs (fs/path state "prompts"))
    {:working-dir root
     :script-dir (fs/path "forge" "scripts")
     :prompts-dir (fs/path state "prompts")
     :roles-dir (fs/path root "roles")
     :terminal-backend "none"
     :tmux-socket "sock"
     :window-ids-file (fs/path state "window-ids")
     :roles [{:role "coder" :session "swarmforge-coder"}]}))

(defn- launch-row [agent & [overrides]]
  (merge {:role "coder" :agent agent :display-name "Coder"
          :worktree-path "wt" :extra-args nil}
         overrides))

(deftest swarmforge-launch-command-builds-the-cli-for-each-agent
  (let [root (tmp-dir)
        ctx (launch-ctx root)
        command (fn [agent & [overrides]] (swarmforge/launch-command ctx 1 (launch-row agent overrides)))]
    (try
      (let [claude (command "claude")
            codex (command "codex")
            copilot (command "copilot")
            grok (command "grok")
            deepseek (command "deepseek")]
        (is (str/includes? claude "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 claude --append-system-prompt-file "))
        (is (str/includes? claude "--permission-mode bypassPermissions -n 'SwarmForge Coder' "))
        (is (str/includes? codex "codex -C 'wt' --no-alt-screen --yolo "))
        (is (not (str/includes? codex "--profile")))
        (is (str/includes? copilot "copilot -C 'wt' --no-alt-screen --name 'SwarmForge Coder' --yolo -i "))
        (is (str/includes? grok "grok --cwd 'wt' --permission-mode bypassPermissions --minimal --rules "))
        (is (str/includes? grok " --verbatim "))
        (is (str/includes? deepseek "codex -C 'wt' --profile deepseek --no-alt-screen --yolo ")))
      (is (str/includes? (command "codex" {:extra-args "--foo"}) "--yolo --foo "))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-launch-command-prefixes-the-role-environment
  (let [root (tmp-dir)
        ctx (launch-ctx root)
        row (launch-row "codex")
        tool-bin (str (fs/path root ".swarmforge" "bin"))]
    (try
      (let [in-worktree (swarmforge/launch-command ctx 1 row)
            in-forge (swarmforge/launch-command (assoc ctx :definition-project? true) 1 row)
            at-root (swarmforge/launch-command ctx 1 (assoc row :worktree-path root))]
        (is (str/starts-with? in-worktree
                              (str "export SWARMFORGE_ROLE='coder' && export PATH='" tool-bin
                                   "':'" (fs/path "wt" "swarmforge" "scripts") "':$PATH && cd 'wt' && ")))
        (is (str/includes? in-forge (str ":'" (fs/path "forge" "scripts") "':$PATH")))
        (is (str/includes? at-root (str ":'" (fs/path "forge" "scripts") "':$PATH"))))
      (is (str/includes? (slurp (str (fs/path root ".swarmforge" "prompts" "coder.md")))
                         ".swarmforge/project-pack/swarmforge/roles/coder.prompt"))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-launch-command-of-the-first-role-cleans-up-when-it-exits
  (let [root (tmp-dir)
        ctx (launch-ctx root)]
    (try
      (let [first-role (swarmforge/launch-command ctx 0 (launch-row "codex"))]
        (is (str/includes? first-role "; exit_code=$?; SWARMFORGE_TERMINAL_BACKEND='none' nohup '"))
        (is (str/includes? first-role (str (fs/path "forge" "scripts" "swarm-cleanup.sh") "' 'sock' '"
                                           (fs/path root ".swarmforge" "window-ids") "' 'swarmforge-coder'")))
        (is (str/ends-with? first-role " >/dev/null 2>&1 &!; exit $exit_code")))
      (is (not (str/includes? (swarmforge/launch-command ctx 1 (launch-row "codex")) "nohup")))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-launch-command-gives-the-lieutenant-no-initial-prompt
  (let [root (tmp-dir)
        ctx (launch-ctx root)]
    (try
      (fs/create-dirs (:roles-dir ctx))
      (spit (str (fs/path (:roles-dir ctx) "lieutenant.prompt")) "lieutenant\n")
      (let [command (swarmforge/launch-command ctx 1 (launch-row "codex" {:role "lieutenant"}))]
        (is (str/ends-with? command "--yolo "))
        (is (= "lieutenant\n" (slurp (str (fs/path root ".swarmforge" "prompts" "lieutenant.md"))))))
      (finally
        (fs/delete-tree root)))))

(defn- with-user-home [home f]
  (let [previous (System/getProperty "user.home")]
    (System/setProperty "user.home" (str home))
    (try (f)
         (finally (System/setProperty "user.home" previous)))))

(deftest swarmforge-locates-the-claude-config-and-settings-files
  (let [home (tmp-dir)]
    (try
      (with-user-home home
        (fn []
          (is (= (or (not-empty (System/getenv "CLAUDE_CONFIG_FILE"))
                     (str (fs/path home ".claude.json")))
                 (swarmforge/claude-config-file)))
          (is (= (str (fs/path (or (not-empty (System/getenv "CLAUDE_CONFIG_DIR"))
                                   (fs/path home ".claude"))
                               "settings.json"))
                 (swarmforge/claude-settings-file)))))
      (finally
        (fs/delete-tree home)))))

(deftest swarmforge-claude-trust-accepts-the-worktree-and-the-bypass-disclaimer
  (let [root (tmp-dir)
        cfg (fs/path root "claude.json")
        settings (fs/path root "settings.json")
        worktree (str (fs/path root "wt"))]
    (try
      (with-redefs [swarmforge/claude-config-file (constantly (str cfg))
                    swarmforge/claude-settings-file (constantly (str settings))]
        (spit (str settings) "{\"theme\":\"dark\"}")
        (spit (str cfg) "{\"projects\":{\"other\":{\"hasTrustDialogAccepted\":true}}}")
        (swarmforge/ensure-claude-trust! worktree)
        (swarmforge/ensure-claude-trust! worktree)
        (swarmforge/ensure-claude-trust! ""))
      (let [config (json/parse-string (slurp (str cfg)))
            saved (json/parse-string (slurp (str settings)))]
        (is (= true (get-in config ["projects" worktree "hasTrustDialogAccepted"])))
        (is (= true (get-in config ["projects" "other" "hasTrustDialogAccepted"])))
        (is (= {"theme" "dark" "skipDangerousModePermissionPrompt" true} saved)))
      (finally
        (fs/delete-tree root)))))

(deftest swarmforge-launch-role-trusts-the-worktree-for-claude-and-codex-backends
  (let [home (tmp-dir)
        sent (atom [])
        claude-config (fs/path home "claude.json")
        launch (fn [agent]
                 (with-redefs [swarmforge/claude-config-file (constantly (str claude-config))
                               swarmforge/claude-settings-file (constantly (str (fs/path home "settings.json")))
                               swarmforge/launch-command (constantly "the-command")
                               swarmforge/sh (fn [& args] (swap! sent conj (vec args)))]
                   (with-out-str
                     (swarmforge/launch-role! {:tmux-socket "sock" :tmux-pane-base-index 0} 1
                                              {:agent agent :worktree-path (str (fs/path home "wt"))
                                               :session "swarmforge-coder" :display-name "Coder"}))))]
    (try
      (with-user-home home
        (fn []
          (launch "grok")
          (let [codex-config (fs/path (swarmforge/codex-home) "config.toml")]
            (is (not (fs/exists? claude-config)))
            (is (not (fs/exists? codex-config)))
            (launch "deepseek")
            (is (str/includes? (slurp (str codex-config)) (str (fs/path home "wt")))))
          (is (not (fs/exists? claude-config)))
          (launch "claude")
          (is (true? (get-in (json/parse-string (slurp (str claude-config)))
                             ["projects" (str (fs/path home "wt")) "hasTrustDialogAccepted"])))))
      (is (= ["tmux" "-S" "sock" "send-keys" "-t" "swarmforge-coder:Coder.0" "the-command" "Enter"]
             (first @sent)))
      (finally
        (fs/delete-tree home)))))

(deftest swarmforge-entry-points-default-the-project-root-to-the-working-directory
  (is (= "given" (swarmforge/root-arg ["--test-parse" "given"])))
  (is (= (System/getProperty "user.dir") (swarmforge/root-arg ["--test-parse"]))))

(deftest pack-board-helpers
  (is (= "hello" (pack-board/slug "Hello!")))
  (is (= 3 (pack-board/parse-count "3")))
  (is (= 0 (pack-board/parse-count "x")))
  (is (re-find #"\tlane2\t"
               (pack-board/rewrite-lane nil
                                        "n\tlane1\tc\tu\tid\t0\tcomponent"
                                        "n"
                                        "lane2"))))

(deftest stop-daemon-with-no-pid
  (let [root (tmp-dir)]
    (try
      (stop-handoff-daemon/stop! (str root) :timeout-ms 10)
      (is (not (fs/exists? (fs/path root ".swarmforge/daemon/handoffd.pid"))))
      (finally
        (fs/delete-tree root)))))

(deftest handoffd-skips-board-update-without-board
  (is (nil? (handoffd/update-board! {} {"type" "note"})))
  (is (false? (handoffd/non-forwarding? {})))
  (is (nil? (handoffd/recipient-list {}))))

(deftest dashboard-request-helpers
  (is (string? pack-dashboard-request/usage-text)))

(deftest swarm-tool-usage
  (is (fn? swarm-tool/-main)))
