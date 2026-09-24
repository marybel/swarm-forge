;; Role launch commands and handoff daemon. Loaded into swarmforge.

(defn require-ensure-lines [tools]
  (apply str
         (for [tool tools]
           (str "- `" tool "` (" (get aps-tool-purpose tool) "): `swarm_tool.sh require " tool "`\n"
                "  If missing, run exactly: `swarm_tool.sh ensure " tool "`\n"))))

(defn parse-dry-check-lines [tools]
  (str (when (some #{"gherkin-parser"} tools)
         "- Parse with the two-arg form: `gherkin-parser <feature> ./tmp/<stem>.json`\n")
       (when (some #{"ir-dry-checker"} tools)
         "- Dry-check with the two-arg form: `ir-dry-checker <ir> ./tmp/<stem>.dry.json`\n")))

(defn tool-startup-section [role last-role?]
  (let [tools (get role-required-tools role [])]
    (str "## Tool Startup

"
         "- Do not search `$HOME` or run `find` for APS tools.\n"
         (require-ensure-lines tools)
         (parse-dry-check-lines tools)
         "- Write scratch files and handoff drafts in `./tmp/` in the assigned worktree.\n"
         "- Do not use `/tmp` or `.swarmforge/handoffs/outbox/tmp/` as scratch.\n"
         "- Receive with `ready_for_next.sh`. Send with `swarm_handoff.sh ./tmp/<draft>`.\n"
         "- If the pane says the lieutenant stopped this card, stop executing it immediately. That overrides a busy window.\n"
         "- Do not search the tree or `$HOME` for those scripts.\n"
         "- Do not invoke helpers as `./swarmforge/scripts/...`. They are already on PATH.\n"
         "- Board cards live in `.swarmforge/board/tasks.tsv`. Use that card name as `task:`.\n"
         "- Operator task documents live in `tasks/<task-name>.md`. Re-read that file as operator intent. The receive helper commits it when the task starts.\n"
         "- A retry audit may include remedial comments on named documents. Read those comments as findings.\n"
         "- Do not search the worktree for `.swarmforge/board/tasks.tsv`. That file is on the project (master).\n"
         "- Use TASK_NAME from `ready_for_next.sh` or the inbound `task:` header. For a batch, that name is the top item. The helper fills `task:` from the in-process batch, else the sender-lane card.\n"
         "- Do not invent a name or hunt `sessions.tsv`.\n"
         "- Constitution tools: `swarm_tool.sh require crap4clj` (also dry4clj, clj-mutate, cloverage, speclj, speclj-structure-check, APS, or the language table). If missing, `swarm_tool.sh ensure <tool>`. Do not invent project `bb` proxies.\n"
         "- Run constitution tools one at a time. Worker-limited tools use `--max-workers 4` or `--workers 4`. Mutation is differential: no `--mutate-all`, no `--level full`.\n"
         "- Do not clone those repos into `./tmp`.\n"
         "- If merge_and_process.sh or ready_for_next reports a merge conflict, resolve the conflicted files, git add, and commit. Do not invent git merge. Parallel cards on one tree will conflict; that is expected.\n"
         "- Operator follow-ups arrive as `[id] text` in this pane. Answer with `pack_dashboard_request.sh answer <id> ./tmp/answer.txt`.\n"
         "- Ask the operator with `pack_dashboard_request.sh clarify ./tmp/question.txt`. Do not ask in the pane.\n"
         "- Do not ask for approval in the pane. Queue `git_handoff`; the operator uses Attention.\n"
         (when last-role?
           (str "- You are the last role in this pack. After this pack step, queue a git_handoff. The helper marks the card Done. Do not list every other role on to: to finish the card.\n"))
         (when (= role "specifier")
           (str "- Specify from the board card and the current product tree. Do not import behavior from sibling projects.\n"
                "- Do not ask the operator what new feature to specify or what the card already states.\n"
                "- Finish the assigned TASK_NAME and payload (the whole card), then one git_handoff. Do not hand off after the first feature in a folder.\n"))
         (when (= role "QA")
           (str "- One commit is one git_handoff. Do not send two git_handoffs of the same SHA.\n")))))

(defn last-pack-role? [ctx role]
  (and (not= role "lieutenant")
       (= role (:role (last (:roles ctx))))))

(defn write-agent-instruction-file! [ctx role prompt-file last-role?]
  (if (= role "lieutenant")
    (fs/copy (fs/path (:roles-dir ctx) "lieutenant.prompt")
             prompt-file
             {:replace-existing true})
    (spit (str prompt-file)
          (str "Read swarmforge/constitution.prompt, then read every file it refers to recursively, and obey all of those instructions.\n"
               "Read .swarmforge/project-pack/swarmforge/roles/" role ".prompt, then read every file it refers to recursively, and follow all of those instructions.\n"
               "\n"
               (tool-startup-section role last-role?)))))

(defn extra-args-prefix [row]
  (let [args (:extra-args row)]
    (if (str/blank? args) "" (str args " "))))

(defn extra-has? [row needle]
  (str/includes? (or (:extra-args row) "") needle))

(defn yolo-flag [agent row]
  (case agent
    "codex" (if (extra-has? row "--yolo") "" "--yolo ")
    "copilot" (if (extra-has? row "--yolo") "" "--yolo ")
    "claude" (if (extra-has? row "bypassPermissions") "" "--permission-mode bypassPermissions ")
    ""))

(defn grok-permission-prefix [row]
  "--permission-mode bypassPermissions ")

(defn alt-screen-env [agent row]
  (if (and (= agent "claude")
           (not (extra-has? row "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN")))
    "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 "
    ""))

(defn no-alt-screen-flag [agent row]
  (if (and (#{"codex" "copilot"} agent)
           (not (extra-has? row "--no-alt-screen")))
    "--no-alt-screen "
    ""))

(defn codex-cli-command [role-worktree row initial-prompt? prompt profile-prefix]
  (str "codex -C " (sq (str role-worktree)) " "
       profile-prefix
       (no-alt-screen-flag "codex" row) (yolo-flag "codex" row)
       (extra-args-prefix row)
       (when initial-prompt? prompt)))

(defn claude-cli-command [row prompt-file prompt initial-prompt?]
  (str (alt-screen-env "claude" row)
       "claude --append-system-prompt-file " (sq (str prompt-file)) " "
       (yolo-flag "claude" row) "-n " (sq (str "SwarmForge " (:display-name row))) " "
       (extra-args-prefix row)
       (when initial-prompt? prompt)))

(defn copilot-cli-command [row prompt initial-prompt?]
  (str "copilot -C " (sq (str (:worktree-path row))) " "
       (no-alt-screen-flag "copilot" row)
       "--name " (sq (str "SwarmForge " (:display-name row))) " "
       (yolo-flag "copilot" row) (extra-args-prefix row)
       (when initial-prompt? (str "-i " prompt))))

(defn grok-cli-command [row prompt initial-prompt?]
  (str "grok --cwd " (sq (str (:worktree-path row))) " "
       (grok-permission-prefix row) (extra-args-prefix row)
       "--minimal --rules " prompt
       (when initial-prompt? (str " --verbatim " prompt))))

(defn agent-cli-command [row prompt-file prompt]
  (let [initial-prompt? (not= (:role row) "lieutenant")
        role-worktree (:worktree-path row)]
    (case (:agent row)
      "claude" (claude-cli-command row prompt-file prompt initial-prompt?)
      "codex" (codex-cli-command role-worktree row initial-prompt? prompt "")
      "copilot" (copilot-cli-command row prompt initial-prompt?)
      "grok" (grok-cli-command row prompt initial-prompt?)
      "deepseek" (codex-cli-command role-worktree row initial-prompt? prompt
                                    "--profile deepseek "))))

(defn role-script-dir [ctx row]
  (if (or (:definition-project? ctx)
          (= (str (:worktree-path row)) (str (:working-dir ctx))))
    (:script-dir ctx)
    (fs/path (:worktree-path row) "swarmforge" "scripts")))

(defn launch-preamble [ctx row]
  (str "export SWARMFORGE_ROLE=" (sq (:role row))
       " && export PATH=" (sq (str (fs/path (:working-dir ctx) ".swarmforge" "bin")))
       ":" (sq (str (role-script-dir ctx row))) ":$PATH"
       " && cd " (sq (str (:worktree-path row)))
       " && "))

(defn cleanup-on-exit-suffix [ctx]
  (str "; exit_code=$?; SWARMFORGE_TERMINAL_BACKEND=" (sq (:terminal-backend ctx))
       " nohup " (sq (str (fs/path (:script-dir ctx) "swarm-cleanup.sh")))
       " " (sq (:tmux-socket ctx))
       " " (sq (str (:window-ids-file ctx)))
       (apply str (map #(str " " (sq (:session %))) (:roles ctx)))
       " >/dev/null 2>&1 &!; exit $exit_code"))

(defn launch-command [ctx index row]
  (let [role (:role row)
        prompt-file (fs/path (:prompts-dir ctx) (str role ".md"))
        prompt (str "\"$(cat " (sq (str prompt-file)) ")\"")]
    (write-agent-instruction-file! ctx role prompt-file (last-pack-role? ctx role))
    (str (launch-preamble ctx row)
         (agent-cli-command row prompt-file prompt)
         (when (= index 0) (cleanup-on-exit-suffix ctx)))))

(defn codex-home []
  (or (not-empty (System/getenv "CODEX_HOME"))
      (str (fs/path (System/getProperty "user.home") ".codex"))))

(defn project-table-header [dir]
  (str "[projects." (pr-str (str (fs/absolutize dir))) "]"))

(defn ensure-newline [text]
  (cond
    (str/blank? text) ""
    (str/ends-with? text "\n") text
    :else (str text "\n")))

(defn ensure-codex-trust! [dir]
  (when-not (str/blank? (str dir))
    (let [home (codex-home)
          cfg (fs/path home "config.toml")
          header (project-table-header dir)
          text (if (fs/exists? cfg) (slurp (str cfg)) "")]
      (when-not (str/includes? text header)
        (fs/create-dirs home)
        (spit (str cfg)
              (str (ensure-newline text)
                   "\n" header "\ntrust_level = \"trusted\"\n"))))))

(defn claude-config-file []
  (or (not-empty (System/getenv "CLAUDE_CONFIG_FILE"))
      (str (fs/path (System/getProperty "user.home") ".claude.json"))))

(defn claude-settings-file []
  (str (fs/path (or (not-empty (System/getenv "CLAUDE_CONFIG_DIR"))
                     (str (fs/path (System/getProperty "user.home") ".claude")))
                "settings.json")))

(defn read-json-file [file]
  (if (fs/exists? file)
    (json/parse-string (slurp (str file)))
    {}))

(defn write-json-file! [file data]
  (fs/create-dirs (fs/parent file))
  (spit (str file) (json/generate-string data)))

(defn ensure-claude-worktree-trusted! [dir]
  (let [cfg (claude-config-file)
        config (read-json-file cfg)
        trust-path ["projects" (str (fs/absolutize dir)) "hasTrustDialogAccepted"]]
    (when-not (get-in config trust-path)
      (write-json-file! cfg (assoc-in config trust-path true)))))

;; The bypass-permissions disclaimer's "Yes, I accept" writes
;; skipDangerousModePermissionPrompt into ~/.claude/settings.json (the
;; userSettings scope) -- not .claude.json, whose bypassPermissionsModeAccepted
;; key is only consulted by a one-time migration this install never triggered.
(defn ensure-claude-bypass-permissions-accepted! []
  (let [settings-file (claude-settings-file)
        settings (read-json-file settings-file)]
    (when-not (get settings "skipDangerousModePermissionPrompt")
      (write-json-file! settings-file
                        (assoc settings "skipDangerousModePermissionPrompt" true)))))

(defn ensure-claude-trust! [dir]
  (when-not (str/blank? (str dir))
    (ensure-claude-worktree-trusted! dir)
    (ensure-claude-bypass-permissions-accepted!)))

(defn launch-role! [ctx index row]
  (when (codex-backed? (:agent row))
    (ensure-codex-trust! (:worktree-path row)))
  (when (= "claude" (:agent row))
    (ensure-claude-trust! (:worktree-path row)))
  (let [session (:session row)
        display (:display-name row)
        command (launch-command ctx index row)]
    (sh "tmux" "-S" (:tmux-socket ctx) "send-keys" "-t"
        (tmux-agent-target display (:tmux-pane-base-index ctx) session)
        command "Enter")
    (println (str "  " cyan "[" display "]" reset " started in session " session))))

(defn stop-handoff-daemon! [ctx]
  (process/sh {:continue true}
              "bb" (str (fs/path (:script-dir ctx) "stop_handoff_daemon.bb"))
              (str (:working-dir ctx))))

(defn uname []
  (str/trim (:out (process/sh {:continue true} "uname" "-s"))))

(defn linux-systemd-running? []
  (let [result (process/sh {:continue true} "systemctl" "is-system-running")
        state (str/trim (:out result))]
    (#{"running" "degraded"} state)))

(defn sleep-inhibitor-prefix []
  (when-not (= "0" (System/getenv "SWARMFORGE_PREVENT_SLEEP"))
    (case (uname)
      "Darwin" (when (command-exists? "caffeinate")
                 ["caffeinate" "-dims"])
      "Linux" (when (and (command-exists? "systemd-inhibit")
                         (command-exists? "systemctl")
                         (linux-systemd-running?))
                ["systemd-inhibit"
                 "--what=sleep:idle"
                 "--who=SwarmForge"
                 "--why=SwarmForge swarm is active"])
      nil)))

(defn start-handoff-daemon! [ctx]
  (fs/delete-if-exists (fs/path (:daemon-dir ctx) "stop"))
  (let [command (into (vec (sleep-inhibitor-prefix))
                      [(str (fs/path (:script-dir ctx) "handoffd.bb"))
                       (str (:working-dir ctx))])]
    (process/process command
                     {:out (str (:handoff-daemon-log ctx))
                      :err :out})
    (println (str green "Started handoff daemon"
                  (when (> (count command) 2) " with OS sleep prevention")
                  "."
                  reset))))
