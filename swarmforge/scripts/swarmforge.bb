#!/usr/bin/env bb

(ns swarmforge
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def session-prefix "swarmforge")
(def agent-window "swarm")
(def pane-history-limit 10000)
(def red "\u001b[0;31m")
(def green "\u001b[0;32m")
(def yellow "\u001b[1;33m")
(def cyan "\u001b[0;36m")
(def bold "\u001b[1m")
(def reset "\u001b[0m")

(defn sh [& args]
  (apply process/sh args))

(defn sh-ok? [& args]
  (zero? (:exit (apply process/sh (concat [{:continue true}] args)))))

(defn sh-out [& args]
  (str/trim (:out (apply process/sh args))))

(defn command-exists? [command]
  (sh-ok? "sh" "-c" (str "command -v " command " >/dev/null 2>&1")))

(defn env-long [name default-value]
  (if-let [value (System/getenv name)]
    (if (re-matches #"[0-9]+" value)
      (Long/parseLong value)
      default-value)
    default-value))

(defn fail! [message]
  (binding [*out* *err*]
    (println message))
  (System/exit 1))

(defn sq [value]
  (str "'" (str/replace (str value) #"'" "'\"'\"'") "'"))

(defn normalize-terminal-backend [backend]
  (case (str/lower-case backend)
    ("iterm" "iterm2" "iterm.app") "iterm2"
    ("terminal" "terminal-app" "terminal.app") "terminal-app"
    ("windows" "windows-terminal" "wt") "windows-terminal"
    ("none" "current" "fallback") "none"
    (str/lower-case backend)))

(defn detect-terminal-backend []
  (if-let [backend (System/getenv "SWARMFORGE_TERMINAL")]
    (normalize-terminal-backend backend)
    (cond
      (command-exists? "osascript") (if (= (System/getenv "TERM_PROGRAM") "iTerm.app")
                                      "iterm2"
                                      "terminal-app")
      (command-exists? "wt.exe") "windows-terminal"
      :else "none")))

(defn display-name-for-role [role]
  (->> (str/split (str/replace role #"[-_]" " ") #"\s+")
       (remove str/blank?)
       (map str/capitalize)
       (str/join " ")))

(defn session-name-for-role [role]
  (str session-prefix "-" role))

(defn worktree-path-for-name [worktrees-dir worktree]
  (fs/path worktrees-dir worktree))

(defn tmux-agent-target [window pane-base-index session]
  (str session ":" window "." pane-base-index))

(defn tmux-option [tmux-socket option scope default-value]
  (let [args (case scope
               :session ["tmux" "-S" tmux-socket "show-options" "-gqv" option]
               :window ["tmux" "-S" tmux-socket "show-options" "-gwqv" option])
        result (apply process/sh (concat [{:continue true}] args))
        value (str/trim (:out result))]
    (if (re-matches #"[0-9]+" value)
      (Long/parseLong value)
      default-value)))

(defn detect-tmux-base-indexes [ctx]
  (fs/create-dirs (:tmux-socket-dir ctx))
  (let [probe-session (when-not (sh-ok? "tmux" "-S" (:tmux-socket ctx) "info")
                        (let [session (str "swarmforge-probe-" (.pid (java.lang.ProcessHandle/current)))]
                          (sh "tmux" "-S" (:tmux-socket ctx) "new-session" "-d" "-s" session "sleep 60")
                          session))
        window-base (tmux-option (:tmux-socket ctx) "base-index" :session 0)
        pane-base (tmux-option (:tmux-socket ctx) "pane-base-index" :window 0)]
    (when probe-session
      (process/sh {:continue true} "tmux" "-S" (:tmux-socket ctx) "kill-session" "-t" probe-session))
    (assoc ctx :tmux-window-base-index window-base :tmux-pane-base-index pane-base)))

(def ignore-begin "# BEGIN SWARMFORGE RUNTIME")
(def ignore-end "# END SWARMFORGE RUNTIME")
(def old-runtime-ignore-lines #{".swarmforge/" ".worktrees/"
                                "/.swarmforge/" "/.worktrees/"})

(defn without-managed-ignore [text]
  (loop [lines (str/split-lines (or text "")) inside? false out []]
    (if-let [line (first lines)]
      (cond
        (= line ignore-begin) (recur (next lines) true out)
        (= line ignore-end) (recur (next lines) false out)
        inside? (recur (next lines) true out)
        (old-runtime-ignore-lines line) (recur (next lines) false out)
        :else (recur (next lines) false (conj out line)))
      (->> out
           (drop-while str/blank?)
           reverse
           (drop-while str/blank?)
           reverse
           vec))))

(defn write-runtime-ignore-block! [file]
  (fs/create-dirs (fs/parent file))
  (let [prior (if (fs/regular-file? file) (slurp (str file)) "")
        kept (without-managed-ignore prior)
        lines (concat kept
                      (when (seq kept) [""])
                      [ignore-begin "/.swarmforge/" "/.worktrees/" ignore-end])]
    (spit (str file) (str (str/join "\n" lines) "\n"))))

(defn ensure-initial-gitignore! [ctx]
  (write-runtime-ignore-block! (fs/path (:working-dir ctx) ".gitignore")))

(defn ensure-runtime-git-excludes! [ctx]
  (let [exclude-file (fs/path (sh-out "git" "-C" (str (:working-dir ctx)) "rev-parse" "--git-path" "info/exclude"))]
    (write-runtime-ignore-block! exclude-file)))

(defn initialize-git-repo! [ctx]
  (when-not (fs/exists? (fs/path (:working-dir ctx) ".git"))
    (sh "git" "init" (str (:working-dir ctx)))
    (sh "git" "-C" (str (:working-dir ctx)) "branch" "-M" "master")
    (ensure-initial-gitignore! ctx)
    (sh "git" "-C" (str (:working-dir ctx)) "add" ".")
    (sh "git" "-C" (str (:working-dir ctx)) "commit" "-m" "Initial swarmforge repository")))


(def swarmforge-script-dir (fs/parent *file*))
(load-file (str (fs/path swarmforge-script-dir "swarmforge_config.bb")))

(def agent-required-command
  {"deepseek" "codex"})

(defn required-command [agent]
  (get agent-required-command agent agent))

(defn codex-backed? [agent]
  (= "codex" (required-command agent)))

(defn check-dependency! [command]
  (when-not (command-exists? command)
    (fail! (str red "Error:" reset " '" command "' is required but not installed."))))

(defn check-backend-dependencies! [ctx]
  (doseq [agent (map :agent (:roles ctx))]
    (check-dependency! (required-command agent))))

(defn create-role-session! [ctx session title pane-log-file]
  (sh "tmux" "-S" (:tmux-socket ctx) "new-session" "-d" "-s" session "-n" agent-window)
  (sh "tmux" "-S" (:tmux-socket ctx) "set-option" "-t" session "history-limit" (str pane-history-limit))
  (sh "tmux" "-S" (:tmux-socket ctx) "rename-window" "-t" (str session ":" agent-window) title)
  (sh "tmux" "-S" (:tmux-socket ctx) "set-window-option" "-t" (str session ":" title) "allow-rename" "off")
  ;; Piped before anything is ever typed into the pane, so a crash in the
  ;; launch command (or in the agent itself) is still captured even if the
  ;; session/tmux-server gets torn down right after.
  (fs/create-dirs (fs/parent pane-log-file))
  (sh "tmux" "-S" (:tmux-socket ctx) "pipe-pane" "-o" "-t" session
      (str "cat >> " (sq (str pane-log-file)))))

(def aps-tool-purpose
  {"gherkin-parser" "APS parsing"
   "ir-dry-checker" "IR DRY"
   "gherkin-mutator" "Gherkin mutation"})

(def role-required-tools
  {"specifier" ["gherkin-parser" "ir-dry-checker"]
   "coder" ["gherkin-parser"]
   "refactorer" ["gherkin-parser"]
   "hardender" ["gherkin-parser" "gherkin-mutator"]
   "architect" ["gherkin-parser" "gherkin-mutator"]
   "QA" ["gherkin-parser"]})


(load-file (str (fs/path swarmforge-script-dir "safe_paths.bb")))


(load-file (str (fs/path swarmforge-script-dir "swarmforge_launch.bb")))


(load-file (str (fs/path swarmforge-script-dir "swarmforge_terminal.bb")))


(defn context [working-dir]
  (let [working-dir (fs/absolutize (fs/path working-dir))
        script-dir (fs/parent *file*)
        swarm-forge-dir (fs/path (safe-paths/definition-root working-dir) "swarmforge")
        state-dir (fs/path working-dir ".swarmforge")
        daemon-dir (fs/path state-dir "daemon")
        crc (java.util.zip.CRC32.)
        _ (.update crc (.getBytes (str working-dir) java.nio.charset.StandardCharsets/UTF_8))
        socket-id (str (.getValue crc))
        tmux-socket-dir (fs/path "/tmp" (str "swarmforge-" (or (System/getenv "UID") (System/getProperty "user.name"))))
        tmux-socket (str (fs/path tmux-socket-dir (str socket-id ".sock")))]
    {:working-dir working-dir
     :script-dir script-dir
     :swarm-forge-dir swarm-forge-dir
     :worktrees-dir (fs/path working-dir ".worktrees")
     :config-file (fs/path swarm-forge-dir "swarmforge.conf")
     :roles-dir (fs/path swarm-forge-dir "roles")
     :constitution-file (fs/path swarm-forge-dir "constitution.prompt")
     :state-dir state-dir
     :logs-dir (fs/path state-dir "logs")
     :notify-dir (fs/path state-dir "notify")
     :window-ids-file (fs/path state-dir "window-ids")
     :window-state-file (fs/path state-dir "windows.tsv")
     :window-watchdog-log (fs/path state-dir "window-watchdog.log")
     :sessions-file (fs/path state-dir "sessions.tsv")
     :roles-file (fs/path state-dir "roles.tsv")
     :routes-file (fs/path state-dir "routes.tsv")
     :prompts-dir (fs/path state-dir "prompts")
     :daemon-dir daemon-dir
     :handoff-daemon-log (fs/path daemon-dir "handoffd.log")
     :tmux-socket-dir tmux-socket-dir
     :tmux-socket tmux-socket
     :tmux-socket-file (fs/path state-dir "tmux-socket")
     :tmux-env-file (fs/path state-dir "tmux-env")
     :tmux-window-base-index 0
     :tmux-pane-base-index 0}))

(defn prepare-ctx [ctx]
  (-> ctx
      parse-config
      (assoc :terminal-backend (detect-terminal-backend))))

(defn visibility-label [row]
  (if (:visible? row) "visible" "invisible"))

(defn test-parse! [root]
  (let [ctx (prepare-ctx (context root))]
    (prepare-workspace! ctx)
    (doseq [row (:roles ctx)]
      (println (str (:role row) " " (:display-name row) " " (:worktree-path row) " "
                    (:receive-mode row) " " (:propagation row)
                    (when-let [extra (:extra-args row)] (str " " extra))
                    " " (visibility-label row))))
    (print (slurp (str (:roles-file ctx))))
    (print (slurp (str (:sessions-file ctx))))))

(defn test-required-helpers! []
  (doseq [helper required-helpers]
    (println helper)))

(defn test-launch-plan! [root]
  (doseq [line (launch-plan-lines (prepare-ctx (context root)))]
    (println line)))

(defn test-start-order! [_root]
  (println "pack_web start")
  (println "start-agents")
  (println "open-terminals"))

(defn kill-existing-sessions! [ctx]
  (doseq [row (:roles ctx)]
    (when (sh-ok? "tmux" "-S" (:tmux-socket ctx) "has-session" "-t" (:session row))
      (println (str yellow "Existing SwarmForge session found: " (:session row) ". Killing it..." reset))
      (sh "tmux" "-S" (:tmux-socket ctx) "kill-session" "-t" (:session row)))))

(defn announce-ready! [ctx]
  (println)
  (println (str green bold "SwarmForge is ready." reset))
  (println "Working directory:" (str (:working-dir ctx)))
  (println "Sessions:")
  (doseq [row (:roles ctx)]
    (println (str "  " (:display-name row) ": " (:session row))))
  (println)
  (println (str green "Tip: Write a handoff draft and run swarm_handoff.sh while the swarm is running." reset))
  (println (str green "Tip: Reattach manually with 'tmux -S " (:tmux-socket ctx) " attach-session -t <session-name>' if needed." reset))
  (println))

(defn launch-roles! [ctx]
  (println (str green "Starting agents..." reset))
  (let [delay-ms (env-long "SWARMFORGE_AGENT_START_DELAY_MS" 1500)]
    (doseq [[index row] (map-indexed vector (:roles ctx))]
      (when (pos? index)
        (Thread/sleep delay-ms))
      (launch-role! ctx index row))))

(defn pane-log-file-for [ctx role]
  (fs/path (:logs-dir ctx) (str role ".log")))

(defn boot-sessions! [ctx]
  (println (str cyan bold))
  (println "  SwarmForge v1.0 Starting")
  (println "  Disciplined agents build better software")
  (println reset)
  (println (str green "Launching SwarmForge tmux sessions..." reset))
  (doseq [row (:roles ctx)]
    (create-role-session! ctx (:session row) (:display-name row)
                          (pane-log-file-for ctx (:role row))))
  (write-tmux-env-file! ctx))

(defn run-main! [root]
  (check-dependency! "tmux")
  (check-dependency! "git")
  (check-dependency! "bb")
  (let [ctx (-> (context root)
                detect-tmux-base-indexes)]
    (initialize-git-repo! ctx)
    (ensure-runtime-git-excludes! ctx)
    (install-commit-msg-hook! ctx)
    (let [ctx (prepare-ctx ctx)]
      (check-backend-dependencies! ctx)
      (prepare-workspace! ctx)
      (prepare-worktrees! ctx)
      (prepare-handoff-dirs! ctx)
      (let [ctx (assoc ctx :terminal-backend (detect-terminal-backend))]
        (stop-handoff-daemon! ctx)
        (kill-existing-sessions! ctx)
        (boot-sessions! ctx)
        (sync-worktree-scripts! ctx)
        (start-handoff-daemon! ctx)
        (start-pack-web! ctx)
        (launch-roles! ctx)
        (announce-ready! ctx)
        (open-terminal-surfaces! ctx)))))

(defn parse-lieutenant-config [ctx]
  (let [file (:config-file ctx)
        fallback (str/lower-case (or (not-empty (System/getenv "SWARMFORGE_LIEUTENANT_AGENT")) "grok"))]
    (if-not (fs/regular-file? file)
      {:agent fallback :extra-args nil}
      (or (some (fn [raw]
                  (let [line (str/trim raw)]
                    (when-not (skip-config-line? line)
                      (let [fields (str/split line #"\s+")]
                        (when (and (>= (count fields) 2)
                                   (= (str/lower-case (first fields)) "lieutenant"))
                          (let [agent (str/lower-case (second fields))]
                            (reject-if (not (known-agents agent))
                                       (str "Unsupported agent '" (second fields)
                                            "' for lieutenant"))
                            {:agent agent
                             :extra-args (extra-args-str (drop 2 fields))}))))))
                (str/split-lines (slurp (str file))))
          {:agent fallback :extra-args nil}))))

(defn lieutenant-row [ctx]
  (let [{:keys [agent extra-args]} (parse-lieutenant-config ctx)]
    (window-row ctx "lieutenant" agent "master" "task" "forward-only" extra-args false)))

(defn forge-root? [root]
  (or (fs/directory? (fs/path root "packs"))
      (fs/directory? (fs/path root "projects"))))

(defn session-names-from-file [ctx]
  (let [file (:sessions-file ctx)]
    (if (fs/regular-file? file)
      (->> (str/split-lines (slurp (str file)))
           (remove str/blank?)
           (map #(nth (str/split % #"\t") 2))
           vec)
      [])))

(defn run-stop-project! [root]
  (let [ctx (context root)
        socket (when (fs/regular-file? (:tmux-socket-file ctx))
                 (not-empty (str/trim (slurp (str (:tmux-socket-file ctx))))))
        sessions (session-names-from-file ctx)
        script (str (fs/path (:script-dir ctx) "swarm-cleanup.sh"))]
    (stop-handoff-daemon! ctx)
    (when socket
      (apply process/sh {:continue true}
             (into [script socket (str (:window-ids-file ctx))] sessions)))))

(defn run-host! [root]
  (check-dependency! "tmux")
  (check-dependency! "git")
  (check-dependency! "bb")
  (let [ctx (-> (context root)
                detect-tmux-base-indexes)
        row (lieutenant-row ctx)
        ctx (assoc ctx :roles [row] :host? true :terminal-backend (detect-terminal-backend))]
    (when-not (fs/exists? (fs/path (:roles-dir ctx) "lieutenant.prompt"))
      (fail! (str red "Error:" reset " Missing lieutenant prompt at "
                  (fs/path (:roles-dir ctx) "lieutenant.prompt"))))
    (check-backend-dependencies! ctx)
    (fs/create-dirs (fs/path (:working-dir ctx) "projects"))
    (prepare-workspace! ctx)
    (fs/create-dirs (:state-dir ctx))
    (kill-existing-sessions! ctx)
    (boot-sessions! ctx)
    (start-pack-web! ctx)
    (launch-roles! ctx)
    (announce-ready! ctx)
    (open-terminal-surfaces! ctx)))

(defn run-project! [root]
  (check-dependency! "tmux")
  (check-dependency! "git")
  (check-dependency! "bb")
  (let [ctx (-> (context root)
                detect-tmux-base-indexes)]
    (initialize-git-repo! ctx)
    (ensure-runtime-git-excludes! ctx)
    (install-commit-msg-hook! ctx)
    (let [ctx (prepare-ctx ctx)]
      (check-backend-dependencies! ctx)
      (prepare-workspace! ctx)
      (prepare-worktrees! ctx)
      (prepare-handoff-dirs! ctx)
      (let [ctx (assoc ctx :terminal-backend (detect-terminal-backend))]
        (stop-handoff-daemon! ctx)
        (kill-existing-sessions! ctx)
        (boot-sessions! ctx)
        (sync-worktree-scripts! ctx)
        (start-handoff-daemon! ctx)
        (launch-roles! ctx)
        (announce-ready! ctx)))))

(defn test-terminal-bridge! [root backend]
  (let [local-script-dir (fs/path root "swarmforge" "scripts")
        ctx (cond-> (assoc (context root) :terminal-backend backend)
              (fs/exists? local-script-dir) (assoc :script-dir local-script-dir))]
    (println (terminal-call-out ctx "terminal_open_session" "swarmforge-specifier" "SwarmForge Specifier" ""))))

(defn test-tmux-base-indexes! [tmux-socket]
  (let [ctx (detect-tmux-base-indexes {:tmux-socket tmux-socket
                                        :tmux-socket-dir (str (fs/parent (fs/path tmux-socket)))})]
    (println (:tmux-window-base-index ctx) (:tmux-pane-base-index ctx))))

(defn test-create-role-session! [tmux-socket session pane-log-file]
  (create-role-session! {:tmux-socket tmux-socket} session "Specifier" pane-log-file)
  (println (sh-out "tmux" "-S" tmux-socket "show-options" "-t" session "-qv" "history-limit")))

(defn test-launch-command! [root agent & [extra-args]]
  (let [ctx (assoc (context root) :terminal-backend "none")
        row {:role "coder"
             :agent agent
             :session "swarmforge-coder"
             :display-name "Coder"
             :worktree-name "master"
             :worktree-path (fs/path root)
             :receive-mode "task"
             :extra-args extra-args}]
    (fs/create-dirs (:prompts-dir ctx))
    (println (launch-command ctx 1 row))))

(defn test-role-launch-command! [root role]
  (let [ctx (assoc (prepare-ctx (context root)) :terminal-backend "none")
        row (first (filter #(= role (:role %)) (:roles ctx)))]
    (fs/create-dirs (:prompts-dir ctx))
    (println (launch-command ctx 1 row))))

(defn test-lieutenant-launch-command! [root]
  (let [ctx (assoc (context root) :terminal-backend "none")
        row (assoc (lieutenant-row ctx) :worktree-path (fs/path root))]
    (fs/create-dirs (:prompts-dir ctx))
    (fs/create-dirs (:roles-dir ctx))
    (when-not (fs/exists? (fs/path (:roles-dir ctx) "lieutenant.prompt"))
      (spit (str (fs/path (:roles-dir ctx) "lieutenant.prompt")) "lieutenant\n"))
    (println (launch-command ctx 1 row))))

(defn test-install-hooks! [root]
  (let [ctx (context root)]
    (install-commit-msg-hook! ctx)
    (println (str (fs/path (git-hooks-dir ctx) "commit-msg")))))

(defn test-sync-worktrees! [root]
  (let [ctx (prepare-ctx (context root))]
    (prepare-workspace! ctx)
    (when-not (fs/regular-file? (:tmux-env-file ctx))
      (spit (str (:tmux-env-file ctx)) "test\n"))
    (sync-worktree-scripts! ctx)))

(defn remove-hooks! [root]
  (remove-commit-msg-hook! (context root)))

(defn test-sleep-inhibitor-prefix! []
  (println (str/join " " (or (sleep-inhibitor-prefix) []))))

(defn test-ensure-codex-trust! [dir]
  (ensure-codex-trust! dir))

(defn test-ensure-claude-trust! [dir]
  (ensure-claude-trust! dir))

(defn test-reset-pack-web-state! [root]
  (let [ctx (context root)]
    (fs/create-dirs (:state-dir ctx))
    (stop-existing-pack-web! ctx)
    (println (str (boolean (fs/exists? (dashboard-url-file ctx))) " "
                  (boolean (fs/exists? (pack-web-pid-file ctx)))))))

(defn -main [& args]
  (case (first args)
    "--test-parse" (test-parse! (or (second args) (System/getProperty "user.dir")))
    "--test-required-helpers" (test-required-helpers!)
    "--test-launch-plan" (test-launch-plan! (or (second args) (System/getProperty "user.dir")))
    "--test-start-order" (test-start-order! (or (second args) (System/getProperty "user.dir")))
    "--test-terminal-bridge" (test-terminal-bridge! (or (second args) (System/getProperty "user.dir")) (nth args 2))
    "--test-launch-command" (apply test-launch-command!
                                     (or (second args) (System/getProperty "user.dir"))
                                     (drop 2 args))
    "--test-role-launch-command" (test-role-launch-command!
                                 (or (second args) (System/getProperty "user.dir"))
                                 (nth args 2))
    "--test-lieutenant-launch-command" (test-lieutenant-launch-command!
                                        (or (second args) (System/getProperty "user.dir")))
    "--test-install-hooks" (test-install-hooks! (second args))
    "--test-sync-worktrees" (test-sync-worktrees! (second args))
    "--remove-hooks" (remove-hooks! (second args))
    "--test-agent-start-delay" (println (env-long "SWARMFORGE_AGENT_START_DELAY_MS" 1500))
    "--test-sleep-inhibitor-prefix" (test-sleep-inhibitor-prefix!)
    "--test-ensure-codex-trust" (test-ensure-codex-trust! (second args))
    "--test-ensure-claude-trust" (test-ensure-claude-trust! (second args))
    "--test-reset-pack-web-state" (test-reset-pack-web-state! (second args))
    "--test-tmux-base-indexes" (test-tmux-base-indexes! (second args))
    "--test-create-role-session" (test-create-role-session! (second args) (nth args 2) (nth args 3))
    "--test-required-command" (println (required-command (second args)))
    "--test-codex-backed" (println (codex-backed? (second args)))
    "--start-project" (run-project! (second args))
    "--stop-project" (run-stop-project! (second args))
    "--test-forge-root" (println (boolean (forge-root? (second args))))
    (let [root (or (first args) (System/getProperty "user.dir"))]
      (if (forge-root? root)
        (run-host! root)
        (run-main! root)))))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
