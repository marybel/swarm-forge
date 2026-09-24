(ns swarmforge.swarmforge-script-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [swarmforge.script-test-support :refer :all]))

(deftest swarmforge-launcher-parses-config-and-writes-state-files
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "# comment\n"
                       "window coder codex master\n"
                       "window cleaner codex cleaner batch\n"))
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path root "swarmforge/roles/cleaner.prompt") "cleaner\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (str/includes? (:out result) "coder Coder"))
        (is (str/includes? (:out result) "cleaner Cleaner"))
        (is (str/includes? (:out result) "cleaner batch"))
        (is (str/includes? (:out result) "swarmforge-coder"))
        (is (str/includes? (:out result) "swarmforge-cleaner"))
        (is (fs/exists? (fs/path root ".swarmforge/tmux-socket"))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-reads-definition-directory-instead-of-tracked-swarmforge
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt") "Tracked constitution.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf") "window coder codex master\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path root ".swarmforge/definition/swarmforge/constitution.prompt")
                  "Definition constitution.\n")
      (write-file (fs/path root ".swarmforge/definition/swarmforge/swarmforge.conf")
                  "window architect codex master\n")
      (write-file (fs/path root ".swarmforge/definition/swarmforge/roles/architect.prompt")
                  "architect\n")
      (let [result (run {:dir root :ok? false} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (str/includes? (:out result) "architect Architect")
            (str "definition role missing from --test-parse output: " (pr-str result)))
        (is (not (str/includes? (:out result) "coder Coder"))
            (str "tracked swarmforge/ role leaked into --test-parse output: " (pr-str result))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-writes-configured-card-routes
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt") "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "card quick coder reviewer\n"
                       "window coder codex master\n"
                       "window reviewer codex reviewer\n"))
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path root "swarmforge/roles/reviewer.prompt") "reviewer\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (zero? (:exit result)))
        (is (= "quick\tcoder,reviewer\n"
               (slurp (str (fs/path root ".swarmforge/routes.tsv"))))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-rejects-card-routes-with-unknown-roles
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt") "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "card quick coder missing\n"
                       "window coder codex master\n"))
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (let [result (run {:dir root :ok? false}
                        (script "swarmforge.bb") "--test-parse" (str root))]
        (is (pos? (:exit result)))
        (is (str/includes? (:err result) "Unknown role 'missing'")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-exactly-mirrors-managed-worktree-trees
  (let [root (tmp-dir)
        worktree (fs/path root ".worktrees/coder")]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt") "current constitution\n")
      (write-file (fs/path root "swarmforge/constitution/current.prompt") "current article\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "card quick master-role coder\n"
                       "window master-role codex master\n"
                       "window coder codex coder\n"))
      (write-file (fs/path root "swarmforge/roles/master-role.prompt") "master\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path worktree "swarmforge/scripts/stale.bb") "stale\n")
      (write-file (fs/path worktree "swarmforge/roles/stale.prompt") "stale\n")
      (write-file (fs/path worktree "swarmforge/constitution/stale.prompt") "stale\n")
      (write-file (fs/path worktree "product.txt") "keep\n")
      (write-file (fs/path worktree ".swarmforge/keep.state") "keep\n")
      (let [result (run {:dir root} (script "swarmforge.bb")
                        "--test-sync-worktrees" (str root))]
        (is (zero? (:exit result)) (:err result)))
      (is (not (fs/exists? (fs/path worktree "swarmforge/scripts/stale.bb"))))
      (is (not (fs/exists? (fs/path worktree "swarmforge/roles/stale.prompt"))))
      (is (not (fs/exists? (fs/path worktree "swarmforge/constitution/stale.prompt"))))
      (is (fs/regular-file? (fs/path worktree "swarmforge/scripts/safe_paths.bb")))
      (is (fs/regular-file? (fs/path worktree "swarmforge/constitution/current.prompt")))
      (is (= "keep\n" (slurp (str (fs/path worktree "product.txt")))))
      (is (= "keep\n" (slurp (str (fs/path worktree ".swarmforge/keep.state")))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-sync-leaves-role-worktree-swarmforge-alone-for-a-definition-project
  (let [root (tmp-dir)
        worktree (fs/path root ".worktrees/coder")]
    (try
      (write-definition-pack! root)
      (write-tracked-swarmforge! worktree)
      (let [before (file-contents (fs/path worktree "swarmforge"))
            result (run {:dir root} (script "swarmforge.bb") "--test-sync-worktrees" (str root))
            after (file-contents (fs/path worktree "swarmforge"))]
        (is (zero? (:exit result)) (:err result))
        (is (= before after)
            (str "worktree swarmforge/ files after sync: " (pr-str (keys after)))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-sync-still-copies-state-into-role-worktrees-for-a-definition-project
  (let [root (tmp-dir)
        worktree (fs/path root ".worktrees/coder")]
    (try
      (write-definition-pack! root)
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-sync-worktrees" (str root))]
        (is (zero? (:exit result)) (:err result))
        (doseq [state-file ["sessions.tsv" "roles.tsv" "routes.tsv"]]
          (let [copied (fs/path worktree ".swarmforge" state-file)
                copied-text (when (fs/exists? copied) (slurp (str copied)))]
            (is (= (slurp (str (fs/path root ".swarmforge" state-file))) copied-text)
                (str state-file " in the role worktree: " (pr-str copied-text))))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-role-path-uses-the-running-forge-scripts-for-a-definition-project
  (let [root (tmp-dir)]
    (try
      (write-definition-pack! root)
      (let [out (:out (run {:dir root} (script "swarmforge.bb")
                           "--test-role-launch-command" (str root) "coder"))
            worktree-scripts (str (fs/path root ".worktrees/coder/swarmforge/scripts"))]
        (is (str/includes? out (str ":'" scripts-dir "':$PATH"))
            (str "running forge scripts dir not on PATH: " out))
        (is (not (str/includes? out worktree-scripts))
            (str "worktree scripts dir on PATH: " out)))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-uses-portable-tmux-socket-dir
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "window coder codex master\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))
      (let [socket-path (str/trim (slurp (str (fs/path root ".swarmforge/tmux-socket"))))]
        (is (str/starts-with? socket-path "/tmp/swarmforge-"))
        (is (not (str/starts-with? socket-path "/private/tmp/"))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-launcher-rejects-invalid-config
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "window coder codex master\n"
                       "window coder codex other\n"))
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (let [result (run {:dir root :ok? false} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (= 1 (:exit result)))
        (is (str/includes? (:err result) "Duplicate role 'coder'")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-parses-window-invisible
  ;; Given window-invisible specifier codex master
  ;; When --test-parse
  ;; Then specifier is listed and visible? is false
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "window-invisible specifier codex master\n")
      (write-file (fs/path root "swarmforge/roles/specifier.prompt") "specifier\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (str/includes? (:out result) "specifier"))
        (is (str/includes? (:out result) "invisible")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-required-helpers-include-pack-scripts
  ;; Given the launcher required-helpers list
  ;; When --test-required-helpers
  ;; Then pack_web.sh and pack_board.sh are listed
  (let [result (run {:dir repo-root} (script "swarmforge.bb") "--test-required-helpers")
        names (set (str/split-lines (str/trim (:out result))))]
    (is (contains? names "pack_web.sh"))
    (is (contains? names "pack_board.sh"))
    (is (contains? names "pack_dashboard_request.sh"))))
(deftest swarmforge-launch-plan-starts-pack-web-and-skips-invisible-terminals
  ;; Given window-invisible specifier and a visible coder window
  ;; When --test-launch-plan
  ;; Then pack_web starts, specifier skips Terminal, and coder still opens Terminal
  (let [root (tmp-dir)]
    (try
      (write-pack-conf! root
                        (str "window-invisible specifier codex master\n"
                             "window coder codex coder\n"))
      (let [out (:out (run {:dir root} (script "swarmforge.bb")
                           "--test-launch-plan" (str root)))]
        (is (str/includes? out "pack_web start"))
        (is (str/includes? out "skip-terminal specifier"))
        (is (str/includes? out "open-terminal coder"))
        (is (not (str/includes? out "skip-terminal coder")))
        (is (not (str/includes? out "open-terminal specifier"))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-fails-without-a-master-worktree
  ;; Given only window coder codex coder
  ;; When --test-parse
  ;; Then exit 1 and error mentions master
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "window coder codex coder\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (let [result (run {:dir root :ok? false} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (= 1 (:exit result)))
        (is (str/includes? (:err result) "master")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-fails-with-two-master-worktrees
  ;; Given two windows whose worktree is master
  ;; When --test-parse
  ;; Then exit 1 and error mentions master
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "window specifier codex master\n"
                       "window coder codex master\n"))
      (write-file (fs/path root "swarmforge/roles/specifier.prompt") "specifier\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (let [result (run {:dir root :ok? false} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (= 1 (:exit result)))
        (is (str/includes? (:err result) "master")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-terminal-bridge-preserves-adapter-globals
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/scripts/swarm-terminal-adapter.sh")
                  (str "load_terminal_backend() {\n"
                       "  source \"$SCRIPT_DIR/terminal-adapters/$1.sh\"\n"
                       "}\n"))
      (write-file (fs/path root "swarmforge/scripts/terminal-adapters/probe.sh")
                  (str "terminal_open_session() {\n"
                       "  printf '%s\\n' \"$WORKING_DIR|$TMUX_SOCKET|$1|$2|$3\"\n"
                       "}\n"))
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-terminal-bridge"
                        (str root)
                        "probe")]
        (is (str/includes? (:out result) (str root "|")))
        (is (str/includes? (:out result) "|swarmforge-specifier|SwarmForge Specifier|"))
        (is (not (str/includes? (:out result) "cd ''")))
        (is (not (str/includes? (:out result) "-S ''"))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-agent-start-delay-is-configurable
  (let [default-result (run {:dir repo-root}
                            (script "swarmforge.bb")
                            "--test-agent-start-delay")
        configured-result (run {:dir repo-root
                                :env {"SWARMFORGE_AGENT_START_DELAY_MS" "2750"}}
                               (script "swarmforge.bb")
                               "--test-agent-start-delay")
        invalid-result (run {:dir repo-root
                             :env {"SWARMFORGE_AGENT_START_DELAY_MS" "fast"}}
                            (script "swarmforge.bb")
                            "--test-agent-start-delay")]
    (is (= "1500" (str/trim (:out default-result))))
    (is (= "2750" (str/trim (:out configured-result))))
    (is (= "1500" (str/trim (:out invalid-result))))))
(deftest swarmforge-sleep-prevention-can-be-disabled
  (let [result (run {:dir repo-root
                     :env {"SWARMFORGE_PREVENT_SLEEP" "0"}}
                    (script "swarmforge.bb")
                    "--test-sleep-inhibitor-prefix")]
    (is (= "" (str/trim (:out result))))))
(deftest swarmforge-launcher-parses-extra-cli-args
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "window coder copilot master --yolo\n"
                       "window cleaner copilot cleaner batch --allow-all-tools\n"))
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path root "swarmforge/roles/cleaner.prompt") "cleaner\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (str/includes? (:out result) "coder Coder"))
        (is (str/includes? (:out result) "task forward-only --yolo"))
        (is (str/includes? (:out result) "batch forward-only --allow-all-tools")))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-parses-propagation-tokens
  ;; Given omitted, back-one, and back-all after receive-mode, plus extra CLI args
  ;; When --test-parse
  ;; Then omitted is forward-only, tokens round-trip in roles.tsv, extra args still apply
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  (str "window specifier grok master\n"
                       "window coder grok coder task --yolo\n"
                       "window refactorer grok refactorer task back-one\n"
                       "window architect grok architect batch back-all --allow-all-tools\n"))
      (write-file (fs/path root "swarmforge/roles/specifier.prompt") "specifier\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (write-file (fs/path root "swarmforge/roles/refactorer.prompt") "refactorer\n")
      (write-file (fs/path root "swarmforge/roles/architect.prompt") "architect\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))
            out (:out result)]
        (is (zero? (:exit result)))
        (is (str/includes? out "specifier Specifier"))
        (is (str/includes? out "task forward-only"))
        (is (str/includes? out "task forward-only --yolo"))
        (is (str/includes? out "task back-one"))
        (is (str/includes? out "batch back-all --allow-all-tools"))
        (let [roles (slurp (str (fs/path root ".swarmforge/roles.tsv")))
              lines (str/split-lines roles)]
          (is (str/ends-with? (first lines) "\ttask\tforward-only"))
          (is (str/includes? (nth lines 1) "\ttask\tforward-only"))
          (is (str/ends-with? (nth lines 2) "\ttask\tback-one"))
          (is (str/ends-with? (nth lines 3) "\tbatch\tback-all"))))
      (finally
        (fs/delete-tree root)))))
(deftest copilot-launch-command-passes-extra-cli-args
  (let [root (tmp-dir)]
    (try
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-launch-command"
                        (str root)
                        "copilot"
                        "--yolo")
            command (:out result)]
        (is (str/includes? command "copilot -C "))
        (is (re-find #"--name 'SwarmForge Coder' --yolo -i" command)))
      (finally
        (fs/delete-tree root)))))
(deftest grok-launch-command-passes-initial-prompt
  (let [root (tmp-dir)]
    (try
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-launch-command"
                        (str root)
                        "grok")
            command (:out result)]
        (is (str/includes? command "grok --cwd "))
        (is (str/includes? command "--permission-mode bypassPermissions"))
        (is (str/includes? command "--rules \"$(cat "))
        (is (str/includes? command "--verbatim \"$(cat "))
        (is (str/includes? command ".swarmforge/prompts/coder.md"))
        (is (fs/exists? (fs/path root ".swarmforge/prompts/coder.md"))))
      (finally
        (fs/delete-tree root)))))

(deftest deepseek-launch-command-uses-codex-with-deepseek-profile
  ;; Given a pack role configured with backend deepseek
  ;; When SwarmForge builds the launch command
  ;; Then it invokes the codex CLI with the DeepSeek model profile
  (let [root (tmp-dir)]
    (try
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-launch-command"
                        (str root)
                        "deepseek")
            command (:out result)]
        (is (str/includes? command "codex -C "))
        (is (str/includes? command "--profile deepseek"))
        (is (str/includes? command "--no-alt-screen"))
        (is (str/includes? command "--yolo")))
      (finally
        (fs/delete-tree root)))))

(defn- deepseek-flag-output [flag]
  (str/trim (:out (run {:dir repo-root} (script "swarmforge.bb") flag "deepseek"))))

(deftest deepseek-backend-is-codex-backed
  ;; Given the deepseek backend runs through the codex CLI under the hood
  ;; When codex-backed? checks it
  ;; Then it reports true, so launch-role! pre-trusts the worktree for it too
  (is (= "true" (deepseek-flag-output "--test-codex-backed"))))

(deftest deepseek-backend-requires-codex-binary
  ;; Given the deepseek backend runs through the codex CLI under the hood
  ;; When the dependency-check command is resolved for it
  ;; Then it checks for codex, not a nonexistent deepseek binary
  (is (= "codex" (deepseek-flag-output "--test-required-command"))))

(deftest swarmforge-accepts-deepseek-as-known-agent
  ;; Given a role configured with backend deepseek
  ;; When --test-parse validates swarmforge.conf
  ;; Then it parses successfully instead of rejecting deepseek as unsupported
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/constitution.prompt")
                  "Read articles.\n")
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "window coder deepseek master\n")
      (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n")
      (let [result (run {:dir root} (script "swarmforge.bb") "--test-parse" (str root))]
        (is (str/includes? (:out result) "coder Coder")))
      (finally
        (fs/delete-tree root)))))

(deftest start-pack-web-drops-stale-dashboard-url
  ;; Given a leftover dashboard-url and pack_web.pid from a prior run
  ;; When SwarmForge prepares to start the dashboard
  ;; Then those stale files are removed so the new port is recorded
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root ".swarmforge/dashboard-url") "http://127.0.0.1:64002\n")
      (write-file (fs/path root ".swarmforge/pack_web.pid") "99999999\n")
      (let [out (str/trim (:out (run {:dir root}
                                     (script "swarmforge.bb")
                                     "--test-reset-pack-web-state"
                                     (str root))))]
        (is (= "false false" out))
        (is (not (fs/exists? (fs/path root ".swarmforge/dashboard-url"))))
        (is (not (fs/exists? (fs/path root ".swarmforge/pack_web.pid")))))
      (finally
        (fs/delete-tree root)))))
(deftest lieutenant-forge-with-projects-is-a-host
  (let [root (tmp-dir)]
    (try
      (fs/create-dirs (fs/path root "projects"))
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "# Host lieutenant. Default is grok with no extra args.\n")
      (let [out (str/trim (:out (run {:dir root}
                                     (script "swarmforge.bb")
                                     "--test-forge-root"
                                     (str root))))]
        (is (= "true" out)))
      (finally
        (fs/delete-tree root)))))
(deftest pack-tree-without-projects-is-not-a-host
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/swarmforge.conf")
                  "window specifier grok master\n")
      (let [out (str/trim (:out (run {:dir root}
                                     (script "swarmforge.bb")
                                     "--test-forge-root"
                                     (str root))))]
        (is (= "false" out)))
      (finally
        (fs/delete-tree root)))))
(deftest grok-lieutenant-launch-waits-for-chat
  ;; Given a host lieutenant
  ;; When SwarmForge builds the grok launch command
  ;; Then grok loads rules and stays idle — no initial --verbatim prompt
  (let [root (tmp-dir)]
    (try
      (let [command (:out (run {:dir root}
                               (script "swarmforge.bb")
                               "--test-lieutenant-launch-command"
                               (str root)))]
        (is (str/includes? command "grok --cwd "))
        (is (str/includes? command "--minimal --rules \"$(cat "))
        (is (str/includes? command ".swarmforge/prompts/lieutenant.md"))
        (is (not (str/includes? command "--verbatim")))
        (is (fs/exists? (fs/path root ".swarmforge/prompts/lieutenant.md"))))
      (finally
        (fs/delete-tree root)))))
(deftest lieutenant-launch-reads-host-conf
  ;; Given a host conf line Lieutenant claude --yolo
  ;; When SwarmForge builds the lieutenant launch command
  ;; Then the command uses claude with --yolo
  (let [root (tmp-dir)]
    (try
      (write-file (fs/path root "swarmforge/swarmforge.conf") "Lieutenant claude --yolo\n")
      (let [command (:out (run {:dir root}
                               (script "swarmforge.bb")
                               "--test-lieutenant-launch-command"
                               (str root)))]
        (is (str/includes? command "claude --append-system-prompt-file "))
        (is (str/includes? command "--yolo"))
        (is (not (str/includes? command "grok --cwd "))))
      (finally
        (fs/delete-tree root)))))
(deftest grok-launch-command-uses-minimal-for-scrollback
  ;; Given a grok pack role
  ;; When SwarmForge builds the launch command
  ;; Then grok runs --minimal so finalized chatter is in tmux scrollback
  (let [root (tmp-dir)]
    (try
      (let [command (:out (run {:dir root}
                               (script "swarmforge.bb")
                               "--test-launch-command"
                               (str root)
                               "grok"))]
        (is (str/includes? command " --minimal ")))
      (finally
        (fs/delete-tree root)))))
(deftest launch-command-puts-transcript-in-tmux-scrollback
  ;; Given each pack backend
  ;; When SwarmForge builds the launch command
  ;; Then Codex and Copilot use --no-alt-screen, Claude disables the
  ;; alternate screen, and Grok keeps --minimal
  (doseq [[agent needle] [["codex" "--no-alt-screen"]
                          ["copilot" "--no-alt-screen"]
                          ["claude" "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1"]
                          ["grok" "--minimal"]]]
    (let [root (tmp-dir)]
      (try
        (let [command (:out (run {:dir root}
                                 (script "swarmforge.bb")
                                 "--test-launch-command"
                                 (str root)
                                 agent))]
          (is (str/includes? command needle) agent))
        (finally
          (fs/delete-tree root))))))
(deftest grok-launch-command-uses-bypass-permissions-with-always-approve
  (let [root (tmp-dir)]
    (try
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-launch-command"
                        (str root)
                        "grok"
                        "--always-approve")
            command (:out result)]
        (is (str/includes? command "--permission-mode bypassPermissions"))
        (is (str/includes? command "--always-approve"))
        (is (not (str/includes? command "--permission-mode acceptEdits"))))
      (finally
        (fs/delete-tree root)))))
(deftest launch-command-yolos-every-backend
  ;; Given a pack role with no extra-args
  ;; When --test-launch-command for each backend
  ;; Then the start command bypasses permission prompts
  (doseq [[agent needle] [["codex" "--yolo"]
                          ["copilot" "--yolo"]
                          ["claude" "--permission-mode bypassPermissions"]
                          ["grok" "--permission-mode bypassPermissions"]]]
    (let [root (tmp-dir)]
      (try
        (let [command (:out (run {:dir root}
                                 (script "swarmforge.bb")
                                 "--test-launch-command"
                                 (str root)
                                 agent))]
          (is (str/includes? command needle) agent))
        (finally
          (fs/delete-tree root))))))
(deftest launch-command-puts-project-tool-bin-on-path
  ;; Given a launched role
  ;; When the start command is built
  ;; Then `.swarmforge/bin` is on PATH so require/ensure wrappers are found
  (let [root (tmp-dir)]
    (try
      (let [command (:out (run {:dir root}
                               (script "swarmforge.bb")
                               "--test-launch-command"
                               (str root)
                               "codex"))]
        (is (str/includes? command (str ".swarmforge/bin':'"))))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-trusts-codex-worktree-once
  ;; Given a Codex worktree with no projects block
  ;; When startup ensures trust
  ;; Then config.toml gains trust_level trusted for that exact path, once
  (let [root (tmp-dir)
        home (fs/create-temp-dir {:prefix "codex-home."})
        wt (str (fs/absolutize root))]
    (try
      (doseq [_ [1 2]]
        (run {:dir root :env {"CODEX_HOME" (str home)
                              "HOME" (str home)
                              "PATH" (System/getenv "PATH")
                              "GIT_CONFIG_NOSYSTEM" "1"}}
             (script "swarmforge.bb")
             "--test-ensure-codex-trust"
             wt))
      (let [cfg (slurp (str (fs/path home "config.toml")))
            header (str "[projects." (pr-str wt) "]")
            hits (count (re-seq (re-pattern (java.util.regex.Pattern/quote header)) cfg))]
        (is (str/includes? cfg header))
        (is (str/includes? cfg "trust_level = \"trusted\""))
        (is (= 1 hits)))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree home)))))
(deftest swarmforge-does-not-overwrite-existing-codex-project-block
  ;; Given an existing projects block for the worktree
  ;; When startup ensures trust
  ;; Then that block is left unchanged
  (let [root (tmp-dir)
        home (fs/create-temp-dir {:prefix "codex-home."})
        wt (str (fs/absolutize root))
        header (str "[projects." (pr-str wt) "]")
        original (str header "\ntrust_level = \"untrusted\"\nnote = \"keep\"\n")]
    (try
      (write-file (fs/path home "config.toml") original)
      (run {:dir root :env {"CODEX_HOME" (str home)
                            "PATH" (System/getenv "PATH")
                            "GIT_CONFIG_NOSYSTEM" "1"}}
           (script "swarmforge.bb")
           "--test-ensure-codex-trust"
           wt)
      (is (= original (slurp (str (fs/path home "config.toml")))))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree home)))))
(deftest swarmforge-trust-does-not-duplicate-existing-config
  ;; Given a config.toml that already has another project table
  ;; When startup trusts a new worktree
  ;; Then the old table appears once and the new path appears once
  (let [root (tmp-dir)
        home (fs/create-temp-dir {:prefix "codex-home."})
        wt (str (fs/absolutize root))
        other "[projects.\"/other\"]\ntrust_level = \"trusted\"\n"]
    (try
      (write-file (fs/path home "config.toml") (str "model = \"gpt-5.5\"\n\n" other))
      (run {:dir root :env {"CODEX_HOME" (str home)
                            "PATH" (System/getenv "PATH")
                            "GIT_CONFIG_NOSYSTEM" "1"}}
           (script "swarmforge.bb")
           "--test-ensure-codex-trust"
           wt)
      (let [cfg (slurp (str (fs/path home "config.toml")))]
        (is (= 1 (count (re-seq #"model = \"gpt-5.5\"" cfg))))
        (is (= 1 (count (re-seq #"\[projects\.\"/other\"\]" cfg))))
        (is (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote
                                             (str "[projects." (pr-str wt) "]")))
                                cfg)))))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree home)))))
(defn- ensure-claude-trust! [root env worktree]
  (run {:dir root :env (merge {"PATH" (System/getenv "PATH")
                               "GIT_CONFIG_NOSYSTEM" "1"}
                              env)}
       (script "swarmforge.bb")
       "--test-ensure-claude-trust"
       worktree))
(deftest swarmforge-trusts-workspace-for-claude-launches
  ;; Given a claude worktree with no .claude.json
  ;; When startup ensures trust
  ;; Then .claude.json gains hasTrustDialogAccepted for that exact path, once
  (let [root (tmp-dir)
        home (fs/create-temp-dir {:prefix "claude-home."})
        cfg-file (fs/path home ".claude.json")
        wt (str (fs/absolutize root))]
    (try
      (doseq [_ [1 2]]
        (ensure-claude-trust! root {"CLAUDE_CONFIG_FILE" (str cfg-file)} wt))
      (let [cfg (json/parse-string (slurp (str cfg-file)))]
        (is (= true (get-in cfg ["projects" wt "hasTrustDialogAccepted"]))))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree home)))))
(deftest swarmforge-claude-trust-also-accepts-bypass-permissions-disclaimer
  ;; Given a claude worktree with no ~/.claude/settings.json
  ;; When startup ensures trust
  ;; Then skipDangerousModePermissionPrompt is true in userSettings
  (let [root (tmp-dir)
        config-dir (fs/create-temp-dir {:prefix "claude-config-dir."})
        settings-file (fs/path config-dir "settings.json")
        wt (str (fs/absolutize root))]
    (try
      (ensure-claude-trust! root {"CLAUDE_CONFIG_DIR" (str config-dir)} wt)
      (let [settings (json/parse-string (slurp (str settings-file)))]
        (is (= true (get settings "skipDangerousModePermissionPrompt"))))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree config-dir)))))
(deftest swarmforge-claude-trust-preserves-existing-settings
  ;; Given ~/.claude/settings.json with unrelated settings
  ;; When startup ensures trust
  ;; Then those settings survive, alongside skipDangerousModePermissionPrompt
  (let [root (tmp-dir)
        config-dir (fs/create-temp-dir {:prefix "claude-config-dir."})
        settings-file (fs/path config-dir "settings.json")
        wt (str (fs/absolutize root))]
    (try
      (write-file settings-file (json/generate-string {"theme" "dark"}))
      (ensure-claude-trust! root {"CLAUDE_CONFIG_DIR" (str config-dir)} wt)
      (let [settings (json/parse-string (slurp (str settings-file)))]
        (is (= "dark" (get settings "theme")))
        (is (= true (get settings "skipDangerousModePermissionPrompt"))))
      (finally
        (fs/delete-tree root)
        (fs/delete-tree config-dir)))))
(deftest swarmforge-start-order-opens-dashboard-before-agents
  ;; Given a pack
  ;; When --test-start-order
  ;; Then pack_web starts before agents
  (let [root (tmp-dir)]
    (try
      (write-pack-conf! root
                        (str "window-invisible specifier codex master\n"
                             "window coder codex coder\n"))
      (let [out (:out (run {:dir root} (script "swarmforge.bb")
                           "--test-start-order" (str root)))
            pack (.indexOf out "pack_web start")
            agents (.indexOf out "start-agents")]
        (is (>= pack 0))
        (is (>= agents 0))
        (is (< pack agents)))
      (finally
        (fs/delete-tree root)))))
(deftest swarmforge-detects-nonzero-pane-base-index
  (let [root (tmp-dir)
        sock (tmp-tmux-socket)
        conf (fs/path root "tmux.conf")]
    (try
      (write-file conf "set -g base-index 1\nset -g pane-base-index 1\n")
      (run {:dir root} "tmux" "-S" sock "-f" (str conf) "new-session" "-d" "-s" "probe" "sleep" "120")
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-tmux-base-indexes"
                        sock)]
        (is (= "1 1" (str/trim (:out result)))))
      (finally
        (run {:dir root :ok? false} "tmux" "-S" sock "kill-server")
        (fs/delete-tree root)))))
(deftest role-session-keeps-tmux-scrollback
  ;; Given a tmux socket
  ;; When SwarmForge creates a role session
  ;; Then history-limit keeps thousands of lines
  (let [root (tmp-dir)
        sock (tmp-tmux-socket)
        conf (fs/path root "tmux.conf")]
    (try
      (write-file conf "set -g history-limit 50\n")
      (run {:dir root} "tmux" "-S" sock "-f" (str conf) "new-session" "-d" "-s" "probe" "sleep" "120")
      (let [result (run {:dir root}
                        (script "swarmforge.bb")
                        "--test-create-role-session"
                        sock
                        "swarmforge-specifier"
                        (str (fs/path root "pane.log")))
            limit (Long/parseLong (str/trim (:out result)))]
        (is (zero? (:exit result)))
        (is (>= limit 2000)))
      (finally
        (run {:dir root :ok? false} "tmux" "-S" sock "kill-server")
        (fs/delete-tree root)))))
(defn- eventually? [pred]
  (loop [attempts-left 50]
    (cond
      (pred) true
      (zero? attempts-left) false
      :else (do (Thread/sleep 100) (recur (dec attempts-left))))))

(deftest role-session-pipes-pane-output-to-a-log-file
  ;; Given a tmux socket
  ;; When SwarmForge creates a role session
  ;; Then its pane's output is already being logged before anything is typed into it
  (let [root (tmp-dir)
        sock (tmp-tmux-socket)
        log-file (fs/path root "pane.log")
        session "swarmforge-specifier"]
    (try
      (run {:dir root}
          (script "swarmforge.bb")
          "--test-create-role-session"
          sock
          session
          (str log-file))
      (run {:dir root} "tmux" "-S" sock "send-keys" "-t" session "-l" "hello-from-pane")
      (run {:dir root} "tmux" "-S" sock "send-keys" "-t" session "Enter")
      (is (eventually? #(and (fs/exists? log-file)
                             (str/includes? (slurp (str log-file)) "hello-from-pane")))
          (str "expected " log-file " to contain the pane's output"))
      (finally
        (run {:dir root :ok? false} "tmux" "-S" sock "kill-server")
        (fs/delete-tree root)))))
