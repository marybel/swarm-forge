#!/usr/bin/env bb

(ns pack-web
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

;; *file* has no parent segment when this ns is `require`d from the
;; classpath (as the in-process coverage test does) instead of invoked
;; directly; fall back to the classpath resource location.
(def script-dir
  (or (fs/parent *file*)
      (some-> (io/resource *file*) io/file .getParent fs/path)))
(try
  (require 'safe-paths)
  (catch Exception _
    (load-file (str (fs/path script-dir "safe_paths.bb")))))
(load-file (str (fs/path script-dir "forge.bb")))
(load-file (str (fs/path script-dir "card_type.bb")))

(def usage-text
  (str "Usage:\n"
       "  pack_web.sh --serve <root> [port]\n"
       "  pack_web.sh --test-state <root>\n"
       "  pack_web.sh --test-html\n"
       "  pack_web.sh --test-post-task <root> <name> <text>\n"
       "  pack_web.sh --test-post-chat <root> <text>\n"
       "  pack_web.sh --test-inject-payload [name text]\n"
       "  pack_web.sh --test-inject-argv <root> <file> <text>\n"
       "  pack_web.sh --test-approve <root> <id>\n"
       "  pack_web.sh --test-reject <root> <id>\n"
       "  pack_web.sh --test-pane <root> <role>\n"
       "  pack_web.sh --test-agent-page [role]\n"
       "  pack_web.sh --test-heat <root>\n"
       "  pack_web.sh --test-card-heat <root>\n"
       "  pack_web.sh --test-heat-isolation <root-a> <root-b>\n"
       "  pack_web.sh --test-heat-codex <root>\n"
       "  pack_web.sh --test-heat-reorder <root>\n"
       "  pack_web.sh --test-heat-head <root>\n"
       "  pack_web.sh --test-heat-mail <root>\n"
       "  pack_web.sh --test-heat-grok <root>\n"
       "  pack_web.sh --test-heat-collapse <root>\n"
       "  pack_web.sh --test-status-pane <root> <text>\n"
       "  pack_web.sh --test-status-persist <root> <first> <second>\n"
       "  pack_web.sh --test-answer-clarification <root> <id> <text>\n"
       "  pack_web.sh --test-task <root> <name>\n"
       "  pack_web.sh --test-tree <root> <name> [path]\n"
       "  pack_web.sh --test-file <root> <name> <path>\n"
       "  pack_web.sh --test-delete-task <root> <name>\n"
       "  pack_web.sh --test-delete-approval <root> <id>\n"
       "  pack_web.sh --test-retry-task <root> <id> <comments>\n"
       "  pack_web.sh --test-save-comments <root> <id> <path> <comments>\n"
       "  pack_web.sh --test-doc <root> <path>\n"
       "  pack_web.sh --test-teardown <root> [TEARDOWN]\n"
       "  pack_web.sh --test-new-project <root> <name> <pack> [mission]\n"
       "  pack_web.sh --test-new-project-replace <root> <name> <pack> [mission]\n"
       "  pack_web.sh --test-new-github-project <root> <owner/repo> <pack> [mission] [branch]\n"
       "  pack_web.sh --test-open-project <root> <name>\n"
       "  pack_web.sh --test-close-project <root> <name>\n"
       "  pack_web.sh --test-inferred-name <input> [github]\n"
       "  pack_web.sh --test-mission <root> [project]\n"
       "  pack_web.sh --test-allow <root> <name> <act> [project]\n"
       "  pack_web.sh --test-lieutenant-heat <root>\n"
       "  pack_web.sh --test-pane-merge <history-file> <visible-file>"))

(def example-task-name "htw-console-app")
(def example-task-text
  "Integrate the stories in ~/junk/htw-stories into one console application.")

(def ^:dynamic *tmux-stub* nil)
(def ^:dynamic *pane-text* nil)
(def ^:dynamic *sync-teardown?* false)
(def ^:dynamic *board-tasks-cache* nil)
(def teardown-delay-ms 250)
(def pane-capture-lines 2000)
(def pane-heat (atom {}))
(def pane-status (atom {}))
(def pane-status-lines (atom {}))
(def grok-active-sessions-cache (atom {}))
(def grok-log-status-cache (atom {}))
(def codex-session-catalog-cache (atom {}))
(def codex-role-session-cache (atom {}))
(def codex-log-status-cache (atom {}))

(declare session-name pane-target live-pane-text role-row pane-sample backend-name
         in-process-for-row in-process-task-names approvals
         handoff-files batch-dirs in-process-dir allowed-doc?
         delete-approval! retry-approval! parse-message pane-status-for role-rows
         recorded-pane html-escape worktree-for-role project-query master-role
         session-alive? work-entry file-view)

(defn usage []
  (binding [*out* *err*]
    (println usage-text)))

(defn exit! [status message]
  (binding [*out* *err*]
    (when message
      (println message)))
  (System/exit status))


(load-file (str (fs/path script-dir "pack_web_notify.bb")))
(load-file (str (fs/path script-dir "pack_web_status.bb")))
(load-file (str (fs/path script-dir "pack_web_board.bb")))

(defn query-value [uri key]
  (when-let [q (second (str/split (or uri "") #"\?" 2))]
    (some (fn [pair]
            (let [[k v] (str/split pair #"=" 2)]
              (when (= k key)
                (java.net.URLDecoder/decode (or v "") "UTF-8"))))
          (str/split q #"&"))))

(defn existing-path [root rel]
  (let [path (fs/path root rel)]
    (when (fs/exists? path)
      (fs/canonicalize path))))

(defn under-dir? [file dir]
  (and file dir (fs/starts-with? file dir)))

(defn allowed-doc? [root rel]
  (when-not (str/blank? rel)
    (let [file (existing-path root rel)]
      (and (some? file)
           (fs/regular-file? file)
           (or (under-dir? file (existing-path root "features"))
               (under-dir? file (existing-path root "qa"))
               (under-dir? file (existing-path root "tasks")))))))

(defn get-mission [root]
  (let [file (fs/path root "mission.md")]
    {:status 200
     :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body (if (fs/regular-file? file) (slurp (str file)) "")}))

(defn get-doc [root uri]
  (let [rel (query-value uri "path")]
    (if (allowed-doc? root rel)
      {:status 200
       :headers {"Content-Type" "text/plain; charset=utf-8"}
       :body (slurp (str (existing-path root rel)))}
      {:status 404 :body "Not found"})))

(defn parse-unified-diff [text]
  (->> (str/split-lines (or text ""))
       (drop-while #(not (str/starts-with? % "@@")))
       rest
       (keep (fn [line]
               (cond
                 (str/starts-with? line "+") {:type "add" :text (subs line 1)}
                 (str/starts-with? line "-") {:type "del" :text (subs line 1)}
                 (str/starts-with? line "\\") nil
                 (str/starts-with? line " ") {:type "same" :text (subs line 1)}
                 (str/blank? line) {:type "same" :text ""}
                 :else {:type "same" :text line})))
       vec))

(defn file-diff-lines [root prior commit rel]
  (let [result (apply sh ["git" "-C" (str root) "diff" "--no-color" "-U999999"
                          prior commit "--" rel])]
    (cond
      (not (zero? (:exit result))) nil
      (str/blank? (:out result))
      (mapv (fn [line] {:type "same" :text line})
            (str/split-lines (slurp (str (existing-path root rel)))))
      :else (parse-unified-diff (:out result)))))

(defn pending-headers [root id]
  (let [path (when-not (str/blank? id) (pending-file root id))]
    (when (and path (fs/regular-file? path))
      (:headers (parse-message path)))))

(defn get-api-doc [root uri]
  (let [rel (query-value uri "path")
        id (query-value uri "id")]
    (if-not (allowed-doc? root rel)
      {:status 404 :body "Not found"}
      (let [file (existing-path root rel)
            view (file-view rel file)
            text (or (:text view)
                     (try (slurp (str file)) (catch Exception _ "")))
            headers (pending-headers root id)
            task-id (or (not-empty (get headers "task_id")) (get headers "task"))
            commit (not-empty (get headers "commit"))
            prior (when (and task-id (git-ref-exists? root (rejected-latest task-id)))
                    (rejected-latest task-id))
            lines (when (and prior commit)
                    (file-diff-lines root prior commit rel))
            has-diff (some? lines)
            history (mapv (fn [item]
                            {:at (or (get item "at") (:at item))
                             :text (or (get item "text") (:text item))})
                          (path-review-history root task-id rel))]
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"}
         :body (json/generate-string (merge view
                                            {:path rel
                                             :text text
                                             :has_diff has-diff
                                             :lines (or lines [])
                                             :history history}))}))))

(defn task-query-name [uri]
  (when (str/starts-with? (or uri "") "/task")
    (query-value uri "name")))

(defn board-task-named [root name]
  (some #(when (= name (:name %)) %) (board-tasks root)))

(defn task-document [root name]
  (let [md (safe-paths/task-path! (fs/path root "tasks") name ".md")
        txt (safe-paths/task-path! (fs/path root ".swarmforge" "board") name ".txt")]
    (cond
      (fs/regular-file? md) (slurp (str md))
      (fs/regular-file? txt) (slurp (str txt))
      :else "")))

(defn list-card-audits [root task-id]
  (let [dir (safe-paths/state-key-path! (fs/path root ".swarmforge" "board" "audits")
                                        task-id "")]
    (if (fs/directory? dir)
      (->> (fs/list-dir dir)
           (filter #(re-matches #"[0-9]+\.md" (fs/file-name %)))
           (sort-by #(Long/parseLong (str/replace (fs/file-name %) #"\.md$" "")))
           (mapv (fn [file]
                   (let [n (Long/parseLong (str/replace (fs/file-name file) #"\.md$" ""))]
                     {:n n
                      :label (str "Audit " n)
                      :text (slurp (str file))}))))
      [])))

(defn card-worktree [root task]
  (let [lane (:lane task)
        master (master-role root)]
    (if (#{"waiting" "done" nil} lane)
      (or (worktree-for-role root master) (str root))
      (or (worktree-for-role root lane) (str root)))))

(defn resolve-under [root rel]
  (let [base (try (fs/canonicalize root) (catch Exception _ (fs/absolutize root)))
        target (try
                 (fs/canonicalize (if (str/blank? rel) root (fs/path root rel)))
                 (catch Exception _ nil))]
    (when (and target (fs/starts-with? target base))
      target)))

(defn tree-entries [dir]
  (if (fs/directory? dir)
    (->> (fs/list-dir dir)
         (remove #(= ".git" (fs/file-name %)))
         (sort-by (juxt #(if (fs/directory? %) 0 1) #(str/lower-case (fs/file-name %))))
         (mapv (fn [p]
                 {:name (fs/file-name p)
                  :dir (boolean (fs/directory? p))})))
    []))


(load-file (str (fs/path script-dir "pack_web_view.bb")))

(defn role-row [root role]
  (some #(when (= role (first %)) %) (role-rows root)))

(defn session-for-role [root role]
  (when-let [row (role-row root role)]
    (session-name row)))

(defn worktree-for-role [root role]
  (when-let [row (role-row root role)]
    (nth row 2 nil)))


(load-file (str (fs/path script-dir "pack_web_pane.bb")))

(defn request-project-root [forge uri]
  (if-not (forge/forge? forge)
    forge
    (if-let [name (not-empty (query-value uri "project"))]
      (str (forge/project-dir forge name))
      forge)))

(defn body-map [body]
  (try
    (json/parse-string (or body "{}") true)
    (catch Exception _ {})))

(defn json-ok-data [m]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string (merge {:ok true} m))})

(defn find-approval-root [forge id]
  (or (some (fn [name]
              (let [proot (str (forge/project-dir forge name))]
                (when (fs/regular-file? (pending-file proot id))
                  proot)))
            (forge/read-open-projects forge))
      (throw (ex-info (str "Unknown approval: " id) {:http-status 404}))))

(defn find-clar-root [forge id]
  (or (when (fs/regular-file? (clar-pending-file forge id))
        forge)
      (some (fn [name]
              (let [proot (str (forge/project-dir forge name))]
                (when (fs/regular-file? (clar-pending-file proot id))
                  proot)))
            (forge/read-open-projects forge))
      (throw (ex-info (str "Unknown clarification: " id) {:http-status 404}))))

(defn handle-get [root uri]
  (cond
    (= "/" uri)
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (dashboard-page)}

    (= "/api/state" uri)
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string (api-state root))}

    (agent-pane-role uri)
    (get-agent-pane (request-project-root root uri) uri)

    (agent-role uri)
    (get-agent (request-project-root root uri) uri)

    (task-query-name uri)
    (get-task (request-project-root root uri) uri)

    (str/starts-with? (first (str/split (or uri "") #"\?")) "/api/tree")
    (get-api-tree (request-project-root root uri) uri)

    (str/starts-with? (first (str/split (or uri "") #"\?")) "/api/file")
    (get-api-file (request-project-root root uri) uri)

    (str/starts-with? (first (str/split (or uri "") #"\?")) "/file")
    (get-file-page (request-project-root root uri) uri)

    (str/starts-with? (first (str/split (or uri "") #"\?")) "/api/doc")
    (get-api-doc (request-project-root root uri) uri)

    (str/starts-with? (or uri "") "/doc")
    (get-doc (request-project-root root uri) uri)

    (= "/api/mission" (first (str/split (or uri "") #"\?")))
    (get-mission (request-project-root root uri))

    :else {:status 404 :body "Not found"}))

(defn confirm-teardown? [body]
  (let [text (str/trim (or body ""))]
    (or (= "TEARDOWN" text)
        (try
          (= "TEARDOWN" (:confirm (json/parse-string text true)))
          (catch Exception _ false)))))

(defn current-pid []
  (str (.pid (java.lang.ProcessHandle/current))))

(defn pack-web-pid-file [root]
  (fs/path root ".swarmforge" "pack_web.pid"))

(defn write-pack-web-pid! [root]
  (let [file (pack-web-pid-file root)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (str (current-pid) "\n"))))

(defn stop-pack-web! [root]
  (let [file (pack-web-pid-file root)
        pid (when (fs/exists? file)
              (not-empty (str/trim (slurp (str file)))))]
    (when (and pid (not= pid (current-pid)))
      (sh "kill" "-TERM" pid))
    (fs/delete-if-exists file)))

(defn list-tmux-sessions [socket]
  (if (str/blank? socket)
    []
    (let [result (sh "tmux" "-S" socket "list-sessions" "-F" "#{session_name}")]
      (if (zero? (:exit result))
        (->> (str/split-lines (:out result))
             (remove str/blank?)
             vec)
        []))))

(defn kill-session! [socket session]
  (sh "tmux" "-S" socket "kill-session" "-t" (str "=" session))
  (sh "tmux" "-S" socket "kill-session" "-t" session))

(defn kill-all-sessions-on-socket! [socket]
  (when-not (str/blank? socket)
    (doseq [session (list-tmux-sessions socket)]
      (kill-session! socket session))
    (sh "tmux" "-S" socket "kill-server")))

(defn stop-handoffd! [root]
  (sh "bb" (str (fs/path script-dir "stop_handoff_daemon.bb")) (str root)))

(defn swarm-cleanup! [root socket]
  (let [script (str (fs/path script-dir "swarm-cleanup.sh"))
        ids (str (fs/path root ".swarmforge" "window-ids"))]
    (apply sh (into [script (or socket "none") ids]
                    (list-tmux-sessions socket)))))

(defn close-swarm-bin []
  (let [path (fs/path (fs/parent (fs/parent script-dir)) "close-swarm")]
    (when (fs/exists? path)
      (str path))))

(defn close-swarm! [root]
  (if-let [bin (close-swarm-bin)]
    (sh bin (str root))
    (swarm-cleanup! root (tmux-socket root))))

(defn run-teardown! [root]
  (when (forge/forge? root)
    (forge/close-all-projects! root))
  (close-swarm! root)
  (stop-handoffd! root)
  (kill-all-sessions-on-socket! (tmux-socket root))
  (stop-pack-web! root)
  true)

(defn log-teardown-failure! [root e]
  (binding [*out* *err*]
    (println (str "teardown failed root=" root
                  " error=" (.getMessage e)))
    (flush)))

(defn schedule-teardown! [root]
  (if *sync-teardown?*
    (try
      (run-teardown! root)
      (catch Exception e
        (log-teardown-failure! root e)
        (exit! 1 nil)))
    (future
      (Thread/sleep teardown-delay-ms)
      (try
        (run-teardown! root)
        (System/exit 0)
        (catch Exception e
          (log-teardown-failure! root e)
          (System/exit 1)))))
  true)

(defn teardown-response [root body]
  (if (confirm-teardown? body)
    (do
      (schedule-teardown! root)
      {:status 200
       :headers {"Content-Type" "application/json"}
       :body (json/generate-string {:ok true :status "teardown_started"})})
    {:status 400
     :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body "Teardown requires confirm=TEARDOWN (JSON {\"confirm\":\"TEARDOWN\"}).\n"}))

(defn post-new-project [root body]
  (let [parsed (body-map body)
        created (forge/instantiate! root parsed)]
    (forge/open-project! root (:name created))
    (notify-new-project! root (:name created))
    (json-ok-data created)))

(defn post-open-project [root body]
  (json-ok-data (forge/open-project! root (:name (body-map body)))))

(defn post-close-project [root body]
  (json-ok-data (forge/close-project! root (:name (body-map body)))))

(defn post-board-allow [root body]
  (let [m (body-map body)
        name (:name m)
        act (:act m)
        dest (if (forge/forge? root)
               (let [project (:project m)]
                 (when (str/blank? project)
                   (throw (ex-info "Missing project" {:http-status 400})))
                 (str (forge/project-dir root project)))
               root)]
    (when (or (str/blank? name) (str/blank? act))
      (throw (ex-info "Missing name or act" {:http-status 400})))
    (pack-board dest "allow" "--name" name "--act" act)
    (notify-allow! root dest name act)
    (json-ok)))

(defn scoped-approval-root [root uri body]
  (if-not (forge/forge? root)
    root
    (let [m (body-map body)
          id (or (:id (approval-route uri)) (:id m))
          project (:project m)]
      (cond
        (not (str/blank? project)) (str (forge/project-dir root project))
        (not (str/blank? id)) (find-approval-root root id)
        :else (throw (ex-info "Missing project" {:http-status 400}))))))

(defn scoped-clar-root [root uri]
  (if-not (forge/forge? root)
    root
    (find-clar-root root (clarification-route uri))))

(defn handle-post [root uri body]
  (cond
    (= "/api/projects" uri) (post-new-project root body)
    (= "/api/projects/open" uri) (post-open-project root body)
    (= "/api/projects/close" uri) (post-close-project root body)
    (= "/api/tasks" uri) (post-tasks root body)
    (= "/api/tasks/delete" uri)
    (post-delete-task (scoped-approval-root root uri body) body)
    (= "/api/tasks/retry" uri)
    (post-retry-task (scoped-approval-root root uri body) body)
    (= "/api/chat" uri) (post-chat root body)
    (= "/api/teardown" uri) (teardown-response root body)
    (= "/api/board/allow" uri) (post-board-allow root body)
    (str/starts-with? (or uri "") "/api/approvals/")
    (post-approval (scoped-approval-root root uri body) uri body)
    (str/starts-with? (or uri "") "/api/clarifications/")
    (post-clarification (scoped-clar-root root uri) uri body)
    :else {:status 404 :body "Not found"}))

(defn handle-request [root {:keys [method uri body]}]
  (try
    (case method
      "GET" (handle-get root uri)
      "POST" (handle-post root uri body)
      {:status 404 :body "Not found"})
    (catch Exception e
      (http-error (or (:http-status (ex-data e)) 500) (.getMessage e)))))


(defn request-body [req]
  (when-let [body (:body req)]
    (if (string? body) body (slurp body))))

(defn request-uri [req]
  (let [uri (:uri req)
        qs (:query-string req)]
    (if (str/blank? qs) uri (str uri "?" qs))))

(defn http-handler [root]
  (fn [req]
    (handle-request root {:method (str/upper-case (name (:request-method req)))
                          :uri (request-uri req)
                          :body (request-body req)})))

(defn write-dashboard-url! [root url]
  (let [file (fs/path root ".swarmforge" "dashboard-url")]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (str url "\n"))))

(defn parse-port [port-str]
  (if (str/blank? port-str) 0 (Long/parseLong port-str)))

(defn serve! [root port-str]
  (let [root (require-root! root)
        server (http/run-server (http-handler root)
                                {:ip "127.0.0.1"
                                 :port (parse-port port-str)
                                 :worker-count 8
                                 :legacy-return-value? false})
        url (str "http://127.0.0.1:" (http/server-port server))]
    (write-pack-web-pid! root)
    (write-dashboard-url! root url)
    (println url)
    (flush)
    @(promise)))

(defn -main [& args]
  (case (first args)
    "--serve" (serve! (second args) (nth args 2 nil))
    (do (usage)
        (exit! 1 nil)))
  (System/exit 0))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
