#!/usr/bin/env bb

(ns forge
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def script-dir (fs/parent *file*))
(try
  (require 'safe-paths)
  (catch Exception _
    (load-file (str (fs/path script-dir "safe_paths.bb")))))

(def pack-names ["two-pack" "four-pack" "six-pack"])
(def shared-articles ["engineering.prompt" "workflow.prompt" "handoffs.prompt"])

(defn sh [& args]
  (apply process/sh args))

(defn forge? [root]
  (fs/directory? (fs/path root "projects")))

(defn packs-dir [root]
  (fs/path root "packs"))

(defn project-pack-dir [root]
  (fs/path root ".swarmforge" "project-pack"))

(defn projects-dir [root]
  (fs/path root "projects"))

(defn pack-dir [root _pack]
  (project-pack-dir root))

(defn project-dir [root name]
  (safe-paths/project-path! (projects-dir root) name))

(defn inferred-name
  ([input] (inferred-name input false))
  ([input github?]
   (let [trimmed (str/trim (or input ""))]
     (if github?
       (let [trimmed (str/replace trimmed #"\.git$" "")
             trimmed (str/replace trimmed #"^https?://github.com/" "")
             trimmed (str/replace trimmed #"^git@github.com:" "")]
         (or (last (str/split trimmed #"/")) trimmed))
       trimmed))))

(defn github-clone-url [name]
  (let [base (or (not-empty (System/getenv "SWARMFORGE_GITHUB_BASE"))
                 "https://github.com/")
        name (-> name str/trim
                 (str/replace #"\.git$" "")
                 (str/replace #"^https?://github.com/" "")
                 (str/replace #"^git@github.com:" ""))]
    (if (str/starts-with? base "http")
      (str (str/replace base #"/+$" "") "/" name ".git")
      (str (fs/path (str/replace base #"/+$" "") name)))))

(defn list-pack-names [root]
  (if (fs/regular-file? (fs/path (project-pack-dir root) "swarmforge" "swarmforge.conf"))
    ["lieutenant"]
    []))

(defn pack-conf [root _pack]
  (let [file (fs/path (project-pack-dir root) "swarmforge" "swarmforge.conf")]
    (when (fs/regular-file? file)
      (slurp (str file)))))

(defn list-project-names [root]
  (let [dir (projects-dir root)]
    (if (fs/directory? dir)
      (->> (fs/list-dir dir)
           (filter fs/directory?)
           (map fs/file-name)
           (filter safe-paths/project-name?)
           (remove #(str/starts-with? % "."))
           sort
           vec)
      [])))

(defn open-projects-file [root]
  (fs/path root ".swarmforge" "open-projects"))

(defn project-states-file [root]
  (fs/path root ".swarmforge" "project-states.edn"))

(def project-state-lock (Object.))

(defn read-project-states [root]
  (let [file (project-states-file root)]
    (if (fs/regular-file? file)
      (try
        (let [value (edn/read-string (slurp (str file)))]
          (if (map? value)
            (into {}
                  (filter (fn [[name _]] (safe-paths/project-name? name)))
                  value)
            {}))
        (catch Exception _ {}))
      {})))

(defn write-project-states! [root states]
  (let [file (project-states-file root)]
    (fs/create-dirs (fs/parent file))
    (let [tmp (fs/create-temp-file {:dir (fs/parent file) :prefix ".project-states."})]
      (spit (str tmp) (str (pr-str states) "\n"))
      (fs/move tmp file {:replace-existing true :atomic-move true}))))

(defn read-open-projects [root]
  (let [states (read-project-states root)]
    (if (seq states)
      (->> states
           (keep (fn [[name entry]]
                   (when (= "open" (:state entry)) name)))
           sort
           vec)
      (let [file (open-projects-file root)]
        (if (fs/regular-file? file)
          (->> (str/split-lines (slurp (str file)))
               (map str/trim)
               (filter safe-paths/project-name?)
               (remove str/blank?)
               vec)
          [])))))

(defn write-open-projects! [root names]
  (let [file (open-projects-file root)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (apply str (map #(str % "\n") names)))))

(defn project-open? [root name]
  (boolean (some #{name} (read-open-projects root))))

(defn project-state [root name]
  (safe-paths/require-project-name! name)
  (or (get (read-project-states root) name)
      {:state (if (some #{name} (read-open-projects root)) "open" "closed")
       :error ""}))

(defn set-project-state! [root name state & [error extra]]
  (safe-paths/require-project-name! name)
  (locking project-state-lock
    (let [states (assoc (read-project-states root)
                        name (merge (or extra {})
                                    {:state state
                                     :error (or error "")
                                     :updated-at (str (java.time.Instant/now))}))]
      (write-project-states! root states)
      (write-open-projects! root
                            (->> states
                                 (keep (fn [[project entry]]
                                         (when (= "open" (:state entry)) project)))
                                 sort
                                 vec))))
  (project-state root name))

(defn mark-open! [root name]
  (set-project-state! root name "open"))

(defn mark-closed! [root name]
  (set-project-state! root name "closed"))

(defn pack-name-file [project]
  (fs/path project ".swarmforge" "pack"))

(defn stored-pack [project]
  (let [file (pack-name-file project)]
    (when (fs/regular-file? file)
      (str/trim (slurp (str file))))))

(defn write-pack-name! [project pack]
  (let [file (pack-name-file project)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (str pack "\n"))))

(defn copy-tree-replace! [src dest]
  (when (fs/exists? dest)
    (fs/delete-tree dest))
  (fs/create-dirs (fs/parent dest))
  (fs/copy-tree src dest))

(defn copy-shared-scripts! [forge dest]
  (let [src (fs/path forge "swarmforge" "scripts")]
    (when-not (fs/directory? src)
      (throw (ex-info (str "Missing host scripts at " src) {:http-status 500})))
    (copy-tree-replace! src (fs/path dest "swarmforge" "scripts"))))

(defn copy-shared-articles! [forge dest]
  (let [dest-dir (fs/path dest "swarmforge" "constitution" "articles")]
    (when (fs/exists? dest-dir)
      (fs/delete-tree dest-dir)))
  (doseq [name shared-articles]
    (let [src (fs/path forge "swarmforge" "constitution" "articles" name)]
      (when (fs/regular-file? src)
        (fs/create-dirs (fs/path dest "swarmforge" "constitution" "articles"))
        (fs/copy src (fs/path dest "swarmforge" "constitution" "articles" name)
                 {:replace-existing true})))))

(defn copy-pack-local! [pack-root dest keep-conf?]
  (let [roles (fs/path pack-root "swarmforge" "roles")
        conf (fs/path pack-root "swarmforge" "swarmforge.conf")
        constitution (fs/path pack-root "swarmforge" "constitution.prompt")
        articles (fs/path pack-root "swarmforge" "constitution" "articles")]
    (when-not (fs/directory? roles)
      (throw (ex-info (str "Missing pack roles at " roles) {:http-status 500})))
    (copy-tree-replace! roles (fs/path dest "swarmforge" "roles"))
    (when (and (not keep-conf?) (not (fs/regular-file? conf)))
      (throw (ex-info (str "Missing pack config at " conf) {:http-status 500})))
    (when-not keep-conf?
      (fs/create-dirs (fs/path dest "swarmforge"))
      (fs/copy conf (fs/path dest "swarmforge" "swarmforge.conf") {:replace-existing true}))
    (when-not (fs/regular-file? constitution)
      (throw (ex-info (str "Missing pack constitution at " constitution) {:http-status 500})))
    (fs/copy constitution (fs/path dest "swarmforge" "constitution.prompt") {:replace-existing true})
    (when (fs/directory? articles)
      (fs/create-dirs (fs/path dest "swarmforge" "constitution" "articles"))
      (doseq [file (fs/list-dir articles)]
        (when (fs/regular-file? file)
          (let [name (fs/file-name file)]
            (when-not (some #{name} shared-articles)
              (fs/copy file (fs/path dest "swarmforge" "constitution" "articles" name)
                       {:replace-existing true}))))))))

(defn overlay-pack! [forge dest _pack keep-conf?]
  (let [swarmforge-dir (fs/path dest "swarmforge")
        conf-file (fs/path swarmforge-dir "swarmforge.conf")
        saved-conf (when (and keep-conf? (fs/regular-file? conf-file))
                     (slurp (str conf-file)))]
    (when (fs/exists? swarmforge-dir)
      (fs/delete-tree swarmforge-dir))
    (copy-shared-scripts! forge dest)
    (copy-shared-articles! forge dest)
    (copy-pack-local! (project-pack-dir forge) dest false)
    (when saved-conf
      (spit (str conf-file) saved-conf))))

(def ignore-begin "# BEGIN SWARMFORGE RUNTIME")
(def ignore-end "# END SWARMFORGE RUNTIME")
(def old-runtime-ignore-lines #{".swarmforge/" ".worktrees/"
                                "/.swarmforge/" "/.worktrees/"})

(defn ensure-project-ignore! [dir]
  (let [file (fs/path dir ".gitignore")
        prior (if (fs/regular-file? file) (slurp (str file)) "")
        kept (loop [lines (str/split-lines prior) inside? false out []]
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
                      vec)))
        lines (concat kept
                      (when (seq kept) [""])
                      [ignore-begin "/.swarmforge/" "/.worktrees/" ignore-end])]
    (spit (str file) (str (str/join "\n" lines) "\n"))))

(defn init-git-if-needed! [dir]
  (when-not (fs/exists? (fs/path dir ".git"))
    (sh "git" "init" (str dir))
    (sh "git" "-C" (str dir) "config" "user.email" "swarmforge@local")
    (sh "git" "-C" (str dir) "config" "user.name" "SwarmForge")
    (sh "git" "-C" (str dir) "branch" "-M" "master")
    (let [gitignore (fs/path dir ".gitignore")]
      (when-not (fs/exists? gitignore)
        (spit (str gitignore)
              "# BEGIN SWARMFORGE RUNTIME\n/.swarmforge/\n/.worktrees/\n# END SWARMFORGE RUNTIME\n")))
    (sh {:continue true} "git" "-C" (str dir) "add" ".")
    (sh {:continue true} "git" "-C" (str dir) "commit" "-q" "-m" "Initial swarmforge project")))

(defn git-output [dir & args]
  (let [result (apply sh (concat [{:continue true} "git" "-C" (str dir)] args))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str/trim (str (:err result) "\n" (:out result)))
                      {:http-status 500 :error "git-failed"})))
    (str/trim (:out result))))

(defn ensure-git-identity! [dir]
  (when (str/blank? (:out (sh {:continue true} "git" "-C" (str dir)
                              "config" "--get" "user.email")))
    (git-output dir "config" "user.email" "swarmforge@local"))
  (when (str/blank? (:out (sh {:continue true} "git" "-C" (str dir)
                              "config" "--get" "user.name")))
    (git-output dir "config" "user.name" "SwarmForge")))

(defn commit-managed-project-state! [dir]
  (let [paths (cond-> ["swarmforge" ".gitignore"]
                (fs/exists? (fs/path dir "mission.md")) (conj "mission.md"))]
    (ensure-git-identity! dir)
    (apply git-output dir (concat ["add" "-A" "--"] paths))
    (when-not (str/blank? (apply git-output dir
                                 (concat ["status" "--porcelain" "--"] paths)))
      (apply git-output dir
             (concat ["commit" "--only" "-m" "Update SwarmForge project definition" "--"]
                     paths)))))

(defn clone-github! [url dest]
  (let [result (sh "git" "clone" "--" url (str dest))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "Clone failed: " (str/trim (str (:err result) "\n" (:out result))))
                      {:http-status 400 :error "clone-failed"})))
    dest))

(declare close-project!)

(defn instantiate! [forge {:keys [name github pack conf mission replace]}]
  (let [dir-name (inferred-name name (boolean github))]
    (when (str/blank? dir-name)
      (throw (ex-info "Missing project name" {:http-status 400})))
    (safe-paths/require-project-name! dir-name)
    (let [dest (project-dir forge dir-name)]
      (when-not (fs/regular-file? (fs/path (project-pack-dir forge) "swarmforge" "swarmforge.conf"))
        (throw (ex-info "Missing lieutenant pack at .swarmforge/project-pack" {:http-status 500})))
      (when (and (fs/exists? dest) (not replace))
        (throw (ex-info (str "Project already exists: " dir-name)
                        {:http-status 409 :error "exists"})))
      (fs/create-dirs (projects-dir forge))
      (when (fs/exists? dest)
        (close-project! forge dir-name)
        (fs/delete-tree dest)
        (set-project-state! forge dir-name "closed"))
      (let [staging (fs/path (projects-dir forge)
                             (str ".creating-" dir-name "-" (java.util.UUID/randomUUID)))]
        (try
          (if github
            (clone-github! (github-clone-url name) staging)
            (fs/create-dirs staging))
          (overlay-pack! forge staging pack false)
          (when-not (str/blank? conf)
            (fs/create-dirs (fs/path staging "swarmforge"))
            (spit (str (fs/path staging "swarmforge" "swarmforge.conf")) conf))
          (when-not (nil? mission)
            (spit (str (fs/path staging "mission.md"))
                  (if (str/ends-with? (or mission "") "\n") mission (str mission "\n"))))
          (write-pack-name! staging "lieutenant")
          (ensure-project-ignore! staging)
          (init-git-if-needed! staging)
          (commit-managed-project-state! staging)
          (fs/move staging dest)
          {:name dir-name :path (str dest)}
          (finally
            (when (fs/exists? staging)
              (fs/delete-tree staging))))))))

(defn refresh! [forge name]
  (let [dest (project-dir forge name)
        pack (stored-pack dest)]
    (when-not (fs/directory? dest)
      (throw (ex-info (str "Unknown project: " name) {:http-status 404})))
    (when (str/blank? pack)
      (throw (ex-info (str "No pack recorded for " name) {:http-status 400})))
    (overlay-pack! forge (safe-paths/definition-root dest) pack true)
    {:name name :pack pack}))

(defn skip-start? []
  (= "1" (System/getenv "SWARMFORGE_SKIP_START")))

(defn swarmforge-bb [forge]
  (str (fs/path forge "swarmforge" "scripts" "swarmforge.bb")))

(defn start-project-runtime! [forge name]
  (let [dest (project-dir forge name)
        script (swarmforge-bb forge)
        log (fs/path dest ".swarmforge" "start.log")]
    (fs/create-dirs (fs/parent log))
    (fs/delete-if-exists log)
    (process/process ["bb" script "--start-project" (str dest)]
                     {:out (str log) :err :out})))

(defn stop-project-runtime! [forge name]
  (let [dest (str (project-dir forge name))
        script (swarmforge-bb forge)]
    (process/sh {:continue true} "bb" script "--stop-project" dest)))

(defn runtime-timeout-ms []
  (let [value (System/getenv "SWARMFORGE_RUNTIME_TIMEOUT_MS")]
    (if (and value (re-matches #"[0-9]+" value))
      (Long/parseLong value)
      30000)))

(defn process-alive? [pid]
  (and (re-matches #"[0-9]+" (or pid ""))
       (zero? (:exit (process/sh {:continue true} "kill" "-0" pid)))))

(defn trimmed-file [path]
  (when (fs/regular-file? path)
    (not-empty (str/trim (slurp (str path))))))

(defn runtime-session-names [project]
  (let [file (fs/path project ".swarmforge" "sessions.tsv")]
    (if (fs/regular-file? file)
      (->> (str/split-lines (slurp (str file)))
           (remove str/blank?)
           (keep #(nth (str/split % #"\t" -1) 2 nil))
           (remove str/blank?)
           vec)
      [])))

(defn session-alive? [socket session]
  (and socket
       (zero? (:exit (process/sh {:continue true} "tmux" "-S" socket
                                 "has-session" "-t" (str "=" session))))))

(defn daemon-alive? [project]
  (process-alive? (trimmed-file (fs/path project ".swarmforge" "daemon" "handoffd.pid"))))

(defn runtime-ready? [project]
  (let [socket (trimmed-file (fs/path project ".swarmforge" "tmux-socket"))
        sessions (runtime-session-names project)]
    (and (seq sessions)
         (every? #(session-alive? socket %) sessions)
         (daemon-alive? project))))

(defn runtime-stopped? [project]
  (let [socket (trimmed-file (fs/path project ".swarmforge" "tmux-socket"))]
    (and (not (daemon-alive? project))
         (not-any? #(session-alive? socket %) (runtime-session-names project)))))

(defn wait-for! [pred timeout-ms]
  (loop [waited 0]
    (cond
      (pred) true
      (>= waited timeout-ms) false
      :else (do (Thread/sleep 100) (recur (+ waited 100))))))

(defn wait-for-runtime! [project started timeout-ms]
  (loop [waited 0]
    (cond
      (runtime-ready? project) true
      (and (:proc started) (not (.isAlive ^java.lang.Process (:proc started)))) false
      (>= waited timeout-ms) false
      :else (do (Thread/sleep 100) (recur (+ waited 100))))))

(defn start-log-error [project]
  (let [log (fs/path project ".swarmforge" "start.log")]
    (if (fs/regular-file? log)
      (or (not-empty (->> (str/split-lines (slurp (str log)))
                          (take-last 20)
                          (str/join "\n")
                          str/trim))
          "Project runtime did not become ready")
      "Project runtime did not become ready")))

(defn open-project! [forge name]
  (safe-paths/require-project-name! name)
  (locking project-state-lock
    (let [project (project-dir forge name)
          current (:state (project-state forge name))]
      (when-not (fs/directory? project)
        (throw (ex-info (str "Unknown project: " name) {:http-status 404})))
      (when (#{"open" "starting" "stopping"} current)
        (throw (ex-info (str "Project already " current ": " name)
                        {:http-status 409 :error "project-busy"})))
      (set-project-state! forge name "starting" "" {:managed-runtime (not (skip-start?))})
      (try
        (refresh! forge name)
        (if (safe-paths/has-definition? project)
          (ensure-git-identity! project)
          (commit-managed-project-state! project))
        (if (skip-start?)
          (set-project-state! forge name "open" "" {:managed-runtime false})
          (do
            (let [started (start-project-runtime! forge name)]
              (when-not (wait-for-runtime! project started (runtime-timeout-ms))
              (throw (ex-info (start-log-error project)
                              {:http-status 500 :error "startup-failed"}))))
            (set-project-state! forge name "open" "" {:managed-runtime true})))
        {:name name :open true :state "open"}
        (catch Exception e
          (when-not (skip-start?)
            (stop-project-runtime! forge name))
          (set-project-state! forge name "error" (.getMessage e) {:managed-runtime true})
          (throw e))))))

(defn close-project! [forge name]
  (safe-paths/require-project-name! name)
  (locking project-state-lock
    (let [project (project-dir forge name)
          entry (project-state forge name)]
      (when-not (fs/exists? project)
        (throw (ex-info (str "Unknown project: " name) {:http-status 404})))
      (when (#{"starting" "stopping"} (:state entry))
        (throw (ex-info (str "Project already " (:state entry) ": " name)
                        {:http-status 409 :error "project-busy"})))
      (set-project-state! forge name "stopping" "" entry)
      (try
        (when (and (fs/directory? project)
                   (not (skip-start?))
                   (or (:managed-runtime entry)
                       (not (runtime-stopped? project))))
          (let [result (stop-project-runtime! forge name)]
            (when-not (zero? (:exit result))
              (throw (ex-info (str/trim (str (:err result) "\n" (:out result)))
                              {:http-status 500 :error "stop-failed"})))
            (when-not (wait-for! #(runtime-stopped? project) (runtime-timeout-ms))
              (throw (ex-info "Project runtime did not stop"
                              {:http-status 500 :error "stop-failed"})))))
        (set-project-state! forge name "closed" "" {:managed-runtime (:managed-runtime entry)})
        {:name name :open false :state "closed"}
        (catch Exception e
          (set-project-state! forge name "error" (.getMessage e)
                              {:managed-runtime (:managed-runtime entry)})
          (throw e))))))

(defn reconcile-project-states! [forge]
  (locking project-state-lock
    (doseq [[name entry] (read-project-states forge)
            :when (:managed-runtime entry)
            :let [project (project-dir forge name)
                  ready? (and (fs/directory? project) (runtime-ready? project))
                  stopped? (or (not (fs/directory? project)) (runtime-stopped? project))]]
      (cond
        ready? (when-not (= "open" (:state entry))
                 (set-project-state! forge name "open" "" entry))
        stopped? (cond
                   (#{"starting" "stopping"} (:state entry))
                   (set-project-state! forge name "closed" "" entry)

                   (= "open" (:state entry))
                   (set-project-state! forge name "error" "Recorded open, but runtime is stopped" entry))
        :else (set-project-state! forge name "error" "Runtime is only partially running" entry))))
  (read-project-states forge))

(defn close-all-projects! [forge]
  (doseq [name (read-open-projects forge)]
    (close-project! forge name)))
