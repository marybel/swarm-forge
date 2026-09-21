#!/usr/bin/env bb

(require '[babashka.fs :as fs])

(def repo-root (-> *file* fs/file fs/parent fs/parent fs/parent))
(try
  (require 'pack-web)
  (catch Exception _
    (load-file (str (fs/path repo-root "swarmforge" "scripts" "pack_web.bb")))))
(in-ns 'pack-web)

(defn test-state! [root]
  (println (:body (handle-request (require-root! root) {:method "GET" :uri "/api/state"}))))

(defn test-html! []
  (print (:body (handle-request nil {:method "GET" :uri "/"})))
  (flush))

(defn test-post-task! [root name text & [project type]]
  (let [resp (handle-request (require-root! root)
                             {:method "POST"
                              :uri "/api/tasks"
                              :body (json/generate-string
                                     (cond-> {:name name :text (or text "")}
                                       (not (str/blank? project)) (assoc :project project)
                                       (not (str/blank? type)) (assoc :type type)))})]
    (print (:body resp))
    (flush)
    (when-not (= 200 (:status resp))
      (binding [*out* *err*]
        (println (:body resp)))
      (System/exit 1))))

(defn test-delete-task! [root name]
  (let [resp (handle-request (require-root! root)
                             {:method "POST"
                              :uri "/api/tasks/delete"
                              :body (json/generate-string {:name name})})]
    (print (:body resp))
    (flush)
    (when-not (= 200 (:status resp))
      (binding [*out* *err*]
        (println (:body resp)))
      (System/exit 1))))

(defn test-delete-approval! [root id]
  (let [resp (handle-request (require-root! root)
                             {:method "POST"
                              :uri "/api/tasks/delete"
                              :body (json/generate-string {:id id})})]
    (print (:body resp))
    (flush)
    (when-not (= 200 (:status resp))
      (binding [*out* *err*]
        (println (:body resp)))
      (System/exit 1))))

(defn test-retry-task! [root id comments]
  (let [resp (handle-request (require-root! root)
                             {:method "POST"
                              :uri "/api/tasks/retry"
                              :body (json/generate-string {:id id :comments (or comments "")})})]
    (print (:body resp))
    (flush)
    (when-not (= 200 (:status resp))
      (binding [*out* *err*]
        (println (:body resp)))
      (System/exit 1))))

(defn test-post-chat! [root text]
  (handle-request (require-root! root)
                  {:method "POST"
                   :uri "/api/chat"
                   :body (json/generate-string {:text (or text "")})}))

(defn test-inject-payload! [name text]
  (println (if (and name text)
             (task-payload name text)
             (task-payload))))

(defn test-inject-argv! [root file text]
  (when (str/blank? file)
    (exit! 1 "Missing argv file"))
  (binding [*tmux-stub* file]
    (inject-master! (require-root! root) text)))

(defn test-http! [resp]
  (print (:body resp))
  (flush)
  (when-not (= 200 (:status resp))
    (binding [*out* *err*]
      (println (:body resp)))
    (System/exit 1)))

(defn test-approval! [root id action]
  (when (str/blank? id)
    (exit! 1 "Missing approval id"))
  (test-http! (handle-request (require-root! root)
                              {:method "POST"
                               :uri (str "/api/approvals/" id "/" action)})))

(defn test-allow! [root name act & [project]]
  (when (or (str/blank? name) (str/blank? act))
    (exit! 1 "Missing name or act"))
  (test-http! (handle-request (require-root! root)
                              {:method "POST"
                               :uri "/api/board/allow"
                               :body (json/generate-string
                                      (cond-> {:name name :act act}
                                        (not (str/blank? project))
                                        (assoc :project project)))})))

(defn test-save-comments! [root id path comments]
  (when (str/blank? id)
    (exit! 1 "Missing approval id"))
  (test-http! (handle-request (require-root! root)
                              {:method "POST"
                               :uri (str "/api/approvals/" id "/comments")
                               :body (json/generate-string {:path path :comments (or comments "")})})))

(defn test-pane! [root role & [project]]
  (when (str/blank? role)
    (exit! 1 "Missing role"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/api/agents/" role "/pane"
                                          (or (project-query project) ""))})))
  (flush))

(defn test-pane-merge! [history-file visible-file]
  (when (or (str/blank? history-file) (str/blank? visible-file))
    (exit! 1 "Missing history or visible file"))
  (print (merge-pane-capture (slurp history-file) (slurp visible-file)))
  (flush))

(defn test-agent-page! [role & [project]]
  (print (pane-page (or role "specifier") "" project))
  (flush))

(defn print-heat-pair! [root before-text after-text]
  (require-root! root)
  (reset! pane-heat {})
  (binding [*pane-text* before-text]
    (let [before (:activity (first (work-in-flight root)))]
      (binding [*pane-text* after-text]
        (let [after (:activity (first (work-in-flight root)))]
          (println (json/generate-string {:before before :after after})))))))

(defn test-heat! [root]
  (print-heat-pair! root "alpha\nline two\n" "beta\nline two\nchanged output\n"))

(defn test-card-heat! [root]
  (require-root! root)
  (reset! pane-heat {})
  (let [before-text "alpha\nline two\n"
        after-text "beta\nline two\nchanged output\n"]
    (binding [*pane-text* before-text]
      (let [h1 (:role_heats (dashboard-state root))]
        (binding [*pane-text* after-text]
          (let [h2 (:role_heats (dashboard-state root))
                role (or (ffirst h2) "specifier")]
            (println (json/generate-string
                      {:before (get h1 role 0)
                       :after (get h2 role 0)
                       :other {:Grenade false}}))))))))

(defn test-lieutenant-heat! [root]
  (require-root! root)
  (reset! pane-heat {})
  (let [before-text "alpha\nline two\n"
        after-text "beta\nline two\nchanged output\n"]
    (binding [*pane-text* before-text]
      (let [before (or (:lieutenant_activity (api-state root)) 0)]
        (binding [*pane-text* after-text]
          (let [after (or (:lieutenant_activity (api-state root)) 0)]
            (println (json/generate-string {:before before :after after}))))))))

(defn print-heat-isolation! [root-a root-b]
  (reset! pane-heat {})
  (binding [*pane-text* "stable-a\n"]
    (work-in-flight root-a))
  (binding [*pane-text* "stable-b\n"]
    (work-in-flight root-b))
  (let [changed (binding [*pane-text* "changed-a\nmore\n"]
                  (:activity (first (work-in-flight root-a))))
        stable (binding [*pane-text* "stable-b\n"]
                 (:activity (first (work-in-flight root-b))))]
    (println (json/generate-string {:changed changed :stable stable}))))

(defn test-heat-isolation! [root-a root-b]
  (require-root! root-a)
  (require-root! root-b)
  (print-heat-isolation! root-a root-b))

(defn test-heat-codex! [root]
  (print-heat-pair! root
                    "I'll load the SwarmForge instructions.\n\nesc to interrupt · 3s\n"
                    "I'll load the SwarmForge instructions.\n\nesc to interrupt · 4s\n"))

(defn test-heat-reorder! [root]
  (let [lines (mapv #(str "line-" %) (range 20))]
    (print-heat-pair! root
                      (str (str/join "\n" lines) "\n")
                      (str (str/join "\n" (reverse lines)) "\n"))))

(defn test-heat-head! [root]
  (let [tail (mapv #(str "tail-" %) (range 20))
        before (str (str/join "\n" (concat ["a" "b" "c" "d" "e"] tail)) "\n")
        after (str (str/join "\n" (concat ["v" "w" "x" "y" "z"] tail)) "\n")]
    (print-heat-pair! root before after)))

(defn test-heat-mail! [root]
  (print-heat-pair! root
                    (str "stable\n"
                         "› You have new handoff mail. If idle, run ready_for_next.sh.\n"
                         "work line A\n"
                         "• Working (1s • esc to interrupt)\n"
                         "›\n")
                    (str "stable\n"
                         "› You have new handoff mail. If idle, run ready_for_next.sh.\n"
                         "work line B\n"
                         "• Working (2s • esc to interrupt)\n"
                         "›\n")))

(defn test-heat-grok! [root]
  (print-heat-pair! root
                    (str "I'll write the cave stories.\n"
                         "You have new handoff mail. If idle, run ready_for_next.sh.\n"
                         "always-approve  shift+tab\n"
                         "Waiting for response 1s\n"
                         "enter:send  Esc:cancel\n")
                    (str "I'll write the cave stories.\n"
                         "You have new handoff mail. If idle, run ready_for_next.sh.\n"
                         "always-approve  shift+tab\n"
                         "Waiting for response 2s\n"
                         "enter:send  Esc:cancel\n")))

(defn test-heat-collapse! [root]
  (print-heat-pair! root
                    (str "… +28 lines (ctrl + t to view transcript)\n"
                         "• Working (1s • esc to interrupt)\n")
                    (str "… +29 lines (ctrl + t to view transcript)\n"
                         "• Working (2s • esc to interrupt)\n")))

(defn test-status-pane! [root text]
  (require-root! root)
  (binding [*pane-text* (or text "")]
    (println (:body (handle-request root {:method "GET" :uri "/api/state"})))))

(defn test-status-persist! [root first-text second-text]
  (require-root! root)
  (reset! pane-status {})
  (reset! pane-status-lines {})
  (binding [*pane-text* (or first-text "")]
    (let [first-status (:status (first (:tasks (json/parse-string
                                                (:body (handle-request root {:method "GET" :uri "/api/state"}))
                                                true))))]
      (binding [*pane-text* (or second-text "")]
        (let [second-status (:status (first (:tasks (json/parse-string
                                                     (:body (handle-request root {:method "GET" :uri "/api/state"}))
                                                     true))))]
          (println (json/generate-string {:first first-status :second second-status})))))))

(defn test-answer-clarification! [root id text]
  (when (str/blank? id)
    (exit! 1 "Missing clarification id"))
  (test-http! (handle-request (require-root! root)
                              {:method "POST"
                               :uri (str "/api/clarifications/" id "/answer")
                               :body (json/generate-string {:text (or text "")})})))

(defn test-task! [root name]
  (when (str/blank? name)
    (exit! 1 "Missing task name"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/task?name=" name)})))
  (flush))

(defn test-tree! [root name path]
  (when (str/blank? name)
    (exit! 1 "Missing task name"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/api/tree?name=" name
                                           (when-not (str/blank? path)
                                             (str "&path=" path)))})))
  (flush))

(defn test-file! [root name path]
  (when (or (str/blank? name) (str/blank? path))
    (exit! 1 "Missing task name or path"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/api/file?name=" name "&path=" path)})))
  (flush))

(defn test-mission! [root & [project]]
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/api/mission"
                                           (when-not (str/blank? project)
                                             (str "?project=" project)))})))
  (flush))

(defn test-doc! [root path]
  (when (str/blank? path)
    (exit! 1 "Missing path"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/doc?path=" path)})))
  (flush))

(defn test-api-doc! [root path id]
  (when (str/blank? path)
    (exit! 1 "Missing path"))
  (print (:body (handle-request (require-root! root)
                                {:method "GET"
                                 :uri (str "/api/doc?path=" path
                                           (when-not (str/blank? id)
                                             (str "&id=" id)))})))
  (flush))

(defn test-project-http! [resp]
  (print (:body resp))
  (flush)
  (when-not (= 200 (:status resp))
    (binding [*out* *err*]
      (println (:body resp)))
    (System/exit 1)))

(defn test-new-project!
  ([root name pack mission] (test-new-project! root name pack mission false false))
  ([root name pack mission github replace]
   (test-new-project! root name pack mission github replace nil nil))
  ([root name pack mission github replace branch conf]
   (test-project-http!
    (handle-request (require-root! root)
                    {:method "POST"
                     :uri "/api/projects"
                     :body (json/generate-string
                            (cond-> {:name name
                                     :pack pack
                                     :mission (or mission "")
                                     :github github
                                     :replace replace}
                              branch (assoc :branch branch)
                              conf (assoc :conf conf)))}))))

(defn test-open-project! [root name]
  (test-project-http!
   (handle-request (require-root! root)
                   {:method "POST"
                    :uri "/api/projects/open"
                    :body (json/generate-string {:name name})})))

(defn test-close-project! [root name]
  (test-project-http!
   (handle-request (require-root! root)
                   {:method "POST"
                    :uri "/api/projects/close"
                    :body (json/generate-string {:name name})})))

(defn test-inferred-name! [input github]
  (println (forge/inferred-name input (= "github" github))))

(defn test-teardown! [root confirm]
  (binding [*sync-teardown?* true]
    (let [resp (handle-request (require-root! root)
                               {:method "POST"
                                :uri "/api/teardown"
                                :body (when confirm
                                        (json/generate-string {:confirm confirm}))})]
      (when-not (= 200 (:status resp))
        (exit! 2 (:body resp)))
      (print (:body resp))
      (flush))))


(defn test-teardown-throw! [root]
  (binding [*sync-teardown?* true]
    (with-redefs [run-teardown! (fn [_] (throw (ex-info "boom" {})))]
      (schedule-teardown! (require-root! root)))))

(defn test-cli! [& args]
  (case (first args)
    "--test-state" (test-state! (second args))
    "--test-html" (test-html!)
    "--test-post-task" (test-post-task! (second args) (nth args 2 nil) (nth args 3 nil)
                                        (nth args 4 nil) (nth args 5 nil))
    "--test-post-chat" (test-post-chat! (second args) (nth args 2 nil))
    "--test-inject-payload" (test-inject-payload! (second args) (nth args 2 nil))
    "--test-inject-argv" (test-inject-argv! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-approve" (test-approval! (second args) (nth args 2 nil) "approve")
    "--test-reject" (test-approval! (second args) (nth args 2 nil) "reject")
    "--test-pane" (test-pane! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-agent-page" (test-agent-page! (second args) (nth args 2 nil))
    "--test-heat" (test-heat! (second args))
    "--test-card-heat" (test-card-heat! (second args))
    "--test-heat-isolation" (test-heat-isolation! (second args) (nth args 2 nil))
    "--test-heat-codex" (test-heat-codex! (second args))
    "--test-heat-reorder" (test-heat-reorder! (second args))
    "--test-heat-head" (test-heat-head! (second args))
    "--test-heat-mail" (test-heat-mail! (second args))
    "--test-heat-grok" (test-heat-grok! (second args))
    "--test-heat-collapse" (test-heat-collapse! (second args))
    "--test-status-pane" (test-status-pane! (second args) (nth args 2 nil))
    "--test-status-persist" (test-status-persist! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-answer-clarification" (test-answer-clarification! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-task" (test-task! (second args) (nth args 2 nil))
    "--test-tree" (test-tree! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-file" (test-file! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-doc" (test-doc! (second args) (nth args 2 nil))
    "--test-api-doc" (test-api-doc! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-delete-task" (test-delete-task! (second args) (nth args 2 nil))
    "--test-delete-approval" (test-delete-approval! (second args) (nth args 2 nil))
    "--test-retry-task" (test-retry-task! (second args) (nth args 2 nil) (nth args 3 nil))
    "--test-save-comments" (test-save-comments! (second args) (nth args 2 nil) (nth args 3 nil) (nth args 4 nil))
    "--test-teardown" (test-teardown! (second args) (nth args 2 nil))
    "--test-teardown-throw" (test-teardown-throw! (second args))
    "--test-new-project" (test-new-project! (second args) (nth args 2 nil) (nth args 3 nil) (nth args 4 nil))
    "--test-new-project-replace" (test-new-project! (second args) (nth args 2 nil) (nth args 3 nil)
                                                      (nth args 4 nil) false true)
    "--test-new-github-project" (test-new-project! (second args) (nth args 2 nil) (nth args 3 nil)
                                                     (nth args 4 nil) true false (nth args 5 nil) (nth args 6 nil))
    "--test-open-project" (test-open-project! (second args) (nth args 2 nil))
    "--test-close-project" (test-close-project! (second args) (nth args 2 nil))
    "--test-inferred-name" (test-inferred-name! (second args) (nth args 2 nil))
    "--test-mission" (test-mission! (second args) (nth args 2 nil))
    "--test-allow" (test-allow! (second args) (nth args 2 nil) (nth args 3 nil) (nth args 4 nil))
    "--test-lieutenant-heat" (test-lieutenant-heat! (second args))
    "--test-pane-merge" (test-pane-merge! (second args) (nth args 2 nil))
    (do (usage)
        (exit! 1 nil))))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply test-cli! *command-line-args*)
  (System/exit 0))
