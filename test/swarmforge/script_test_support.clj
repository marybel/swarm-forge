(ns swarmforge.script-test-support
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def repo-root (fs/cwd))
(def scripts-dir (fs/path repo-root "swarmforge" "scripts"))

(defn write-file [path text]
  (fs/create-dirs (fs/parent path))
  (spit (str path) text))
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
(defn init-repo! [root]
  (run {:dir root} "git" "init" "-q")
  (run {:dir root} "git" "config" "user.email" "test@example.com")
  (run {:dir root} "git" "config" "user.name" "Test User")
  (write-file (fs/path root "README.md") "initial\n")
  (run {:dir root} "git" "add" "README.md")
  (run {:dir root} "git" "commit" "-q" "-m" "Initial commit"))
(defn tmp-dir []
  (fs/create-temp-dir {:prefix "swarmforge-script-test."}))
(defn tmp-tmux-socket []
  (str "/tmp/swarmforge-test-" (System/nanoTime) ".sock"))
(defn script [name]
  (str (fs/path scripts-dir name)))
(defn write-pack-conf! [root conf]
  (write-file (fs/path root "swarmforge/constitution.prompt") "Read articles.\n")
  (write-file (fs/path root "swarmforge/swarmforge.conf") conf)
  (write-file (fs/path root "swarmforge/roles/specifier.prompt") "specifier\n")
  (write-file (fs/path root "swarmforge/roles/coder.prompt") "coder\n"))
(defn write-definition-pack! [root]
  (let [definition (fs/path root ".swarmforge/definition/swarmforge")]
    (write-file (fs/path definition "constitution.prompt") "definition constitution\n")
    (write-file (fs/path definition "constitution/current.prompt") "definition article\n")
    (write-file (fs/path definition "swarmforge.conf")
                (str "window master-role codex master\n"
                     "window coder codex coder\n"))
    (write-file (fs/path definition "roles/master-role.prompt") "master\n")
    (write-file (fs/path definition "roles/coder.prompt") "coder\n")))
(defn write-tracked-swarmforge! [worktree]
  (doseq [[name text] {"scripts/tracked.bb" "tracked script\n"
                       "roles/coder.prompt" "tracked role\n"
                       "constitution.prompt" "tracked constitution\n"
                       "constitution/article.prompt" "tracked article\n"
                       "swarmforge.conf" "tracked conf\n"}]
    (write-file (fs/path worktree "swarmforge" name) text)))
(defn file-contents [dir]
  (into (sorted-map)
        (for [file (fs/glob dir "**")
              :when (fs/regular-file? file)]
          [(str (fs/relativize dir file)) (slurp (str file))])))
(defn commit-body [root]
  (:out (run {:dir root} "git" "log" "-1" "--format=%B")))
(defn close-swarm []
  (str (fs/path repo-root "close-swarm")))
(defn write-echo-tool! [root tool]
  (write-file (fs/path root ".swarmforge/roles.tsv")
              (format "specifier\tmaster\t%s\tsession\tSpecifier\tcodex\ttask\n" root))
  (write-file (fs/path root ".swarmforge/tools" tool "bb.edn")
              (str "{:tasks {" tool " (apply println *command-line-args*)}}\n")))
(defn seed-installer-host! [base]
  (doseq [name ["swarmforge.sh" "handoffd.bb" "done_with_current.sh"]]
    (write-file (fs/path base "swarmforge/scripts" name) (str name "\n")))
  (write-file (fs/path base "swarm") "#!/bin/sh\necho swarm\n")
  (write-file (fs/path base "swarmforge/constitution.prompt") "MAIN-CONSTITUTION\n")
  (write-file (fs/path base "swarmforge/roles/lieutenant.prompt") "LIEUTENANT\n")
  (write-file (fs/path base "swarmforge/swarmforge.conf") "# Lieutenant grok\n")
  (write-file (fs/path base "swarmforge/constitution/articles/engineering.prompt") "MAIN-ENGINEERING\n")
  (write-file (fs/path base "swarmforge/constitution/articles/workflow.prompt") "MAIN-WORKFLOW\n")
  (write-file (fs/path base "swarmforge/constitution/articles/handoffs.prompt") "MAIN-HANDOFFS\n"))
(defn seed-installer-pack! [pack]
  (write-file (fs/path pack "swarm") "#!/bin/sh\necho pack-swarm\n")
  (write-file (fs/path pack "README.md") "pack-readme\n")
  (write-file (fs/path pack "bb.edn") "pack-bb\n")
  (write-file (fs/path pack "swarmforge/swarmforge.conf")
              "window specifier grok master\n")
  (write-file (fs/path pack "swarmforge/constitution.prompt") "PACK-CONSTITUTION\n")
  (write-file (fs/path pack "swarmforge/roles/specifier.prompt") "specifier\n")
  (write-file (fs/path pack "swarmforge/constitution/articles/engineering.prompt") "PACK-STALE-ENGINEERING\n")
  (write-file (fs/path pack "swarmforge/constitution/articles/project.prompt") "PACK-PROJECT\n")
  (write-file (fs/path pack "swarmforge/constitution/articles/local-workflow.prompt") "PACK-LOCAL-WORKFLOW\n"))
(defn assert-installer-host [host]
  (is (= "host-readme\n" (slurp (str (fs/path host "README.md")))))
  (is (= "{:paths [\"test\"]}\n" (slurp (str (fs/path host "bb.edn")))))
  (is (= "keep\n" (slurp (str (fs/path host "test/keep.clj")))))
  (is (= "MAIN-ENGINEERING\n" (slurp (str (fs/path host "swarmforge/constitution/articles/engineering.prompt")))))
  (is (= "MAIN-WORKFLOW\n" (slurp (str (fs/path host "swarmforge/constitution/articles/workflow.prompt")))))
  (is (= "MAIN-HANDOFFS\n" (slurp (str (fs/path host "swarmforge/constitution/articles/handoffs.prompt")))))
  (is (= "MAIN-CONSTITUTION\n" (slurp (str (fs/path host "swarmforge/constitution.prompt")))))
  (is (fs/exists? (fs/path host "swarmforge/roles/lieutenant.prompt")))
  (is (fs/exists? (fs/path host "swarmforge/swarmforge.conf")))
  (is (not (fs/exists? (fs/path host "swarmforge/roles/specifier.prompt"))))
  (is (not (fs/exists? (fs/path host "swarmforge/constitution/articles/project.prompt"))))
  (is (fs/directory? (fs/path host "projects")))
  (is (fs/exists? (fs/path host "swarm"))))
