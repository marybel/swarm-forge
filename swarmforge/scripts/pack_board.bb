#!/usr/bin/env bb

(ns pack-board
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str])
  (:import [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]))

(def usage-text
  (str "Usage:\n"
       "  pack_board.sh create --name <name> --type <configured-type> [--waiting] [--merge-from <role>] [--root <dir>] [--text <text>] [--caller lieutenant]\n"
       "  pack_board.sh create <name> --type <configured-type> [--waiting]\n"
       "  pack_board.sh move --name <name> --lane <lane> [--merge-from <role>] [--root <dir>]\n"
       "  pack_board.sh move <name> <lane>\n"
       "  pack_board.sh done --name <name> [--root <dir>]\n"
       "  pack_board.sh done <name>\n"
       "  pack_board.sh transition-batch --task-ids <edn-vector> --lane <lane> --caller handoffd [--root <dir>]\n"
       "  pack_board.sh list [--root <dir>]\n"
       "  pack_board.sh lanes [--root <dir>]\n"
       "  pack_board.sh master-lane [--root <dir>]\n"
       "  pack_board.sh archive --archive <window> [--root <dir>]\n"
       "  pack_board.sh archive --role <window> [--root <dir>]\n"
       "  pack_board.sh archive <window>\n"
       "  pack_board.sh archive-all [--root <dir>]\n"
       "  pack_board.sh increment-audit --task-id <task-id> --caller <handoffd|lieutenant> [--root <dir>]\n"
       "  pack_board.sh request-allow --name <name> --act <move|done|increment-audit|stop|create> [--root <dir>]\n"
       "  pack_board.sh allow --name <name> --act <move|done|increment-audit|stop|create> [--root <dir>]\n"
       "  pack_board.sh delete --name <name> [--root <dir>]\n"
       "  pack_board.sh delete <name>\n"
       "  pack_board.sh stop --name <name> [--root <dir>]"))

(def flags {"--root" :root "--name" :name "--lane" :lane "--text" :text
            "--role" :role "--task-id" :task-id "--type" :type
            "--task-ids" :task-ids
            "--caller" :caller "--archive" :archive "--act" :act
            "--merge-from" :merge-from "--waiting" :waiting})
(def bool-flags #{"--waiting"})
;; *file* has no parent segment when this ns is `require`d from the
;; classpath (as the in-process coverage test does) instead of invoked
;; directly; fall back to the classpath resource location.
(def script-dir
  (or (fs/parent *file*)
      (some-> (io/resource *file*) io/file .getParent fs/path)))
(try
  (require 'handoff-lib)
  (catch Exception _
    (load-file (str (fs/path script-dir "handoff_lib.bb")))))
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

(declare role-rows halt-live-card!)

(defn usage []
  (binding [*out* *err*]
    (println usage-text)))

(defn exit! [status message]
  (binding [*out* *err*]
    (when message
      (println message)))
  (System/exit status))

(defn command [dir & args]
  (apply sh (concat args [:dir (str dir)])))

(defn git-root []
  (handoff-lib/git-toplevel))

(defn git-common-dir []
  (handoff-lib/git-common-dir))

(defn roles-at? [root]
  (handoff-lib/roles-at? root))

(defn project-root []
  (try
    (handoff-lib/project-root)
    (catch clojure.lang.ExceptionInfo e
      (exit! (or (:exit (ex-data e)) 1) (ex-message e)))))

(defn parse-args [args]
  (loop [args args opts {} positionals []]
    (if (empty? args)
      (assoc opts :positional positionals)
      (let [head (first args)
            flag (get flags head)]
        (cond
          (nil? flag)
          (recur (rest args) opts (conj positionals head))

          (contains? bool-flags head)
          (recur (rest args) (assoc opts flag true) positionals)

          (nil? (second args))
          (exit! 1 (str "Missing value for " head))

          :else
          (recur (drop 2 args) (assoc opts flag (second args)) positionals))))))

(defn resolve-root [opts]
  (or (:root opts) (project-root)))


(load-file (str (fs/path script-dir "pack_board_store.bb")))
(load-file (str (fs/path script-dir "pack_board_allow.bb")))

(defn list! [opts]
  (let [file (tasks-file (resolve-root opts))]
    (when (fs/exists? file)
      (print (slurp (str file)))
      (flush))))

(defn roles-file [root]
  (fs/path root ".swarmforge" "roles.tsv"))

(defn role-rows [root]
  (map #(str/split % #"\t" -1) (read-rows (roles-file root))))

(defn lanes! [opts]
  (doseq [cols (role-rows (resolve-root opts))]
    (println (first cols))))

(defn master-lane! [opts]
  (let [masters (filterv #(= "master" (second %)) (role-rows (resolve-root opts)))]
    (when-not (= 1 (count masters))
      (exit! 1 "Config must name exactly one master worktree"))
    (println (ffirst masters))))


(load-file (str (fs/path script-dir "pack_board_tmux.bb")))
(load-file (str (fs/path script-dir "pack_board_halt.bb")))

(def commands
  {"create" create!
   "move" move!
   "done" done!
   "transition-batch" transition-batch!
   "stop" stop!
   "list" list!
   "lanes" lanes!
   "master-lane" master-lane!
   "archive" archive!
   "archive-all" archive-all!
   "increment-audit" increment-audit!
   "request-allow" request-allow!
   "allow" allow!
   "delete" delete!})

(defn -main [& args]
  (let [opts (parse-args args)
        command (get commands (first (:positional opts)))]
    (if command
      (command opts)
      (do (usage)
          (exit! 1 nil))))
  (System/exit 0))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
