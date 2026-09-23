#!/usr/bin/env bb

(ns swarm-handoff
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]
           [java.security MessageDigest]))

(def usage-text
  (str "Usage:\n"
       "  swarm_handoff.sh <draft-file>\n"
       "  swarm_handoff.sh --help\n\n"
       "Write the draft under ./tmp/ in the assigned worktree.\n"
       "Do not use /tmp or the handoff outbox as scratch.\n\n"
       "Draft formats:\n\n"
       "type: git_handoff\n"
       "to: <role>[,<role>...]\n"
       "priority: NN\n"
       "task: <short-stable-task-name>\n\n"
       "The helper fills priority 50, commit, artifacts, and task_id from current work or the board card.\n"
       "Do not type a SHA or a hidden task_id. Extra headers (coverage, CRAP) are invalid.\n"
       "Extra lines after the headers are ignored.\n\n"
       "type: note\n"
       "to: <role>[,<role>...]\n"
       "priority: NN\n"
       "message: <one line, max 80 chars>"))

(def reserved-fields #{"id" "from" "role" "recipient" "created_at" "enqueued_at"
                       "dequeued_at" "completed_at" "task_base_commit" "non-forwarding"
                       "delivery_kind" "card_type" "batch_id" "batch_task_ids"})
(def allowed-fields #{"type" "to" "priority" "task_id" "task" "commit" "message"})
(def allowed-types #{"git_handoff" "note"})
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

(defn usage []
  (binding [*out* *err*]
    (println usage-text)))

(defn exit! [status message]
  (binding [*out* *err*]
    (when message
      (println message)))
  (System/exit status))

(defn command
  ([dir & args]
   (let [result (apply sh (concat args [:dir (str dir)]))]
     result)))

(defn lib-fail [e]
  (exit! (or (:exit (ex-data e)) 1) (ex-message e)))


(load-file (str (fs/path script-dir "swarm_handoff_sender.bb")))
(load-file (str (fs/path script-dir "swarm_handoff_current.bb")))
(load-file (str (fs/path script-dir "swarm_handoff_headers.bb")))

(defn next-sequence []
  (let [dir (state-dir)
        seq-file (fs/path dir "sequence")
        lock-dir (fs/path dir "sequence.lock")]
    (fs/create-dirs dir)
    (loop []
      (if (try
            (fs/create-dir lock-dir)
            true
            (catch java.nio.file.FileAlreadyExistsException _
              false))
        nil
        (do
          (Thread/sleep 50)
          (recur))))
    (try
      (let [last-value (if (fs/exists? seq-file)
                         (try
                           (Long/parseLong (str/trim (slurp (str seq-file))))
                           (catch Exception _ 0))
                         0)
            next-value (inc last-value)
            formatted (format "%06d" next-value)]
        (spit (str seq-file) (str formatted "\n"))
        formatted)
      (finally
        (fs/delete lock-dir)))))

(defn structure-instruction [handback?]
  (if handback?
    "The inbound tree is the structure. Replay this role's current task onto that shape."
    "This role's current tree is the structure. Replay the inbound work onto that shape."))

(defn body [type sender canonical-commit note-message handback?]
  (case type
    "git_handoff" (str "Re-read your role and constitution.\n\nmerge_and_process.sh " sender " " canonical-commit
                       "\n\n" (structure-instruction handback?))
    "note" (str "Re-read your role and constitution.\n\n" note-message)))

(defn write-handoff! [{:keys [headers recipients canonical-commit artifacts sender
                              priority non-forwarding delivery-kind reverse?]}]
  (let [timestamp-id (id-timestamp)
        created-at (timestamp)
        sequence (next-sequence)
        id (str timestamp-id "_" sequence "_from_" sender)
        recipient-slug (str/join "_" recipients)
        priority (or priority (get headers "priority"))
        type (get headers "type")
        delivery-kind (or delivery-kind (get headers "delivery_kind"))
        non-forwarding? (if (some? non-forwarding)
                          non-forwarding
                          (= "true" (get headers "non-forwarding")))
        filename (str priority "_" timestamp-id "_" sequence "_from_" sender "_to_" recipient-slug ".handoff")
        outbox-dir (fs/path (state-dir) "outbox")
        tmp-dir (fs/path outbox-dir "tmp")
        tmp-file (fs/path tmp-dir (str filename ".tmp"))
        outbox-file (fs/path outbox-dir filename)
        handoff-body (body type sender canonical-commit (get headers "message")
                           (or reverse? non-forwarding?))
        lines (cond-> [(str "id: " id)
                       (str "from: " sender)
                       (str "to: " (str/join "," recipients))
                       (str "priority: " priority)
                       (str "type: " type)]
                (= "git_handoff" type)
                (conj (str "role: " sender)
                      (str "task_id: " (get headers "task_id"))
                      (str "task: " (get headers "task"))
                      (str "commit: " canonical-commit)
                      (str "artifacts: " artifacts))
                (and (= "git_handoff" type) (not (str/blank? (get headers "batch_task_ids"))))
                (conj (str "batch_task_ids: " (get headers "batch_task_ids")))
                (and (= "git_handoff" type) (not (str/blank? (get headers "batch_id"))))
                (conj (str "batch_id: " (get headers "batch_id")))
                (and (= "git_handoff" type) (not (str/blank? (get headers "card_type"))))
                (conj (str "card_type: " (get headers "card_type")))
                (and (= "git_handoff" type) (not (str/blank? delivery-kind)))
                (conj (str "delivery_kind: " delivery-kind))
                (and (= "git_handoff" type) (not (str/blank? (current-task-base))))
                (conj (str "task_base_commit: " (current-task-base)))
                non-forwarding?
                (conj "non-forwarding: true")
                (= "note" type)
                (conj (str "message: " (get headers "message")))
                true
                (conj (str "created_at: " created-at)
                      ""
                      handoff-body))]
    (doseq [dir [tmp-dir outbox-dir (fs/path (state-dir) "sent") (fs/path (state-dir) "failed")]]
      (fs/create-dirs dir))
    (spit (str tmp-file) (str (str/join "\n" lines) "\n"))
    (fs/move tmp-file outbox-file)
    outbox-file))

(defn write-handoffs! [ctx]
  (let [git? (= "git_handoff" (get-in ctx [:headers "type"]))
        terminal? (= "terminal" (get-in ctx [:headers "delivery_kind"]))
        forward (write-handoff! (assoc ctx :reverse? false))
        reverse (when (and git? (not terminal?))
                  (mapv (fn [role]
                          (write-handoff! (assoc ctx
                                                 :recipients [role]
                                                 :priority "00"
                                                 :non-forwarding true
                                                 :delivery-kind "reverse"
                                                 :reverse? true)))
                        (reverse-roles (:sender ctx) (get-in ctx [:headers "task"]))))]
    (into [forward] reverse)))

(defn error-report [draft errors]
  (binding [*out* *err*]
    (println "HANDOFF INVALID:" (str draft))
    (println)
    (println "Errors:")
    (doseq [error errors]
      (println "-" error))
    (println)
    (println usage-text)))

(defn help-arg? [args]
  (boolean (some #{"--help" "-h"} args)))

(defn -main [& args]
  (when (help-arg? args)
    (usage)
    (System/exit 0))
  (when (not= 1 (count args))
    (usage)
    (System/exit 1))
  (let [draft (fs/path (first args))]
    (when-not (fs/regular-file? draft)
      (exit! 1 (str "Draft file not found: " draft)))
    (let [sender (sender-role)]
      (when-not (role-known? sender)
        (exit! 1 (str "Unknown sender role: " sender)))
      (require-worktree-tmp-draft! draft)
      (let [{:keys [headers ordered errors]} (parse-draft draft)
            headers (prepare-headers headers sender)
            ordered (-> ordered
                        (ensure-field "priority")
                        (cond-> (= "git_handoff" (get headers "type"))
                          (ensure-field "commit")))
            sha (get headers "commit")]
        (invalidate-changed-invocation-audits!
         sender (invocation-fingerprint draft sender headers))
        (when (and (= "git_handoff" (get headers "type"))
                   (inbound-non-forwarding?))
          (exit! 1 "Current inbound handoff is non-forwarding; do not send a git_handoff."))
        (when (and (= "git_handoff" (get headers "type"))
                   (not (commit-on-sender-branch? sha)))
          (exit! 1 (str "Result commit " sha " is not reachable from sender worktree")))
        (let [validation (validate headers ordered)
              all-errors (vec (concat errors
                                      (:errors validation)
                                      (current-work-state-errors headers)
                                      (task-state-errors headers sender)
                                      (ancestry-errors headers (:canonical-commit validation))
                                      (current-batch-ancestry-errors (:canonical-commit validation))
                                      (task-document-errors headers (:canonical-commit validation))
                                      (duplicate-errors sender
                                                        (:recipients validation)
                                                        headers
                                                        (:canonical-commit validation))))]
          (when (seq all-errors)
            (error-report draft all-errors)
            (System/exit 2))
          (let [files (when (= "git_handoff" (get headers "type"))
                        (commit-artifacts sha))
                added (when (= "git_handoff" (get headers "type"))
                        (commit-added sha))
                path-errors (banned-path-errors (get headers "card_type") files added)]
            (when (seq path-errors)
              (error-report draft path-errors)
              (System/exit 2))
            (when (and (= "git_handoff" (get headers "type")) (empty? files))
              (exit! 1 (str "Result commit " sha " has no changed files")))
            (let [submit! #(write-handoffs! {:headers headers
                                             :recipients (:recipients validation)
                                             :canonical-commit (:canonical-commit validation)
                                             :artifacts (when files (str/join "," files))
                                             :sender sender})
                  outbox-files (if (= "git_handoff" (get headers "type"))
                                 (submit-after-audit!
                                  (audit-candidate draft sender headers
                                                   (:recipients validation)
                                                   (:canonical-commit validation)
                                                   files)
                                  submit!)
                                 (submit!))]
              (when outbox-files
                (fs/delete draft)
                (doseq [outbox-file outbox-files]
                  (println "HANDOFF QUEUED:" (str outbox-file)))
                (complete-current-after-git-handoff! headers)))))))))

(when (= (str *file*) (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
