(ns swarmforge.swarm-handoff-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [swarmforge.handoff-test-support :refer :all]))

(use-fixtures :once once-fixture)

(deftest swarm-handoff-help-is-usage-not-a-draft
  ;; Given the handoff helper
  ;; When it is run with --help or -h
  ;; Then it prints usage and does not treat the flag as a missing draft file
  (doseq [flag ["--help" "-h"]]
    (let [result (run {:dir repo-root :ok? false}
                      (script "swarm_handoff.sh") flag)
          text (str (:err result) (:out result))]
      (is (zero? (:exit result)) flag)
      (is (str/includes? text "Usage:") flag)
      (is (not (str/includes? text "Draft file not found")) flag))))
(deftest swarm-handoff-queues-on-the-project-from-a-worktree
  ;; Given a sender worktree and a commit only made there
  ;; When swarm_handoff runs in that worktree
  ;; Then the queued file is on the project, and the commit is the worktree HEAD
  (let [root (tmp-dir)
        _ (init-repo! root)
        wt (add-worktree! root "sender")
        _ (setup-project! root {"sender" "task" "receiver" "task"})
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              wt root))
        _ (write-file (fs/path wt "slice.md") "from the worktree\n")
        _ (run {:dir wt} "git" "add" "slice.md")
        _ (run {:dir wt} "git" "commit" "-q" "-m" "Worktree slice")
        wt-head (str/trim (:out (run {:dir wt} "git" "rev-parse" "--short=10" "HEAD")))
        master-head (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD")))
        draft (fs/path wt "tmp" "from-wt.handoff")]
    (is (not= wt-head master-head))
    (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: task-from-worktree\ncommit: %s\n" wt-head))
    (let [result (audit-and-submit-git-handoff
                  {:dir wt :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))
          content (read-file queued)
          outbox (str (fs/canonicalize (fs/path root ".swarmforge" "handoffs" "outbox")))]
      (is (zero? (:exit result)))
      (is (str/starts-with? (str (fs/canonicalize queued)) outbox))
      (is (not (str/includes? queued "/.worktrees/")))
      (is (str/includes? content (str "commit: " wt-head "\n")))
      (is (not (str/includes? content (str "commit: " master-head "\n")))))))
(deftest swarm-handoff-infers-role-and-fills-worktree-head
  ;; Given a sender worktree and no SWARMFORGE_ROLE
  ;; When swarm_handoff runs there with a draft that names master's SHA or omits commit
  ;; Then it infers the role and queues the worktree HEAD
  (let [root (tmp-dir)
        _ (init-repo! root)
        wt (add-worktree! root "sender")
        _ (setup-project! root {"sender" "task" "receiver" "task"})
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              wt root))
        _ (write-file (fs/path wt "slice.md") "from the worktree\n")
        _ (run {:dir wt} "git" "add" "slice.md")
        _ (run {:dir wt} "git" "commit" "-q" "-m" "Worktree slice")
        wt-head (str/trim (:out (run {:dir wt} "git" "rev-parse" "--short=10" "HEAD")))
        master-head (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD")))]
    (is (not= wt-head master-head))
    (testing "infers role from worktree when env is missing"
      (let [draft (fs/path wt "tmp" "no-env.handoff")]
        (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: inferred-role\ncommit: %s\n" wt-head))
        (let [result (audit-and-submit-git-handoff {:dir wt :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) "from: sender\n"))
          (is (str/includes? (str content) (str "commit: " wt-head "\n"))))))
    (testing "fills worktree HEAD even when the draft names master's SHA"
      (let [draft (fs/path wt "tmp" "wrong-sha.handoff")]
        (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: ignore-typed-sha\ncommit: %s\n" master-head))
        (let [result (audit-and-submit-git-handoff
                      {:dir wt :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) (str "commit: " wt-head "\n")))
          (is (not (str/includes? (str content) (str "commit: " master-head "\n")))))))
    (testing "fills HEAD when the draft omits commit"
      (let [draft (fs/path wt "tmp" "no-commit.handoff")]
        (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: omit-commit\n")
        (let [result (audit-and-submit-git-handoff
                      {:dir wt :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) (str "commit: " wt-head "\n"))))))))
(deftest swarm-handoff-rejects-drafts-outside-worktree-tmp
  ;; Given a git_handoff draft
  ;; When it lives in /tmp or the handoff outbox tmp
  ;; Then swarm_handoff refuses it and asks for ./tmp/ in the worktree
  (let [root (tmp-dir)
        commit (init-repo! root)]
    (setup-project! root)
    (testing "rejects a draft in /tmp"
      (let [draft (fs/path "/tmp" (str "swarmforge-bad-draft-" (System/currentTimeMillis) ".handoff"))]
        (try
          (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: scratch-tmp\ncommit: %s\n" commit))
          (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                            (script "swarm_handoff.sh") (str draft))]
            (is (= 1 (:exit result)))
            (is (str/includes? (str (:err result) (:out result)) "./tmp/"))
            (is (fs/exists? draft)))
          (finally
            (fs/delete-if-exists draft)))))
    (testing "rejects a draft in the handoff outbox tmp"
      (let [draft (fs/path root ".swarmforge/handoffs/outbox/tmp/htw-console-app-coder.draft")]
        (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: outbox-scratch\ncommit: %s\n" commit))
        (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                          (script "swarm_handoff.sh") (str draft))]
          (is (= 1 (:exit result)))
          (is (str/includes? (str (:err result) (:out result)) "./tmp/"))
          (is (fs/exists? draft)))))))
(deftest swarm-handoff-validates-and-queues-git-handoffs
  (let [root (tmp-dir)
        commit (init-repo! root)]
    (setup-project! root)
    (testing "git_handoff requires a task name"
      (let [draft (fs/path root "tmp" "missing-task.handoff")]
        (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ncommit: %s\n" commit))
        (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                          (script "swarm_handoff.sh") (str draft))]
          (is (= 2 (:exit result)))
          (is (str/includes? (:err result) "Missing required header 'task'"))
          (is (fs/exists? draft)))))
    (testing "valid git_handoff writes task, canonical commit, and generated payload"
      (let [draft (fs/path root "tmp" "valid.handoff")]
        (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: task-1-cave-setup\ncommit: %s\n" commit))
        (let [result (audit-and-submit-git-handoff
                      {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
              queued (queued-path (:out result))
              content (read-file queued)]
          (is (str/includes? content "task: task-1-cave-setup\n"))
          (is (str/includes? content (str "commit: " commit "\n")))
          (is (str/includes? content "artifacts: README.md\n"))
          (is (str/includes? content (str "merge_and_process.sh sender " commit)))
          (is (fs/exists? queued))
          (is (not (fs/exists? draft))))))))
(deftest swarm-handoff-uses-hidden-task-id
  ;; Given a sender has a current hidden task id
  ;; When a draft names a different task id
  ;; Then the handoff is rejected before it can become active stale work
  (let [root (tmp-dir)
        commit (init-repo! root)]
    (setup-project! root)
    (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                "Visible Task\tsender\tcreated\tupdated\t20260825T120000000000Z-visible-task\n")
    (put-handoff! root "in_process" "50_current.handoff"
                  {:id "current"
                   :from "master"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "20260825T120000000000Z-visible-task"
                   :task "Visible Task"})
    (let [draft (fs/path root "tmp" "stale.handoff")]
      (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask_id: old-task-id\ntask: Visible Task\ncommit: %s\n" commit))
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                        (script "swarm_handoff.sh") (str draft))]
        (is (= 2 (:exit result)))
        (is (str/includes? (:err result) "does not match current in-process task_id"))
        (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff")))))))
(deftest swarm-handoff-fills-task-id-from-in-process-name-only-draft
  (let [root (tmp-dir)
        commit (init-repo! root)
        hidden "20260826T162611432618Z-htw"]
    (setup-project! root)
    (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                (str "HTW\tcoder\tcreated\tupdated\t" hidden "\n"
                     "extras\tsender\tcreated\tupdated\textras-id\n"))
    (put-handoff! root "in_process" "50_retry.handoff"
                  {:id "retry"
                   :from "(Retry)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id hidden
                   :task "HTW"})
    (let [draft (fs/path root "tmp" "htw.handoff")]
      (write-file draft (format "type: git_handoff\nto: receiver\npriority: 50\ntask: HTW\ncommit: %s\n" commit))
      (let [result (audit-and-submit-git-handoff
                    {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
            queued (queued-path (:out result))
            content (when queued (read-file queued))]
        (is (zero? (:exit result)))
        (is (some? queued))
        (is (str/includes? (str content) (str "task_id: " hidden "\n")))
        (is (str/includes? (str content) "task: HTW\n"))
        (is (not (str/includes? (str (:err result) (:out result)) "does not match")))))))
(deftest swarm-handoff-help-does-not-require-draft-task-id
  (let [result (run {:dir repo-root :ok? false}
                    (script "swarm_handoff.sh") "--help")
        text (str (:err result) (:out result))]
    (is (zero? (:exit result)))
    (is (str/includes? text "task: <short-stable-task-name>"))
    (is (not (str/includes? text "task_id: <hidden-task-id>")))))
(deftest swarm-handoff-auto-completes-current-after-git-handoff
  ;; Given a sender has current work and another item waiting
  ;; When swarm_handoff requests an audit and is then resubmitted unchanged
  ;; Then the first call retains current work and the second queues and completes it
  (let [root (tmp-dir)
        base (init-repo! root)
        current-file "50_20260615T000001Z_000001_from_planner_to_sender.handoff"
        next-file "50_20260615T000002Z_000002_from_planner_to_sender.handoff"
        completed (fs/path root ".swarmforge/handoffs/inbox/completed" current-file)
        queued-next (fs/path root ".swarmforge/handoffs/inbox/new" next-file)
        draft (fs/path root "tmp" "jump.handoff")]
    (setup-project! root {"sender" "task" "receiver" "task"})
    (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                (str "jump\tsender\tcreated\tupdated\tjump-id\n"
                     "extras\tsender\tcreated\tupdated\textras-id\n"))
    (is (= 0 (board-audit-count root "jump")))
    (is (= 0 (board-audit-count root "extras")))
    (put-handoff! root "in_process" current-file
                  {:id "20260615T000001Z_000001_from_planner"
                   :from "planner" :to "sender" :recipient "sender"
                   :priority "50" :type "note"
                   :task-id "jump-id" :task "jump"
                   :task-base-commit base
                   :body "jump"})
    (put-handoff! root "new" next-file
                  {:id "20260615T000002Z_000002_from_planner"
                   :from "planner" :to "sender" :recipient "sender"
                   :priority "50" :type "note"
                   :task-id "extras-id" :task "extras"
                   :body "extras"})
    (write-file (fs/path root "jump.md") "jump\n")
    (run {:dir root} "git" "add" "jump.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Jump")
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: jump\n")
    (let [first-call (run {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
                          (script "swarm_handoff.sh") (str draft))
          audit-files (fs/glob (fs/path root ".swarmforge/handoffs/audit_pending") "**/*.edn")]
      (is (zero? (:exit first-call)))
      (is (str/includes? (:out first-call) "AUDIT_REQUIRED"))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff")))
      (is (= 1 (count audit-files)))
      (is (= 1 (board-audit-count root "jump")))
      (is (= 0 (board-audit-count root "extras")))
      (is (fs/exists? (handoff-path root "in_process" current-file)))
      (is (not (fs/exists? completed)))
      (is (fs/exists? draft)))
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
                      (script "swarm_handoff.sh") (str draft))
          queued (queued-path (:out result))
          content (read-file queued)]
      (is (zero? (:exit result)))
      (is (str/includes? (:out result) "HANDOFF QUEUED:"))
      (is (str/includes? (:out result) "COMPLETED:"))
      (is (str/includes? (:out result) "MAIL_WAITING"))
      (is (str/includes? content "task_id: jump-id\n"))
      (is (= 1 (board-audit-count root "jump")))
      (is (empty? (audit-edn-files root)))
      (is (empty? (empty-audit-sender-dirs root)))
      (is (some? (header completed "completed_at")))
      (is (fs/exists? queued-next))
      (is (nil? (header queued-next "dequeued_at"))))))
(deftest swarm-handoff-requires-a-new-audit-after-the-commit-changes
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      "changed-commit\tsender\tcreated\tupdated\tchanged-commit-id\t0\n")
        draft (fs/path root "tmp" "changed-commit.handoff")
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: changed-commit\n")
    (let [first-call (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out first-call) "AUDIT_REQUIRED"))
      (is (= 1 (board-audit-count root "changed-commit"))))
    (write-file (fs/path root "changed.md") "changed\n")
    (run {:dir root} "git" "add" "changed.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Change after audit")
    (let [changed-call (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out changed-call) "AUDIT_REQUIRED"))
      (is (= 2 (board-audit-count root "changed-commit")))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff"))))
    (let [submitted (run opts (script "swarm_handoff.sh") (str draft))
          queued (queued-path (:out submitted))]
      (is (some? queued))
      (is (= 2 (board-audit-count root "changed-commit")))
      (is (str/includes? (read-file queued) (str "commit: " (head-sha root) "\n"))))))
(deftest swarm-handoff-invalidates-an-audit-before-rejecting-a-changed-commit
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        draft (fs/path root "tmp" "invalid-commit-change.handoff")
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: invalid-commit-change\n")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (run {:dir root} "git" "commit" "-q" "--allow-empty" "-m" "Empty change")
    (let [invalid-change (run (assoc opts :ok? false)
                              (script "swarm_handoff.sh") (str draft))]
      (is (= 1 (:exit invalid-change)))
      (is (str/includes? (:err invalid-change) "has no changed files")))
    (run {:dir root} "git" "reset" "--hard" "HEAD^")
    (let [after-restore (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out after-restore) "AUDIT_REQUIRED"))
      (is (nil? (queued-path (:out after-restore)))))
    (is (some? (queued-path (:out (run opts (script "swarm_handoff.sh") (str draft))))))))
(deftest swarm-handoff-requires-a-new-audit-after-the-draft-changes
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        draft (fs/path root "tmp" "changed-draft.handoff")
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: changed-draft\n")
    (let [first-call (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out first-call) "AUDIT_REQUIRED")))
    (write-file draft "type: git_handoff\nto: receiver\npriority: 40\ntask: changed-draft\n")
    (let [changed-call (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out changed-call) "AUDIT_REQUIRED"))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff"))))
    (let [submitted (run opts (script "swarm_handoff.sh") (str draft))
          queued (queued-path (:out submitted))]
      (is (some? queued))
      (is (str/includes? (read-file queued) "priority: 40\n")))))
(deftest swarm-handoff-invalidates-an-older-task-audit-for-the-same-sender
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        draft (fs/path root "tmp" "switch-task.handoff")
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask_id: first-id\ntask: first\n")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask_id: second-id\ntask: second\n")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (is (= 1 (count (fs/glob (fs/path root ".swarmforge/handoffs/audit_pending") "**/*.edn"))))
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask_id: first-id\ntask: first\n")
    (let [return-to-first (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out return-to-first) "AUDIT_REQUIRED"))
      (is (nil? (queued-path (:out return-to-first)))))
    (is (some? (queued-path (:out (run opts (script "swarm_handoff.sh") (str draft))))))))
(deftest swarm-handoff-invalidates-an-audit-when-the-changed-draft-is-invalid
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        draft (fs/path root "tmp" "invalid-change.handoff")
        valid "type: git_handoff\nto: receiver\npriority: 50\ntask_id: task-id\ntask: task\n"
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}]
    (write-file draft valid)
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (write-file draft (str valid "unknown: value\n"))
    (is (= 2 (:exit (run (assoc opts :ok? false)
                         (script "swarm_handoff.sh") (str draft)))))
    (is (empty? (audit-edn-files root)))
    (is (empty? (empty-audit-sender-dirs root)))
    (write-file draft valid)
    (let [after-repair (run opts (script "swarm_handoff.sh") (str draft))]
      (is (str/includes? (:out after-repair) "AUDIT_REQUIRED"))
      (is (nil? (queued-path (:out after-repair)))))
    (is (some? (queued-path (:out (run opts (script "swarm_handoff.sh") (str draft))))))))
(deftest swarm-handoff-keeps-audits-isolated-by-sender
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        sender-draft (fs/path root "tmp" "sender.handoff")
        receiver-draft (fs/path root "tmp" "receiver.handoff")
        sender-opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
        receiver-opts {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}]
    (write-file sender-draft "type: git_handoff\nto: receiver\npriority: 50\ntask_id: first-id\ntask: first\n")
    (write-file receiver-draft "type: git_handoff\nto: sender\npriority: 50\ntask_id: second-id\ntask: second\n")
    (run sender-opts (script "swarm_handoff.sh") (str sender-draft))
    (run receiver-opts (script "swarm_handoff.sh") (str receiver-draft))
    (is (= 2 (count (audit-edn-files root))))
    (is (some? (queued-path (:out (run sender-opts (script "swarm_handoff.sh")
                                       (str sender-draft))))))
    (is (= 1 (count (audit-edn-files root))))
    (is (empty? (empty-audit-sender-dirs root)))
    (is (some? (queued-path (:out (run receiver-opts (script "swarm_handoff.sh")
                                       (str receiver-draft))))))
    (is (empty? (audit-edn-files root)))
    (is (empty? (empty-audit-sender-dirs root)))))
(deftest swarm-handoff-removes-empty-audit-pending-sender-directories
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        draft (fs/path root "tmp" "empty-dirs.handoff")
        opts {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
        lock (fs/path (audit-pending-dir root) ".lock")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: empty-dirs\n")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (is (= 1 (count (audit-edn-files root))))
    (is (= 1 (count (audit-sender-dirs root))))
    (is (empty? (empty-audit-sender-dirs root)))
    (is (fs/exists? lock))
    (is (some? (queued-path (:out (run opts (script "swarm_handoff.sh") (str draft))))))
    (is (empty? (audit-edn-files root)))
    (is (empty? (audit-sender-dirs root)))
    (is (empty? (empty-audit-sender-dirs root)))
    (is (fs/directory? (audit-pending-dir root)))
    (is (fs/exists? lock))
    (doseq [path (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff")]
      (fs/delete-if-exists path))
    (write-file (fs/path root "next.md") "next\n")
    (run {:dir root} "git" "add" "next.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Next slice")
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: empty-dirs-next\n")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (is (= 1 (count (audit-edn-files root))))
    (is (= 1 (count (audit-sender-dirs root))))
    (is (empty? (empty-audit-sender-dirs root)))
    (write-file (fs/path root "changed.md") "changed\n")
    (run {:dir root} "git" "add" "changed.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Change after audit")
    (is (str/includes? (:out (run opts (script "swarm_handoff.sh") (str draft)))
                       "AUDIT_REQUIRED"))
    (is (= 1 (count (audit-edn-files root))))
    (is (empty? (empty-audit-sender-dirs root)))
    (is (some? (queued-path (:out (run opts (script "swarm_handoff.sh") (str draft))))))
    (is (empty? (audit-edn-files root)))
    (is (empty? (audit-sender-dirs root)))
    (is (empty? (empty-audit-sender-dirs root)))
    (is (fs/directory? (audit-pending-dir root)))
    (is (fs/exists? lock))))
(deftest swarm-handoff-refuses-ambiguous-current-before-queueing
  ;; Given a sender has ambiguous current work
  ;; When swarm_handoff is asked to queue a git_handoff
  ;; Then it refuses before writing an outbox file
  (let [root (tmp-dir)
        _ (init-repo! root)
        draft (fs/path root "tmp" "ambiguous.handoff")]
    (setup-project! root {"sender" "task" "receiver" "task"})
    (doseq [filename ["40_20260615T000001Z_000001_from_planner_to_sender.handoff"
                      "50_20260615T000002Z_000002_from_planner_to_sender.handoff"]]
      (put-handoff! root "in_process" filename
                    {:id filename
                     :from "planner" :to "sender" :recipient "sender"
                     :priority "50" :type "note"
                     :task-id "jump-id" :task "jump"
                     :body "jump"}))
    (write-file (fs/path root "jump.md") "jump\n")
    (run {:dir root} "git" "add" "jump.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Jump")
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: jump-id\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))
          outbox-files (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff")]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "Ambiguous current work: multiple tasks are in process."))
      (is (empty? outbox-files))
      (is (fs/exists? draft)))))
(deftest swarm-handoff-refuses-features-on-utility-and-new-qa-on-review
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]
                                ["architect" "batch" "back-all"]])]
    (pack-board root true "create" "--root" (str root) "--name" "util" "--type" "utility")
    (write-file (fs/path root "features/console.feature") "Feature: console\n")
    (run {:dir root} "git" "add" "features/console.feature" "tasks/util.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Add feature")
    (let [draft (fs/path root "tmp" "util.handoff")]
      (write-file draft "type: git_handoff\nto: cleaner\npriority: 50\ntask: util\n")
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "coder"} :ok? false}
                        (script "swarm_handoff.sh") (str draft))]
        (is (= 2 (:exit result)))
        (is (str/includes? (:err result) "must not add features"))
        (is (empty? (remove #(str/includes? (str %) "New_Task") (outbox-handoffs root))))))
    (pack-board root true "create" "--root" (str root) "--name" "rev" "--type" "review")
    (write-file (fs/path root "qa/headed.md") "# headed\n")
    (run {:dir root} "git" "add" "qa/headed.md" "tasks/rev.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Add qa")
    (let [draft (fs/path root "tmp" "rev.handoff")]
      (write-file draft "type: git_handoff\nto: architect\npriority: 50\ntask: rev\n")
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"} :ok? false}
                        (script "swarm_handoff.sh") (str draft))]
        (is (= 2 (:exit result)))
        (is (str/includes? (:err result) "must not add QA procedures"))
        (is (empty? (remove #(str/includes? (str %) "New_Task") (outbox-handoffs root))))))))
(deftest swarm-handoff-refuses-qa-on-component-and-allows-it-on-qa-type
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["specifier" "task" "forward-only"]
                                ["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]
                                ["architect" "batch" "back-all"]
                                ["hardender" "batch" "forward-only"]
                                ["QA" "batch" "back-all"]])]
    (pack-board root true "create" "--root" (str root) "--name" "comp" "--type" "component")
    (write-file (fs/path root "qa/headed.md") "# headed\n")
    (run {:dir root} "git" "add" "qa/headed.md" "tasks/comp.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Add qa")
    (let [draft (fs/path root "tmp" "comp.handoff")]
      (write-file draft "type: git_handoff\nto: coder\npriority: 50\ntask: comp\n")
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "specifier"} :ok? false}
                        (script "swarm_handoff.sh") (str draft))]
        (is (= 2 (:exit result)))
        (is (str/includes? (:err result) "must not add QA procedures"))
        (is (empty? (remove #(str/includes? (str %) "New_Task") (outbox-handoffs root))))))
    (pack-board root true "create" "--root" (str root) "--name" "full" "--type" "QA")
    (run {:dir root} "git" "add" "tasks/full.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Add full task document")
    (let [draft (fs/path root "tmp" "full.handoff")]
      (write-file draft "type: git_handoff\nto: coder\npriority: 50\ntask: full\n")
      (let [result (audit-and-submit-git-handoff
                    {:dir root :env {"SWARMFORGE_ROLE" "specifier"}} draft)]
        (is (zero? (:exit result)))
        (is (seq (remove #(str/includes? (str %) "New_Task") (outbox-handoffs root))))))))
(deftest swarm-handoff-fills-artifacts-from-the-commit
  ;; Given a git_handoff of a commit that added a file
  ;; When it is queued
  ;; Then artifacts lists that file
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "with-files.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: fill-artifacts\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))
          content (read-file queued)]
      (is (zero? (:exit result)))
      (is (str/includes? content "artifacts: slice.md\n"))
      (is (not (str/includes? content "artifacts: none"))))))
(deftest swarm-handoff-includes-committed-task-document
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "tasks/htw.md") "# htw\n\nImplement the stories.\n")
        _ (run {:dir root} "git" "add" "tasks/htw.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add task document")
        draft (fs/path root "tmp" "task-doc.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: htw\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
          content (read-file (queued-path (:out result)))]
      (is (zero? (:exit result)))
      (is (str/includes? content "artifacts: tasks/htw.md\n")))))
(deftest swarm-handoff-requires-current-operator-task-document-in-commit
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        task-id "task-document-id"
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      (str "Document task\tsender\tcreated\tupdated\t" task-id
                           "\t0\tcomponent\n"))
        document "# Document task\n\nType: component\n\nPreserve this intent.\n"
        _ (write-file (fs/path root "tasks/Document task.md") document)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add work")
        draft (fs/path root "tmp" "document-task.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: Document task\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "does not include the task document"))
      (is (empty? (outbox-handoffs root))))
    (run {:dir root} "git" "add" "tasks/Document task.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Record task document")
    (write-file (fs/path root "tasks/Document task.md")
                "# Document task\n\nType: component\n\nUpdated operator intent.\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "contains a stale task document"))
      (is (empty? (outbox-handoffs root))))
    (run {:dir root} "git" "add" "tasks/Document task.md")
    (run {:dir root} "git" "commit" "-q" "-m" "Update task document")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)]
      (is (zero? (:exit result)))
      (is (some? (queued-path (:out result)))))))
(deftest swarm-handoff-allows-missing-task-document-when-tasks-excluded
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        task-id "excluded-task-id"
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      (str "Excluded task\tsender\tcreated\tupdated\t" task-id
                           "\t0\tcomponent\n"))
        document "# Excluded task\n\nType: component\n\nPreserve this intent.\n"
        _ (write-file (fs/path root ".git/info/exclude") "/tasks/\n")
        _ (write-file (fs/path root "tasks/Excluded task.md") document)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add work")
        draft (fs/path root "tmp" "excluded-task.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: Excluded task\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)]
      (is (zero? (:exit result)) (str (:err result) (:out result)))
      (is (some? (queued-path (:out result))))
      (is (= document (read-file (fs/path root "tasks/Excluded task.md")))))))
(deftest swarm-handoff-still-excludes-a-previously-force-committed-task-document
  ;; Given tasks/ is excluded but a historical force-add already tracked the
  ;; task document (the force-add-past-the-exclude workaround pattern)
  ;; When a later git_handoff's result commit does not touch that document
  ;; Then the handoff is still allowed, not blocked as missing or stale
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        task-id "excluded-task-id"
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      (str "Excluded task\tsender\tcreated\tupdated\t" task-id
                           "\t0\tcomponent\n"))
        document "# Excluded task\n\nType: component\n\nPreserve this intent.\n"
        _ (write-file (fs/path root ".git/info/exclude") "/tasks/\n")
        _ (write-file (fs/path root "tasks/Excluded task.md") document)
        _ (run {:dir root} "git" "add" "-f" "--" "tasks/Excluded task.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Force-commit excluded doc")
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add work")
        draft (fs/path root "tmp" "excluded-task.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: Excluded task\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)]
      (is (zero? (:exit result)) (str (:err result) (:out result)))
      (is (some? (queued-path (:out result)))))))
(deftest swarm-handoff-excludes-deleted-artifacts
  ;; Given a commit deletes one file and changes another
  ;; When it is queued
  ;; Then the deleted file is not listed as an approval document
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "keep.md") "before\n")
        _ (write-file (fs/path root "gone.md") "delete me\n")
        _ (run {:dir root} "git" "add" "keep.md" "gone.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add docs")
        _ (write-file (fs/path root "keep.md") "after\n")
        _ (fs/delete (fs/path root "gone.md"))
        _ (run {:dir root} "git" "add" "keep.md" "gone.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Update docs")
        draft (fs/path root "tmp" "deleted-artifact.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: docs\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))
          content (read-file queued)]
      (is (zero? (:exit result)))
      (is (str/includes? content "artifacts: keep.md\n"))
      (is (not (str/includes? content "gone.md"))))))
(deftest swarm-handoff-uses-task-base-for-merge-artifacts
  ;; Given HEAD is a merge whose first-parent diff is unrelated to the current task
  ;; When a git_handoff is queued from an in-process task with a base commit
  ;; Then artifacts come from task_base_commit..HEAD, not HEAD^..HEAD
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      "extras\tsender\tcreated\tupdated\textras-id\t0\tcomponent\n")
        _ (run {:dir root} "git" "checkout" "-q" "-b" "jump")
        _ (write-file (fs/path root "features/console/wumpus_jump.feature") "jump\n")
        _ (run {:dir root} "git" "add" "features/console/wumpus_jump.feature")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Jump spec")
        jump (head-sha root)
        _ (run {:dir root} "git" "checkout" "-q" "master")
        _ (write-file (fs/path root "features/console/command_extras.feature") "commands\n")
        _ (write-file (fs/path root "features/console/holy_hand_grenade.feature") "grenade\n")
        _ (run {:dir root} "git" "add" "features/console/command_extras.feature"
                 "features/console/holy_hand_grenade.feature")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Extras spec")
        _ (run {:dir root} "git" "merge" "--no-ff" "jump" "-m" "Merge jump into extras")
        merge-head (head-sha root)
        draft (fs/path root "tmp" "extras.handoff")]
    (put-handoff! root "in_process" "50_extras.handoff"
                  {:id "current"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "extras-id"
                   :task "extras"
                   :task-base-commit jump
                   :body "extras"})
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: extras\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))]
      (is (zero? (:exit result)) (pr-str result))
      (is (some? queued) (pr-str result))
      (when queued
        (let [content (read-file queued)]
          (is (str/includes? content (str "commit: " merge-head "\n")))
          (is (str/includes? content "artifacts: features/console/command_extras.feature,features/console/holy_hand_grenade.feature\n"))
          (is (not (str/includes? content "wumpus_jump.feature"))))))))
(deftest swarm-handoff-refuses-a-merge-with-no-changed-files
  ;; Given HEAD is a merge whose first-parent diff is empty
  ;; When swarm_handoff queues a git_handoff
  ;; Then it refuses and does not write artifacts: none
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (run {:dir root} "git" "checkout" "-q" "-b" "side")
        _ (write-file (fs/path root "side.md") "side\n")
        _ (run {:dir root} "git" "add" "side.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Side")
        _ (run {:dir root} "git" "checkout" "-q" "master")
        _ (run {:dir root} "git" "merge" "-q" "--no-ff" "-s" "ours" "-m" "Ours" "side")
        draft (fs/path root "tmp" "merge.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: merge-empty\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))
          outbox (fs/path root ".swarmforge" "handoffs" "outbox")
          queued (when (fs/exists? outbox) (fs/glob outbox "*.handoff"))]
      (is (not (zero? (:exit result))))
      (is (str/includes? (str (:err result) (:out result)) "no changed files"))
      (is (not (str/includes? (str (:err result) (:out result)) "artifacts: none")))
      (is (empty? queued))
      (is (fs/exists? draft)))))
(deftest swarm-handoff-rejects-evidence-headers
  ;; Given a git draft with coverage: or a note with an extra header
  ;; When swarm_handoff validates it
  ;; Then the draft is invalid; notes stay type/to/priority/message
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)]
    (testing "git_handoff with coverage: is invalid"
      (let [draft (fs/path root "tmp" "coverage.handoff")]
        (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: cave\ncoverage: 92\n")
        (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                          (script "swarm_handoff.sh") (str draft))]
          (is (= 2 (:exit result)))
          (is (str/includes? (:err result) "unknown header 'coverage'"))
          (is (fs/exists? draft)))))
    (testing "note extra headers are invalid"
      (let [draft (fs/path root "tmp" "note-extra.handoff")]
        (write-file draft "type: note\nto: receiver\npriority: 50\nmessage: hello\ncoverage: 92\n")
        (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                          (script "swarm_handoff.sh") (str draft))]
          (is (= 2 (:exit result)))
          (is (str/includes? (:err result) "unknown header 'coverage'"))
          (is (fs/exists? draft)))))
    (testing "note still accepts only type to priority message"
      (let [draft (fs/path root "tmp" "note-ok.handoff")]
        (write-file draft "type: note\nto: receiver\npriority: 50\nmessage: hello\n")
        (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                          (script "swarm_handoff.sh") (str draft))]
          (is (zero? (:exit result)))
          (is (str/includes? (:out result) "HANDOFF QUEUED:"))
          (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs") "audit_pending/**/*.edn"))))))))
(deftest swarm-handoff-fills-missing-or-invalid-priority
  ;; Given a git_handoff draft that omits priority, or writes priority: normal
  ;; When swarm_handoff queues it
  ;; Then the queued file has priority: 50
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")]
    (testing "omitted priority becomes 50"
      (let [draft (fs/path root "tmp" "no-priority.handoff")]
        (write-file draft "type: git_handoff\nto: receiver\ntask: fill-priority\n")
        (let [result (audit-and-submit-git-handoff
                      {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) "priority: 50\n")))))
    (testing "priority: normal becomes 50"
      (let [draft (fs/path root "tmp" "word-priority.handoff")]
        (write-file draft "type: git_handoff\nto: receiver\npriority: normal\ntask: fill-priority-word\n")
        (let [result (audit-and-submit-git-handoff
                      {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) "priority: 50\n"))
          (is (not (str/includes? (str content) "priority: normal\n"))))))
    (testing "valid two-digit priority is kept"
      (let [draft (fs/path root "tmp" "keep-priority.handoff")]
        (write-file draft "type: git_handoff\nto: receiver\npriority: 00\ntask: keep-priority\n")
        (let [result (audit-and-submit-git-handoff
                      {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
              queued (queued-path (:out result))
              content (when (zero? (:exit result)) (read-file queued))]
          (is (zero? (:exit result)))
          (is (str/includes? (str content) "priority: 00\n")))))))
(deftest swarm-handoff-strips-extra-draft-payload
  ;; Given a git_handoff draft with prose after the headers
  ;; When swarm_handoff queues it
  ;; Then it is valid and the queued body is the helper payload, not the prose
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        sha (str/trim (:out (run {:dir root} "git" "rev-parse" "--short=10" "HEAD")))
        draft (fs/path root "tmp" "with-payload.handoff")]
    (write-file draft (str "type: git_handoff\nto: receiver\npriority: 50\ntask: strip-payload\n\n"
                           "Please merge this and run the tests.\n"))
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (str/includes? (str content) (str "merge_and_process.sh sender " sha)))
      (is (not (str/includes? (str content) "Please merge this and run the tests."))))))
(deftest swarm-handoff-last-role-tags-git-handoff-non-forwarding
  ;; Given receiver is the last pack role
  ;; When it queues a git_handoff
  ;; Then the queued file has non-forwarding: true
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "last-role.handoff")]
    (write-file draft "type: git_handoff\nto: sender\npriority: 00\ntask: HTW\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "receiver"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (str/includes? (str content) "non-forwarding: true\n"))
      (is (str/includes? (str content) "delivery_kind: terminal\n"))
      (is (= 1 (count (outbox-handoffs root)))))))
(deftest swarm-handoff-non-last-role-does-not-tag-non-forwarding
  ;; Given sender is not the last pack role
  ;; When it queues a git_handoff
  ;; Then the queued file has no non-forwarding header
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "mid-role.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: HTW\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (not (str/includes? (str content) "non-forwarding:")))
      (is (str/includes? (str content) "delivery_kind: forward\n")))))
(deftest swarm-handoff-refactorer-back-one-writes-reverse-copy
  ;; Given four-pack refactorer back-one
  ;; When it queues git_handoff to architect
  ;; Then coder gets a separate non-forwarding 00 copy; architect is on to:
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root four-pack-role-rows)
        sha (commit-work! root)
        result (queue-git-from! root "refactorer" "architect" "HTW")
        forward (outbox-to root "architect")
        reverse (outbox-to root "coder")
        extra "Please also rewrite the layout."]
    (is (zero? (:exit result)))
    (is (some? forward))
    (is (some? reverse))
    (is (not= (str forward) (str reverse)))
    (is (str/starts-with? (fs/file-name reverse) "00_"))
    (is (str/starts-with? (fs/file-name forward) "50_"))
    (is (= "architect" (header forward "to")))
    (is (= "coder" (header reverse "to")))
    (is (not (str/includes? (header forward "to") "coder")))
    (is (= "true" (header reverse "non-forwarding")))
    (is (= "reverse" (header reverse "delivery_kind")))
    (is (= "forward" (header forward "delivery_kind")))
    (is (not= "true" (header forward "non-forwarding")))
    (is (str/includes? (handoff-body reverse) (str "merge_and_process.sh refactorer " sha)))
    (is (str/includes? (handoff-body reverse) "inbound tree is the structure"))
    (is (str/includes? (handoff-body forward) (str "merge_and_process.sh refactorer " sha)))
    (is (str/includes? (handoff-body forward) "current tree is the structure"))
    (is (not (str/includes? (handoff-body forward) "inbound tree is the structure")))
    (is (not (str/includes? (handoff-body forward) extra)))
    (is (not (str/includes? (handoff-body reverse) extra)))
    (is (= 2 (count (outbox-handoffs root))))))
(deftest swarm-handoff-terminal-back-all-writes-one-broadcast
  ;; Given four-pack architect last with back-all
  ;; When it queues git_handoff
  ;; Then it queues one terminal broadcast, not a broadcast plus reverse copies
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root four-pack-role-rows)
        _ (commit-work! root)
        result (queue-git-from! root "architect" "specifier,coder,refactorer" "HTW")
        queued (first (outbox-handoffs root))
        extra "Please also rewrite the layout."]
    (is (zero? (:exit result)))
    (is (= 1 (count (outbox-handoffs root))))
    (is (= "specifier,coder,refactorer" (header queued "to")))
    (is (= "terminal" (header queued "delivery_kind")))
    (is (= "true" (header queued "non-forwarding")))
    (is (str/starts-with? (fs/file-name queued) "50_"))
    (is (str/includes? (handoff-body queued) "merge_and_process.sh architect"))
    (is (str/includes? (handoff-body queued) "inbound tree is the structure"))
    (is (not (str/includes? (handoff-body queued) "current tree is the structure")))
    (is (not (str/includes? (handoff-body queued) extra)))))
(deftest swarm-handoff-six-pack-architect-back-all-skips-downstream
  ;; Given six-pack architect back-all (not last)
  ;; When it queues git_handoff to hardender
  ;; Then specifier, coder, cleaner get reverse copies; QA does not
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root six-pack-role-rows)
        _ (commit-work! root)
        result (queue-git-from! root "architect" "hardender" "HTW")]
    (is (zero? (:exit result)))
    (is (= "hardender" (header (outbox-to root "hardender") "to")))
    (is (not= "true" (header (outbox-to root "hardender") "non-forwarding")))
    (is (= "forward" (header (outbox-to root "hardender") "delivery_kind")))
    (doseq [role ["specifier" "coder" "cleaner"]]
      (is (= "true" (header (outbox-to root role) "non-forwarding")) role)
      (is (= "reverse" (header (outbox-to root role) "delivery_kind")) role)
      (is (str/starts-with? (fs/file-name (outbox-to root role)) "00_") role))
    (is (nil? (outbox-to root "QA")))))
(deftest swarm-handoff-six-pack-qa-back-all-writes-one-terminal-broadcast
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root six-pack-role-rows)
        _ (commit-work! root)
        result (queue-git-from! root "QA" "specifier,coder,cleaner,architect,hardender" "HTW")
        terminal (first (outbox-handoffs root))]
    (is (zero? (:exit result)))
    (is (= 1 (count (outbox-handoffs root))))
    (is (= "specifier,coder,cleaner,architect,hardender" (header terminal "to")))
    (is (= "terminal" (header terminal "delivery_kind")))
    (is (= "true" (header terminal "non-forwarding")))))
(deftest swarm-handoff-two-pack-cleaner-back-one-writes-one-terminal
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]])
        _ (commit-work! root)
        result (queue-git-from! root "cleaner" "coder" "HTW")]
    (is (zero? (:exit result)))
    (is (= "true" (header (outbox-to root "coder") "non-forwarding")))
    (is (= "terminal" (header (outbox-to root "coder") "delivery_kind")))
    (is (str/starts-with? (fs/file-name (outbox-to root "coder")) "50_"))
    (is (= 1 (count (outbox-handoffs root))))))
(deftest swarm-handoff-last-window-forward-only-has-no-reverse-copies
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["coder" "task" "forward-only"]
                                ["cleaner" "task" "forward-only"]])
        _ (commit-work! root)
        result (queue-git-from! root "cleaner" "coder" "HTW")]
    (is (zero? (:exit result)))
    (is (= 1 (count (outbox-handoffs root))))
    (is (= "true" (header (outbox-to root "coder") "non-forwarding")))
    (is (= "terminal" (header (outbox-to root "coder") "delivery_kind")))
    (is (str/includes? (handoff-body (outbox-to root "coder"))
                       "inbound tree is the structure"))
    (is (not (str/includes? (handoff-body (outbox-to root "coder"))
                            "current tree is the structure")))))
(deftest swarm-handoff-refuses-git-handoff-when-inbound-is-non-forwarding
  ;; Given an in-process inbound git_handoff tagged non-forwarding
  ;; When swarm_handoff queues another git_handoff
  ;; Then it refuses
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        inbound (fs/path root ".swarmforge/handoffs/inbox/in_process/00_from_architect.handoff")
        draft (fs/path root "tmp" "forward.handoff")]
    (write-file inbound (str "from: architect\nto: sender\npriority: 00\ntype: git_handoff\n"
                             "task: HTW\nnon-forwarding: true\n\nmerge\n"))
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: HTW\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))]
      (is (not (zero? (:exit result))))
      (is (str/includes? (str (:err result) (:out result)) "non-forwarding"))
      (is (fs/exists? draft)))))
(deftest swarm-handoff-keeps-draft-task-that-names-a-lane-card
  ;; Given Command syntax and Holy Hand Grenade cards in the sender lane
  ;; When swarm_handoff queues a git_handoff with task: Holy Hand Grenade
  ;; Then the queued file keeps that task name
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge" "board" "tasks.tsv")
                      (str "Command syntax\tsender\t2026-06-15T00:00:00Z\t2026-06-15T00:00:00Z\n"
                           "Holy Hand Grenade\tsender\t2026-06-15T00:00:01Z\t2026-06-15T00:00:01Z\n"))
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "hhg.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: Holy Hand Grenade\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (str/includes? (str content) "task: Holy Hand Grenade\n"))
      (is (not (str/includes? (str content) "task: Command syntax\n"))))))
(deftest swarm-handoff-from-worktree-uses-master-outbox-when-roles-copied
  ;; Given a sender worktree with a copied roles.tsv
  ;; When swarm_handoff queues a git_handoff there
  ;; Then the file is on the master project outbox
  (let [root (tmp-dir)
        _ (init-repo! root)
        wt (add-worktree! root "sender")
        _ (setup-project! root {"sender" "task" "receiver" "task"})
        roles (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                      wt root)
        _ (write-file (fs/path root ".swarmforge" "roles.tsv") roles)
        _ (write-file (fs/path wt ".swarmforge" "roles.tsv") roles)
        _ (write-file (fs/path wt "slice.md") "from the worktree\n")
        _ (run {:dir wt} "git" "add" "slice.md")
        _ (run {:dir wt} "git" "commit" "-q" "-m" "Worktree slice")
        draft (fs/path wt "tmp" "copied-roles.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: copied-roles\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir wt :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))]
      (is (zero? (:exit result)))
      (is (str/starts-with? (str (fs/canonicalize queued))
                           (str (fs/canonicalize (fs/path root ".swarmforge" "handoffs" "outbox")))))
      (is (not (str/includes? queued "/.worktrees/"))))))
(deftest swarm-handoff-queues-a-merge-with-first-parent-files
  ;; Given HEAD is a merge that added a file versus the first parent
  ;; When swarm_handoff queues a git_handoff
  ;; Then it succeeds and artifacts lists that file
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        _ (run {:dir root} "git" "checkout" "-q" "-b" "side")
        _ (write-file (fs/path root "side.md") "side\n")
        _ (run {:dir root} "git" "add" "side.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Side")
        _ (run {:dir root} "git" "checkout" "-q" "master")
        _ (write-file (fs/path root "main.md") "main\n")
        _ (run {:dir root} "git" "add" "main.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Main")
        _ (run {:dir root} "git" "merge" "-q" "--no-edit" "side")
        draft (fs/path root "tmp" "merge-files.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: merge-files\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (str/includes? (str content) "artifacts:"))
      (is (str/includes? (str content) "side.md")))))
(deftest swarm-handoff-carries-every-in-process-batch-task-id
  ;; Given an in-process batch whose first item is Command syntax, and HTW still in the sender lane
  ;; When swarm_handoff queues a git_handoff drafted as HTW
  ;; Then the queued file uses Command syntax
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root {"sender" "batch" "receiver" "task"})
        batch (fs/path root ".swarmforge/handoffs/inbox/in_process/batch_20260824T182225Z_000001")
        _ (fs/create-dirs batch)
        _ (write-file (fs/path batch "50_20260824T181141Z_000002_from_coder_to_sender.handoff")
                      (handoff {:id "20260824T181141Z_000002_from_coder"
                                :from "coder" :to "sender" :recipient "sender"
                                :priority "50" :type "git_handoff"
                                :task-id "command-id" :task "Command syntax"
                                :commit (head-sha root)}))
        _ (write-file (fs/path batch "50_20260824T181302Z_000003_from_coder_to_sender.handoff")
                      (handoff {:id "20260824T181302Z_000003_from_coder"
                                :from "coder" :to "sender" :recipient "sender"
                                :priority "50" :type "git_handoff"
                                :task-id "validate-id" :task "validate"
                                :commit (head-sha root)}))
        _ (write-file (fs/path root ".swarmforge" "board" "tasks.tsv")
                      (str "HTW\tsender\t2026-08-24T18:05:33Z\t2026-08-24T18:05:33Z\n"
                           "Command syntax\tsender\t2026-08-24T18:06:05Z\t2026-08-24T18:06:05Z\tcommand-id\n"
                           "validate\tsender\t2026-08-24T18:06:45Z\t2026-08-24T18:06:45Z\tvalidate-id\n"))
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "htw.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 00\ntask: HTW\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false} draft)
          queued (queued-path (:out result))
          content (when (zero? (:exit result)) (read-file queued))]
      (is (zero? (:exit result)))
      (is (str/includes? (str content) "task: Command syntax\n"))
      (is (= "[\"command-id\" \"validate-id\"]"
             (header queued "batch_task_ids")))
      (is (= "batch_20260824T182225Z_000001"
             (header queued "batch_id")))
      (is (= 1 (board-audit-count root "Command syntax")))
      (is (= 1 (board-audit-count root "validate")))
      (is (not (str/includes? (str content) "task: HTW\n"))))))

(deftest swarm-handoff-rejects-a-result-missing-a-batch-member-commit
  (let [root (tmp-dir)
        base (init-repo! root)
        _ (setup-project! root {"sender" "batch" "receiver" "task"})
        _ (write-file (fs/path root "adaptive.md") "adaptive\n")
        _ (run {:dir root} "git" "add" "adaptive.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Adaptive result")
        adaptive (str/trim (:out (run {:dir root} "git" "rev-parse" "HEAD")))
        _ (run {:dir root} "git" "branch" "complete-batch")
        _ (run {:dir root} "git" "checkout" "-q" "complete-batch")
        _ (write-file (fs/path root "tactical.md") "tactical\n")
        _ (run {:dir root} "git" "add" "tactical.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Tactical result")
        tactical (str/trim (:out (run {:dir root} "git" "rev-parse" "HEAD")))
        _ (run {:dir root} "git" "checkout" "-q" "master")
        batch-id "batch_20260904T120000Z_000001"
        batch (fs/path root ".swarmforge/handoffs/inbox/in_process" batch-id)
        members [{:file "50_adaptive.handoff" :handoff-id "adaptive"
                  :task "adaptive" :task-id "adaptive-id" :task-ids ["adaptive-id"]
                  :from "coder" :type "git_handoff" :commit adaptive :merge-from "coder"}
                 {:file "50_tactical.handoff" :handoff-id "tactical"
                  :task "tactical" :task-id "tactical-id" :task-ids ["tactical-id"]
                  :from "coder" :type "git_handoff" :commit tactical :merge-from "coder"}]
        _ (fs/create-dirs batch)
        _ (write-file (fs/path batch "50_adaptive.handoff")
                      (handoff {:id "adaptive" :from "coder" :to "sender" :recipient "sender"
                                :priority "50" :type "git_handoff" :task-id "adaptive-id"
                                :task "adaptive" :commit adaptive :task-base-commit base}))
        _ (write-file (fs/path batch "50_tactical.handoff")
                      (handoff {:id "tactical" :from "coder" :to "sender" :recipient "sender"
                                :priority "50" :type "git_handoff" :task-id "tactical-id"
                                :task "tactical" :commit tactical :task-base-commit base}))
        _ (write-file (fs/path batch "batch_manifest.edn")
                      (str (pr-str {:version 1 :batch-id batch-id
                                    :task-ids ["adaptive-id" "tactical-id"]
                                    :members members :selected-commit tactical
                                    :selected-from "coder"}) "\n"))
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      (str "adaptive\tsender\tcreated\tupdated\tadaptive-id\n"
                           "tactical\tsender\tcreated\tupdated\ttactical-id\n"))
        draft (fs/path root "tmp" "partial-batch.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: adaptive\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "is incomplete for batch"))
      (is (fs/directory? batch))
      (is (fs/regular-file? draft))
      (is (empty? (outbox-handoffs root)))
      (is (= ["adaptive\tsender\tcreated\tupdated\tadaptive-id"
              "tactical\tsender\tcreated\tupdated\ttactical-id"]
             (str/split-lines (read-file (fs/path root ".swarmforge/board/tasks.tsv"))))))))

(deftest swarm-handoff-preserves-a-propagated-batch-through-task-mode
  (let [root (tmp-dir)
        base (init-repo! root)
        _ (setup-project! root {"sender" "task" "receiver" "task"})
        current (fs/path root ".swarmforge/handoffs/inbox/in_process/50_batch.handoff")
        _ (write-file current
                      (handoff {:id "batch-from-cleaner"
                                :from "cleaner" :to "sender" :recipient "sender"
                                :priority "50" :type "git_handoff"
                                :task-id "pits-id" :task "pits"
                                :batch-task-ids ["pits-id" "bats-id" "wumpus-id"]
                                :task-base-commit base
                                :commit (head-sha root)}))
        _ (write-file (fs/path root ".swarmforge/board/tasks.tsv")
                      (str "pits\tsender\tcreated\tupdated\tpits-id\n"
                           "bats\tsender\tcreated\tupdated\tbats-id\n"
                           "wumpus\tsender\tcreated\tupdated\twumpus-id\n"))
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Add slice")
        draft (fs/path root "tmp" "batch.handoff")]
    (write-file draft "type: git_handoff\nto: receiver\npriority: 50\ntask: pits\n")
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "sender"}} draft)
          queued (queued-path (:out result))]
      (is (zero? (:exit result)))
      (is (= "[\"pits-id\" \"bats-id\" \"wumpus-id\"]"
             (header queued "batch_task_ids")))
      (is (= 1 (board-audit-count root "pits")))
      (is (= 1 (board-audit-count root "bats")))
      (is (= 1 (board-audit-count root "wumpus"))))))
(deftest swarm-handoff-review-terminal-includes-upstream-including-before-start
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["specifier" "task" "forward-only"]
                                ["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]
                                ["architect" "batch" "back-all"]
                                ["hardender" "batch" "forward-only"]
                                ["QA" "batch" "back-all"]])
        _ (pack-board root true "create" "--root" (str root) "--name" "rev" "--type" "review")
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md" "tasks/rev.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Review work")
        ok (fs/path root "tmp" "rev-ok.handoff")
        bad (fs/path root "tmp" "rev-bad.handoff")]
    (write-file ok (str "type: git_handoff\n"
                        "to: specifier,coder,cleaner,architect,hardender\n"
                        "priority: 50\ntask: rev\n"))
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "QA"}} ok)]
      (is (zero? (:exit result))))
    (write-file bad "type: git_handoff\nto: architect\npriority: 50\ntask: rev\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "QA"} :ok? false}
                      (script "swarm_handoff.sh") (str bad))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "upstream")))))
(deftest swarm-handoff-component-hardender-refuses-qa
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["specifier" "task" "forward-only"]
                                ["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]
                                ["architect" "batch" "back-all"]
                                ["hardender" "batch" "forward-only"]
                                ["QA" "batch" "back-all"]])
        _ (pack-board root true "create" "--root" (str root)
                      "--name" "comp" "--type" "component")
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Component work")
        draft (fs/path root "tmp" "comp.handoff")]
    (write-file draft "type: git_handoff\nto: QA\npriority: 50\ntask: comp\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "hardender"} :ok? false}
                      (script "swarm_handoff.sh") (str draft))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "upstream")))))
(deftest swarm-handoff-utility-cleaner-terminal-includes-specifier-and-coder
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root [["specifier" "task" "forward-only"]
                                ["coder" "task" "forward-only"]
                                ["cleaner" "task" "back-one"]
                                ["architect" "batch" "back-all"]
                                ["hardender" "batch" "forward-only"]
                                ["QA" "batch" "back-all"]])
        _ (pack-board root true "create" "--root" (str root)
                      "--name" "shim" "--type" "utility")
        _ (pack-board root true "move" "--root" (str root)
                      "--name" "shim" "--lane" "cleaner" "--caller" "handoffd")
        _ (write-file (fs/path root "slice.md") "work\n")
        _ (run {:dir root} "git" "add" "slice.md" "tasks/shim.md")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Utility work")
        ok (fs/path root "tmp" "util-ok.handoff")
        architect (fs/path root "tmp" "util-architect.handoff")
        coder-only (fs/path root "tmp" "util-coder.handoff")]
    (write-file ok (str "type: git_handoff\n"
                        "to: specifier,coder\n"
                        "priority: 50\ntask: shim\n"))
    (let [result (audit-and-submit-git-handoff
                  {:dir root :env {"SWARMFORGE_ROLE" "cleaner"}} ok)]
      (is (zero? (:exit result))))
    (write-file architect "type: git_handoff\nto: architect\npriority: 50\ntask: shim\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"} :ok? false}
                      (script "swarm_handoff.sh") (str architect))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "upstream")))
    (write-file coder-only "type: git_handoff\nto: coder\npriority: 50\ntask: shim\n")
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"} :ok? false}
                      (script "swarm_handoff.sh") (str coder-only))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "upstream")))))
