(ns swarmforge.ready-handoff-test
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [swarmforge.handoff-test-support :refer :all]))

(use-fixtures :once once-fixture)

(deftest pack-board-project-root-from-worktree-matches-handoff-lib
  (let [root (tmp-dir)
        _ (init-repo! root)
        wt (add-worktree! root "coder")
        _ (setup-project! root {"coder" "task"})
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "coder\tmaster\t%s\tsession\tCoder\tcodex\ttask\n" wt))]
    (run {:dir root} (script "pack_board.sh") "create" "--name" "HTW" "--type" "utility" "--root" (str root))
    (let [from-lib (run {:dir wt} (script "handoff_lib.bb") "project-root")
          listed (run {:dir wt} (script "pack_board.sh") "list")]
      (is (= (str (fs/canonicalize root))
             (str (fs/canonicalize (str/trim (:out from-lib))))))
      (is (str/includes? (:out listed) "HTW")))))
(deftest ready-for-next-prints-note-task-name-and-body
  ;; Given a (New Task) note in the receiver inbox
  ;; When ready_for_next runs
  ;; Then it prints TASK_NAME and the card body
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "task"})
    (make-queued-handoff! root "50_20260615T000001Z_000001_from_New_Task_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_New_Task"
                           :from "(New Task)"
                           :type "note"
                           :task "Holy Hand Grenade"
                           :body "The grenade is placed at setup.\n"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          out (:out result)]
      (is (zero? (:exit result)))
      (is (str/includes? out "FROM: (New Task)"))
      (is (str/includes? out "TYPE: note"))
      (is (str/includes? out "TASK_NAME: Holy Hand Grenade"))
      (is (str/includes? out "The grenade is placed at setup.")))))
(deftest ready-for-next-commits-operator-task-document-in-role-worktree
  (let [root (tmp-dir)
        _ (init-repo! root)
        receiver (add-worktree! root "receiver")
        document "# Utility task\n\nType: utility\n\nBuild the shim.\n"]
    (seed-operator-task-document! root receiver document)
    (let [result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          committed (run {:dir receiver}
                         "git" "show" "HEAD:tasks/Utility task.md")]
      (is (zero? (:exit result)))
      (is (= document (read-file (fs/path receiver "tasks/Utility task.md"))))
      (is (= document (:out committed))))))
(deftest ready-for-next-copies-excluded-task-document-without-committing
  (let [root (tmp-dir)
        _ (init-repo! root)
        receiver (add-worktree! root "receiver")
        document "# Utility task\n\nType: utility\n\nBuild the shim.\n"]
    (seed-operator-task-document! root receiver document)
    (write-file (fs/path root ".git/info/exclude") "/tasks/\n")
    (let [head-before (head-sha receiver)
          result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                      (script "ready_for_next.sh"))
          head-after (head-sha receiver)
          copied (fs/path receiver "tasks/Utility task.md")
          copied-text (when (fs/exists? copied) (read-file copied))]
      (is (zero? (:exit result)) (str (:err result) (:out result)))
      (is (= document copied-text)
          (str "task document in the worktree: " (pr-str copied-text)))
      (is (= head-before head-after)
          (str "HEAD moved from " head-before " to " head-after)))))
(deftest ready-for-next-batch-commits-each-operator-task-document
  (let [root (tmp-dir)
        _ (init-repo! root)
        receiver (add-worktree! root "receiver")]
    (setup-project! root {"receiver" "batch"})
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "receiver\treceiver\t%s\tsession\tReceiver\tcodex\tbatch\n"
                        receiver))
    (doseq [[priority task] [["40" "First"] ["40" "Second"]]]
      (write-file (fs/path root "tasks" (str task ".md"))
                  (str "# " task "\n\nType: utility\n\nDo " task ".\n"))
      (put-handoff! receiver "new" (str priority "_" task ".handoff")
                    {:id task
                     :from "(New Task)"
                     :to "receiver"
                     :recipient "receiver"
                     :priority priority
                     :type "note"
                     :task-id (str task "-id")
                     :task task
                     :body (str "Do " task ".")}))
    (let [result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))]
      (is (zero? (:exit result)))
      (is (str/includes? (:out result) "COUNT: 2"))
      (doseq [task ["First" "Second"]]
        (is (zero? (:exit (run {:dir receiver}
                               "git" "cat-file" "-e"
                               (str "HEAD:tasks/" task ".md")))))))))
(deftest ready-for-next-task-accepts-and-resumes-single-tasks
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "task"})
    (testing "accepts one queued task and prints task name"
      (make-queued-handoff! root "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                            {:id "20260615T000001Z_000001_from_sender"
                             :task "task-alpha"})
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                        (script "ready_for_next.sh"))
            out (:out result)
            in-process (fs/path root ".swarmforge/handoffs/inbox/in_process/50_20260615T000001Z_000001_from_sender_to_receiver.handoff")]
        (is (str/includes? out "TASK:"))
        (is (str/includes? out "TASK_NAME: task-alpha"))
        (is (fs/exists? in-process))
        (is (some? (header in-process "dequeued_at")))))
    (testing "returns existing in-process task before queued tasks"
      (make-queued-handoff! root "40_20260615T000002Z_000002_from_sender_to_receiver.handoff"
                            {:id "20260615T000002Z_000002_from_sender"
                             :priority "40"
                             :task "task-beta"})
      (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                        (script "ready_for_next.sh"))]
        (is (str/includes? (:out result) "task-alpha"))
        (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/40_20260615T000002Z_000002_from_sender_to_receiver.handoff")))))))
(deftest ready-for-next-waits-while-outbound-approval-is-active
  ;; Given sender has an outbound git_handoff pending approval
  ;; When sender asks for another task
  ;; Then no new task is dequeued from the inbox
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root)
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "sender\tmaster\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                        root (fs/path root ".worktrees/receiver")))
    (write-file (fs/path root ".swarmforge/handoffs/pending_approval/50_pending.handoff")
                "from: sender\nto: receiver\npriority: 50\ntype: git_handoff\ntask_id: task-one\ntask: task-one\ncommit: 1234567890\n\npayload\n")
    (put-handoff! root "new" "50_next.handoff"
                  {:id "next"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "task-two"
                   :task "task-two"
                   :body "next task"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "ready_for_next.sh"))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "WAITING_FOR_APPROVAL"))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_next.handoff")))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/inbox/in_process") "*.handoff"))))))
(deftest ready-for-next-waits-while-outbound-handoff-is-in-outbox
  ;; Given sender has queued a git_handoff that handoffd has not processed yet
  ;; When sender asks for another task
  ;; Then sender is still treated as busy
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root)
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "sender\tmaster\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                        root (fs/path root ".worktrees/receiver")))
    (write-file (fs/path root ".swarmforge/handoffs/outbox/50_outbound.handoff")
                "id: outbound\nfrom: sender\nto: receiver\npriority: 50\ntype: git_handoff\ntask_id: task-one\ntask: task-one\ncommit: 1234567890\n\npayload\n")
    (put-handoff! root "new" "50_next.handoff"
                  {:id "next"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "task-two"
                   :task "task-two"
                   :body "next task"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "ready_for_next.sh"))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "WAITING_FOR_APPROVAL"))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_next.handoff")))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/inbox/in_process") "*.handoff"))))))
(deftest ready-for-next-starts-next-task-after-outbound-approval-delivered
  ;; Given sender's prior git_handoff is already approved and in receiver's process
  ;; When sender asks for another task
  ;; Then the next queued task starts
  (let [root (tmp-dir)
        receiver (fs/path root ".worktrees/receiver")]
    (init-repo! root)
    (fs/create-dirs receiver)
    (setup-project! root)
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "sender\tmaster\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                        root receiver))
    (write-file (fs/path receiver ".swarmforge/handoffs/inbox/in_process/50_prior.handoff")
                "from: sender\nto: receiver\nrecipient: receiver\npriority: 50\ntype: git_handoff\ntask_id: task-one\ntask: task-one\ncommit: 1234567890\napproved: true\n\npayload\n")
    (put-handoff! root "new" "50_next.handoff"
                  {:id "next"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "task-two"
                   :task "task-two"
                   :body "next task"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
                      (script "ready_for_next.sh"))
          in-process (fs/path root ".swarmforge/handoffs/inbox/in_process/50_next.handoff")]
      (is (zero? (:exit result)))
      (is (str/includes? (:out result) "TASK_NAME: task-two"))
      (is (fs/exists? in-process))
      (is (not (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_next.handoff")))))))
(deftest handoffd-wakes-sender-after-approved-handoff-unblocks-queued-work
  ;; Given an approved sender handoff is ready to deliver and sender has queued mail
  ;; When handoffd delivers the approved handoff to the receiver
  ;; Then the receiver and the now-unblocked sender are notified
  (let [root (tmp-dir)
        bin (fs/path root "bin")
        fake-tmux (fs/path bin "tmux")
        tmux-log (fs/path root "tmux.log")
        receiver (fs/path root ".worktrees/receiver")]
    (init-repo! root)
    (setup-project! root)
    (fs/create-dirs bin)
    (write-file fake-tmux
                (str "#!/usr/bin/env bb\n"
                     "(when-let [log (System/getenv \"TMUX_LOG\")]\n"
                     "  (spit log (str (pr-str *command-line-args*) \"\\n\") :append true))\n"))
    (run {:dir root} "chmod" "+x" (str fake-tmux))
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "sender\tmaster\t%s\tsender-session\tSender\tcodex\ttask\nreceiver\treceiver\t%s\treceiver-session\tReceiver\tcodex\ttask\n"
                        root receiver))
    (fs/create-dirs receiver)
    (write-file (fs/path root ".swarmforge/tmux-socket") "/tmp/fake.sock\n")
    (write-file (fs/path root ".swarmforge/handoffs/outbox/50_approved.handoff")
                "id: approved\nfrom: sender\nto: receiver\npriority: 50\ntype: git_handoff\ntask_id: task-one\ntask: task-one\ncommit: 1234567890\napproved: true\n\npayload\n")
    (put-handoff! root "new" "50_next.handoff"
                  {:id "next"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "task-two"
                   :task "task-two"
                   :body "next task"})
    (let [result (run {:dir root
                       :env {"PATH" (str bin ":" (System/getenv "PATH"))
                             "TMUX_LOG" (str tmux-log)}}
                      "bb" (script "handoffd.bb") "--once" (str root))]
      (is (zero? (:exit result)))
      (is (fs/exists? (fs/path receiver ".swarmforge/handoffs/inbox/new/50_approved.handoff")))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_next.handoff")))
      (is (seq (submitted-texts (read-argv tmux-log) "receiver-session")))
      (is (seq (submitted-texts (read-argv tmux-log) "sender-session")))
      (is (str/includes? (read-file (fs/path root ".swarmforge/daemon/handoffd.log"))
                         "notified-unblocked-sender sender")))))
(deftest ready-for-next-batch-waits-while-outbound-approval-is-active
  ;; Given a batch-mode sender has an outbound git_handoff pending approval
  ;; When sender asks for the next batch
  ;; Then no batch is created from queued inbox work
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"sender" "batch" "receiver" "task"})
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (format "sender\tmaster\t%s\tsession\tSender\tcodex\tbatch\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                        root (fs/path root ".worktrees/receiver")))
    (write-file (fs/path root ".swarmforge/handoffs/pending_approval/50_pending.handoff")
                "from: sender\nto: receiver\npriority: 50\ntype: git_handoff\ntask_id: task-one\ntask: task-one\ncommit: 1234567890\n\npayload\n")
    (put-handoff! root "new" "50_next.handoff"
                  {:id "next"
                   :from "(New Task)"
                   :to "sender"
                   :recipient "sender"
                   :priority "50"
                   :type "note"
                   :task-id "task-two"
                   :task "task-two"
                   :body "next task"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"} :ok? false}
                      (script "ready_for_next.sh"))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "WAITING_FOR_APPROVAL"))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_next.handoff")))
      (is (empty? (fs/glob (fs/path root ".swarmforge/handoffs/inbox/in_process") "batch_*"))))))
(deftest ready-for-next-batch-groups-equal-priority-handoffs
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "batch"})
    (make-queued-handoff! root "10_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_sender" :priority "10" :task "task-a"})
    (make-queued-handoff! root "10_20260615T000002Z_000002_from_sender_to_receiver.handoff"
                          {:id "20260615T000002Z_000002_from_sender" :priority "10" :task "task-b"})
    (make-queued-handoff! root "20_20260615T000003Z_000003_from_sender_to_receiver.handoff"
                          {:id "20260615T000003Z_000003_from_sender" :priority "20" :task "task-c"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          out (:out result)
          batch-dir (->> (str/split-lines out)
                         (filter #(str/starts-with? % "BATCH: "))
                         first
                         (#(subs % 7)))]
      (is (str/includes? out "COUNT: 2"))
      (is (str/includes? out "TASK_NAME: task-a"))
      (is (str/includes? out "TASK_NAME: task-b"))
      (is (not (str/includes? out "TASK_NAME: task-c")))
      (let [lines (str/split-lines out)
            batch-i (first (keep-indexed (fn [i line] (when (str/starts-with? line "BATCH:") i)) lines))
            name-i (first (keep-indexed (fn [i line] (when (str/starts-with? line "TASK_NAME:") i)) lines))
            item-i (first (keep-indexed (fn [i line] (when (str/starts-with? line "BATCH_ITEM:") i)) lines))]
        (is (< batch-i name-i item-i))
        (is (= "TASK_NAME: task-a" (nth lines name-i))))
      (is (= 2 (count (fs/glob batch-dir "*.handoff"))))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/20_20260615T000003Z_000003_from_sender_to_receiver.handoff"))))))

(deftest ready-for-next-batch-merges-only-the-ancestry-complete-commit
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root {"sender" "task" "receiver" "batch"})
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\tbatch\n"
                              sender receiver))
        _ (write-file (fs/path sender "adaptive.md") "adaptive\n")
        _ (run {:dir sender} "git" "add" "adaptive.md")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Adaptive polling")
        adaptive (str/trim (:out (run {:dir sender} "git" "rev-parse" "HEAD")))
        _ (write-file (fs/path sender "tactical.md") "tactical\n")
        _ (run {:dir sender} "git" "add" "tactical.md")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Tactical persistence")
        tactical (str/trim (:out (run {:dir sender} "git" "rev-parse" "HEAD")))]
    (doseq [dir ["new" "in_process" "completed"]]
      (fs/create-dirs (fs/path receiver ".swarmforge/handoffs/inbox" dir)))
    (make-queued-handoff! receiver "50_adaptive.handoff"
                          {:id "adaptive" :from "sender" :to "receiver"
                           :recipient "receiver" :task-id "adaptive-id"
                           :task "adaptive-polling" :commit adaptive})
    (make-queued-handoff! receiver "50_tactical.handoff"
                          {:id "tactical" :from "sender" :to "receiver"
                           :recipient "receiver" :task-id "tactical-id"
                           :task "tactical-persistence" :commit tactical})
    (let [result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          batch-dir (->> (str/split-lines (:out result))
                         (some #(when (str/starts-with? % "BATCH: ")
                                  (subs % (count "BATCH: ")))))
          manifest (edn/read-string
                    (read-file (fs/path batch-dir "batch_manifest.edn")))
          reflog (str/split-lines
                  (:out (run {:dir receiver} "git" "reflog" "--format=%gs")))]
      (is (= tactical (:selected-commit manifest)))
      (is (= [adaptive tactical] (mapv :commit (:members manifest))))
      (is (= tactical (str/trim (:out (run {:dir receiver} "git" "rev-parse" "HEAD")))))
      (is (= 1 (count (filter #(str/starts-with? % "merge ") reflog)))))))

(deftest ready-for-next-batch-rejects-non-linear-commits-before-dequeue
  (let [root (tmp-dir)
        base (init-repo! root)
        sender-a (add-worktree! root "sender-a")
        sender-b (add-worktree! root "sender-b")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root {"sender-a" "task" "sender-b" "task" "receiver" "batch"})
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender-a\tsender-a\t%s\tsession\tA\tcodex\ttask\nsender-b\tsender-b\t%s\tsession\tB\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\tbatch\n"
                              sender-a sender-b receiver))
        _ (write-file (fs/path sender-a "a.md") "a\n")
        _ (run {:dir sender-a} "git" "add" "a.md")
        _ (run {:dir sender-a} "git" "commit" "-q" "-m" "A")
        a (head-sha sender-a)
        _ (write-file (fs/path sender-b "b.md") "b\n")
        _ (run {:dir sender-b} "git" "add" "b.md")
        _ (run {:dir sender-b} "git" "commit" "-q" "-m" "B")
        b (head-sha sender-b)]
    (doseq [dir ["new" "in_process" "completed"]]
      (fs/create-dirs (fs/path receiver ".swarmforge/handoffs/inbox" dir)))
    (make-queued-handoff! receiver "50_a.handoff"
                          {:id "a" :from "sender-a" :to "receiver" :recipient "receiver"
                           :task-id "a-id" :task "a" :commit a})
    (make-queued-handoff! receiver "50_b.handoff"
                          {:id "b" :from "sender-b" :to "receiver" :recipient "receiver"
                           :task-id "b-id" :task "b" :commit b})
    (let [result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                      (script "ready_for_next.sh"))]
      (is (= 2 (:exit result)))
      (is (str/includes? (:err result) "NON_LINEAR_BATCH"))
      (is (= base (head-sha receiver)))
      (is (= 2 (count (fs/glob (fs/path receiver ".swarmforge/handoffs/inbox/new")
                               "*.handoff"))))
      (is (empty? (fs/glob (fs/path receiver ".swarmforge/handoffs/inbox/in_process")
                           "batch_*"))))))

(deftest ready-for-next-batch-declares-and-retains-complete-commit-on-conflict
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (write-file (fs/path root "shared.txt") "base\n")
        _ (run {:dir root} "git" "add" "shared.txt")
        _ (run {:dir root} "git" "commit" "-q" "-m" "Shared base")
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root {"sender" "task" "receiver" "batch"})
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\tbatch\n"
                              sender receiver))
        _ (write-file (fs/path sender "shared.txt") "adaptive\n")
        _ (run {:dir sender} "git" "add" "shared.txt")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Adaptive polling")
        adaptive (head-sha sender)
        _ (write-file (fs/path sender "shared.txt") "adaptive and tactical\n")
        _ (run {:dir sender} "git" "add" "shared.txt")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Tactical persistence")
        tactical (str/trim (:out (run {:dir sender} "git" "rev-parse" "HEAD")))
        _ (write-file (fs/path receiver "shared.txt") "receiver structure\n")
        _ (run {:dir receiver} "git" "add" "shared.txt")
        _ (run {:dir receiver} "git" "commit" "-q" "-m" "Receiver structure")]
    (doseq [dir ["new" "in_process" "completed"]]
      (fs/create-dirs (fs/path receiver ".swarmforge/handoffs/inbox" dir)))
    (make-queued-handoff! receiver "50_adaptive.handoff"
                          {:id "adaptive" :from "sender" :to "receiver" :recipient "receiver"
                           :task-id "adaptive-id" :task "adaptive-polling" :commit adaptive})
    (make-queued-handoff! receiver "50_tactical.handoff"
                          {:id "tactical" :from "sender" :to "receiver" :recipient "receiver"
                           :task-id "tactical-id" :task "tactical-persistence" :commit tactical})
    (let [first-result (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                            (script "ready_for_next.sh"))
          batch-dir (->> (str/split-lines (:out first-result))
                         (some #(when (str/starts-with? % "BATCH: ")
                                  (subs % (count "BATCH: ")))))
          manifest (edn/read-string
                    (read-file (fs/path batch-dir "batch_manifest.edn")))
          merge-head (str/trim (:out (run {:dir receiver} "git" "rev-parse" "MERGE_HEAD")))
          resumed (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                       (script "ready_for_next.sh"))]
      (is (= 1 (:exit first-result)))
      (is (some? batch-dir))
      (is (str/includes? (:out first-result)
                         (str "SELECTED_COMMIT: " (subs tactical 0 10))))
      (is (= 2 (count (filter #(str/starts-with? % "BATCH_MEMBER: ")
                              (str/split-lines (:out first-result))))))
      (is (= tactical (:selected-commit manifest)))
      (is (= tactical merge-head))
      (is (str/includes? (:out resumed) (str "BATCH_ID: " (:batch-id manifest))))
      (is (fs/regular-file? (fs/path batch-dir "batch_manifest.edn"))))))
(deftest ready-for-next-batch-keeps-same-priority-of-one-card-type
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "batch"})
    (make-queued-handoff! root "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_sender"
                           :priority "50" :task "jump" :card-type "component"})
    (make-queued-handoff! root "50_20260615T000002Z_000002_from_sender_to_receiver.handoff"
                          {:id "20260615T000002Z_000002_from_sender"
                           :priority "50" :task "input" :card-type "QA"})
    (make-queued-handoff! root "50_20260615T000003Z_000003_from_sender_to_receiver.handoff"
                          {:id "20260615T000003Z_000003_from_sender"
                           :priority "50" :task "hhg" :card-type "component"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          out (:out result)
          batch-dir (->> (str/split-lines out)
                         (filter #(str/starts-with? % "BATCH: "))
                         first
                         (#(subs % 7)))]
      (is (str/includes? out "COUNT: 2"))
      (is (str/includes? out "TASK_NAME: jump"))
      (is (str/includes? out "TASK_NAME: hhg"))
      (is (not (str/includes? out "TASK_NAME: input")))
      (is (= 2 (count (fs/glob batch-dir "*.handoff"))))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_20260615T000002Z_000002_from_sender_to_receiver.handoff"))))))
(deftest ready-for-next-batch-does-not-mix-reverse-with-forward
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "batch"})
    (make-queued-handoff! root "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_sender"
                           :priority "50" :task "domain" :card-type "component"
                           :non-forwarding true})
    (make-queued-handoff! root "50_20260615T000002Z_000002_from_sender_to_receiver.handoff"
                          {:id "20260615T000002Z_000002_from_sender"
                           :priority "50" :task "jump" :card-type "component"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "ready_for_next.sh"))
          out (:out result)
          batch-dir (->> (str/split-lines out)
                         (filter #(str/starts-with? % "BATCH: "))
                         first
                         (#(subs % 7)))]
      (is (str/includes? out "COUNT: 1"))
      (is (str/includes? out "TASK_NAME: domain"))
      (is (not (str/includes? out "TASK_NAME: jump")))
      (is (= 1 (count (fs/glob batch-dir "*.handoff"))))
      (is (fs/exists? (fs/path root ".swarmforge/handoffs/inbox/new/50_20260615T000002Z_000002_from_sender_to_receiver.handoff"))))))
(deftest ready-for-next-task-prints-card-type-and-this-card
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root [["specifier" "task" "forward-only"]
                          ["coder" "task" "forward-only"]
                          ["cleaner" "task" "back-one"]
                          ["architect" "batch" "back-all"]
                          ["hardender" "batch" "forward-only"]
                          ["QA" "batch" "back-all"]])
    (pack-board root true "create" "--root" (str root)
                "--name" "jump" "--type" "component")
    (make-queued-handoff! root "50_20260615T000001Z_000001_from_New_Task_to_specifier.handoff"
                          {:id "20260615T000001Z_000001_from_New_Task"
                           :from "(New Task)"
                           :to "specifier"
                           :recipient "specifier"
                           :priority "50"
                           :type "note"
                           :task "jump"
                           :card-type "component"
                           :body "Specify jump.\n"})
    (let [out (:out (run {:dir root :env {"SWARMFORGE_ROLE" "specifier"}}
                         (script "ready_for_next.sh")))]
      (is (str/includes? out "CARD_TYPE: component"))
      (is (str/includes? out "THIS_CARD: next coder")))))
(deftest ready-for-next-batch-prints-card-type-and-this-card
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root [["specifier" "task" "forward-only"]
                          ["coder" "task" "forward-only"]
                          ["cleaner" "batch" "back-one"]
                          ["architect" "batch" "back-all"]
                          ["hardender" "batch" "forward-only"]
                          ["QA" "batch" "back-all"]])
    (pack-board root true "create" "--root" (str root)
                "--name" "util" "--type" "utility")
    (make-queued-handoff! root "50_20260615T000001Z_000001_from_coder_to_cleaner.handoff"
                          {:id "20260615T000001Z_000001_from_coder"
                           :from "coder"
                           :to "cleaner"
                           :recipient "cleaner"
                           :priority "50"
                           :type "note"
                           :task "util"
                           :card-type "utility"
                           :body "cleanup util\n"})
    (let [out (:out (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"}}
                         (script "ready_for_next.sh")))]
      (is (str/includes? out "CARD_TYPE: utility"))
      (is (str/includes? out "THIS_CARD: last; terminal to: specifier,coder")))
    (pack-board root true "create" "--root" (str root)
                "--name" "jump" "--type" "component")
    (make-queued-handoff! root "50_20260615T000002Z_000002_from_coder_to_cleaner.handoff"
                          {:id "20260615T000002Z_000002_from_coder"
                           :from "coder"
                           :to "cleaner"
                           :recipient "cleaner"
                           :priority "50"
                           :type "note"
                           :task "jump"
                           :card-type "component"
                           :body "cleanup jump\n"})
    (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"}} (script "done_with_current.sh"))
    (let [out (:out (run {:dir root :env {"SWARMFORGE_ROLE" "cleaner"}}
                         (script "ready_for_next.sh")))]
      (is (str/includes? out "CARD_TYPE: component"))
      (is (str/includes? out "THIS_CARD: next architect")))))
(deftest done-with-current-replaces-an-existing-completed-file
  (let [root (tmp-dir)
        name "50_retry_htw.handoff"]
    (init-repo! root)
    (setup-project! root {"receiver" "task"})
    (put-handoff! root "in_process" name
                  {:id "retry"
                   :from "(Retry)" :to "receiver" :recipient "receiver"
                   :priority "50" :type "note" :task "htw"})
    (put-handoff! root "completed" name
                  {:id "retry-old"
                   :from "(Retry)" :to "receiver" :recipient "receiver"
                   :priority "50" :type "note" :task "htw"
                   :completed-at "2026-08-26T22:45:36.178441Z"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "done_with_current.sh"))
          completed (fs/path root ".swarmforge/handoffs/inbox/completed" name)
          in-process (fs/path root ".swarmforge/handoffs/inbox/in_process" name)]
      (is (zero? (:exit result)))
      (is (str/includes? (:out result) "COMPLETED:"))
      (is (not (fs/exists? in-process)))
      (is (fs/exists? completed))
      (is (not= "2026-08-26T22:45:36.178441Z" (header completed "completed_at"))))))
(deftest done-with-current-task-completes-without-accepting-next
  ;; Given a current task and more mail in the inbox
  ;; When done_with_current runs
  ;; Then it completes the current task, leaves the next item queued, and prints MAIL_WAITING
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "task"})
    (put-handoff! root "in_process" "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                  {:id "20260615T000001Z_000001_from_sender"
                   :from "sender" :to "receiver" :recipient "receiver"
                   :priority "50" :type "git_handoff" :task "task-current"
                   :commit (head-sha root)})
    (make-queued-handoff! root "50_20260615T000002Z_000002_from_sender_to_receiver.handoff"
                          {:id "20260615T000002Z_000002_from_sender"
                           :task "task-next"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "done_with_current.sh"))
          completed (fs/path root ".swarmforge/handoffs/inbox/completed/50_20260615T000001Z_000001_from_sender_to_receiver.handoff")
          next-file (fs/path root ".swarmforge/handoffs/inbox/new/50_20260615T000002Z_000002_from_sender_to_receiver.handoff")]
      (is (str/includes? (:out result) "COMPLETED:"))
      (is (str/includes? (:out result) "MAIL_WAITING"))
      (is (not (str/includes? (:out result) "TASK_NAME: task-next")))
      (is (some? (header completed "completed_at")))
      (is (fs/exists? next-file))
      (is (nil? (header next-file "dequeued_at"))))))
(deftest done-with-current-batch-completes-without-accepting-next
  ;; Given a current batch and more mail in the inbox
  ;; When done_with_current runs
  ;; Then it completes the batch, leaves the next item queued, and prints MAIL_WAITING
  (let [root (tmp-dir)
        batch (fs/path root ".swarmforge/handoffs/inbox/in_process/batch_20260615T000001Z_000001")]
    (init-repo! root)
    (setup-project! root {"receiver" "batch"})
    (fs/create-dirs batch)
    (write-file (fs/path batch "10_20260615T000001Z_000001_from_sender_to_receiver.handoff")
                (handoff {:id "20260615T000001Z_000001_from_sender"
                          :from "sender" :to "receiver" :recipient "receiver"
                          :priority "10" :type "git_handoff" :task "task-a"
                          :commit (head-sha root)}))
    (write-file (fs/path batch "10_20260615T000002Z_000002_from_sender_to_receiver.handoff")
                (handoff {:id "20260615T000002Z_000002_from_sender"
                          :from "sender" :to "receiver" :recipient "receiver"
                          :priority "10" :type "git_handoff" :task "task-b"
                          :commit (head-sha root)}))
    (write-file (fs/path batch "batch_manifest.edn")
                (str (pr-str {:version 1 :batch-id (fs/file-name batch)}) "\n"))
    (make-queued-handoff! root "20_20260615T000003Z_000003_from_sender_to_receiver.handoff"
                          {:id "20260615T000003Z_000003_from_sender"
                           :priority "20"
                           :task "task-c"})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"}}
                      (script "done_with_current.sh"))
          completed-batch (fs/path root ".swarmforge/handoffs/inbox/completed/batch_20260615T000001Z_000001")
          next-file (fs/path root ".swarmforge/handoffs/inbox/new/20_20260615T000003Z_000003_from_sender_to_receiver.handoff")]
      (is (str/includes? (:out result) "COMPLETED_BATCH:"))
      (is (str/includes? (:out result) "MAIL_WAITING"))
      (is (not (str/includes? (:out result) "TASK_NAME: task-c")))
      (is (= 2 (count (fs/glob completed-batch "*.handoff"))))
      (is (every? #(some? (header % "completed_at"))
                  (fs/glob completed-batch "*.handoff")))
      (is (fs/regular-file? (fs/path completed-batch "batch_manifest.edn")))
      (is (fs/exists? next-file)))))
(deftest stop-handoff-daemon-stops-running-process-and-removes-pid-file
  (let [root (tmp-dir)]
    (init-repo! root)
    (fs/create-dirs (fs/path root ".swarmforge/daemon"))
    (write-file (fs/path root ".swarmforge/roles.tsv")
                (str "coder\tmaster\t" root "\tsession\tCoder\tcodex\ttask\n"))
    (write-file (fs/path root ".swarmforge/tmux-socket") "/tmp/fake.sock\n")
    (run {:dir root :ok? false}
         "sh" "-c"
         (str "bb " (script "handoffd.bb") " " root " >/dev/null 2>&1 &"))
    (Thread/sleep 1500)
    (let [pid-file (fs/path root ".swarmforge/daemon/handoffd.pid")]
      (is (fs/exists? pid-file))
      (let [pid (str/trim (read-file pid-file))
            stop (run {:dir root} (script "stop_handoff_daemon.bb") (str root))]
        (is (= 0 (:exit stop)))
        (Thread/sleep 300)
        (is (not (fs/exists? pid-file)))
        (is (not= 0 (:exit (run {:dir root :ok? false} "kill" "-0" pid))))))))
(deftest receive-and-complete-infer-role-from-worktree
  ;; Given a receiver worktree and no SWARMFORGE_ROLE
  ;; When ready_for_next then done_with_current run there
  ;; Then they infer the role and accept / complete the task
  (let [root (tmp-dir)
        _ (init-repo! root)
        wt (add-worktree! root "receiver")
        _ (setup-project! root {"receiver" "task"})
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              root wt))]
    (doseq [dir [".swarmforge/handoffs/outbox/tmp"
                 ".swarmforge/handoffs/sent"
                 ".swarmforge/handoffs/failed"
                 ".swarmforge/handoffs/inbox/new"
                 ".swarmforge/handoffs/inbox/in_process"
                 ".swarmforge/handoffs/inbox/completed"]]
      (fs/create-dirs (fs/path wt dir)))
    (make-queued-handoff! wt "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_sender"
                           :task "task-inferred"})
    (let [lib (run {:dir wt :ok? false} (script "handoff_lib.bb") "role")
          ready (run {:dir wt :ok? false} (script "ready_for_next.sh"))
          done (run {:dir wt :ok? false} (script "done_with_current.sh"))]
      (is (zero? (:exit lib)))
      (is (= "receiver" (str/trim (:out lib))))
      (is (zero? (:exit ready)))
      (is (str/includes? (:out ready) "TASK_NAME: task-inferred"))
      (is (zero? (:exit done)))
      (is (str/includes? (:out done) "COMPLETED:"))
      (is (str/includes? (:out done) "NO_TASK")))))
(deftest merge-and-process-merges-the-inbound-commit
  ;; Given a receiver worktree behind a sender commit
  ;; When merge_and_process runs with that sender and SHA
  ;; Then the receiver HEAD contains the commit
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              sender receiver))
        _ (write-file (fs/path sender "slice.md") "from sender\n")
        _ (run {:dir sender} "git" "add" "slice.md")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Sender slice")
        sha (str/trim (:out (run {:dir sender} "git" "rev-parse" "--short=10" "HEAD")))
        result (run {:dir receiver :ok? false}
                    (script "merge_and_process.sh") "sender" sha)
        merged? (run {:dir receiver :ok? false}
                     "git" "merge-base" "--is-ancestor" sha "HEAD")]
    (is (zero? (:exit result)))
    (is (str/includes? (str (:out result) (:err result)) "MERGED:"))
    (is (zero? (:exit merged?)))
    (is (fs/exists? (fs/path receiver "slice.md")))))
(deftest ready-for-next-merges-an-inbound-git-handoff
  ;; Given a receiver worktree with a queued git_handoff
  ;; When ready_for_next runs
  ;; Then it merges that commit; the agent does not run git merge
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge" "roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              sender receiver))
        _ (write-file (fs/path sender "slice.md") "from sender\n")
        _ (run {:dir sender} "git" "add" "slice.md")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Sender slice")
        sha (str/trim (:out (run {:dir sender} "git" "rev-parse" "--short=10" "HEAD")))]
    (doseq [dir [".swarmforge/handoffs/inbox/new"
                 ".swarmforge/handoffs/inbox/in_process"
                 ".swarmforge/handoffs/inbox/completed"]]
      (fs/create-dirs (fs/path receiver dir)))
    (make-queued-handoff! receiver "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                          {:id "20260615T000001Z_000001_from_sender"
                           :from "sender"
                           :to "receiver"
                           :commit sha
                           :task "merge-on-receive"
                           :body (str "merge_and_process sender " sha)})
    (let [ready (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                     (script "ready_for_next.sh"))
          merged? (run {:dir receiver :ok? false}
                       "git" "merge-base" "--is-ancestor" sha "HEAD")]
      (is (zero? (:exit ready)))
      (is (str/includes? (:out ready) "TASK_NAME: merge-on-receive"))
      (is (zero? (:exit merged?)))
      (is (fs/exists? (fs/path receiver "slice.md"))))))
(deftest merge-and-process-takes-inbound-task-docs
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              sender receiver))
        _ (write-file (fs/path sender "tasks/UiShim.md") "from sender\n")
        _ (run {:dir sender} "git" "add" "tasks/UiShim.md")
        _ (run {:dir sender} "git" "commit" "-q" "-m" "Sender task doc")
        sha (str/trim (:out (run {:dir sender} "git" "rev-parse" "--short=10" "HEAD")))
        _ (write-file (fs/path receiver "tasks/UiShim.md") "untracked local\n")
        result (run {:dir receiver :ok? false}
                    (script "merge_and_process.sh") "sender" sha)]
    (is (zero? (:exit result)))
    (is (str/includes? (str (:out result) (:err result)) "MERGED:"))
    (is (= "from sender\n" (slurp (str (fs/path receiver "tasks/UiShim.md")))))))
(deftest ready-for-next-leaves-handback-while-in-process
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              sender receiver))]
    (doseq [dir [".swarmforge/handoffs/inbox/new"
                 ".swarmforge/handoffs/inbox/in_process"
                 ".swarmforge/handoffs/inbox/completed"]]
      (fs/create-dirs (fs/path receiver dir)))
    (write-file (fs/path sender "slice.md") "from sender\n")
    (run {:dir sender} "git" "add" "slice.md")
    (run {:dir sender} "git" "commit" "-q" "-m" "Sender slice")
    (let [sha (str/trim (:out (run {:dir sender} "git" "rev-parse" "--short=10" "HEAD")))]
      (write-file
       (fs/path receiver ".swarmforge/handoffs/inbox/in_process/50_ui.handoff")
       (str "from: (New Task)\nto: receiver\npriority: 50\ntype: note\n"
            "task: Ui\ntask_id: ui-1\n\nBuild Ui\n"))
      (write-file
       (fs/path receiver ".swarmforge/handoffs/inbox/new/00_from_sender_to_receiver.handoff")
       (str "from: sender\nto: receiver\npriority: 00\ntype: git_handoff\n"
            "non-forwarding: true\ncommit: " sha "\ntask: UiShim\n\nmerge\n"))
      (let [ready (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                       (script "ready_for_next.sh"))]
        (is (zero? (:exit ready)))
        (is (str/includes? (:out ready) "TASK_NAME: Ui"))
        (is (not (str/includes? (:out ready) "TASK_NAME: UiShim")))
        (is (fs/exists? (fs/path receiver ".swarmforge/handoffs/inbox/new/00_from_sender_to_receiver.handoff")))
        (is (not (fs/exists? (fs/path receiver "slice.md"))))
        (is (fs/exists? (fs/path receiver ".swarmforge/handoffs/inbox/in_process/50_ui.handoff"))))
      (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"}}
           (script "done_with_current.sh"))
      (let [ready (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                       (script "ready_for_next.sh"))]
        (is (zero? (:exit ready)))
        (is (str/includes? (:out ready) "TASK_NAME: UiShim"))
        (is (fs/exists? (fs/path receiver "slice.md")))
        (is (not (fs/exists? (fs/path receiver ".swarmforge/handoffs/inbox/new/00_from_sender_to_receiver.handoff"))))))))
(deftest ready-for-next-merges-from-named-role-head
  (let [root (tmp-dir)
        _ (init-repo! root)
        sender (add-worktree! root "sender")
        receiver (add-worktree! root "receiver")
        _ (setup-project! root)
        _ (write-file (fs/path root ".swarmforge/roles.tsv")
                      (format "sender\tsender\t%s\tsession\tSender\tcodex\ttask\nreceiver\treceiver\t%s\tsession\tReceiver\tcodex\ttask\n"
                              sender receiver))]
    (doseq [dir [".swarmforge/handoffs/inbox/new"
                 ".swarmforge/handoffs/inbox/in_process"
                 ".swarmforge/handoffs/inbox/completed"]]
      (fs/create-dirs (fs/path receiver dir)))
    (write-file (fs/path sender "api.md") "coder api\n")
    (run {:dir sender} "git" "add" "api.md")
    (run {:dir sender} "git" "commit" "-q" "-m" "Coder API")
    (write-file (fs/path root "tasks/UiShim.md")
                "# UiShim\n\nType: component\nMerge-from: sender\n\nBuild it\n")
    (write-file
     (fs/path receiver ".swarmforge/handoffs/inbox/new/50_from_New_Task.handoff")
     (str "from: (New Task)\nto: receiver\npriority: 50\ntype: note\n"
          "task: UiShim\n\nBuild it\n"))
    (let [ready (run {:dir receiver :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                     (script "ready_for_next.sh"))]
      (is (zero? (:exit ready)))
      (is (str/includes? (:out ready) "TASK_NAME: UiShim"))
      (is (fs/exists? (fs/path receiver "api.md"))))))
(deftest done-with-current-after-reverse-copy-does-not-queue-git-handoff
  ;; Given an in-process reverse git_handoff
  ;; When done_with_current runs
  ;; Then the inbound is completed and no outbox git_handoff is written
  (let [root (tmp-dir)
        _ (init-repo! root)
        _ (setup-project! root)
        inbound (fs/path root ".swarmforge/handoffs/inbox/in_process/00_from_architect.handoff")]
    (write-file inbound (str "from: architect\nto: sender\npriority: 00\ntype: git_handoff\n"
                             "task: HTW\nnon-forwarding: true\n\nmerge\n"))
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "sender"}}
                      (script "done_with_current.sh"))
          completed (fs/path root ".swarmforge/handoffs/inbox/completed/00_from_architect.handoff")
          outbox (fs/glob (fs/path root ".swarmforge/handoffs/outbox") "*.handoff")]
      (is (zero? (:exit result)))
      (is (str/includes? (:out result) "COMPLETED:"))
      (is (fs/exists? completed))
      (is (not (fs/exists? inbound)))
      (is (empty? outbox)))))
(deftest done-with-current-archives-the-completing-role-pane
  ;; Given a current task and a pane stub
  ;; When done_with_current runs
  ;; Then the completing role's session pane is archived
  (let [root (tmp-dir)]
    (init-repo! root)
    (setup-project! root {"receiver" "task"})
    (put-handoff! root "in_process" "50_20260615T000001Z_000001_from_sender_to_receiver.handoff"
                  {:id "20260615T000001Z_000001_from_sender"
                   :from "sender" :to "receiver" :recipient "receiver"
                   :priority "50" :type "git_handoff" :task "task-current"
                   :commit (head-sha root)})
    (let [result (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"
                                       "SWARMFORGE_PANE_STUB" "receiver pane\n"}}
                      (script "done_with_current.sh"))
          pane (fs/path root ".swarmforge/sessions/receiver/pane.txt")]
      (is (zero? (:exit result)))
      (is (fs/exists? pane))
      (is (= "receiver pane\n" (read-file pane))))))
(deftest helpers-refuse-wrong-current-work-shape
  (let [root (tmp-dir)
        batch (fs/path root ".swarmforge/handoffs/inbox/in_process/batch_20260615T000001Z_000001")]
    (init-repo! root)
    (setup-project! root {"receiver" "batch"})
    (fs/create-dirs batch)
    (write-file (fs/path batch "10_20260615T000001Z_000001_from_sender_to_receiver.handoff")
                (handoff {:id "20260615T000001Z_000001_from_sender"
                          :from "sender" :to "receiver" :recipient "receiver"
                          :priority "10" :type "git_handoff" :task "task-a"
                          :commit (head-sha root)}))
    (testing "task helpers refuse an in-process batch"
      (let [ready (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                       (script "ready_for_next_task.sh"))
            done (run {:dir root :env {"SWARMFORGE_ROLE" "receiver"} :ok? false}
                      (script "done_with_current_task.sh"))]
        (is (= 2 (:exit ready)))
        (is (str/includes? (:err ready) "TASK_IN_PROCESS_IS_BATCH"))
        (is (= 2 (:exit done)))
        (is (str/includes? (:err done) "CURRENT_WORK_IS_BATCH"))))))
