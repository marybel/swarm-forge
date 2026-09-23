(ns swarmforge.handoffd-spec
  (:require [speclj.core :refer :all]))

(load-file "swarmforge/scripts/handoffd.bb")

(defn validate-header-fields! [headers sender-role]
  ((ns-resolve 'handoffd 'validate-header-fields!) headers sender-role))

(defn validate-recipient-set! [recipients]
  ((ns-resolve 'handoffd 'validate-recipient-set!) recipients))

(defn thrown-message [f]
  (try (f) nil
       (catch Exception e (.getMessage e))))

(def valid-git-handoff-headers
  {"id" "20260922T000000000000Z-card"
   "from" "coder"
   "to" "cleaner"
   "type" "git_handoff"
   "priority" "50"
   "task" "task-alpha"})

(describe "validate-header-fields!"
  (it "accepts a fully valid git_handoff header set"
    (should-not-throw (validate-header-fields! valid-git-handoff-headers "coder")))

  (it "accepts a valid note header set without a task header"
    (should-not-throw
     (validate-header-fields! (dissoc (assoc valid-git-handoff-headers "type" "note") "task")
                               "coder")))

  (it "rejects a blank id"
    (should= "missing id header"
             (thrown-message #(validate-header-fields! (dissoc valid-git-handoff-headers "id") "coder"))))

  (it "rejects an id with invalid characters"
    (should= "invalid id header"
             (thrown-message #(validate-header-fields!
                                (assoc valid-git-handoff-headers "id" "not/a valid id")
                                "coder"))))

  (it "rejects a blank sender role"
    (should= "missing from header"
             (thrown-message #(validate-header-fields! valid-git-handoff-headers ""))))

  (it "rejects a missing or invalid type"
    (should= "missing or invalid type header"
             (thrown-message #(validate-header-fields!
                                (assoc valid-git-handoff-headers "type" "bogus") "coder"))))

  (it "rejects a missing or invalid priority"
    (should= "missing or invalid priority header"
             (thrown-message #(validate-header-fields!
                                (assoc valid-git-handoff-headers "priority" "5") "coder"))))

  (it "rejects a git_handoff with no task header"
    (should= "missing task header"
             (thrown-message #(validate-header-fields!
                                (dissoc valid-git-handoff-headers "task") "coder"))))

  (it "rejects malformed batch_task_ids"
    (should= "invalid batch_task_ids header"
             (thrown-message #(validate-header-fields!
                                (assoc valid-git-handoff-headers "batch_task_ids" "not-edn[")
                                "coder"))))

  (it "rejects a batch_id with no batch_task_ids"
    (should= "invalid batch_id header"
             (thrown-message #(validate-header-fields!
                                (assoc valid-git-handoff-headers "batch_id" "20260922T000000000000Z-batch")
                                "coder")))))

(describe "validate-recipient-set!"
  (it "accepts distinct, non-blank recipients"
    (should-not-throw (validate-recipient-set! ["cleaner" "hardender"])))

  (it "rejects an empty recipient list"
    (should= "missing or empty recipient" (thrown-message #(validate-recipient-set! []))))

  (it "rejects a blank recipient"
    (should= "missing or empty recipient" (thrown-message #(validate-recipient-set! ["cleaner" ""]))))

  (it "rejects duplicate recipients"
    (should= "duplicate recipient" (thrown-message #(validate-recipient-set! ["cleaner" "cleaner"])))))

(run-specs)
