(ns soranoha.za.oracle-test
  "The run-report boundary decode: external evidence is rejected before
  the oracle can consume it: duplicate keys, malformed coordinate
  values, non-hex trace keys, and stages the coordinate table does not
  cover all refuse decoding."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.za.oracle :as oracle]))

(def ^:private hex-a (apply str (repeat 64 "a")))
(def ^:private hex-b (apply str (repeat 64 "b")))

(defn- report-json
  "A minimal valid one-stage, one-work report, with optional string
  surgery applied before decoding."
  [& {:keys [replace-from replace-to]}]
  (let [s (str "{\"aozora_git_commit\":\"" (apply str (repeat 40 "c")) "\","
               "\"stages\":{\"parse\":{\"stage_id\":\"parse\","
               "\"stage_version\":\"1\",\"toolchain_id\":\"tc\"}},"
               "\"works\":{\"w_1\":{"
               "\"parser-ir\":\"" hex-a "\","
               "\"plaintext\":\"" hex-a "\","
               "\"tei\":\"" hex-a "\","
               "\"tei-validation\":\"" hex-a "\","
               "\"source_zip\":\"" hex-b "\","
               "\"source_content_hash\":\"sha256:" hex-a "\","
               "\"cached\":{\"parse\":false},"
               "\"trace_keys\":{\"parse\":\"" hex-b "\"}}}}")]
    (.getBytes ^String (if replace-from
                         (str/replace s replace-from replace-to)
                         s)
               "UTF-8")))

(defn- reject-reason [bytes]
  (try (oracle/decode-run bytes)
       :accepted
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest report-decode-round-trips-into-the-oracle-shape
  (let [run (oracle/decode-run (report-json))]
    (is (= {:parse {"stage_id" "parse" "stage_version" "1"
                    "toolchain_id" "tc"}}
           (:stage-coordinates run)))
    (is (= {"w_1" hex-b} (:zip-hashes run)))
    (is (= {"w_1" {:cached {:parse false} :trace-keys {:parse hex-b}}}
           (:results run)))
    (is (= hex-a (get-in run [:works "w_1" "tei"])))
    (testing "two decoded runs feed the invariant directly"
      (is (= [{:slug "w_1" :stage :parse :trace-key hex-b}]
             (oracle/unexplained-executions run run))))))

(deftest markdown-stage-requires-its-artifacts-and-exposes-their-changes
  (let [base (str/replace (String. ^bytes (report-json) "UTF-8") "\"parse\"" "\"markdown\"")
        decode #(oracle/decode-run (.getBytes ^String % "UTF-8"))
        with-artifacts (fn [hash]
                         (str/replace base "\"parser-ir\":"
                                      (str "\"markdown\":\"" hash "\",\"markdown-projection\":\"" hex-a "\",\"parser-ir\":")))]
    (is (thrown? clojure.lang.ExceptionInfo (decode base)))
    (is (= #{"w_1"}
           (:changed (oracle/report-artifact-delta (decode (with-artifacts hex-a))
                                                   (decode (with-artifacts hex-b))))))))

(deftest report-decode-rejects-noncontractual-evidence
  (testing "duplicate keys die at parse"
    (is (= :parse-invalid
           (reject-reason (report-json
                           :replace-from "\"cached\":"
                           :replace-to "\"cached\":{\"parse\":true},\"cached\":")))))
  (testing "non-hex trace key"
    (is (= :malformed-work-evidence
           (reject-reason (report-json
                           :replace-from (str "{\"parse\":\"" hex-b "\"}")
                           :replace-to "{\"parse\":\"not-a-hash\"}")))))
  (testing "coordinate value missing a field"
    (is (= :malformed-stage-coordinate
           (reject-reason (report-json :replace-from "\"toolchain_id\":\"tc\""
                                       :replace-to "\"toolchain\":\"tc\"")))))
  (testing "blank coordinate value"
    (is (= :malformed-stage-coordinate
           (reject-reason (report-json :replace-from "\"toolchain_id\":\"tc\""
                                       :replace-to "\"toolchain_id\":\"\"")))))
  (testing "work evidence naming a stage the table does not cover"
    (is (= :malformed-work-evidence
           (reject-reason (report-json :replace-from "\"cached\":{\"parse\":false}"
                                       :replace-to "\"cached\":{\"ghost\":false}")))))
  (testing "work evidence omitting a declared stage: an execution silently
    erased from both maps must refuse decoding, not pass as a subset"
    (is (= :malformed-work-evidence
           (reject-reason (report-json :replace-from "\"cached\":{\"parse\":false}"
                                       :replace-to "\"cached\":{}"))))
    (is (= :malformed-work-evidence
           (reject-reason (report-json
                           :replace-from (str "\"trace_keys\":{\"parse\":\"" hex-b "\"}")
                           :replace-to "\"trace_keys\":{}")))))
  (testing "non-hex artifact hash"
    (is (= :not-a-content-hash
           (reject-reason (report-json :replace-from hex-a
                                       :replace-to (str/upper-case hex-a)))))))

(deftest accountability-only-artifact-changes-are-visible
  (let [with-accountability (fn [hash]
                              (.getBytes
                               ^String (str/replace
                                        (String. ^bytes (report-json :replace-from "\"source_zip\":"
                                                                     :replace-to (str "\"source-accountability\":\"" hash "\",\"source_zip\":")) "UTF-8")
                                        "\"parse\"" "\"accountability\"")
                               "UTF-8"))
        before (oracle/decode-run (with-accountability hex-a))
        after (oracle/decode-run (with-accountability hex-b))]
    (is (= #{"w_1"} (:changed (oracle/report-artifact-delta before after))))
    (is (= #{"w_1"} (:retained (oracle/report-artifact-delta before before))))
    (is (= :not-a-content-hash (reject-reason (with-accountability "invalid"))))))
