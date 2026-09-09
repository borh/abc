(ns soranoha.snh.governance-event-prepare-test
  "The online half of the offline governance ceremony: turning candidate
  event content into the exact bytes the governance key signs.

  Every rejection here is one that would otherwise surface only after the
  owner had opened a governance medium, and every normalization here is a
  byte difference that would otherwise have invalidated the signature that
  session produced."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.main :as main]))

(defn- vector-bytes ^bytes [name]
  (with-open [in (io/input-stream (io/resource (str "snh/vectors/" name)))]
    (.readAllBytes in)))

(defn- prepare
  "Runs the subcommand over `candidate` in a fresh directory and returns
  {:report <parsed JSON> :bytes <written bytes>}."
  [^bytes candidate]
  (let [dir (fs/create-temp-dir {:prefix "governance-event-prepare"})
        in (str (fs/path dir "candidate.json"))
        out (str (fs/path dir "event.json"))]
    (try
      (fs/write-bytes in candidate)
      (let [report (json/read-json
                    (with-out-str
                      (main/governance-event-prepare! {:event in :out out})))]
        {:report report :bytes (fs/read-all-bytes out)})
      (finally (fs/delete-tree dir)))))

(defn- rejection-reason [^bytes candidate]
  (try (prepare candidate)
       :accepted
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))

(deftest canonical-candidate-is-reproduced-byte-for-byte
  (let [canonical (vector-bytes "governance-event-withdrawal-valid.json")
        {:keys [report bytes]} (prepare canonical)]
    (is (= (seq canonical) (seq bytes))
        "canonical input must not be altered")
    (is (true? (get report "input_was_canonical")))
    (is (= (hash/sha256-bytes canonical) (get report "sha256")))
    (is (= (str "snh:1:governance-event:" (get report "sha256")) (get report "event")))
    (is (= "withdrawal" (get report "kind")))
    (is (= 1 (get report "entries")))))

(deftest key-order-and-trailing-newline-are-decided-here-not-offline
  ;; Same content as the valid withdrawal vector, written the way a person
  ;; writes it: kind before entries, an indented body, and a trailing
  ;; newline from the editor. Signing these bytes produces a signature that
  ;; the chain transactor rejects, and the rejection arrives only after the
  ;; offline session is over.
  (let [canonical (vector-bytes "governance-event-withdrawal-valid.json")
        candidate (.getBytes (str "{\n"
                                  "  \"kind\": \"withdrawal\",\n"
                                  "  \"schema\": \"snh-governance-event/1\",\n"
                                  "  \"entries\": [\n"
                                  "    {\n"
                                  "      \"statement\": \"Withdrawn at documented rights-holder request.\",\n"
                                  "      \"slug\": \"kumo_no_ito_000879_92\",\n"
                                  "      \"reason_code\": \"takedown-request\"\n"
                                  "    }\n"
                                  "  ]\n"
                                  "}\n")
                             "UTF-8")
        {:keys [report bytes]} (prepare candidate)]
    (is (= (seq canonical) (seq bytes))
        "the written bytes are the canonicalization, not the candidate")
    (is (false? (get report "input_was_canonical"))
        "the report must say the candidate file is not the file to sign")
    (is (= (hash/sha256-bytes canonical) (get report "sha256")))
    (is (not= (hash/sha256-bytes candidate) (get report "sha256"))
        "the candidate's own sha256 is not the one the ceremony signs")))

(deftest content-the-transactor-would-reject-is-rejected-before-signing
  (doseq [[label candidate reason]
          [["duplicate object key"
            (str "{\"entries\":[{\"reason_code\":\"rights\",\"slug\":\"a\","
                 "\"statement\":\"x\"}],\"kind\":\"withdrawal\","
                 "\"kind\":\"withdrawal\",\"schema\":\"snh-governance-event/1\"}")
            :parse-invalid]
           ["non-integral number"
            (str "{\"entries\":[{\"reason_code\":\"rights\",\"slug\":\"a\","
                 "\"statement\":1.5}],\"kind\":\"withdrawal\","
                 "\"schema\":\"snh-governance-event/1\"}")
            :non-integral-number]
           ["reason code outside the enumeration"
            (str "{\"entries\":[{\"reason_code\":\"because\",\"slug\":\"a\","
                 "\"statement\":\"x\"}],\"kind\":\"withdrawal\","
                 "\"schema\":\"snh-governance-event/1\"}")
            :schema-invalid]
           ["withdrawal entry carrying amends"
            (String. (vector-bytes "governance-event-invalid-withdrawal-with-amends.json") "UTF-8")
            :schema-invalid]
           ["entries not sorted by slug"
            (str "{\"entries\":[{\"reason_code\":\"rights\",\"slug\":\"b\",\"statement\":\"x\"},"
                 "{\"reason_code\":\"rights\",\"slug\":\"a\",\"statement\":\"y\"}],"
                 "\"kind\":\"withdrawal\",\"schema\":\"snh-governance-event/1\"}")
            :entries-not-sorted-unique]
           ["one slug withdrawn twice"
            (str "{\"entries\":[{\"reason_code\":\"rights\",\"slug\":\"a\",\"statement\":\"x\"},"
                 "{\"reason_code\":\"rights\",\"slug\":\"a\",\"statement\":\"y\"}],"
                 "\"kind\":\"withdrawal\",\"schema\":\"snh-governance-event/1\"}")
            :entries-not-sorted-unique]]]
    (testing label
      (is (= reason (rejection-reason (.getBytes ^String candidate "UTF-8")))))))

(deftest a-rejected-candidate-writes-no-event-file
  ;; The owner must not carry a stale file from an earlier attempt, so a
  ;; rejection leaves nothing behind at the output path.
  (let [dir (fs/create-temp-dir {:prefix "governance-event-prepare-reject"})
        in (str (fs/path dir "candidate.json"))
        out (str (fs/path dir "event.json"))]
    (try
      (fs/write-bytes in (.getBytes "{\"kind\":\"withdrawal\"}" "UTF-8"))
      (is (thrown? clojure.lang.ExceptionInfo
                   (main/governance-event-prepare! {:event in :out out})))
      (is (not (fs/exists? out)))
      (finally (fs/delete-tree dir)))))

(deftest the-subcommand-reports-its-rejection-reason-to-a-shell
  ;; The ceremony runs this from a terminal, so the reason has to reach
  ;; stderr with a failing exit rather than a stack trace.
  (let [dir (fs/create-temp-dir {:prefix "governance-event-prepare-cli"})
        in (str (fs/path dir "candidate.json"))
        out (str (fs/path dir "event.json"))]
    (try
      (fs/write-bytes in (.getBytes (str "{\"entries\":[{\"reason_code\":\"because\","
                                         "\"slug\":\"a\",\"statement\":\"x\"}],"
                                         "\"kind\":\"withdrawal\","
                                         "\"schema\":\"snh-governance-event/1\"}")
                                    "UTF-8"))
      (let [{:keys [exit err]}
            (process/sh {:out :string :err :string}
                        (str (fs/path (System/getProperty "java.home") "bin" "java"))
                        "-cp" (System/getProperty "java.class.path")
                        "clojure.main" "-m" "soranoha.main"
                        "governance-event-prepare" "--event" in "--out" out)]
        (is (= 1 exit))
        (is (str/includes? err ":reason :schema-invalid"))
        (is (not (str/includes? err "Execution error"))))
      (finally (fs/delete-tree dir)))))
