(ns soranoha.snh.conformance-test
  "Table-driven conformance tests over the FROZEN vectors (spec section 11).
  The vector files under resources/snh/vectors are the frozen objects; these
  tests recompute every claim from the stored bytes — they never regenerate."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.schema :as schema]
            [soranoha.snh.sign :as sign]))

(defn- vector-bytes ^bytes [name]
  (with-open [in (io/input-stream (io/resource (str "snh/vectors/" name)))]
    (.readAllBytes in)))

(defn- vector-json [name]
  (json/read-json (String. (vector-bytes name) "UTF-8")))

(def expected (delay (vector-json "expected.json")))

;; --- section 11 item 2/3: decode accepts and ids ---------------------------

(deftest accept-vectors-decode-to-their-frozen-ids
  (doseq [{:strs [file type id]} (get @expected "accept")]
    (testing file
      (let [{got-id :id} (decode/decode type (vector-bytes file))]
        (is (= id got-id))))))

(deftest reject-vectors-fail-with-the-frozen-reason
  (doseq [{:strs [file type reason]} (get @expected "reject")]
    (testing file
      (let [outcome (try (decode/decode type (vector-bytes file))
                         :accepted
                         (catch clojure.lang.ExceptionInfo e
                           (name (:reason (ex-data e)))))]
        (is (= reason outcome))))))

(deftest duplicate-key-rejected-at-parse-before-schema-validation
  ;; F142: the duplicate-key vector is otherwise schema-VALID content, so a
  ;; :parse-invalid outcome proves the parser rejected it before schema
  ;; validation ever ran.
  (let [outcome (try (decode/decode "release-manifest"
                                    (vector-bytes "release-manifest-duplicate-key.json"))
                     :accepted
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))]
    (is (= :parse-invalid outcome))))

(deftest boundary-decode-applies-only-to-the-four-protocol-types
  (is (thrown? clojure.lang.ExceptionInfo
               (decode/decode "tei-validation" (.getBytes "{}" "UTF-8")))))

;; --- section 11 item 4: signatures and cross-role table --------------------

(def sig-vectors (delay (vector-json "signature-vectors.json")))

(defn- pinned-keys []
  {:release [(get-in @sig-vectors ["keys" "release" "pub"])]
   :governance [(get-in @sig-vectors ["keys" "governance" "pub"])]})

(deftest frozen-signatures-verify-and-are-deterministic
  (doseq [k ["manifest" "event"]]
    (testing k
      (let [{:strs [message sig signer]} (get @sig-vectors k)
            seed (sign/hex->bytes (get-in @sig-vectors ["keys" signer "seed"]))
            sig-bytes (sign/hex->bytes sig)]
        (is (= 64 (alength sig-bytes)))
        ;; Ed25519 is deterministic: re-signing reproduces the frozen bytes
        (is (= sig (hash/bytes->hex (sign/sign seed message))))
        (is (sign/verify? (get-in @sig-vectors ["keys" signer "pub"])
                          message sig-bytes))))))

(deftest frozen-messages-match-the-domain-separated-encodings
  (let [{:strs [manifest event]} @sig-vectors]
    (is (= (get manifest "message")
           (sign/manifest-message (get manifest "manifest_id"))))
    (is (= (get event "message")
           (sign/event-message (get event "event_hex"))))
    ;; the message subjects are the frozen artifact ids
    (let [m (decode/decode "release-manifest"
                           (vector-bytes (get manifest "artifact")))
          e (decode/decode "governance-event"
                           (vector-bytes (get event "artifact")))]
      (is (= (get manifest "manifest_id") (:hex m)))
      (is (= (get event "event_hex") (:hex e))))))

(deftest cross-role-table-f126
  (doseq [{:strs [case type message sig expect]} (get @sig-vectors "cross_role")]
    (testing case
      (is (= expect
             (sign/verify-signed? (pinned-keys) type message
                                  (sign/hex->bytes sig)))))))

(deftest invalid-pinned-configurations-rejected
  (let [pub-r (get-in @sig-vectors ["keys" "release" "pub"])
        pub-g (get-in @sig-vectors ["keys" "governance" "pub"])]
    (testing "overlapping roles"
      (is (thrown? clojure.lang.ExceptionInfo
                   (sign/validate-pinned-keys! {:release [pub-r]
                                                :governance [pub-r]}))))
    (testing "un-roled flat configuration"
      (is (thrown? clojure.lang.ExceptionInfo
                   (sign/validate-pinned-keys! {:keys [pub-r pub-g]}))))
    (testing "missing role"
      (is (thrown? clojure.lang.ExceptionInfo
                   (sign/validate-pinned-keys! {:release [pub-r]}))))
    (testing "empty role set"
      (is (thrown? clojure.lang.ExceptionInfo
                   (sign/validate-pinned-keys! {:release [pub-r]
                                                :governance []}))))
    (testing "the fixture configuration is valid"
      (is (= (pinned-keys) (sign/validate-pinned-keys! (pinned-keys)))))))

(deftest fingerprints-are-over-decoded-raw-key-bytes
  (doseq [[role k] (get @sig-vectors "keys")]
    (testing role
      (is (= (get k "fingerprint")
             (hash/sha256-bytes (sign/hex->bytes (get k "pub"))))))))

;; --- section 11 item 5: byte-exact .pub and releases/HEAD fixtures ---------

(deftest pub-and-head-fixtures-are-exactly-65-bytes
  (doseq [[file hex] [["fixture-release.pub"
                       (get-in @sig-vectors ["keys" "release" "pub"])]
                      ["fixture-governance.pub"
                       (get-in @sig-vectors ["keys" "governance" "pub"])]
                      ["releases-HEAD-zero" sign/zero-head-hex]
                      ["releases-HEAD-example"
                       (get-in @sig-vectors ["manifest" "manifest_id"])]]]
    (testing file
      (let [bs (vector-bytes file)]
        (is (= 65 (alength bs)))
        (is (= hex (sign/parse-hex64-lf bs)))
        (is (java.util.Arrays/equals bs (sign/hex64-lf-bytes hex)))))))

(deftest hex64-lf-rejects-malformed-files
  (doseq [[label bs] [["missing LF" (.getBytes (str sign/zero-head-hex "0") "US-ASCII")]
                      ["64 bytes (no LF)" (.getBytes sign/zero-head-hex "US-ASCII")]
                      ["uppercase hex" (.getBytes (str (apply str (repeat 64 "A")) "\n") "US-ASCII")]
                      ["66 bytes" (.getBytes (str sign/zero-head-hex "\n\n") "US-ASCII")]]]
    (testing label
      (is (thrown? clojure.lang.ExceptionInfo (sign/parse-hex64-lf bs))))))

;; --- cross-artifact consistency of the fixture family ----------------------

(deftest fixture-family-is-cross-consistent
  (let [manifest (:value (decode/decode "release-manifest"
                                        (vector-bytes "release-manifest-valid.json")))
        snapshot (decode/decode "assessment-snapshot"
                                (vector-bytes "assessment-snapshot-valid.json"))
        report (decode/decode "admission-report"
                              (vector-bytes "admission-report-valid.json"))]
    (testing "manifest admission ids name the fixture evidence artifacts"
      (is (= (:id snapshot) (get-in manifest ["admission" "assessment_snapshot"])))
      (is (= (:id report) (get-in manifest ["admission" "admission_report"]))))
    (testing "report binds the snapshot and matches admission field-for-field (F89)"
      (is (= (:id snapshot) (get (:value report) "assessment_snapshot")))
      (doseq [field ["policy_hash" "inclusion_rule_id" "inclusion_rule_hash"]]
        (is (= (get-in manifest ["admission" field])
               (get (:value report) field)))))
    (testing "admitted/excluded/quarantined partition the snapshot candidates"
      (let [candidates (set (map #(get % "slug")
                                 (get (:value snapshot) "candidates")))
            admitted (get (:value report) "admitted")
            excluded (map #(get % "slug") (get (:value report) "excluded"))
            quarantined (map #(get % "slug") (get (:value report) "quarantined"))
            all (concat admitted excluded quarantined)]
        (is (= candidates (set all)))
        (is (= (count all) (count (set all))))))
    (testing "works = admitted minus withdrawn"
      (is (= (get (:value report) "admitted")
             (mapv #(get % "slug") (get manifest "works"))))
      (is (= [] (get manifest "withdrawn"))))
    (testing "genesis explicit values (F85)"
      (is (= sign/zero-head-hex (get manifest "prev_manifest")))
      (is (nil? (get manifest "governance_event"))))
    (testing "the amendment amends the frozen withdrawal event"
      (let [withdrawal (decode/decode "governance-event"
                                      (vector-bytes "governance-event-withdrawal-valid.json"))
            amendment (:value (decode/decode "governance-event"
                                             (vector-bytes "governance-event-amendment-valid.json")))]
        (is (= (:id withdrawal)
               (get-in amendment ["entries" 0 "amends"])))))))

;; --- the schemas themselves are valid 2020-12 schemas ----------------------

(deftest frozen-schemas-are-valid-json-schemas
  (doseq [[type _] schema/schema-resources]
    (testing type
      (is (map? (schema/schema-for type)))
      (is (string? (schema/schema-file-hash type))))))
