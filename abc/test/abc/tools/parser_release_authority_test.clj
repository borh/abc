(ns abc.tools.parser-release-authority-test
  "The release-parser-identity authentication boundary: integrity (both content
  refs recompute from the record's own bytes) PLUS approval (the Accepted
  release-parser-identity-approval decision binds this exact candidate_ref,
  schema_version, and record path). Neither alone authenticates."
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-release-authority :as authority]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(def record-path "data/release-parser-identity-v1.edn")
(def decisions-path "docs/adr/decisions.edn")

(def recorded-ab-aozora-sha
  "sha256:7f75b8f94de9170bf913e6081790ee20e741526027003414034a97ce8a0451dd")

;; The identical content-ref recipe the record's producer minted with: keyword
;; keys are stringified before JCS (jcs throws on keyword-keyed maps).
(defn- canonical-value [v]
  (walk/postwalk
   (fn [x]
     (cond
       (keyword? x) (name x)
       (map? x) (into {} (map (fn [[k vv]] [(if (keyword? k) (name k) k) vv])) x)
       :else x))
   v))

(defn- ref-of [v]
  (hash/format-sha256 (hash/sha256-json-jcs (canonical-value v))))

(defn- read-record []
  (edn/read-string (slurp record-path)))

(defn- fake-sha [seed]
  (hash/format-sha256 (apply str (take 64 (cycle seed)))))

(defn- temp-path [name]
  (str (fs/file (fs/create-temp-dir {:prefix "parser-release-authority-test"}) name)))

(defn- write-temp-edn! [name value]
  (let [path (temp-path name)]
    (spit path (pr-str value))
    path))

(defn- authenticate-problems
  "Call authority/authenticate opts, requiring it to throw. Returns the thrown
  ex-info's :problems vector, or throws if authenticate unexpectedly succeeded."
  [opts]
  (let [outcome (try {:ok (authority/authenticate opts)}
                     (catch clojure.lang.ExceptionInfo error
                       {:problems (:problems (ex-data error))}))]
    (if (contains? outcome :ok)
      (throw (ex-info "authenticate unexpectedly succeeded" {:result (:ok outcome)}))
      (:problems outcome))))

(defn- decisions-corpus []
  (edn/read-string (slurp decisions-path)))

(defn- corpus-with-target [f]
  (update (decisions-corpus) :decisions
          (fn [records]
            (mapv (fn [record]
                    (if (= authority/release-qualification-slug (:slug record))
                      (f record)
                      record))
                  records))))

;; --- positive: the committed decision-bound record authenticates ------------

(deftest authenticate-projects-bound-record-test
  (testing "returns the record's coordinates when the decision binds it"
    (let [r (authority/authenticate {:release_parser_identity_path record-path
                                     :decisions_path decisions-path})]
      (is (= "ab-aozora" (get-in r [:qualification-identity :aat_adapter])))
      (is (= (:candidate_ref (read-record)) (:candidate-ref r)))
      (is (= authority/release-qualification-slug (:slug (:decision r))))
      (is (= :accepted (:status (:decision r))))
      (is (= recorded-ab-aozora-sha
             (:sha256 (first (filter #(= "ab-aozora" (:name %))
                                     (get-in r [:executable-provenance :executables]))))))
      (is (= (hash/format-sha256 (hash/sha256-file record-path))
             (get-in r [:authority-hashes :record-file]))))))

;; --- integrity: a tampered content ref is rejected --------------------------

(deftest authenticate-recomputes-and-rejects-tampered-ref-test
  (testing "an asserted content ref that does not recompute from the record is rejected"
    (let [tampered (assoc (read-record) :qualification_identity_ref (fake-sha "ab"))
          path (write-temp-edn! "release-parser-identity.edn" tampered)
          problems (authenticate-problems {:release_parser_identity_path path
                                           :decisions_path decisions-path})]
      (is (some #(= :qualification-identity-ref-mismatch (:kind %)) problems)))))

;; --- binding: integrity alone is not enough (Blocker-1 guard) ---------------

(deftest authenticate-rejects-record-the-decision-does-not-bind-test
  (testing "a shape-valid, self-consistent record the decision does not bind is rejected"
    ;; New executable + qualification hashes, with self-refs RECOMPUTED so the
    ;; record passes integrity; the unchanged decision still binds the committed
    ;; candidate_ref, so approval fails.
    (let [base (read-record)
          new-qi (assoc (:qualification_identity base)
                        :mapping_hash (fake-sha "cd")
                        :parser_ir_schema_hash (fake-sha "ef"))
          rebuilt (-> base
                      (assoc :executables
                             [{:name "ab-aozora" :sha256 (fake-sha "12")}
                              {:name "ab-aat-to-parser-ir" :sha256 (fake-sha "34")}])
                      (assoc :qualification_identity new-qi))
          rebuilt (assoc rebuilt :qualification_identity_ref (ref-of new-qi))
          rebuilt (assoc rebuilt :candidate_ref (ref-of (dissoc rebuilt :candidate_ref)))
          path (write-temp-edn! "release-parser-identity.edn" rebuilt)
          problems (authenticate-problems {:release_parser_identity_path path
                                           :decisions_path decisions-path})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authentication failed"
                            (authority/authenticate {:release_parser_identity_path path
                                                     :decisions_path decisions-path})))
      (is (empty? (filter #(#{:qualification-identity-ref-mismatch
                              :candidate-ref-mismatch} (:kind %))
                          problems))
          "integrity passes — the rebuilt record's self-refs recompute")
      (is (some #(= :decision-does-not-bind-record-candidate-ref (:kind %)) problems)
          "approval fails — the decision binds a different candidate_ref"))))

;; --- approval: a non-accepted / non-publication decision is rejected ---------

(deftest authenticate-rejects-non-accepted-or-non-publication-decision-test
  (testing "a superseded decision is not accepted"
    (let [path (write-temp-edn! "decisions.edn"
                                (corpus-with-target #(assoc % :status :superseded)))
          problems (authenticate-problems {:release_parser_identity_path record-path
                                           :decisions_path path})]
      (is (some #(and (= :decision-not-accepted (:kind %)) (= :superseded (:status %)))
                problems))))
  (testing "a non-publication release authority is rejected"
    (doseq [wrong [:development :none]]
      (let [path (write-temp-edn! "decisions.edn"
                                  (corpus-with-target #(assoc % :release-authority wrong)))
            problems (authenticate-problems {:release_parser_identity_path record-path
                                             :decisions_path path})]
        (is (some #(and (= :release-authority-not-publication (:kind %))
                        (= wrong (:release-authority %)))
                  problems))))))

;; --- decisions corpus problems propagate ------------------------------------

(deftest malformed-decisions-corpus-is-rejected-test
  (let [path (temp-path "decisions.edn")]
    (spit path (str (slurp decisions-path) " {:extra true}"))
    (let [problems (authenticate-problems {:release_parser_identity_path record-path
                                           :decisions_path path})]
      (is (some #(= :invalid-decisions-corpus (:kind %)) problems))
      (is (some #(re-find #"exactly one EDN form" (:message %)) problems)))))

(deftest missing-release-parser-identity-decision-is-rejected-test
  (let [path (write-temp-edn! "decisions.edn"
                              (update (decisions-corpus) :decisions
                                      (fn [records]
                                        (vec (remove #(= authority/release-qualification-slug
                                                         (:slug %))
                                                     records)))))
        problems (authenticate-problems {:release_parser_identity_path record-path
                                         :decisions_path path})]
    (is (some #(= :missing-decision (:kind %)) problems))
    (is (some #(re-find #"release-parser-identity-approval" (:message %)) problems))))

;; --- an unreadable record is a fatal authentication failure -----------------

(deftest unreadable-record-is-rejected-test
  (let [problems (authenticate-problems {:release_parser_identity_path "no/such/record.edn"
                                         :decisions_path decisions-path})]
    (is (some #(= :invalid-release-parser-identity (:kind %)) problems))))

;; --- a non-map / malformed-shape record fails closed, not with a raw --------
;; --- ClassCastException / IllegalArgumentException --------------------------

(deftest non-map-record-fails-closed-test
  (testing "a record file whose EDN parses to a non-map value is rejected with :problems"
    (doseq [contents [[1 2 3] "nope" 42 :keyword]]
      (let [path (write-temp-edn! "release-parser-identity.edn" contents)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authentication failed"
                              (authority/authenticate {:release_parser_identity_path path
                                                       :decisions_path decisions-path}))
            (str "contents: " (pr-str contents)))
        (let [problems (authenticate-problems {:release_parser_identity_path path
                                               :decisions_path decisions-path})]
          (is (some #(= :invalid-release-parser-identity (:kind %)) problems)
              (str "contents: " (pr-str contents))))))))

(deftest non-sequential-executables-fails-closed-test
  (testing "a map record whose :executables is a non-sequential scalar is rejected with :problems"
    (let [malformed (assoc (read-record) :executables "not-a-sequence")
          path (write-temp-edn! "release-parser-identity.edn" malformed)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authentication failed"
                            (authority/authenticate {:release_parser_identity_path path
                                                     :decisions_path decisions-path})))
      (let [problems (authenticate-problems {:release_parser_identity_path path
                                             :decisions_path decisions-path})]
        (is (some #(= :invalid-release-parser-identity (:kind %)) problems))))))
