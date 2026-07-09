(ns abc.tools.request-set-fixture-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.request-set-resolver :as resolver]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn request-set-labels []
  (->> (file-seq (io/file "data/request-sets"))
       (filter #(.isFile %))
       (map #(.getName %))
       (filter #(.endsWith % ".json"))
       (map #(subs % 0 (- (count %) (count ".json"))))
       sort
       vec))

(deftest request-set-fixtures-carry-computed-request-set-ids-test
  (is (seq (request-set-labels)))
  (doseq [label (request-set-labels)]
    (testing label
      (let [request-set (files/read-json (str "data/request-sets/" label ".json"))]
        (is (= label (get request-set "label")))
        (is (= "request-set-v1" (get request-set "request_set_schema_version")))
        (is (nil? (get request-set "fixture_role")))
        (is (= (analysis-identity/request-set-id request-set)
               (get request-set "request_set_id")))
        (is (= (resolver/resolve-request-set label)
               request-set))
        (is (= "no-pack-v1"
               (get-in request-set ["resolved_pack_policy_label"
                                    "policy_id"])))
        (is (= (get-in request-set ["request_set_identity_object"
                                    "pack_policy_hash"])
               (get-in request-set ["resolved_pack_policy_label"
                                    "pack_policy_hash"])))
        (is (= [] (get-in request-set ["request_set_identity_object"
                                       "tokenizer_profile_hashes"])))))))

;; D6 (2026-07-09 design, widened 2026-07-10): annotation views participate in
;; request-set input_views as {"input_view_kind" "parser-ir-body-annotations-v1",
;; "policy_hash" <annotation-policy-hash>}. demo-annotation-ja is the resolved
;; fixture; the round-trip deftest above covers its request_set_id and resolver
;; equality like every other label, and input-view-kinds-schema-and-allow-list-
;; agree-test pins the schema/allow-list agreement.
(deftest annotation-input-view-resolves-in-demo-annotation-ja-test
  (let [request-set (files/read-json "data/request-sets/demo-annotation-ja.json")
        policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
        views (get-in request-set ["request_set_identity_object" "input_views"])
        ann-views (filterv #(= "parser-ir-body-annotations-v1"
                               (get % "input_view_kind"))
                           views)]
    (testing "exactly one annotation input view, carrying the ruby-gaiji-v1 hash"
      (is (= 1 (count ann-views)))
      (is (= (analysis-identity/annotation-policy-hash policy)
             (get (first ann-views) "policy_hash"))))
    (testing "the D6 two-key shape, no normalization coordinate (deferred D7)"
      (is (= #{"input_view_kind" "policy_hash"}
             (set (keys (first ann-views))))))
    (testing "the plaintext view rides alongside for the token join"
      (is (some #(= "parser-ir-plaintext-body-v1" (get % "input_view_kind"))
                views)))))

(deftest input-view-kinds-schema-and-allow-list-agree-test
  (let [schema (files/read-json "schemas/request-set.schema.json")
        variants (get-in schema ["$defs" "inputView" "oneOf"])
        schema-kinds (into #{}
                           (mapcat #(get-in % ["properties" "input_view_kind" "enum"]))
                           variants)]
    (testing "inputView is a oneOf over per-kind variants"
      (is (seq variants)))
    (testing "every kind the schema admits is exactly the resolver allow-list"
      (is (= analysis-identity/allowed-input-view-kinds schema-kinds)))))
