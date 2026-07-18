(ns abc.tools.parser-rq-member-test
  (:require [abc.tools.parser-rq-diagnostic-completeness :as diagnostic]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.parser-rq-core-attempt :as core]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.parser-rq-member :as member]
            [abc.tools.parser-rq-parser-ir-conformance :as parser-ir]
            [abc.tools.parser-rq-publication :as publication]
            [abc.tools.parser-rq-resource :as resource]
            [abc.tools.parser-rq-source-accountability :as source]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(def identity-ref (str "sha256:" (apply str (repeat 64 "a"))))

(defn envelope [value]
  {:value value :identity_ref identity-ref})

(deftest member-projections-delegate-to-existing-pure-analyzers
  (with-redefs [core/authenticate-index
                (fn [policy candidate index blob-reader]
                  (is (= [:core-policy :core-candidate :core-index]
                         [policy candidate index]))
                  (let [result (blob-reader {:locator "core.json"})]
                    (is (= :ok (:status result)))
                    (is (= [99 111 114 101] (vec (:bytes result)))))
                  :authenticated-core)
                core/derive-aggregate
                (fn [authenticated]
                  (is (= :authenticated-core authenticated))
                  :core-aggregate)
                core/observation-envelopes
                (fn [candidate aggregate]
                  (is (= [:core-candidate :core-aggregate]
                         [candidate aggregate]))
                  {:fatal_failures (envelope 0.0)
                   :wall_time_seconds (envelope 12.5)
                   :timeouts (envelope 0.0)})
                source/derive-source-recognition-envelope
                (fn [store manifest aggregate identity]
                  (is (= [:store :manifest :aggregate :identity]
                         [store manifest aggregate identity]))
                  (envelope 1.0))
                source/silent-drops-envelope
                (fn [store manifest identity]
                  (is (= [:store :manifest :identity] [store manifest identity]))
                  (envelope 0))
                diagnostic/derive-observation
                (fn [policy ref aggregate]
                  (is (= identity-ref ref))
                  (is (= [:diagnostic-policy :diagnostic-aggregate]
                         [policy aggregate]))
                  (envelope :invalid-diagnostic-envelope))
                parser-ir/derive-observation
                (fn [policy ref aggregate]
                  (is (= identity-ref ref))
                  (is (= [:parser-ir-policy :parser-ir-aggregate]
                         [policy aggregate]))
                  (envelope 1.0))
                publication/derive-publication-envelope
                (fn [store manifest index identity]
                  (is (= [:publication-store :publication-manifest
                          :publication-index :publication-identity]
                         [store manifest index identity]))
                  (envelope 1M))
                resource/analyze
                (fn [policy identity index records]
                  (is (= [:resource-policy :resource-identity
                          :resource-index [:resource-record]]
                         [policy identity index records]))
                  {:value 1024 :identity_ref identity-ref})
                resource/observation-envelope identity]
    (is (= {:fatal_failures (envelope 0.0)
            :wall_time_seconds (envelope 12.5)
            :timeouts (envelope 0.0)}
           (member/project-core
            {:qualification_identity_ref identity-ref
             :store {:root :core-store}
             :policy :core-policy :candidate :core-candidate
             :index :core-index
             :blob_reader (fn [store blob]
                            (is (= {:root :core-store} store))
                            (is (= {:locator "core.json"} blob))
                            {:status :ok :bytes (.getBytes "core" "UTF-8")})})))
    (is (= {:source_span_coverage (envelope 1.0)}
           (member/project-source-recognition
            {:qualification_identity_ref identity-ref
             :store :store :manifest :manifest :aggregate :aggregate
             :identity :identity})))
    (is (= {:silent_drops (envelope 0)}
           (member/project-diagnostic-gap
            {:qualification_identity_ref identity-ref
             :store :store :manifest :manifest :identity :identity})))
    (is (= {:diagnostic_completeness (envelope :invalid-diagnostic-envelope)
            :parser_ir_schema_validation (envelope 1.0)}
           (member/project-predicate-pair
            {:qualification_identity_ref identity-ref
             :diagnostic_policy :diagnostic-policy
             :diagnostic_aggregate :diagnostic-aggregate
             :parser_ir_policy :parser-ir-policy
             :parser_ir_aggregate :parser-ir-aggregate})))
    (is (= {:publication_structure (envelope 1M)}
           (member/project-publication
            {:qualification_identity_ref identity-ref
             :store :publication-store :manifest :publication-manifest
             :index :publication-index :identity :publication-identity})))
    (is (= {:peak_cgroup_memory_bytes (envelope 1024)}
           (member/project-resource
            {:qualification_identity_ref identity-ref
             :policy :resource-policy :identity :resource-identity
             :index :resource-index :records [:resource-record]})))))

(deftest project-member-enforces-operation-output-and-identity-closure
  (testing "one assigned output cannot grow an extra key"
    (with-redefs [member/project-source-recognition
                  (fn [_] {:source_span_coverage (envelope 1.0)
                           :silent_drops (envelope 0)})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (member/project-member
                    :source-recognition {:qualification_identity_ref identity-ref})))))
  (testing "an analyzer cannot emit an envelope for another candidate"
    (with-redefs [source/derive-source-recognition-envelope
                  (fn [& _] {:value 1.0 :identity_ref
                             (str "sha256:" (apply str (repeat 64 "b")))})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (member/project-member
                    :source-recognition
                    {:qualification_identity_ref identity-ref
                     :store {} :manifest {} :aggregate {} :identity {}})))))
  (testing "predicate-pair owns exactly two outputs"
    (with-redefs [member/project-predicate-pair
                  (fn [_] {:diagnostic_completeness (envelope 1.0)})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (member/project-member
                    :predicate-pair {:qualification_identity_ref identity-ref}))))))

(deftest predicate-pair-authenticates-and-folds-raw-indexes
  (let [diagnostic-index {:expected_work_ids ["work"]
                          :records [{:work_id "work" :exit_code 0
                                     :raw_diagnostics {:sha256 "diagnostic"}}]}
        parser-index {:expected_work_ids ["work"]
                      :records [{:work_id "work"
                                 :record {:sha256 "record"
                                          :locator "record.json"}}]}
        calls (atom [])]
    (with-redefs [capture/read-blob
                  (fn [_ blob]
                    (case (:sha256 blob)
                      "diagnostic" {:status :ok :bytes (.getBytes "diagnostic")}
                      "record" {:status :ok
                                :bytes (.getBytes
                                        "{\"parser_ir\":{\"sha256\":\"nested\",\"bytes\":1,\"media_type\":\"application/json\",\"locator\":\"nested.json\"}}")}
                      {:status :unavailable}))
                  diagnostic/derive-work
                  (fn [_ ref input]
                    (swap! calls conj [:diagnostic-work ref (:work_id input)
                                       (:attempt_disposition input)])
                    {:record {:work_id "work"}})
                  diagnostic/aggregate
                  (fn [_ expected records]
                    (swap! calls conj [:diagnostic-aggregate expected records])
                    :diagnostic-aggregate)
                  parser-ir/authenticate-record
                  (fn [store _ ref row]
                    (swap! calls conj [:parser-record ref (:work_id row)
                                       @(:locators store)])
                    {:work_id "work"})
                  parser-ir/aggregate
                  (fn [_ expected records]
                    (swap! calls conj [:parser-aggregate expected records])
                    :parser-aggregate)
                  diagnostic/derive-observation
                  (fn [_ _ aggregate]
                    (is (= :diagnostic-aggregate aggregate))
                    (envelope 1.0))
                  parser-ir/derive-observation
                  (fn [_ _ aggregate]
                    (is (= :parser-aggregate aggregate))
                    (envelope 1.0))]
      (is (= {:diagnostic_completeness (envelope 1.0)
              :parser_ir_schema_validation (envelope 1.0)}
             (member/project-predicate-pair
              {:qualification_identity_ref identity-ref
               :store {:root "store"}
               :diagnostic_policy {}
               :parser_ir_policy {}
               :diagnostic_index diagnostic-index
               :parser_ir_index parser-index})))
      (is (some #(= [:diagnostic-work identity-ref "work" "parsed"] %)
                @calls))
      (is (some #(and (= :parser-record (first %))
                      (= {"record" "record.json" "nested" "nested.json"}
                         (nth % 3)))
                @calls)))))

(deftest command-membership-is-closed
  (is (= #{:core :source-recognition :diagnostic-gap :predicate-pair
           :publication :resource}
         (set (keys member/projectors))))
  (is (thrown? clojure.lang.ExceptionInfo
               (member/project-member :unknown
                                      {:qualification_identity_ref identity-ref}))))

(deftest member-cli-writes-only-the-projected-value-atomically
  (let [root (fs/create-temp-dir {:prefix "parser-rq-member"})
        input (fs/file root "inputs.json")
        output (fs/file root "member.json")
        expected {:source_span_coverage (envelope 1.0)}]
    (json/write-deterministic-json-file!
     input {"qualification_identity_ref" identity-ref})
    (with-redefs [member/project-member
                  (fn [operation inputs]
                    (is (= :source-recognition operation))
                    (is (= identity-ref (:qualification_identity_ref inputs)))
                    expected)]
      (is (= "ok\n"
             (with-out-str
               (member/-main "source-recognition"
                             "--inputs" (str input)
                             "--out" (str output))))))
    (is (= {"source_span_coverage"
            {"value" 1.0 "identity_ref" identity-ref}}
           (files/read-json output)))
    (is (empty? (fs/glob root ".parser-rq-member-*.tmp")))))
