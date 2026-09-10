(ns soranoha.assessment.graph-test
  "The evaluation on its own. Every assertion here runs with no store, which is
  the property the namespace exists for: the logic that decides publication
  admissibility can be read and exercised without one, and staging is what the
  store is for."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.fixtures :as fixtures]
            [soranoha.assessment.graph :as graph]
            [soranoha.assessment.records :as records]
            [soranoha.kura.engine :as engine]
            [soranoha.snh.schema :as snh-schema]
            [soranoha.snh.schema-walk :as walk]))

(def ^:private captures
  {"bundle" (str "sha256:" (apply str (repeat 64 "1"))) "catalog" ["author:000001"]})

(def ^:private candidates {"work" ["author:000001"] "independent" ["author:000002"]})

(def ^:private inputs
  {:observations captures :candidates candidates :as-of "2026-09-05"
   :rule graph/pure-rule})

(defn- source [] (fixtures/assessed-source "work" ["author:000001"] captures))

(defn- minted-source
  "A contribution set resting on a minted person identity, which is the premise
  kind whose grounding observations become dependency edges."
  []
  (let [identity {"id" "soranoha-example" "assessor" "Synthetic reviewer"
                  "basis" "Synthetic identity evidence" "evidence" ["bundle"]}]
    (-> (fixtures/assessed-source "work" ["translator:soranoha-example"] captures)
        (assoc "identities" [identity])
        (update-in ["findings" 0 "premises"] conj
                   {"kind" "identity" "ref" "soranoha-example"
                    "fingerprint" (graph/identity-fingerprint identity captures)}))))

(defn- resolve-source [source options]
  (graph/validate-view! source (:observations options) (:as-of options))
  (graph/validate-acyclic! source (:candidates options))
  (graph/resolve! source options))

(defn- with-store [f]
  (let [root (fs/create-temp-dir)
        store (engine/open-store! {:cas-dir (str (fs/path root "cas"))
                                   :db-path (str (fs/path root "trace.sqlite"))})]
    (try (f store) (finally (engine/close-store! store) (fs/delete-tree root)))))

(deftest a-work-is-decided-with-no-store-in-sight
  (let [resolved (resolve-source (source) inputs)
        work (get-in resolved [:facts (records/fact-key "work" "work-status")])]
    (is (= :assessment/available (:state work)))
    (is (= "public-domain" (:value work)))
    (testing "and a candidate with no findings at all is decided too, as absent"
      (let [absent (get-in resolved [:facts (records/fact-key "independent" "work-status")])]
        (is (= :assessment/unavailable (:state absent)))
        (is (= :assessment/unavailable-contribution-completeness (:reason absent)))))
    (testing "every authored finding is reviewed, not only those a candidate reaches"
      (is (= (set (map #(get % "id") (get (source) "findings")))
             (set (keys (:findings resolved))))))
    (testing "and the order the facts resolved in is recorded rather than inferred"
      (is (= (set (keys (:facts resolved))) (set (:order resolved))))
      (is (= (count (:facts resolved)) (count (:order resolved)))))))

(deftest staging-adds-two-identifiers-and-decides-nothing
  ;; The characterization gate for the split. A snapshot-byte comparison would
  ;; not cover it: stage-result! stages only the semantic value and the basis,
  ;; so :dependencies never reaches the snapshot and a provenance defect would
  ;; pass such a gate unchanged. This compares the whole resolved result.
  (with-store
    (fn [store]
      (let [staged (:facts (evaluate/evaluate! store (source)
                                               {:observations captures :candidates candidates
                                                :as-of "2026-09-05" :toolchain-id "graph-test"}))
            resolved (:facts (resolve-source (source) inputs))]
        (is (= (set (keys resolved)) (set (keys staged))))
        (is (= resolved
               (into {} (map (fn [[key result]] [key (dissoc result :semantic-id :basis-id)]))
                     staged)))
        (is (every? :semantic-id (vals staged)))
        (is (every? :basis-id (vals staged)))))))

(deftest a-derived-conclusion-carries-the-observations-it-rests-on
  ;; What a snapshot cannot show. The work's status is derived from a
  ;; contribution set that rests on a minted identity, and the identity's own
  ;; grounding is the evidence observation it was minted over. If that edge is
  ;; dropped the conclusion still reads public-domain and the snapshot bytes do
  ;; not move; what is lost is the ability to trace it back.
  (let [resolved (resolve-source (minted-source) inputs)
        work (get-in resolved [:facts (records/fact-key "work" "work-status")])
        observations (fn observations [edges]
                       (mapcat (fn [edge]
                                 (if-let [id (:observation edge)]
                                   [id]
                                   (observations (:dependencies edge))))
                               edges))]
    (is (= :assessment/available (:state work)))
    (is (= ["bundle"] (distinct (observations (:dependencies work)))))
    (testing "and the review that consumed it names the same evidence"
      (let [review (get-in resolved [:findings "complete"])
            identity-edge (first (filter #(= "identity" (get-in % [:premise "kind"]))
                                         (:dependencies review)))]
        (is (= [{:observation "bundle" :state :assessment/available :reason nil}]
               (:dependencies identity-edge)))))))

(deftest the-rule-is-a-seam-rather-than-a-store-call
  (let [calls (atom [])
        rule (fn [key inputs compute]
               (swap! calls conj (get key "predicate"))
               (graph/pure-rule key inputs compute))
        resolved (resolve-source (source) (assoc inputs :rule rule))]
    (is (= ["contribution-status"] @calls)
        "only the derived term predicate reaches the rule")
    (is (= "public-domain"
           (:value (get-in resolved [:facts (records/fact-key "work/author:000001"
                                                              "contribution-status")]))))))

(deftest one-cycle-check-decides-whether-the-recursion-terminates
  ;; graph/resolve! has no cycle guard of its own, so this is the check that
  ;; keeps it from recurring forever. Its failure is what a caller sees.
  (let [key (records/fact-key "person:000001" "death-year")
        cyclic (assoc records/empty-source "findings"
                      [(fixtures/finding "cycle" "person:000001" "death-year" 1900
                                         [{"kind" "fact" "ref" key
                                           "fingerprint" (apply str (repeat 64 "0"))}])])]
    (is (= :assessment-cycle
           (try (graph/validate-acyclic! cyclic candidates) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn- snapshot-statuses
  "The assessment statuses a published snapshot may carry, read off the schema
  a verifier enforces rather than listed here."
  []
  (into #{} (mapcat #(get-in % ["properties" "status" "enum"]))
        (walk/nodes (snh-schema/schema-for "assessment-snapshot"))))

(deftest a-status-nobody-has-classified-restricts-rather-than-permits
  (is (not (graph/restrictive? graph/permitting-status))
      "the one status a work may be published under")
  (testing "every other status the snapshot vocabulary carries stands in the way"
    (let [statuses (snapshot-statuses)]
      (is (contains? statuses graph/permitting-status))
      (doseq [status (sort (disj statuses graph/permitting-status))]
        (is (graph/restrictive? status) status))))
  (testing "including one the vocabulary does not carry yet, which is the point"
    (is (graph/restrictive? "orphan-work")))
  (testing "and an absent value is neither, because the fact carries no assessment"
    (is (not (graph/restrictive? nil)))))
