(ns soranoha.assessment.version-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.fixtures :as fixtures]
            [soranoha.assessment.graph :as graph]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.rdf :as rdf]
            [soranoha.kura.engine :as engine]))

(defn- with-store [f]
  (let [root (fs/create-temp-dir)
        store (engine/open-store! {:cas-dir (str (fs/path root "cas"))
                                   :db-path (str (fs/path root "trace.sqlite"))})]
    (try (f store)
         (finally (engine/close-store! store) (fs/delete-tree root)))))

(def ^:private captures
  {"bundle" (str "sha256:" (apply str (repeat 64 "1"))) "catalog" ["author:000001"]})

(deftest rule-version-binds-cache-and-published-basis
  (with-store
    (fn [store]
      (let [source (fixtures/assessed-source "work" ["author:000001"] captures)
            options {:observations captures :candidates {"work" ["author:000001"]}
                     :as-of "2026-09-05" :toolchain-id "unchanged-runtime"}
            before (evaluate/evaluate! store source options)
            warm (evaluate/evaluate! store source options)
            next-version (str graph/rule-version "-changed")
            expected-rule (str "jp-conservative-term/" next-version)
            changed (with-redefs-fn {#'graph/rule-version next-version}
                      #(evaluate/evaluate! store source options))
            rule-stage #(first (filter :rule? (:stages %)))
            key (records/fact-key "work/author:000001" "contribution-status")
            basis (filter #(str/starts-with? (get % "id") "jp-conservative-term/")
                          (get-in changed [:facts key :basis]))]
        (is (:cached? (rule-stage warm)))
        (is (false? (:cached? (rule-stage changed))))
        (is (not= (:trace-key (rule-stage before)) (:trace-key (rule-stage changed))))
        (is (= (get-in before [:facts key :semantic-id]) (get-in changed [:facts key :semantic-id])))
        (is (= [expected-rule] (mapv #(get % "id") basis)))
        (is (every? #(str/includes? (get % "text") expected-rule) basis))))))

(deftest missing-identity-evidence-retains-its-unavailability-reason
  (with-store
    (fn [store]
      (let [identity {"id" "soranoha-example" "basis" "Synthetic identity" "assessor" "Synthetic assessor"
                      "evidence" ["bundle"]}
            source (assoc records/empty-source
                          "observations" [{"id" "bundle" "selector" "canonical-source-bundle" "slug" "work"}]
                          "identities" [identity]
                          "findings" [(fixtures/finding
                                       "death" "person:soranoha-example" "death-year" 1900
                                       [{"kind" "identity" "ref" "soranoha-example"
                                         "fingerprint" (graph/identity-fingerprint identity captures)}])])
            view (evaluate/evaluate! store source
                                     {:observations {"bundle" graph/missing-selected-work}
                                      :candidates {} :as-of "2026-09-05" :toolchain-id "test"})]
        (is (= :assessment/unavailable (get-in view [:findings 0 :state])))
        (is (= :assessment/missing-selected-work (get-in view [:findings 0 :dependencies 0 :reason])))))))

(deftest review-premises-express-usage-without-inferring-derivation
  (with-store
    (fn [store]
      (let [source (fixtures/assessed-source "work" ["author:000001"] captures)
            view (evaluate/evaluate! store source
                                     {:observations captures :candidates {"work" ["author:000001"]}
                                      :as-of "2026-09-05" :toolchain-id "test"})
            text (rdf/nquads view {:base-iri "urn:assessment/" :mapping-profile rdf/default-mapping-profile})]
        (is (str/includes? text "<http://www.w3.org/ns/prov#qualifiedUsage>"))
        (is (not (re-find #"(?m)^<urn:assessment/claim/[^>]+> <http://www.w3.org/ns/prov#wasDerivedFrom>" text)))
        (is (re-find #"(?m)^<urn:assessment/conclusion/[^>]+> <http://www.w3.org/ns/prov#wasDerivedFrom>" text))))))
