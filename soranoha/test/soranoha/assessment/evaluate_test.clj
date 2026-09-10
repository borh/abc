(ns soranoha.assessment.evaluate-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.fixtures :as fixtures]
            [soranoha.assessment.graph :as graph]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.snapshot :as snapshot]
            [soranoha.kura.engine :as engine]))

(defn with-store [f]
  (let [root (fs/create-temp-dir)
        store (engine/open-store! {:cas-dir (str (fs/path root "cas"))
                                   :db-path (str (fs/path root "trace.sqlite"))})]
    (try (f store)
         (finally (engine/close-store! store) (fs/delete-tree root)))))

(def captures {"bundle" (str "sha256:" (apply str (repeat 64 "1"))) "catalog" ["author:000001"]})
(def candidate-map {"work" ["author:000001"] "independent" ["author:000002"]})
(def options {:observations captures :candidates candidate-map
              :as-of "2026-09-05" :toolchain-id "test-runtime"})
(defn source [] (fixtures/assessed-source "work" ["author:000001"] captures))
(defn status [view slug]
  (get-in (:value (snapshot/encode view))
          ["candidates" (if (= slug "independent") 0 1) "work_assessment" "status"]))
(defn snapshot-value [view] (:value (snapshot/encode view)))
(defn failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest closed-canonical-source-boundary
  (is (= (source) (:value (records/encode (source)))))
  (doseq [[label mutate] [["unknown predicate" #(assoc-in % ["findings" 0 "fact" "predicate"] "nationality")]
                          ["unknown key" #(assoc % "approved" true)]
                          ["unknown projection" #(assoc-in % ["findings" 0 "premises" 0 "projection"] "code")]]]
    (testing label (is (= :assessment-schema-invalid (failure #(records/encode (mutate (source))))))))
  (is (= :parse-invalid (failure #(records/decode (.getBytes "{\"schema\":1,\"schema\":2}" "UTF-8")))))
  (is (= :noncanonical (failure #(records/decode (.getBytes (str " " (String. ^bytes (:bytes (records/encode records/empty-source)) "UTF-8")) "UTF-8")))))
  (is (= :dangling-assessment-control
         (failure #(records/encode (assoc records/empty-source "controls"
                                          [{"id" "withdraw" "kind" "withdrawal" "target" "missing"}]))))))

(deftest incremental-transitions-equal-clean-evaluation
  (with-store
    (fn [store]
      (let [initial (source)
            alternate (update initial "findings" conj
                              (assoc (last (get initial "findings")) "id" "alternative"))
            first-withdrawn (assoc alternate "controls"
                                   [{"id" "w1" "kind" "withdrawal" "target" (get (last (get initial "findings")) "id")}])
            all-withdrawn (update first-withdrawn "controls" conj
                                  {"id" "w2" "kind" "withdrawal" "target" "alternative"})
            transitions [[initial options "public-domain"]
                         [alternate options "public-domain"]
                         [first-withdrawn options "public-domain"]
                         [all-withdrawn options "not-evaluated"]
                         [initial (assoc-in options [:observations "catalog"] ["author:000001" "translator:000003"]) "not-evaluated"]
                         [initial (assoc-in options [:observations "bundle"] (str "sha256:" (apply str (repeat 64 "2")))) "not-evaluated"]
                         [initial (assoc options :as-of "2027-09-05") "public-domain"]]]
        (doseq [[records inputs expected] transitions]
          (let [warm (evaluate/evaluate! store records inputs)]
            (is (= expected (status warm "work")))
            (is (= "not-evaluated" (status warm "independent")))
            (with-store
              (fn [clean]
                (is (= (snapshot-value warm)
                       (snapshot-value (evaluate/evaluate! clean records inputs))))))))
        (let [one (evaluate/evaluate! store initial options)
              two (evaluate/evaluate! store initial (assoc options :as-of "2028-01-01"))]
          (is (= (snapshot-value one) (snapshot-value two)))
          (is (every? :cached? (:stages two))))))))

(deftest conflicts-and-cycles-fail-the-entire-view
  (with-store
    (fn [store]
      (let [conflict (update (source) "findings" conj
                             (fixtures/finding "conflict" "unselected" "work-type" "film" []))
            conflict (update conflict "findings" conj
                             (fixtures/finding "other" "unselected" "work-type" "photo" []))]
        (is (= :conflicting-assessment-support
               (failure #(evaluate/evaluate! store conflict options)))))
      (let [key (records/fact-key "person:000001" "death-year")
            cyclic (assoc records/empty-source "findings"
                          [(fixtures/finding "cycle" "person:000001" "death-year" 1900
                                             [{"kind" "fact" "ref" key
                                               "fingerprint" (apply str (repeat 64 "0"))}])])]
        (is (= :assessment-cycle (failure #(evaluate/evaluate! store cyclic options))))))))

(deftest strict-evidence-premises-and-semantic-opt-in
  (with-store
    (fn [store]
      (let [death (fixtures/finding "death" "person:000001" "death-year" 1900 [])
            base (assoc records/empty-source "findings" [death])
            result (get (:facts (evaluate/evaluate! store base options)) (get death "fact"))
            premise {"kind" "fact" "ref" (get death "fact")
                     "fingerprint" (graph/premise-fingerprint result "evidence-version")}
            child (fixtures/finding "dependent" "person:000002" "death-year" 1901 [premise])
            strict (assoc base "findings" [death child])
            changed (assoc-in strict ["findings" 0 "basis"] "Corrected citation")
            value-premise (assoc premise "projection" "value" "rationale" "Only established year affects this synthetic calculation."
                                 "fingerprint" (graph/premise-fingerprint result "value"))
            semantic (assoc-in changed ["findings" 1 "premises"] [value-premise])]
        (is (= :assessment/unavailable (:state (get (:facts (evaluate/evaluate! store changed options)) (get child "fact")))))
        (is (= :assessment/available (:state (get (:facts (evaluate/evaluate! store semantic options)) (get child "fact")))))
        (is (= :unjustified-semantic-premise
               (failure #(records/encode (assoc-in semantic ["findings" 1 "premises" 0] (dissoc value-premise "rationale"))))))))))

(defn arithmetic-source []
  (let [root (source)
        conditions [["death" "person:000001" "death-year" 1967]
                    ["attribution" "work/author:000001" "attribution-form" "real-name"]
                    ["wartime" "work/author:000001" "no-wartime-addition" true]
                    ["type" "work" "work-type" "non-film-non-photo"]
                    ["publication" "work" "publication-timing" "posthumous"]
                    ["chain" "work" "derivative-chain" []]]]
    (assoc root "findings"
           (into [(assoc (first (get root "findings"))
                         "effective_date" "2010-01-01" "reviewed_at" "2010-01-01")]
                 (map (fn [[id subject predicate value]]
                        (assoc (fixtures/finding id subject predicate value [])
                               "effective_date" "2010-01-01" "reviewed_at" "2010-01-01")))
                 conditions))))

(deftest conservative-term-rule-has-an-explicit-time-floor
  (with-store
    (fn [store]
      (let [source (arithmetic-source)
            before (evaluate/evaluate! store source (assoc options :as-of "2018-12-28"))
            crossing (evaluate/evaluate! store source (assoc options :as-of "2018-12-29"))
            later (evaluate/evaluate! store source options)]
        (is (= "not-evaluated" (status before "work")))
        (is (= "public-domain" (status crossing "work")))
        (is (= "2018-12-29" (get-in (snapshot-value crossing)
                                    ["candidates" 1 "work_assessment" "effective_date"])))
        (is (= (snapshot-value crossing) (snapshot-value later)))
        (is (every? :cached? (:stages later)))
        (doseq [[label index value] [["screen failure is not proof of protection" 1 1968]
                                     ["unestablished attribution" 2 "other"]
                                     ["wartime addition not discharged" 3 false]
                                     ["photo excluded" 4 "photo"]
                                     ["publication timing unknown" 5 "unknown"]
                                     ["underlying work unassessed" 6 ["underlying"]]]]
          (testing label
            (let [view (evaluate/evaluate! store (assoc-in source ["findings" index "value"] value) options)]
              (is (= "not-evaluated" (status view "work"))))))))))

(deftest minting-is-an-explicit-source-identity-and-cache-is-disposable
  (with-store
    (fn [store]
      (let [ids ["author:000001" "translator:soranoha-example"]
            source (fixtures/assessed-source "work" ids captures)]
        (is (= :missing-minted-person (failure #(evaluate/evaluate! store source options))))
        (let [source (assoc source "identities"
                            [{"id" "soranoha-example" "assessor" "Synthetic reviewer"
                              "basis" "Synthetic identity evidence" "evidence" ["bundle"]}])
              identity (first (get source "identities"))
              source (update-in source ["findings" 0 "premises"] conj
                                {"kind" "identity" "ref" (get identity "id")
                                 "fingerprint" (graph/identity-fingerprint identity captures)})
              view (evaluate/evaluate! store source options)]
          (is (= "public-domain" (status view "work")))
          (fs/delete-tree (:cas-dir store))
          (is (= (snapshot-value view)
                 (snapshot-value (evaluate/evaluate! store source options)))))))))

(deftest structural-cycles-fail-before-cache-execution
  (let [calls (atom 0)
        cycle-key (records/fact-key "person:000001" "death-year")
        cycle (assoc records/empty-source "findings"
                     [(fixtures/finding "cycle" "person:000001" "death-year" 1900
                                        [{"kind" "fact" "ref" cycle-key
                                          "fingerprint" (apply str (repeat 64 "0"))}])])]
    (with-redefs [engine/run-stage! (fn [& _] (swap! calls inc))]
      (is (= :assessment-cycle (failure #(evaluate/evaluate! nil cycle options)))))
    (is (zero? @calls))))

(deftest identity-revision-requires-renewed-completeness-support
  (with-store
    (fn [store]
      (let [identity {"id" "soranoha-example" "assessor" "Synthetic reviewer"
                      "basis" "Synthetic identity evidence" "evidence" ["bundle"]}
            source (-> (fixtures/assessed-source "work" ["translator:soranoha-example"] captures)
                       (assoc "identities" [identity])
                       (update-in ["findings" 0 "premises"] conj
                                  {"kind" "identity" "ref" "soranoha-example"
                                   "fingerprint" (graph/identity-fingerprint identity captures)}))]
        (is (= "public-domain" (status (evaluate/evaluate! store source options) "work")))
        (is (= "not-evaluated"
               (status (evaluate/evaluate! store
                                           (assoc-in source ["identities" 0 "basis"] "Revised identity evidence")
                                           options) "work")))))))

(deftest missing-selected-edition-is-local-unavailability
  (with-store
    (fn [store]
      (let [inputs (assoc options :observations {"bundle" graph/missing-selected-work
                                                 "catalog" graph/missing-selected-work}
                          :candidates {"independent" ["author:000002"]})
            view (evaluate/evaluate! store (source) inputs)
            complete (get (:facts view) (records/fact-key "work" "contribution-set"))]
        (is (= :assessment/unavailable (:state complete)))
        (is (= ["independent"] (mapv #(get % "slug") (get (snapshot-value view) "candidates"))))
        (is (= :invalid-captured-observation
               (failure #(evaluate/evaluate! store (source)
                                             (assoc-in inputs [:observations "bundle"] nil)))))))))

(deftest revised-citations-reuse-the-term-calculation
  (with-store
    (fn [store]
      (let [source (arithmetic-source)
            original (evaluate/evaluate! store source options)
            corrected (evaluate/evaluate! store (assoc-in source ["findings" 1 "basis"] "Corrected synthetic death-year citation") options)
            key (records/fact-key "work/author:000001" "contribution-status")]
        (is (= (get-in original [:facts key :semantic-id])
               (get-in corrected [:facts key :semantic-id])))
        (is (not= (get-in original [:facts key :basis-id])
                  (get-in corrected [:facts key :basis-id])))
        (is (every? :cached? (filter :rule? (:stages corrected))))
        (is (= (get-in original [:facts (records/fact-key "independent" "work-status") :semantic-id])
               (get-in corrected [:facts (records/fact-key "independent" "work-status") :semantic-id])))))))

(deftest reviewed-public-domain-cannot-bypass-term-premises
  (doseq [[subject predicate] [["work/author:000001" "contribution-status"]
                               ["work" "work-status"]]]
    (is (= :assessment-schema-invalid
           (failure #(records/encode
                      (assoc records/empty-source "findings"
                             [(fixtures/finding "unsupported" subject predicate "public-domain" [])])))))))

(deftest restrictive-reviewed-statuses-remain-explicit-findings
  (with-store
    (fn [store]
      (doseq [[subject predicate] [["work/author:000001" "contribution-status"]
                                   ["work" "work-status"]]
              reviewed-status ["in-copyright" "undetermined"]]
        (let [review (fixtures/finding "review" subject predicate reviewed-status [])
              complete (first (get (source) "findings"))
              input (assoc (source) "findings" [complete review])
              view (evaluate/evaluate! store input options)
              candidate (get-in (snapshot-value view) ["candidates" 1])
              projected (if (= predicate "work-status")
                          (get candidate "work_assessment")
                          (first (get candidate "contributions")))]
          (is (= reviewed-status (get projected "status")))
          (is (= :conflicting-assessment-support
                 (failure #(evaluate/evaluate! store (update (source) "findings" conj review) options)))))))))
(deftest supersession-targets-and-rule-identities-are-unambiguous
  (let [source (assoc records/empty-source "findings"
                      (mapv #(fixtures/finding % "person:000001" "death-year" 1900 []) ["a" "b" "c"]))
        ab {"id" "ab" "kind" "supersession" "target" "a" "replacement" "b"}
        ac {"id" "ac" "kind" "supersession" "target" "a" "replacement" "c"}
        ba {"id" "ba" "kind" "supersession" "target" "b" "replacement" "a"}]
    (doseq [controls [[ab ac ba] [ac ab ba] [ba ab ac] [ab ac]]]
      (is (= :duplicate-supersession-target
             (failure #(records/encode (assoc source "controls" controls))))))
    (doseq [controls [[ab ba] [ba ab]]]
      (is (= :supersession-cycle (failure #(records/encode (assoc source "controls" controls))))))
    (doseq [id ["jp-conservative-term/1" "jp-conservative-term/2"]]
      (is (= :assessment-schema-invalid
             (failure #(records/encode (assoc-in source ["findings" 0 "id"] id))))))))

(deftest assessment-state-does-not-accept-neighboring-domain-vocabulary
  (doseq [state ["available" :available :aozora/available :validation/passed]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid assessment state"
                          (graph/semantic-value {:state state}))))
  (doseq [reason [:aozora/missing-selected-work :validation/failed]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid assessment reason"
                          (graph/reason->wire reason)))))
