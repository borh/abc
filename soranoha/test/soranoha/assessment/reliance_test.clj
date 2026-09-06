(ns soranoha.assessment.reliance-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.fixtures :as fixtures]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.snapshot :as snapshot]
            [soranoha.kura.engine :as engine]
            [soranoha.snh.decode :as decode]
            [soranoha.za.scaffold :as scaffold]))

(defn- with-store [f]
  (let [dir (fs/create-temp-dir)
        store (engine/open-store! {:cas-dir (str (fs/path dir "cas"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})]
    (try (f store) (finally (engine/close-store! store) (fs/delete-tree dir)))))

(defn- declaration [slug]
  {"slug" slug "source_content_hash" (str "sha256:" (apply str (repeat 64 "1")))
   "source_revision" (apply str (repeat 40 "2"))
   "observed_at" "2026-09-05" "decision_date" "2026-09-05"
   "basis" "Synthetic owner reliance on a captured expired-work assertion."
   "catalog_sha256" (apply str (repeat 64 "3"))
   "card_sha256" (apply str (repeat 64 "4"))
   "file_sha256" (apply str (repeat 64 "5"))
   "rules_sha256" (apply str (repeat 64 "6")) "exception" nil})

(def ^:private options
  {:observations {} :candidates {"work" ["author:000001"] "other" ["author:000002"]}
   :as-of "2026-09-06" :toolchain-id "reliance-test"
   :reliance-observations {"work" {:state :aozora/available :reason nil}
                           "other" {:state :aozora/available :reason nil}}})

(defn- input [] (assoc records/empty-source "reliances" [(declaration "work")]))
(defn- reason [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest declarations-have-closed-structure-and-distinct-dates
  (is (= (input) (:value (records/encode (input)))))
  (doseq [[label path value expected]
          [["unknown field" ["reliances" 0 "approved"] true :assessment-schema-invalid]
           ["invalid revision" ["reliances" 0 "source_revision"] "main" :assessment-schema-invalid]
           ["empty exception" ["reliances" 0 "exception"] "" :assessment-schema-invalid]
           ["false calendar day" ["reliances" 0 "observed_at"] "2026-02-30" :invalid-assessment-date]
           ["decision precedes observation" ["reliances" 0 "decision_date"] "2026-09-04" :reliance-decision-before-observation]]]
    (testing label (is (= expected (reason #(records/encode (assoc-in (input) path value)))))))
  (is (= :duplicate-reliance-slug
         (reason #(records/encode (update (input) "reliances" conj (declaration "work"))))))
  (with-store
    (fn [store]
      (is (= :future-reliance-decision
             (reason #(evaluate/evaluate! store (input) (assoc options :as-of "2026-09-04"))))))))

(deftest source-reliance-is-not-an-independent-copyright-fact
  (with-store
    (fn [store]
      (let [view (evaluate/evaluate! store (input) options)
            projected (:value (snapshot/encode view))
            candidate (last (get projected "candidates"))]
        (is (= "snh-assessment-snapshot/2" (get projected "schema")))
        (is (= #{"slug" "reliance"} (set (keys candidate))))
        (is (= "relied-upon" (get-in candidate ["reliance" "status"])))
        (is (= "2026-09-05" (get-in candidate ["reliance" "decision_date"])))
        (is (= "not-evaluated" (get-in projected ["candidates" 0 "work_assessment" "status"])))
        (is (every? #(= :assessment/unavailable (:state %)) (vals (:facts view)))))
      (is (= :assessment-schema-invalid
             (reason #(records/encode (assoc records/empty-source "findings"
                                             [(fixtures/finding "direct" "work" "work-status" "public-domain" [])]))))))))

(deftest independent-source-uses-current-snapshot-format
  (with-store
    (fn [store]
      (let [view (evaluate/evaluate! store records/empty-source options)
            expected (decode/encode
                      "assessment-snapshot"
                      {"schema" "snh-assessment-snapshot/2"
                       "candidates"
                       (mapv (fn [[slug ids]]
                               {"slug" slug "work_assessment" scaffold/not-evaluated
                                "contributions" (mapv #(assoc scaffold/not-evaluated "contribution_id" %) ids)})
                             (sort-by key (:candidates options)))})]
        (is (= (:hex expected) (:hex (snapshot/encode view))))
        (is (= (:hex expected)
               (:hex (snapshot/encode (evaluate/evaluate! store (assoc records/empty-source "reliances" []) options)))))))))

(deftest current-evidence-exceptions-and-restrictions-prevent-reliance
  (with-store
    (fn [store]
      (doseq [[label source opts expected]
              [["missing current acquisition" (input) (dissoc options :reliance-observations) "missing-reliance-observation"]
               ["current public file removed" (input)
                (assoc-in options [:reliance-observations "work"] {:state :aozora/unavailable :reason :aozora/http-status})
                "http-status"]
               ["owner exception" (assoc-in (input) ["reliances" 0 "exception"] "Contradictory evidence requires review.") options "recorded-exception"]
               ["selected edition removed" (input) (update options :candidates dissoc "work") "missing-selected-work"]]]
        (testing label
          (let [view (evaluate/evaluate! store source opts)]
            (is (= "unavailable" (get-in view [:reliances "work" "status"])))
            (is (= expected (get-in view [:reliances "work" "reason"])))
            (when-not (contains? (:candidates opts) "work")
              (is (= ["other"] (mapv #(get % "slug") (get-in (snapshot/encode view) [:value "candidates"]))))))))
      (doseq [[subject predicate] [["work" "work-status"] ["work/author:000001" "contribution-status"]]
              status ["in-copyright" "undetermined"]]
        (let [source (assoc (input) "findings" [(fixtures/finding "restrictive" subject predicate status [])])
              view (evaluate/evaluate! store source options)]
          (is (= "restrictive-independent-assessment" (get-in view [:reliances "work" "reason"]))))))))

(deftest reliance-invalidation-follows-only-real-inputs
  (with-store
    (fn [store]
      (let [source (update (input) "reliances" conj (declaration "other"))
            cold (evaluate/evaluate! store source options)
            warm (evaluate/evaluate! store source (assoc options :as-of "2027-01-01"))
            changed (assoc-in source ["reliances" 0 "basis"] "Revised explanation for work only.")
            revised (evaluate/evaluate! store changed options)
            unrelated-restriction (assoc source "findings"
                                         [(fixtures/finding "unrelated" "unselected" "work-status" "in-copyright" [])])
            unrelated (evaluate/evaluate! store unrelated-restriction options)
            misses (filter #(and (:reliance-slug %) (not (:cached? %))) (:stages revised))]
        (is (= (:hex (snapshot/encode cold)) (:hex (snapshot/encode warm))))
        (is (every? :cached? (:stages warm)))
        (is (= #{"work"} (set (map :reliance-slug misses))))
        (is (= (:reliances cold) (:reliances unrelated)))
        (is (every? :cached? (filter :reliance-slug (:stages unrelated))))
        (with-store
          (fn [fresh]
            (let [clean (evaluate/evaluate! fresh changed options)]
              (is (= (:hex (snapshot/encode revised)) (:hex (snapshot/encode clean)))))))))))
