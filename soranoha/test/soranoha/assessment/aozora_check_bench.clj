(ns soranoha.assessment.aozora-check-bench
  "Paired full-population evidence-validation benchmark without network traffic.
  Run from soranoha/: clojure -J-Xmx4g -Sdeps '{:paths [\"src\" \"test\"]}' -M
  -m soranoha.assessment.aozora-check-bench BASELINE-CLJ AOZORA EVIDENCE SOURCE PAIRS.
  BASELINE-CLJ is aozora.clj extracted from the revision being compared.
  SOURCE must contain available reliances sharing one catalog and rules snapshot.
  Retained response bytes substitute for fresh fetches only in this benchmark;
  measurements exclude network latency and cannot establish current applicability."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [soranoha.assessment.aozora :as aozora])
  (:import [java.util Arrays]
           [java.util.concurrent.atomic LongAdder]))

(defn- responses [root evidence records]
  (doseq [field ["catalog_sha256" "rules_sha256"]]
    (assert (= 1 (count (into #{} (map #(get % field)) records)))
            (str "Benchmark requires one shared " field)))
  (let [candidates (#'aozora/selection root)
        catalog (#'aozora/retained evidence (first records) "catalog_sha256")
        index (#'aozora/catalog-index catalog)]
    (reduce (fn [responses record]
              (let [assertion (#'aozora/assertion index (get candidates (get record "slug")))]
                (reduce (fn [responses [url field]]
                          (let [bytes (#'aozora/retained evidence record field)]
                            (when-let [existing (get responses url)]
                              (assert (Arrays/equals ^bytes existing ^bytes bytes)
                                      "Conflicting retained responses for one URL"))
                            (assoc responses url bytes)))
                        responses [[(:card assertion) "card_sha256"]
                                   [(:file assertion) "file_sha256"]])))
            {aozora/catalog-url catalog
             aozora/rules-url (#'aozora/retained evidence (first records) "rules_sha256")}
            records)))

(defn- measure [check root evidence records responses]
  (let [fetches (LongAdder.)
        start (System/nanoTime)
        result (check root evidence records
                      {:parallelism 4
                       :fetch (fn [url]
                                (.increment fetches)
                                (or (get responses url)
                                    (throw (ex-info "Unmapped benchmark URL" {:url url}))))})]
    {:seconds (/ (- (System/nanoTime) start) 1e9)
     :fetches (.sum fetches) :result result}))

(defn -main [baseline-file root evidence source-file pair-count]
  (load-string (str/replace-first (slurp baseline-file)
                                  "(ns soranoha.assessment.aozora"
                                  "(ns soranoha.assessment.aozora-benchmark-reference"))
  (let [reference (var-get (find-var 'soranoha.assessment.aozora-benchmark-reference/check!))
        records (get (json/read-json (slurp source-file)) "reliances")
        responses (responses root evidence records)
        expected (into {} (map (fn [r] [(get r "slug") {:state :aozora/available :reason nil}])) records)]
    (dotimes [i (parse-long pair-count)]
      (let [order (if (even? i) [:reference :current] [:current :reference])
            results (into {} (map (fn [kind]
                                    [kind (measure (if (= kind :reference) reference aozora/check!)
                                                   root evidence records responses)])) order)]
        (doseq [result (vals results)]
          (assert (= expected (:result result)) "Validation outcomes differ"))
        (assert (apply = (map :fetches (vals results))) "Fresh fetch counts differ")
        (println (json/write-json-str
                  {:pair i :order order :records (count records)
                   :results (into {} (map (fn [[k v]] [k (dissoc v :result)])) results)}))
        (flush))))
  (shutdown-agents))
