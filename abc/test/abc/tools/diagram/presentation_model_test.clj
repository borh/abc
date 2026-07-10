(ns abc.tools.diagram.presentation-model-test
  (:require [abc.tools.diagram.presentation-model :as model]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest committed-presentation-metadata-is-total
  (is (= [] (model/problems (model/load-metadata)
                            (model/canonical-context)))))

(deftest coordinate-and-stage-metadata-are-closed
  (let [metadata (model/load-metadata)
        context (model/canonical-context)]
    (testing "a missing coordinate fails"
      (is (some #(str/includes? % "coordinate metadata mismatch")
                (model/problems
                 (update metadata :coordinates dissoc "manifest_schema_hash")
                 context))))
    (testing "an extra stage fails"
      (is (some #(str/includes? % "stage metadata mismatch")
                (model/problems
                 (assoc-in metadata [:stages :imaginary]
                           {:label "Imaginary" :role :source})
                 context))))))

(deftest reachability-follows-canonical-stage-edges
  (let [edges (:stage-edges (model/canonical-context))]
    (is (model/reachable? edges :aozora-snapshot :analysis))
    (is (not (model/reachable? edges :analysis :aozora-snapshot)))))

(deftest aggregate-backing-and-citations-must-be-canonical
  (let [context (model/canonical-context)]
    (is (seq (model/backing-problems
              context
              {:stages [:missing] :adrs [1]})))
    (is (seq (model/backing-problems
              context
              {:stages [:manifest] :adrs [30]})))
    (is (= [] (model/backing-problems
               context
               {:stages [:manifest] :adrs [1 10 23 27 28]})))))

(deftest current-parser-inset-must-resolve-the-live-path
  (let [metadata (assoc-in (model/load-metadata)
                           [:figures :publication :current-inset :path]
                           [:aozora-snapshot :parser-ir])]
    (is (some #(str/includes? % "current parser inset path")
              (model/problems metadata (model/canonical-context))))))

(deftest validated-model-throws-actionable-data
  (let [bad-metadata (update (model/load-metadata) :coordinates dissoc
                             "manifest_schema_hash")]
    (with-redefs [model/load-metadata (constantly bad-metadata)]
      (try
        (model/validated-model)
        (is false "expected invalid presentation metadata")
        (catch clojure.lang.ExceptionInfo ex
          (is (seq (:problems (ex-data ex)))))))))
