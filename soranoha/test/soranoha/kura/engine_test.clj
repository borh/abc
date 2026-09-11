(ns soranoha.kura.engine-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.kura.trace :as trace]))

(defn- temp-store! []
  (let [dir (str (fs/create-temp-dir {:prefix "kura-test"}))]
    (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                         :db-path (str (fs/path dir "trace.sqlite"))})))

(def upcase-stage
  {:stage-id "upcase"
   :stage-version "1"
   :toolchain-id "tc-a"
   :f (fn [{:keys [blob]} inputs]
        {"out" (.getBytes (.toUpperCase
                           (String. ^bytes (blob (get inputs "text")) "UTF-8"))
                          "UTF-8")})})

(deftest miss-execute-hit-cycle-test
  (let [store (temp-store!)
        input-hex (cas/put-bytes! (:cas-dir store) (.getBytes "hello" "UTF-8"))
        run1 (engine/run-stage! store upcase-stage {"text" input-hex})
        run2 (engine/run-stage! store upcase-stage {"text" input-hex})]
    (testing "first run executes, second is a pure trace hit"
      (is (false? (:cached? run1)))
      (is (true? (:cached? run2)))
      (is (= (:outputs run1) (:outputs run2))))
    (testing "output bytes landed in the CAS before the trace committed"
      (is (= "HELLO"
             (String. (cas/get-bytes (:cas-dir store)
                                     (get (:outputs run1) "out")) "UTF-8"))))
    (engine/close-store! store)))

(deftest derivation-key-completeness-test
  (let [store (temp-store!)
        input-hex (cas/put-bytes! (:cas-dir store) (.getBytes "x" "UTF-8"))
        base (engine/run-stage! store upcase-stage {"text" input-hex})]
    (testing "changing stage-version invalidates (the incomplete-key trap)"
      (is (false? (:cached? (engine/run-stage!
                             store (assoc upcase-stage :stage-version "2")
                             {"text" input-hex})))))
    (testing "changing toolchain-id invalidates"
      (is (false? (:cached? (engine/run-stage!
                             store (assoc upcase-stage :toolchain-id "tc-b")
                             {"text" input-hex})))))
    (testing "identical coordinates stay cached"
      (is (true? (:cached? (engine/run-stage!
                            store upcase-stage {"text" input-hex})))))
    (is (false? (:cached? base)))
    (engine/close-store! store)))

(deftest missing-blob-is-cache-miss-test
  (let [store (temp-store!)
        input-hex (cas/put-bytes! (:cas-dir store) (.getBytes "y" "UTF-8"))
        run1 (engine/run-stage! store upcase-stage {"text" input-hex})
        out-hex (get (:outputs run1) "out")]
    (fs/delete (cas/blob-path (:cas-dir store) out-hex))
    (testing "trace hit with missing output blob re-executes"
      (let [run2 (engine/run-stage! store upcase-stage {"text" input-hex})]
        (is (false? (:cached? run2)))
        (is (some? (cas/get-bytes (:cas-dir store) out-hex)))))
    (engine/close-store! store)))

(deftest determinism-monitor-test
  (let [store (temp-store!)
        input-hex (cas/put-bytes! (:cas-dir store) (.getBytes "z" "UTF-8"))
        flaky-counter (atom 0)
        flaky {:stage-id "flaky" :stage-version "1" :toolchain-id "tc"
               :f (fn [_ _]
                    {"out" (.getBytes (str "result-" (swap! flaky-counter inc))
                                      "UTF-8")})}]
    (engine/run-stage! store flaky {"text" input-hex})
    ;; Force a second execution of the same key by deleting its output blob.
    (let [out-hex (-> (trace/lookup (:trace store)
                                    (trace/derivation-key flaky {"text" input-hex}))
                      (get "out"))]
      (fs/delete (cas/blob-path (:cas-dir store) out-hex)))
    (engine/run-stage! store flaky {"text" input-hex})
    (testing "divergent re-execution is a recorded determinism violation"
      (is (= 1 (count (trace/determinism-violations (:trace store))))))
    (engine/close-store! store)))
