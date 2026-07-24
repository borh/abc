(ns abc.tools.decisions-test
  (:require [abc.tools.decisions :as d]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(defn- load-str [s]
  (let [f (str (fs/create-temp-file {:suffix ".edn"}))]
    (spit f s)
    (d/load-corpus f)))

(deftest missing-file-is-a-problem-map
  (let [{:keys [problems]} (d/load-corpus "no/such/decisions.edn")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest directory-path-is-a-problem-map
  (let [{:keys [problems]} (d/load-corpus "docs")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest malformed-edn-is-a-problem-map
  (doseq [s ["{:decisions [" "{:decisions ]}" "#=(boom)"]]
    (let [{:keys [problems]} (load-str s)]
      (is (= [:invalid-edn] (map :kind problems)) s))))

(deftest trailing-second-form-is-a-problem-map
  (let [{:keys [problems]} (load-str "{:decisions []} {:junk true}")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest empty-file-is-a-problem-map
  (let [{:keys [problems]} (load-str "")]
    (is (= [:invalid-edn] (map :kind problems)))))

(deftest single-form-corpus-loads
  (let [{:keys [corpus problems]} (load-str "{:decisions []}")]
    (is (nil? problems))
    (is (= {:decisions []} corpus))))
