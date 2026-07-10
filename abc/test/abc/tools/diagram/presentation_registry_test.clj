(ns abc.tools.diagram.presentation-registry-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.diagram.presentation-registry :as registry]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def sample-graphs
  [{:id :one :title "One"} {:id :two :title "Two"}])

(deftest check-reports-every-stale-artifact
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          result (registry/run-with!
                  entries sample-graphs {:check? true}
                  {:dot-render #(str "dot-" (name (:id %)) "\n")
                   :svg-render (fn [g _ _] (str "svg-" (name (:id g)) "\n"))})]
      (is (false? (:ok? result)))
      (is (= 4 (count (:drifts result)))))))

(deftest invalid-second-figure-writes-neither-figure
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          result (registry/run-with!
                  entries sample-graphs {:check? false}
                  {:dot-render #(str "dot-" (name (:id %)) "\n")
                   :svg-render (fn [g _ _]
                                 (when (= :two (:id g))
                                   (throw (ex-info "bad second figure" {})))
                                 "svg-one\n")})]
      (is (false? (:ok? result)))
      (is (empty? (fs/list-dir dir))))))

(deftest successful-write-then-check-is-current
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          renderers {:dot-render #(str "dot-" (name (:id %)) "\n")
                     :svg-render (fn [g _ _]
                                   (str "svg-" (name (:id g)) "\n"))}]
      (is (:ok? (registry/run-with! entries sample-graphs
                                    {:check? false} renderers)))
      (is (:ok? (registry/run-with! entries sample-graphs
                                    {:check? true} renderers))))))

(deftest renderer-receives-a-file-compatible-temp-directory
  (with-temp-dir [dir]
    (let [entry {:id :one
                 :dot-path (str (fs/file dir "one.dot"))
                 :svg-path (str (fs/file dir "one.svg"))}
          result (registry/run-with!
                  [entry] [{:id :one}] {:check? false}
                  {:dot-render (constantly "dot\n")
                   :svg-render (fn [_ _ temp]
                                 (io/file temp "candidate.svg")
                                 "svg\n")})]
      (is (:ok? result)))))
