(ns abc.tools.cli-test
  (:require [abc.tools.cli :as cli]
            [clojure.test :refer [deftest is testing]]))

(def cli-options
  [["-i" "--input FILE" "input" :id :input]
   ["-h" "--help" "help"]])

(defn- usage [_summary] "Usage: tool --input FILE")

(deftest strip-double-dash-drops-single-leading-separator-test
  (is (= '("--input" "x") (cli/strip-double-dash ["--" "--input" "x"])))
  (is (= ["--input" "x"] (cli/strip-double-dash ["--input" "x"]))))

(deftest dispatch-maps-outcomes-to-exit-codes-test
  (testing "help prints usage and returns 0 without running"
    (let [ran? (atom false)
          parsed (cli/parse ["--help"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] (reset! ran? true))})]
      (is (= 0 code))
      (is (false? @ran?))))
  (testing "missing required option returns 2 and does not run"
    (let [ran? (atom false)
          parsed (cli/parse [] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage :run (fn [_] (reset! ran? true))})]
      (is (= 2 code))
      (is (false? @ran?))))
  (testing "success returns 0 and runs, run sees {:options :arguments}"
    (let [parsed (cli/parse ["--input" "x" "pos1"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [{:keys [options arguments]}]
                                             (is (= "x" (:input options)))
                                             (is (= ["pos1"] arguments)))})]
      (is (= 0 code))))
  (testing "too many positional args returns 2"
    (let [parsed (cli/parse ["--input" "x" "a" "b"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage :max-args 1 :run (fn [_] :ran)})]
      (is (= 2 code))))
  (testing "too few positional args returns 2"
    (let [parsed (cli/parse ["--input" "x"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage :min-args 1 :run (fn [_] :ran)})]
      (is (= 2 code))))
  (testing "fail? predicate true returns 1"
    (let [parsed (cli/parse ["--input" "x"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] {:bad 3})
                                      :fail? (fn [r] (pos? (:bad r)))})]
      (is (= 1 code))))
  (testing "ExceptionInfo from run returns 2"
    (let [parsed (cli/parse ["--input" "x"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] (throw (ex-info "boom" {:k 1})))})]
      (is (= 2 code)))))
