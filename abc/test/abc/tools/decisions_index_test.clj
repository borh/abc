(ns abc.tools.decisions-index-test
  (:require [abc.tools.decisions-index :as index]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def corpus
  {:decisions
   [{:slug "b-decision" :legacy-number 2 :title "B" :status :accepted
     :date "2026-07-01" :accepted "2026-07-02"
     :validation-scope :structural :release-authority :none
     :source "s" :topics [:parser]
     :relations [{:class :lifecycle :type :supersedes :to "a-decision"}]
     :claims [{:id :c1 :kind :k :statement "s" :evidence ["test/x"]}]}
    {:slug "a-decision" :legacy-number 1 :title "A" :status :superseded
     :date "2026-06-01" :source "s" :topics [:parser :identity]
     :relations [] :claims []}]})

(deftest render-contains-topic-status-and-derived-inverse-sections
  (let [out (index/render corpus)]
    (is (str/includes? out "| [a-decision](a-decision.md) |"))
    (is (str/includes? out "superseded by [b-decision](b-decision.md)"))
    (is (str/includes? out "## Topic: parser"))
    (is (str/includes? out "## Topic: identity"))
    (is (str/includes? out "| 2 | [b-decision](b-decision.md) |")
        "legacy-number table")))

(deftest currency-detects-staleness
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (is (= [:stale-index]
           (map :kind (index/currency-problems corpus root)))
        "missing INDEX.md is stale")
    (spit (str (fs/path root "docs/adr/INDEX.md")) (index/render corpus))
    (is (empty? (index/currency-problems corpus root)))
    (spit (str (fs/path root "docs/adr/INDEX.md")) "stale")
    (is (= [:stale-index]
           (map :kind (index/currency-problems corpus root))))))
