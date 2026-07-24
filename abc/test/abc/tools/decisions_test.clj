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

;; --- shape schema -----------------------------------------------------------

(def valid-record
  {:slug "example-decision" :legacy-number 1
   :title "Example" :status :accepted
   :date "2026-07-24" :accepted "2026-07-24"
   :validation-scope :structural :release-authority :none
   :source "docs/superpowers/specs/example.md"
   :topics [:governance]
   :relations []
   :claims [{:id :c1 :kind :structural-invariant
             :statement "Something holds."
             :evidence ["test/abc/tools/decisions_test.clj"]}]})

(defn- shape-of [record]
  (d/shape-problems {:decisions [record]} "decisions.edn"))

(deftest valid-record-has-no-shape-problems
  (is (empty? (shape-of valid-record)))
  (is (empty? (shape-of (dissoc valid-record :source)))
      "source is optional (several legacy records predate it)")
  (is (empty? (shape-of (dissoc valid-record :legacy-number)))
      "slug-native records have no legacy number"))

(deftest shape-rejections
  (doseq [[label bad] {"bad status" (assoc valid-record :status :acceptedd)
                       "bad date" (assoc valid-record :date "2026-7-24")
                       "impossible date" (assoc valid-record :date "2026-02-31")
                       "bad slug" (assoc valid-record :slug "Bad_Slug")
                       "legacy number outside frozen set"
                       (assoc valid-record :legacy-number 44)
                       "never-used number 36"
                       (assoc valid-record :legacy-number 36)
                       "misspelled lifecycle type"
                       (assoc valid-record :relations
                              [{:class :lifecycle :type :depend-on :to "x"}])
                       "misspelled supersedes"
                       (assoc valid-record :relations
                              [{:class :lifecycle :type :supercedes :to "x"}])
                       "unknown relation class"
                       (assoc valid-record :relations
                              [{:class :informative :type :extends :to "x"}])
                       "annotation shadowing a lifecycle type"
                       (assoc valid-record :relations
                              [{:class :annotation :type :supersedes :to "x"}])
                       "claim id not cN"
                       (assoc-in valid-record [:claims 0 :id] :one)
                       "duplicate claim ids"
                       (assoc valid-record :claims
                              [{:id :c1 :kind :k :statement "a"
                                :evidence ["test/x"]}
                               {:id :c1 :kind :k :statement "b"
                                :evidence ["test/x"]}])}]
    (is (seq (shape-of bad)) label)
    (is (every? #(= :invalid-shape (:kind %)) (shape-of bad)) label)))

(deftest corpus-level-shape-rejections
  (let [two (fn [f] {:decisions [valid-record (f valid-record)]})]
    (is (seq (d/shape-problems (two identity) "decisions.edn"))
        "duplicate slug")
    (is (seq (d/shape-problems
              (two #(assoc % :slug "other-decision")) "decisions.edn"))
        "duplicate legacy number")))

(deftest lifecycle-conditional-shape
  (doseq [[label bad]
          {"draft with accepted date"
           (-> valid-record (assoc :status :draft)
               (dissoc :validation-scope :release-authority))
           "accepted without validation scope"
           (dissoc valid-record :validation-scope)
           "accepted without accepted date"
           (dissoc valid-record :accepted)
           "accepted claim without evidence"
           (update-in valid-record [:claims 0] dissoc :evidence)
           "accepted claim without kind"
           (update-in valid-record [:claims 0] dissoc :kind)}]
    (is (seq (shape-of bad)) label))
  (let [superseded (-> valid-record
                       (assoc :status :superseded)
                       (dissoc :accepted))]
    (is (empty? (shape-of superseded))
        "superseded keeps scope/authority, accepted date optional"))
  (let [superseded-claims (-> valid-record
                              (assoc :status :superseded)
                              (assoc :claims [{:id :c1 :kind :k
                                               :statement "history"}]))]
    (is (empty? (shape-of superseded-claims))
        "superseded claims may lose evidence with their apparatus (ADR 0034)"))
  (let [proposed (-> valid-record
                     (assoc :status :proposed)
                     (dissoc :accepted)
                     (assoc :claims [{:id :c1 :statement "target"}]))]
    (is (empty? (shape-of proposed))
        "proposed may declare target scope/authority (ADR 0035)"))
  (let [draft (-> valid-record
                  (assoc :status :draft)
                  (dissoc :accepted :validation-scope :release-authority)
                  (assoc :claims [{:id :c1 :statement "promotion condition"}]))]
    (is (empty? (shape-of draft))
        "draft claims may omit kind and evidence")))
