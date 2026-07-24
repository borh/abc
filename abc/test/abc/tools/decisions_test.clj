(ns abc.tools.decisions-test
  (:require [abc.tools.decisions :as d]
            [abc.tools.hash :as hash]
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

;; --- shared strict corpus boundary -------------------------------------------
;; load-shape-valid-corpus! and decision-by-slug! promise strict load plus
;; shape only; corpus semantic governance stays out of this boundary.

(defn- load-shape-valid-str [s]
  (let [f (str (fs/create-temp-file {:suffix ".edn"}))]
    (spit f s)
    (d/load-shape-valid-corpus! f)))

(defn- thrown-errors [thunk]
  (try
    (thunk)
    ::did-not-throw
    (catch clojure.lang.ExceptionInfo e
      (:errors (ex-data e)))))

(deftest load-shape-valid-corpus-succeeds-on-one-valid-form
  (let [s (pr-str {:decisions [valid-record]})
        f (str (fs/create-temp-file {:suffix ".edn"}))]
    (spit f s)
    (let [{:keys [corpus content-hash]} (d/load-shape-valid-corpus! f)]
      (is (= {:decisions [valid-record]} corpus))
      (is (= (hash/format-sha256 (hash/sha256-bytes (.getBytes s "UTF-8")))
             content-hash)
          "content-hash is derived from the same bytes read for the corpus"))))

(deftest load-shape-valid-corpus-rejects-empty-file
  (let [errors (thrown-errors #(load-shape-valid-str ""))]
    (is (vector? errors))
    (is (seq errors))))

(deftest load-shape-valid-corpus-rejects-malformed-edn
  (doseq [s ["{:decisions [" "{:decisions ]}" "#=(boom)"]]
    (let [errors (thrown-errors #(load-shape-valid-str s))]
      (is (vector? errors) s)
      (is (seq errors) s))))

(deftest load-shape-valid-corpus-rejects-trailing-second-form
  (let [errors (thrown-errors #(load-shape-valid-str "{:decisions []} {:junk true}"))]
    (is (some #(re-find #"exactly one EDN form" %) errors))))

(deftest load-shape-valid-corpus-rejects-invalid-shape
  (let [errors (thrown-errors
                #(load-shape-valid-str
                  (pr-str {:decisions [(assoc valid-record :status :acceptedd)]})))]
    (is (vector? errors))
    (is (seq errors))))

(deftest load-shape-valid-corpus-rejects-duplicate-slugs
  (let [errors (thrown-errors
                #(load-shape-valid-str
                  (pr-str {:decisions [valid-record valid-record]})))]
    (is (some #(re-find #"unique" %) errors))))

(deftest decision-by-slug-returns-the-one-record
  (let [corpus {:decisions [valid-record]}]
    (is (= valid-record (d/decision-by-slug! corpus (:slug valid-record))))))

(deftest decision-by-slug-rejects-missing-slug
  (let [corpus {:decisions [valid-record]}
        errors (thrown-errors #(d/decision-by-slug! corpus "no-such-slug"))]
    (is (vector? errors))
    (is (seq errors))))

;; --- semantic checks --------------------------------------------------------

(defn- rec [slug & {:as kvs}]
  (merge {:slug slug :title slug :status :draft :date "2026-07-24"
          :source "docs/superpowers/specs/example.md"
          :topics [] :relations [] :claims []}
         kvs))

(defn- accepted-rec [slug & {:as kvs}]
  (merge (rec slug
              :status :accepted :accepted "2026-07-24"
              :validation-scope :structural :release-authority :none
              :claims [{:id :c1 :kind :structural-invariant
                        :statement "s"
                        :evidence ["test/abc/tools/decisions_test.clj"]}])
         kvs))

(defn- lifecycle [type to & [scope]]
  (cond-> {:class :lifecycle :type type :to to}
    scope (assoc :scope scope)))

(defn- sem [decisions]
  (d/semantic-problems {:decisions decisions} "." "decisions.edn"))

(deftest dangling-and-duplicate-relations
  (is (= [:missing-relation-target]
         (map :kind (sem [(rec "a" :relations [(lifecycle :depends-on "ghost")])]))))
  (is (= [:duplicate-relation]
         (map :kind (sem [(rec "a" :relations [(lifecycle :depends-on "b")
                                               (lifecycle :depends-on "b")])
                          (rec "b")])))))

(deftest lifecycle-self-edges-and-cycles-are-rejected
  (is (some #(= :self-relation (:kind %))
            (sem [(rec "a" :relations [(lifecycle :depends-on "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :relations [(lifecycle :depends-on "b")])
                  (rec "b" :relations [(lifecycle :depends-on "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :status :superseded
                       :relations [(lifecycle :supersedes "b")])
                  (rec "b" :status :superseded
                       :relations [(lifecycle :supersedes "a")])])))
  (is (some #(= :relation-cycle (:kind %))
            (sem [(rec "a" :relations [(lifecycle :amends "b" "x")])
                  (rec "b" :relations [(lifecycle :amends "a" "y")])])))
  (is (not-any? #(= :relation-cycle (:kind %))
                (sem [(rec "a" :relations [(lifecycle :depends-on "b")
                                           (lifecycle :amends "b" "x")])
                      (rec "b" :relations [(lifecycle :depends-on "c")])
                      (rec "c")]))
      "diamond-free chains are not cycles"))

(deftest supersession-status-rules
  (is (some #(= :unscoped-supersession-target-not-superseded (:kind %))
            (sem [(rec "a" :relations [(lifecycle :supersedes "b")])
                  (rec "b")]))
      "unscoped supersession of a non-superseded record")
  (is (some #(= :superseded-without-successor (:kind %))
            (sem [(rec "b" :status :superseded
                       :validation-scope :structural)]))
      "superseded record with no incoming unscoped supersession")
  (is (empty? (sem [(rec "a" :relations [(lifecycle :supersedes "b" "one scope")])
                    (accepted-rec "b")]))
      "scoped supersession leaves the target's Accepted status alone"))

(deftest accepted-lifecycle-rules
  (is (some #(= :accepted-before-date (:kind %))
            (sem [(accepted-rec "a" :date "2026-07-25" :accepted "2026-07-24")])))
  (is (some #(= :noncanonical-dependency-path (:kind %))
            (sem [(accepted-rec "a" :relations [(lifecycle :depends-on "b")])
                  (rec "b")]))
      "accepted record depending on a draft")
  (is (some #(= :noncanonical-dependency-path (:kind %))
            (sem [(accepted-rec "a" :relations [(lifecycle :depends-on "b")])
                  (accepted-rec "b" :relations [(lifecycle :depends-on "c")])
                  (rec "c")]))
      "transitive closure is checked")
  (is (some #(= :missing-claims (:kind %))
            (sem [(accepted-rec "a" :claims [])]))
      "accepted records need at least one claim"))

(deftest evidence-path-rules
  (is (some #(= :missing-evidence-path (:kind %))
            (sem [(accepted-rec "a" :claims [{:id :c1 :kind :k :statement "s"
                                              :evidence ["test/no/such/file.clj"]}])])))
  (is (some #(= :evidence-path-traversal (:kind %))
            (sem [(accepted-rec "a" :claims [{:id :c1 :kind :k :statement "s"
                                              :evidence ["test/../deps.edn"]}])])))
  (is (some #(= :evidence-outside-roots (:kind %))
            (sem [(accepted-rec "a" :claims [{:id :c1 :kind :k :statement "s"
                                              :evidence ["src/abc/tools/decisions.clj"]}])]))
      "existing path outside the evidence roots is rejected")
  (is (some #(= :unverified-evidence-directory (:kind %))
            (sem [(accepted-rec "a" :claims [{:id :c1 :kind :k :statement "s"
                                              :evidence ["test/abc/tools"]}])]))
      "directory evidence requires a test/ or nix/ file in the same claim")
  (is (not-any? #(= :unverified-evidence-directory (:kind %))
                (sem [(accepted-rec "a" :claims
                                    [{:id :c1 :kind :k :statement "s"
                                      :evidence ["test/abc/tools"
                                                 "test/abc/tools/decisions_test.clj"]}])]))
      "a companion test file verifies the directory"))

(deftest narrative-file-rules
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (spit (str (fs/path root "docs/adr/a.md")) "# A\n")
    (spit (str (fs/path root "docs/adr/orphan.md")) "# Orphan\n")
    (spit (str (fs/path root "docs/adr/README.md")) "# readme\n")
    (spit (str (fs/path root "docs/adr/INDEX.md")) "# generated\n")
    (let [problems (d/narrative-problems
                    {:decisions [(rec "a") (rec "b")]} root "docs/adr")]
      (is (= #{:missing-narrative :orphan-narrative}
             (set (map :kind problems))))
      (is (= 2 (count problems))
          "README.md and INDEX.md are not orphans"))))
