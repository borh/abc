(ns soranoha.core.rights-test
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.rights :as rights]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.tei-header :as tei-header]
            [soranoha.snh.schema :as snh-schema]
            [soranoha.za.release :as za-release]))

(def ^:private policy-path "data/publication-policy.edn")

(defn- policy-bytes []
  (fs/read-all-bytes policy-path))

(defn- header-xml [rights]
  (tei-header/hiccup->xml-string
   (tei-header/build
    {:work {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"}
     :contributors []
     :rights rights})))

(deftest committed-policy-states-a-publishable-grant-test
  (testing "the policy the release hashes is the policy the build reads"
    (let [grant (rights/grant-from-bytes (policy-bytes))]
      (is (= grant (:rights (za-release/rights-authority! (policy-bytes))))
          "manifest grant and build grant come from one document")
      (is (= #{"encoding" "statement_url"} (set (keys grant)))
          "the standing of the underlying works is per work, not per release"))))

(deftest grant-is-fail-closed-test
  (testing "a policy that authorizes publication without stating terms publishes nothing"
    (doseq [document ["{:rights-publication :assessment-required}"
                      "{:rights-publication :assessment-required :rights-statement {}}"
                      (str "{:rights-publication :assessment-required "
                           ":rights-statement {:works \"public-domain\" :encoding \"CC0-1.0\"}}")]]
      (is (= :missing-rights-statement
             (:reason (ex-data (try (rights/grant-from-bytes (.getBytes ^String document "UTF-8"))
                                    (catch clojure.lang.ExceptionInfo e e)))))
          document))))

(deftest unrenderable-terms-stop-the-build-test
  (testing "terms this build cannot render are a policy change that outran the code"
    (is (= :unknown-encoding-licence
           (:reason (ex-data (try (rights/licence-uri "CC-BY-4.0")
                                  (catch clojure.lang.ExceptionInfo e e))))))
    (is (= :unknown-works-standing
           (:reason (ex-data (try (rights/works-statement "in-copyright")
                                  (catch clojure.lang.ExceptionInfo e e))))))))

(deftest header-carries-both-rights-layers-test
  (testing "a detached TEI file states the terms it travels under"
    (let [grant (rights/work-terms (rights/grant-from-bytes (policy-bytes))
                                   "public-domain")
          xml (header-xml grant)]
      (is (string/includes? xml "<availability status=\"free\">"))
      (is (string/includes?
           xml (str "<licence target=\"" (rights/licence-uri (get grant "encoding")) "\">")))
      (is (string/includes?
           xml (str "<licence target=\"" (rights/works-uri (get grant "works")) "\">"))
          "the underlying work's standing is stated separately from the encoding licence")
      (is (string/includes? xml "Attribution is requested, not required."))
      (is (string/includes? xml (str "<ptr target=\"" (get grant "statement_url") "\"")))))
  (testing "a header built without a grant carries no rights claim at all"
    (is (not (string/includes? (header-xml nil) "availability")))))

(deftest render-stage-refuses-to-build-without-terms-test
  (testing "the published TEI cannot be produced with the rights grant missing"
    (is (= :missing-rights-grant
           (:reason (ex-data (try (stages/render-stage "test-runtime" nil)
                                  (catch clojure.lang.ExceptionInfo e e)))))))
  (testing "the grant is part of stage identity, so changed terms rebuild the TEI"
    (let [grant (rights/grant-from-bytes (policy-bytes))]
      (is (not= (:toolchain-id (stages/render-stage "test-runtime" grant))
                (:toolchain-id (stages/render-stage
                                "test-runtime"
                                (assoc grant "statement_url" "https://example.org/rights"))))))))

(deftest a-work-states-its-own-standing-not-the-releases-test
  (testing "two works of one release carry the terms each is published under"
    (let [grant (rights/grant-from-bytes (policy-bytes))
          pd (header-xml (rights/work-terms grant "public-domain"))
          by (header-xml (rights/work-terms grant "CC-BY-4.0"))]
      (is (string/includes? pd "The underlying work is in the public domain"))
      (is (string/includes? by "under CC BY 4.0"))
      (is (string/includes? by "Attribution is a condition of that licence, not a request.")
          "a reader of the file alone must not take the site-wide request to cover it")
      (is (string/includes? by (str "<licence target=\"" (rights/works-uri "CC-BY-4.0") "\">")))
      (is (not= pd by))
      (testing "the encoding licence is the same for both, since it is Soranoha's own"
        (is (every? #(string/includes? % "Attribution is requested, not required.")
                    [pd by]))))))

(deftest a-standing-outside-the-vocabulary-is-refused-test
  (testing "a work cannot be published under terms this build cannot state"
    (let [grant (rights/grant-from-bytes (policy-bytes))]
      (doseq [standing ["CC-BY-NC-4.0" "in-copyright" "" nil]]
        (is (= :unknown-works-standing
               (:reason (ex-data (try (rights/work-terms grant standing)
                                      (catch clojure.lang.ExceptionInfo e e)))))
            (pr-str standing))))))

(defn- schema-standings
  "The standings each protocol schema will accept for a work, by schema. Read
  out of the schemas rather than listed here, so a schema that starts carrying
  the vocabulary is compared without this test being told about it."
  []
  (into {}
        (keep (fn [type]
                (when-let [enum (get-in (snh-schema/schema-for type)
                                        ["$defs" "works-standing" "enum"])]
                  [type (set enum)])))
        (keys snh-schema/schema-resources)))

(deftest every-statement-of-the-standings-a-work-may-carry-is-the-same-one
  ;; The set is written out three times: the map this build renders from, and
  ;; a closed enum in each of the two payloads that name a work's standing.
  ;; Adding a licence to the code alone would render terms the manifest
  ;; rejects; adding it to one schema alone would admit a catalog entry whose
  ;; work the manifest refuses. Neither is visible until a release is
  ;; assembled, which is where the artifact-set version of this went wrong.
  (let [by-schema (schema-standings)]
    (is (= #{"release-manifest" "catalog"} (set (keys by-schema)))
        "these are the payloads that carry a work's standing")
    (doseq [[type standings] (sort by-schema)]
      (is (= (rights/standings) (into (sorted-set) standings))
          (str type " admits a different set of standings than this build can state")))))
