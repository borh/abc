(ns soranoha.core.rights-test
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.rights :as rights]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.tei-header :as tei-header]
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
      (is (= #{"works" "encoding" "statement_url"} (set (keys grant)))))))

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
    (let [grant (rights/grant-from-bytes (policy-bytes))
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
