(ns soranoha.snh.corpus-delta-test
  "The checkout-against-release reading of a manifest and a source scan. The
  properties under test are the ones the local half would otherwise get wrong:
  that a withdrawal is not reported as the reader's checkout being ahead, that
  a refused archive costs only its own answer, and that a release built by a
  different extract version is refused rather than differenced."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.snh.corpus-delta :as corpus-delta]))

(defn- work [slug source]
  {"slug" slug
   "source_content_hash" source
   "artifacts" []
   "layers" []})

(defn- manifest
  [rev works & {:keys [withdrawn extract-version]}]
  {"corpus" {"upstream_rev" rev}
   "toolchain" {"extract" {"nix_closure_hash" "closure-1"
                           "stage_code_version" (or extract-version
                                                    corpus-delta/source-hash-stage-version)}}
   "works" works
   "withdrawn" (or withdrawn [])})

(defn- scanned [works & {:keys [unreadable]}]
  {:works works :unreadable (or unreadable [])})

(deftest a-checkout-matching-the-release-reports-only-a-count
  (let [d (corpus-delta/delta (manifest "rev-a" [(work "000001_1" "aa") (work "000002_2" "bb")])
                              (scanned {"000001_1" "aa" "000002_2" "bb"})
                              "rev-a")]
    (is (= 2 (get-in d [:works :unchanged])))
    (is (= [] (get-in d [:works :source-changed])))
    (is (= {:release "rev-a" :checkout "rev-a"} (:corpus d)))))

(deftest a-source-difference-names-the-work
  (let [d (corpus-delta/delta (manifest "rev-b" [(work "000001_1" "aa") (work "000002_2" "bb")])
                              (scanned {"000001_1" "aa" "000002_2" "cc"})
                              "rev-a")]
    (is (= ["000002_2"] (get-in d [:works :source-changed])))
    (is (= 1 (get-in d [:works :unchanged])))))

(deftest a-withdrawal-is-not-the-checkout-being-ahead
  (let [d (corpus-delta/delta (manifest "rev-b" [(work "000001_1" "aa")]
                                        :withdrawn [{"slug" "000002_2"
                                                     "event" "snh:1:governance-event:ee"}])
                              (scanned {"000001_1" "aa" "000002_2" "bb" "000003_3" "cc"})
                              "rev-a")]
    (is (= ["000002_2"] (get-in d [:works :withdrawn]))
        "the release is missing this work on purpose")
    (is (= ["000003_3"] (get-in d [:works :only-in-checkout]))
        "this one the release simply does not publish")
    (is (= [] (get-in d [:works :only-in-release])))))

(deftest a-work-the-checkout-does-not-carry-is-reported-from-the-release-side
  (let [d (corpus-delta/delta (manifest "rev-b" [(work "000001_1" "aa") (work "000009_9" "zz")])
                              (scanned {"000001_1" "aa"})
                              "rev-a")]
    (is (= ["000009_9"] (get-in d [:works :only-in-release])))
    (is (= [] (get-in d [:works :only-in-checkout])))))

(deftest a-refused-archive-costs-only-its-own-answer
  (let [d (corpus-delta/delta (manifest "rev-a" [(work "000001_1" "aa") (work "000002_2" "bb")])
                              (scanned {"000001_1" "aa"}
                                       :unreadable [{:slug "000002_2"
                                                     :relpath "cards/000002/files/x.zip"
                                                     :reason "no-primary-text-member"}])
                              "rev-a")]
    (is (= 1 (get-in d [:works :unchanged])))
    (is (= ["000002_2"] (get-in d [:works :only-in-release]))
        "unhashed, so it cannot be claimed unchanged")
    (is (= "no-primary-text-member" (:reason (first (:unreadable d))))
        "and the reason it went unhashed is carried, not lost")))

(deftest another-extract-version-is-refused-rather-than-differenced
  (is (thrown? clojure.lang.ExceptionInfo
               (corpus-delta/delta (manifest "rev-a" [(work "000001_1" "aa")]
                                             :extract-version "not-this-one")
                                   (scanned {"000001_1" "aa"})
                                   "rev-a"))
      "two versions may hash one archive differently"))

(deftest the-report-form-is-string-keyed-and-ordered
  (let [r (corpus-delta/report (manifest "rev-b" [(work "000001_1" "aa")])
                               (scanned {"000001_1" "zz"})
                               "rev-a")]
    (is (= ["corpus" "extract" "unreadable" "works"] (vec (keys r))))
    (is (= {"checkout" "rev-a" "release" "rev-b"} (into {} (get r "corpus"))))
    (is (= ["000001_1"] (get-in r ["works" "source-changed"])))
    (testing "the extract version both sides agreed on is shown, not assumed"
      (is (= corpus-delta/source-hash-stage-version (get-in r ["extract" "release"]))))))
