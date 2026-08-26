(ns soranoha.za.release-test
  "Automated release-driver acceptance over a local origin: consecutive
  scheduled invocations across real upstream movement publish releases the
  published checker verifies end-to-end; an unmoved upstream is the
  scheduled no-op; a malformed assessment snapshot publishes nothing."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.za.corpus :as corpus]
            [soranoha.za.release :as release]))

;; --- fixture corpus works ---------------------------------------------------

(def ^:private merosu {:work-id "000100" :person-id "000001" :card "000001"
                       :book "100" :n "1001" :title "hashire-merosu"
                       :text "メロスは激怒した。\nfixture line two\n"})
(def ^:private kumo {:work-id "000300" :person-id "000002" :card "000002"
                     :book "300" :n "2002" :title "kumo-no-ito"
                     :text "蜘蛛の糸 fixture text\n"})
(def ^:private added {:work-id "001100" :person-id "000006" :card "000006"
                      :book "1100" :n "6006" :title "added-work"
                      :text "arrives at the second revision\n"})

(def ^:private slug-of corpus/work-slug)

(defn- pd [work]
  (let [slug (slug-of work)
        fact (fn [basis] {"status" "public-domain" "jurisdiction" "jp"
                          "effective_date" "2026-08-01" "basis" basis})]
    {"slug" slug
     "work_assessment" (fact (str "edition:" slug))
     "contributions" [(assoc (fact (str "author:" (:work-id work)))
                             "contribution_id"
                             (str "author:" (:work-id work)))]}))

;; --- driver inputs -----------------------------------------------------------

(def ^:private policy-hash (hash/sha256-string "za fixture policy"))

(defn- snapshot-bytes ^bytes [candidates]
  (:bytes (decode/encode "assessment-snapshot"
                         {"schema" "snh-assessment-snapshot/1"
                          "candidates" (vec (sort-by #(get % "slug")
                                                     candidates))})))

(defn- run->report
  "The build run report the driver consumes, projected from a fixture
  kernel run — the same fields main/build! exports."
  [run]
  {"aozora_git_commit" (:commit run)
   "stages" (into {}
                  (map (fn [[stage coordinate]] [(name stage) coordinate]))
                  (:stage-coordinates run))
   "works" (into {}
                 (map (fn [[slug {:keys [outputs]}]]
                        [slug {"plaintext" (get-in outputs [:render "plaintext"])
                               "tei" (get-in outputs [:render "tei"])
                               "tei-validation"
                               (get-in outputs [:validate "tei-validation"])
                               "source_content_hash"
                               (get (corpus/source-facts run slug)
                                    "work_content_hash")}]))
                 (:results run))})

(defn- drive! [clone run snapshot]
  (release/release!
   {:report (run->report run)
    :cas-dir (:cas-dir run)
    :upstream-origin "https://forge.example/za/fixture-corpus.git"
    :selection-params {"config" "za-fixture"}
    :policy-id "za-fixture-policy-v1"
    :policy-hash policy-hash
    :snapshot-bytes snapshot
    :clone clone
    :branch fx/branch
    :pinned-keys (fx/pinned-keys)
    :sign-release fx/sign-release}))

(defn- verified-chain [clone]
  (verify/verify-repository-at (view/git-view clone)
                               (fx/head-of clone)
                               (fx/pinned-keys)))

(defn- temp-store! [] (str (fs/create-temp-dir {:prefix "za-release-store"})))

;; --- acceptance --------------------------------------------------------------

(deftest consecutive-automated-releases-over-a-local-origin
  (let [root (corpus/init-corpus! [merosu kumo])
        store (temp-store!)
        run-1 (corpus/run-corpus! root store)
        {:keys [clone]} (fx/make-repos!)
        result-1 (drive! clone run-1 (snapshot-bytes [(pd merosu) (pd kumo)]))]
    (is (= :published (:outcome result-1)))

    (testing "upstream movement publishes a successor release"
      (corpus/write-work! root added)
      (corpus/write-catalog! root [merosu kumo added])
      (corpus/commit-corpus! root)
      (let [run-2 (corpus/run-corpus! root store)
            result-2 (drive! clone run-2
                             (snapshot-bytes [(pd merosu) (pd kumo) (pd added)]))
            chain (verified-chain clone)
            head (:head-manifest chain)]
        (is (= :published (:outcome result-2)))
        (is (= [(:manifest-id result-2) (:manifest-id result-1)] (:chain chain)))
        (is (= (:manifest-id result-1) (get head "prev_manifest")))
        (is (= #{(slug-of merosu) (slug-of kumo) (slug-of added)}
               (into #{} (map #(get % "slug")) (get head "works"))))
        (is (= (:commit run-2) (get-in head ["corpus" "upstream_rev"])))

        (testing "an unmoved upstream is the scheduled no-op"
          (let [run-3 (corpus/run-corpus! root store)
                result-3 (drive! clone run-3
                                 (snapshot-bytes
                                  [(pd merosu) (pd kumo) (pd added)]))]
            (is (= :already-published (:outcome result-3)))
            (is (= (:manifest-id result-2) (:manifest-id result-3)))
            (is (= [(:manifest-id result-2) (:manifest-id result-1)]
                   (:chain (verified-chain clone))))))))))

(deftest malformed-assessment-snapshot-publishes-nothing
  (let [root (corpus/init-corpus! [merosu])
        run (corpus/run-corpus! root (temp-store!))
        {:keys [clone]} (fx/make-repos!)
        good (snapshot-bytes [(pd merosu)])
        good-str (String. good "UTF-8")
        head-before (fx/head-of clone)
        rejected (fn [^String s]
                   (try (drive! clone run (.getBytes s "UTF-8"))
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (:reason (ex-data e)))))]
    (testing "duplicate keys die at parse, before anything is assembled"
      (is (= :parse-invalid
             (rejected (str/replace good-str "{\"candidates\""
                                    "{\"candidates\":[],\"candidates\"")))))
    (testing "non-canonical bytes are rejected"
      (is (some? (rejected (str " " good-str)))))
    (testing "nothing reached the origin"
      (is (= head-before (fx/head-of clone))))
    (testing "the same run publishes once the snapshot is contractual"
      (is (= :published (:outcome (drive! clone run good)))))))
