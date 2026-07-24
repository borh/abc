(ns abc.tools.parser-release-authority-test
  (:require [abc.tools.decisions :as decisions]
            [abc.tools.hash :as hash]
            [abc.tools.parser-release-authority :as authority]
            [abc.tools.parser-rq-campaign :as campaign]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

;; Exact committed P5 candidate — the accepted parser candidate this task's
;; boundary must authenticate. Values below come from the committed
;; `docs/reports/parser-rq/runs/<candidate-ref>/candidate.edn` and
;; `docs/reports/parser-release-qualification-measurements.edn` tuple; they
;; are never recomputed here.
(def p5-candidate-ref
  "sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab")

(def p5-qualification-identity-ref
  "sha256:6f365a44b975465943da88d0e3fe4f123672e00913285a3e998ab465bc79edca")

(def p5-mapping-hash
  "sha256:9be58ff3fea272c2a94ae16f05e3e362425e8bcdd20c482a4a842c13fe067142")

(def p5-ab-aozora-executable-sha256
  "sha256:482728cad5bc663c0742ca9e8c6d6fa7031c1a84117d024921cd48628e2eb034")

(def decisions-path "docs/adr/decisions.edn")

(defn- base-options []
  (let [run-root (str "docs/reports/parser-rq/runs/" (subs p5-candidate-ref 7))]
    {:runs_root "docs/reports/parser-rq/runs"
     :candidate_ref p5-candidate-ref
     :registry_path "data/aat-parser-ir-compatibility.edn"
     :measurements_path "docs/reports/parser-release-qualification-measurements.edn"
     :report_path "docs/reports/parser-release-qualification-report.json"
     :provenance_path (str run-root "/executable-provenance.json")
     :decisions_path decisions-path}))

(defn- authenticate-problems
  "Call authority/authenticate opts, requiring it to throw. Returns the
  thrown ex-info's :problems vector, or throws if authenticate instead
  unexpectedly succeeded (never masking a bug behind the same catch)."
  [opts]
  (let [outcome (try {:ok (authority/authenticate opts)}
                     (catch clojure.lang.ExceptionInfo error
                       {:problems (:problems (ex-data error))}))]
    (if (contains? outcome :ok)
      (throw (ex-info "authenticate unexpectedly succeeded" {:result (:ok outcome)}))
      (:problems outcome))))

(defn- temp-path [name]
  (str (fs/file (fs/create-temp-dir {:prefix "parser-release-authority-test"}) name)))

(defn- decisions-corpus []
  (edn/read-string (slurp decisions-path)))

(defn- write-temp-edn! [name value]
  (let [path (temp-path name)]
    (spit path (pr-str value))
    path))

(defn- corpus-with-target [f]
  (update (decisions-corpus) :decisions
          (fn [records]
            (mapv (fn [record]
                    (if (= authority/release-qualification-slug (:slug record))
                      (f record)
                      record))
                  records))))

(defn- corpus-without-target []
  (update (decisions-corpus) :decisions
          (fn [records]
            (vec (remove #(= authority/release-qualification-slug (:slug %)) records)))))

;; --- positive: the committed P5 candidate authenticates -----------------

(deftest authenticates-the-committed-p5-candidate-test
  (let [options (base-options)
        result (authority/authenticate options)
        verification (campaign/promotion-verification options)
        decisions-content-hash (:content-hash (decisions/load-shape-valid-corpus! decisions-path))]
    (is (= p5-candidate-ref (:candidate-ref result)))
    (is (= p5-qualification-identity-ref (:qualification-identity-ref result)))
    (is (= p5-mapping-hash (:mapping_hash (:qualification-identity result))))
    (is (= authority/release-qualification-slug (:slug (:decision result))))
    (is (= :accepted (:status (:decision result))))
    (is (= :publication (:release-authority (:decision result))))

    (testing "the ab-aozora executable hash comes from bound provenance, not re-derived"
      (is (= p5-ab-aozora-executable-sha256
             (:sha256 (some #(when (= "ab-aozora" (:name %)) %)
                            (:executables (:executable-provenance result)))))))

    (testing "authority hashes and registry-ref match the campaign's own byte-backed reads"
      (is (= decisions-content-hash (:decisions-file (:authority-hashes result))))
      (is (= (:registry-file-hash verification) (:registry-file (:authority-hashes result))))
      (is (= (:registry-ref verification) (:registry-ref result))))))

;; --- negative: decisions corpus problems ---------------------------------

(deftest malformed-decisions-corpus-is-rejected-test
  (let [path (temp-path "decisions.edn")]
    (spit path (str (slurp decisions-path) " {:extra true}"))
    (let [problems (authenticate-problems (assoc (base-options) :decisions_path path))]
      (is (some #(= :invalid-decisions-corpus (:kind %)) problems))
      (is (some #(re-find #"exactly one EDN form" (:message %)) problems)))))

(deftest missing-release-qualification-decision-is-rejected-test
  (let [path (write-temp-edn! "decisions.edn" (corpus-without-target))
        problems (authenticate-problems (assoc (base-options) :decisions_path path))]
    (is (some #(= :missing-decision (:kind %)) problems))
    (is (some #(re-find #"custom-parser-release-qualification" (:message %)) problems))))

(deftest non-accepted-release-qualification-status-is-rejected-test
  ;; :superseded permits the existing :accepted/:validation-scope/
  ;; :release-authority fields to remain, so this stays shape-valid while
  ;; no longer being an Accepted record.
  (let [path (write-temp-edn! "decisions.edn"
                              (corpus-with-target #(assoc % :status :superseded)))
        problems (authenticate-problems (assoc (base-options) :decisions_path path))]
    (is (some #(and (= :decision-not-accepted (:kind %)) (= :superseded (:status %)))
              problems))))

(deftest wrong-release-authority-is-rejected-test
  (doseq [wrong-authority [:development :none]]
    (testing wrong-authority
      (let [path (write-temp-edn! "decisions.edn"
                                  (corpus-with-target
                                   #(assoc % :release-authority wrong-authority)))
            problems (authenticate-problems (assoc (base-options) :decisions_path path))]
        (is (some #(and (= :release-authority-not-publication (:kind %))
                        (= wrong-authority (:release-authority %)))
                  problems))))))

;; --- negative: campaign-side problems propagate as problem maps ---------

(deftest wrong-candidate-ref-is-rejected-test
  (let [wrong-ref (hash/format-sha256 (apply str (repeat 64 "b")))
        problems (authenticate-problems (assoc (base-options) :candidate_ref wrong-ref))]
    (is (some #(= :promotion-problem (:kind %)) problems))))

(deftest stale-registry-is-rejected-test
  (let [path (write-temp-edn! "registry.edn" {:entries []})
        problems (authenticate-problems (assoc (base-options) :registry_path path))]
    (is (some #(= :promotion-problem (:kind %)) problems))))

(deftest promotion-verification-problems-propagate-test
  (let [path (temp-path "measurements.json")]
    (spit path (str (slurp (:measurements_path (base-options))) " "))
    (let [problems (authenticate-problems (assoc (base-options) :measurements_path path))]
      (is (some #(and (= :promotion-problem (:kind %))
                      (re-find #"canonical projections" (:message %)))
                problems)))))
