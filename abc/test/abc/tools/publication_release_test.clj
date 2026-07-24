(ns abc.tools.publication-release-test
  "The release-admissibility predicate table.

  Layer 1 (pure): release-problems / release-admissible? over an in-memory
  {:index :closure-problems :parser-authority :rights-policy}. The suite starts
  from one valid input and varies exactly one fact per row, asserting the
  expected problem CODE (not prose) and inadmissibility. The pure functions
  read and write no files and are deterministic.

  Layer 2 (impure): verify-release-root! recomputes the closure and authority
  over a completed root; it never trusts a build-time publications-report.json,
  and its authority loaders fail before a hash can be reported."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.publication-release :as release]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.snapshot-index-test :as six]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn h [suffix] (files/example-hash suffix))

;; The authenticated parser-release authority value that AGREES with six's
;; fixture index (adapter ab-aozora; parser/converter/mapping/schema =
;; f1/f2/f3/f4; candidate/qualification refs = ca/cb).
(def matching-authority
  {:candidate-ref (h "ca")
   :qualification-identity-ref (h "cb")
   :qualification-identity {:aat_adapter "ab-aozora"
                            :mapping_hash (h "f3")
                            :parser_ir_schema_hash (h "f4")}
   :executable-provenance {:executables [{:name "ab-aozora" :sha256 (h "f1")}
                                         {:name "ab-aat-to-parser-ir"
                                          :sha256 (h "f2")}]}
   :authority-hashes {:decisions-file (h "d1") :registry-file (h "d2")}})

(def allowed-rights {:rights-publication :assessment-required})

(defn valid-index [& {:as overrides}]
  (snapshot-index/build-snapshot-index (merge (six/build-args) overrides)))

(defn valid-inputs [& {:as overrides}]
  (merge {:index (valid-index)
          :closure-problems []
          :parser-authority matching-authority
          :rights-policy allowed-rights}
         overrides))

(defn codes-of [inputs]
  (set (map :code (release/release-problems inputs))))

;; ── The valid baseline ──────────────────────────────────────────────────────

(deftest valid-release-has-no-problems-test
  (let [inputs (valid-inputs)]
    (is (empty? (release/release-problems inputs))
        "a coherent release closure has no problems")
    (is (true? (release/release-admissible? inputs)))))

(deftest release-admissible?-is-exactly-empty-release-problems-test
  (doseq [inputs [(valid-inputs)
                  (valid-inputs :rights-policy {:rights-publication :blocked})
                  (valid-inputs :closure-problems [{:code "closure-manifest-missing"
                                                    :message "x"}])]]
    (is (= (empty? (release/release-problems inputs))
           (release/release-admissible? inputs)))))

(deftest pure-functions-are-deterministic-test
  (let [inputs (valid-inputs :rights-policy {:rights-publication :blocked}
                             :closure-problems [{:code "closure-content-hash-mismatch"
                                                 :message "x" :path "p"}])]
    (is (= (release/release-problems inputs)
           (release/release-problems inputs))
        "release-problems is a pure function of its inputs"))
  (testing "problems are sorted and deduplicated by [:code :path :expected :actual]"
    (let [dup {:code "closure-x" :message "a" :path "p"}
          inputs (valid-inputs :closure-problems [dup dup
                                                  {:code "closure-a" :message "b"}])
          problems (release/release-problems inputs)]
      (is (= problems (sort-by (juxt :code :path) problems)))
      (is (= 2 (count (filter #(#{"closure-x" "closure-a"} (:code %)) problems)))
          "the duplicate closure problem is collapsed"))))

;; ── Row: closure tamper ─────────────────────────────────────────────────────

(deftest closure-problems-are-concatenated-test
  (let [inputs (valid-inputs
                :closure-problems [{:code "closure-content-hash-mismatch"
                                    :message "content tampered" :path "w/plain.txt"}])]
    (is (contains? (codes-of inputs) "closure-content-hash-mismatch"))
    (is (false? (release/release-admissible? inputs)))))

;; ── Row: dirty / unprovable / fixture source ────────────────────────────────

(deftest fixture-source-is-inadmissible-test
  (let [index (valid-index
               :source-selection (assoc six/source-selection
                                        "trust_mode" "fixture"
                                        "aozora_git_commit" nil))
        inputs (valid-inputs :index index)]
    (is (contains? (codes-of inputs) "release-source-not-official"))
    (is (false? (release/release-admissible? inputs)))))

(deftest official-source-without-proven-commit-is-inadmissible-test
  (let [index (valid-index
               :source-selection (assoc six/source-selection
                                        "aozora_git_commit" nil))
        inputs (valid-inputs :index index)]
    (is (contains? (codes-of inputs) "release-source-commit-missing"))))

;; ── Row: missing / wrong decision authority ─────────────────────────────────

(deftest unauthenticated-parser-authority-is-inadmissible-test
  (doseq [failure [{:problems [{:kind :decision-not-accepted
                                :message "not accepted"}]}
                   {:problems [{:kind :release-authority-not-publication}]}
                   nil]]
    (testing failure
      (let [inputs (valid-inputs :parser-authority failure)]
        (is (contains? (codes-of inputs)
                       "release-parser-authority-unauthenticated"))
        (is (false? (release/release-admissible? inputs)))))))

;; ── Row: absent or mismatched candidate / qualification ─────────────────────

(deftest absent-candidate-or-qualification-ref-is-inadmissible-test
  (testing "null candidate_ref"
    (let [inputs (valid-inputs :index (valid-index :candidate-ref nil))]
      (is (contains? (codes-of inputs) "release-candidate-ref-null"))))
  (testing "null qualification_identity_ref"
    (let [inputs (valid-inputs :index (valid-index :qualification-identity-ref nil))]
      (is (contains? (codes-of inputs) "release-qualification-ref-null")))))

(deftest mismatched-candidate-or-qualification-ref-is-inadmissible-test
  (testing "candidate_ref disagrees with authority"
    (let [inputs (valid-inputs :index (valid-index :candidate-ref (h "99")))]
      (is (contains? (codes-of inputs) "release-candidate-ref-mismatch"))))
  (testing "qualification_identity_ref disagrees with authority"
    (let [inputs (valid-inputs
                  :index (valid-index :qualification-identity-ref (h "99")))]
      (is (contains? (codes-of inputs) "release-qualification-ref-mismatch")))))

;; ── Row: parser build / config / mapping / schema mismatch ──────────────────

(defn- with-runtime [f]
  (valid-index :parser-runtime-identity (f six/parser-runtime-identity)))

(deftest parser-runtime-coordinate-mismatch-is-inadmissible-test
  (testing "parser build hash disagrees with provenance"
    (let [inputs (valid-inputs
                  :index (with-runtime #(assoc % "parser_build_hash" (h "99"))))]
      (is (contains? (codes-of inputs) "release-parser-build-hash-mismatch"))))
  (testing "converter build hash disagrees with provenance"
    (let [inputs (valid-inputs
                  :index (with-runtime #(assoc % "converter_build_hash" (h "99"))))]
      (is (contains? (codes-of inputs) "release-converter-build-hash-mismatch"))))
  (testing "mapping hash disagrees with qualification"
    (let [inputs (valid-inputs
                  :index (with-runtime
                           #(assoc % "aat_parser_ir_mapping_hash" (h "99"))))]
      (is (contains? (codes-of inputs) "release-mapping-hash-mismatch"))))
  (testing "parser-ir schema hash disagrees with qualification"
    (let [inputs (valid-inputs
                  :index (with-runtime #(assoc % "parser_ir_schema_hash" (h "99"))))]
      (is (contains? (codes-of inputs) "release-parser-schema-hash-mismatch"))))
  (testing "adapter id disagrees with qualification"
    (let [inputs (valid-inputs
                  :index (with-runtime #(assoc % "adapter_id" "aozora2html")))]
      (is (contains? (codes-of inputs) "release-parser-adapter-mismatch")))))

;; ── Row: parser runtime-object (config) hash mismatch ───────────────────────

(deftest parser-config-hash-recompute-mismatch-is-inadmissible-test
  (let [index (assoc-in (valid-index)
                        ["snapshot_index_identity_object" "parser_config_hash"]
                        (h "bad"))
        inputs (valid-inputs :index index)]
    (is (contains? (codes-of inputs) "release-parser-config-hash-mismatch"))
    (is (false? (release/release-admissible? inputs)))))

;; ── Row: per-work parser coordinate mismatch vs the runtime object ──────────

(def ^:private good-manifest-coordinates
  {"parser_build_hash" (h "f1")
   "parser_config_hash" (get-in (snapshot-index/build-snapshot-index (six/build-args))
                                ["snapshot_index_identity_object" "parser_config_hash"])
   "aat_parser_ir_mapping_hash" (h "f3")
   "parser_ir_schema_hash" (h "f4")})

(deftest manifest-coordinate-mismatch-is-detected-and-inadmissible-test
  (let [expected (release/expected-manifest-coordinates (valid-index))
        bad-manifest {:work-slug "0005_1234_rashomon" :kind "plaintext"
                      :identity-object (assoc good-manifest-coordinates
                                              "parser_build_hash" (h "99"))}
        coordinate-problems (release/manifest-coordinate-problems expected [bad-manifest])
        inputs (valid-inputs :closure-problems coordinate-problems)]
    (is (= expected good-manifest-coordinates)
        "expected coordinates come from the single runtime object + config hash")
    (is (some #(= "release-manifest-parser-coordinate-mismatch" (:code %))
              coordinate-problems))
    (is (contains? (codes-of inputs) "release-manifest-parser-coordinate-mismatch"))
    (is (false? (release/release-admissible? inputs)))))

;; ── Row: nullable parser coordinates in a release plaintext/TEI manifest ────

(deftest nullable-manifest-parser-coordinates-are-inadmissible-test
  (let [expected (release/expected-manifest-coordinates (valid-index))
        ;; A non-release renderer leaves the triple null but the parser-IR
        ;; document still populates parser_ir_schema_hash. The rejection must
        ;; key on the triple, never on parser_ir_schema_hash.
        null-manifest {:work-slug "0005_1234_rashomon" :kind "tei"
                       :identity-object {"parser_build_hash" nil
                                         "parser_config_hash" nil
                                         "aat_parser_ir_mapping_hash" nil
                                         "parser_ir_schema_hash" (h "f4")}}
        problems (release/manifest-coordinate-problems expected [null-manifest])]
    (is (= 3 (count problems)) "each of the three triple coordinates is rejected")
    (is (every? #(= "release-manifest-parser-coordinate-null" (:code %)) problems))
    (is (= #{"parser_build_hash" "parser_config_hash" "aat_parser_ir_mapping_hash"}
           (set (map :actual problems)))
        "parser_ir_schema_hash nullity has no discriminating power and is not flagged")
    (is (false? (release/release-admissible?
                 (valid-inputs :closure-problems problems))))))

(deftest coherent-manifest-coordinates-produce-no-problems-test
  (let [expected (release/expected-manifest-coordinates (valid-index))
        ok-manifest {:work-slug "0005_1234_rashomon" :kind "plaintext"
                     :identity-object good-manifest-coordinates}]
    (is (empty? (release/manifest-coordinate-problems expected [ok-manifest])))))

(deftest real-manifest-identity-object-keys-produce-no-problems-test
  ;; abc.tools.manifest/identity-object writes exactly these four keys on a
  ;; real per-work manifest. This test pins the manifest's real key names
  ;; (in particular "aat_parser_ir_mapping_hash", NOT "mapping_hash") against
  ;; expected-manifest-coordinates / manifest-coordinate-problems, so a future
  ;; key-name drift between manifest.clj and publication_release.clj is caught
  ;; here instead of silently null-flagging every real release.
  (let [expected (release/expected-manifest-coordinates (valid-index))
        real-manifest {:work-slug "0005_1234_rashomon" :kind "plaintext"
                       :identity-object {"parser_build_hash" (h "f1")
                                         "parser_config_hash" (get expected "parser_config_hash")
                                         "aat_parser_ir_mapping_hash" (h "f3")
                                         "parser_ir_schema_hash" (h "f4")}}]
    (is (empty? (release/manifest-coordinate-problems expected [real-manifest])))))

;; ── Row: nonempty failure set ───────────────────────────────────────────────

(deftest nonempty-failure-set-is-inadmissible-test
  (let [index (valid-index :failures [{"stage" "parse"
                                       "work_slug" "0005_1234_rashomon"
                                       "code" "parser-crash"}])
        inputs (valid-inputs :index index)]
    (is (contains? (codes-of inputs) "release-nonempty-failure-set"))
    (is (false? (release/release-admissible? inputs)))))

;; ── Row: rights policy blocked ──────────────────────────────────────────────

(deftest blocked-rights-policy-is-inadmissible-test
  (doseq [state [:blocked-pending-assessment-migration :blocked nil]]
    (testing state
      (let [inputs (valid-inputs :rights-policy {:rights-publication state})]
        (is (contains? (codes-of inputs) "release-rights-blocked"))
        (is (false? (release/release-admissible? inputs)))))))

(deftest allowed-rights-policy-yields-no-rights-problem-test
  (is (empty? (release/rights-problems {:rights-publication :assessment-required}))))

;; ── publication-policy value + hash envelope ────────────────────────────────

(deftest load-rights-authority-derives-value-and-hash-from-one-read-test
  (with-temp-dir [dir]
    (let [path (str (io/file dir "policy.edn"))
          _ (files/write-text! path "{:rights-publication :assessment-required}\n")
          {:keys [policy content-hash]} (publication-policy/load-rights-authority! path)]
      (is (= :assessment-required (:rights-publication policy)))
      (is (= content-hash (hash/format-sha256 (hash/sha256-file path)))
          "content-hash authenticates the exact bytes read")
      (is (nil? (publication-policy/release-problem policy)))))
  (testing "unreadable / malformed policy is an error"
    (is (thrown? Exception
                 (publication-policy/load-rights-authority! "no/such/policy.edn")))))

;; ── Layer 2: verify-release-root! (impure) ──────────────────────────────────

(def repo-authority-sources
  {:runs-root "docs/reports/parser-rq/runs"
   :registry-path "data/aat-parser-ir-compatibility.edn"
   :measurements-path "docs/reports/parser-release-qualification-measurements.edn"
   :qualification-report-path "docs/reports/parser-release-qualification-report.json"
   :decisions-path "docs/adr/decisions.edn"})

(defn- verify-completed-root [root & {:as source-overrides}]
  (release/verify-release-root!
   {:root root
    :parser-authority-sources (merge repo-authority-sources source-overrides)
    :rights-policy-path (:rights-policy-path source-overrides
                                             publication-policy/policy-path)}))

(deftest verify-recomputes-closure-and-ignores-asserted-report-test
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))]
      ;; The build-time report LIES that the root is admissible.
      (files/write-text! (io/file root "publications" "publications-report.json")
                         "{\"admissible?\":true,\"problems\":[]}")
      (let [result (verify-completed-root root)]
        (is (false? (:admissible? result))
            "a computed closure overrides an asserted admissibility claim")
        (is (seq (:problems result))
            "the fixture root's fake candidate + null manifest coords + blocked rights are recomputed")
        (is (= #{:decisions-file :registry-file :rights-policy-file}
               (set (keys (:authority-hashes result)))))))))

(deftest verify-flags-null-manifest-coordinates-on-a-real-root-test
  (with-temp-dir [dir]
    ;; The shared completed-root fixture renders manifests WITHOUT parser
    ;; identity, so its per-work manifests carry null coordinates — the
    ;; strongest contextual rule: such a candidate release is inadmissible.
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))
          result (verify-completed-root root)]
      (is (false? (:admissible? result)))
      (is (some #(= "release-manifest-parser-coordinate-null" (:code %))
                (:problems result))))))

(deftest verify-rights-policy-hash-comes-from-the-loaded-envelope-test
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))
          result (verify-completed-root root)]
      (is (= (:content-hash (publication-policy/load-rights-authority!
                             publication-policy/policy-path))
             (get-in result [:authority-hashes :rights-policy-file]))))))

(deftest verify-unreadable-decision-file-reports-no-authority-hash-test
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))
          result (verify-completed-root root :decisions-path "no/such/decisions.edn")]
      (is (false? (:admissible? result)))
      (is (nil? (get-in result [:authority-hashes :decisions-file]))
          "a failed decision loader reports no authority hash")
      (is (some #(= "release-parser-authority-unauthenticated" (:code %))
                (:problems result))))))

(deftest verify-unreadable-registry-file-reports-no-authority-hash-test
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))
          result (verify-completed-root root :registry-path "no/such/registry.edn")]
      (is (false? (:admissible? result)))
      (is (nil? (get-in result [:authority-hashes :registry-file]))))))

(deftest verify-unreadable-rights-file-is-an-error-test
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))]
      (is (thrown? Exception
                   (release/verify-release-root!
                    {:root root
                     :parser-authority-sources repo-authority-sources
                     :rights-policy-path "no/such/policy.edn"}))
          "an unreadable rights authority is an error, never an accepted status"))))
