(ns abc.tools.acceptance-criteria-lint-test
  "Layer F self-test: nix/check-acceptance-criteria.sh must (a) pass on the
  current repo and (b) fail when a non-allowlisted ADR loses its executable
  path. Mutation-tested to confirm the verdict flips on a real regression
  (otherwise the lint itself would be a mode-B artifact — asserting a rule
  that doesn't bite)."
  (:require [abc.tools.files :as files]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private script "nix/check-acceptance-criteria.sh")

;; Track temp dirs and delete them in a :each fixture so a failing test
;; cannot orphan them (these tests previously never cleaned up).
(def ^:private created-temp-dirs (atom []))

(defn- temp-dir [prefix]
  (let [d (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! created-temp-dirs conj d)
    d))

(use-fixtures :each
  (fn [f]
    (try
      (f)
      (finally
        (run! files/delete-tree! @created-temp-dirs)
        (reset! created-temp-dirs [])))))

(defn- env [m] (merge (into {} (System/getenv)) m))

(defn- ls-adr [] (.listFiles (io/file "docs/adr")))

(deftest acceptance-lint-passes-on-current-repo-test
  (let [{:keys [exit out err]} (sh "bash" script)]
    (is (zero? exit)
        (str "lint failed on current repo; the allowlist is stale or an ADR\n"
             "lost its executable path:\n" out err))))

(deftest acceptance-lint-fails-when-path-removed-test
  (testing "a non-allowlisted ADR with an Acceptance Criteria section but no
            executable path fails the lint — tested against a TEMP ADR corpus
            so the real repo is untouched"
    (let [tmp (str (temp-dir "abc-adr-lint"))
          adr-tmp (io/file (str tmp "/adr"))]
      (.mkdirs adr-tmp)
      (doseq [f (ls-adr)
              :when (.isFile f)]
        (io/copy f (io/file (str adr-tmp "/" (.getName f)))))
      ;; a synthetic ADR with prose-only Acceptance Criteria (no executable path)
      (spit (io/file (str adr-tmp "/9999-synthetic.md"))
            "# ADR 9999: Synthetic\n\n## Acceptance Criteria\n\n- The system MUST do X.\n")
      (let [{:keys [exit out err]}
            (sh "bash" script
                :env (env {"ADR_DIR" (str adr-tmp)
                           "ALLOWLIST" (str adr-tmp "/.acceptance-legacy-allowlist")}))]
        (is (not (zero? exit))
            (str "lint should have failed on the synthetic prose-only ADR; "
                 "the lint does not bite (exit=" exit " out=" out " err=" err))))))

(deftest acceptance-lint-passes-when-path-present-on-new-adr-test
  (testing "a non-allowlisted ADR WITH an executable path in its Acceptance
            Criteria passes the lint (positive control — distinguishes the
            failure above from a blanket-reject bug)"
    (let [tmp (str (temp-dir "abc-adr-lint-pos"))
          adr-tmp (io/file (str tmp "/adr"))]
      (.mkdirs adr-tmp)
      (io/copy (io/file "docs/adr/.acceptance-legacy-allowlist")
               (io/file (str adr-tmp "/.acceptance-legacy-allowlist")))
      (spit (io/file (str adr-tmp "/9998-good.md"))
            (str "# ADR 9998: Good\n\n"
                 "## Acceptance Criteria\n\n"
                 "- Reproducibility is enforced by test/abc/tools/code_as_spec_test.clj.\n"))
      (let [{:keys [exit out err]}
            (sh "bash" script
                :env (env {"ADR_DIR" (str adr-tmp)
                           "ALLOWLIST" (str adr-tmp "/.acceptance-legacy-allowlist")}))]
        (is (zero? exit)
            (str "lint should have passed on the good synthetic ADR; "
                 "exit=" exit " out=" out " err=" err))))))

(deftest acceptance-lint-handles-lowercase-heading-test
  (testing "a lowercase '## Acceptance criteria' heading WITH a valid
            executable path passes — the presence grep is case-insensitive,
            so section extraction must be too (regression: ADR 0028's
            original lowercase heading extracted an empty section and
            false-failed the lint)"
    (let [tmp (str (temp-dir "abc-adr-lint-case"))
          adr-tmp (io/file (str tmp "/adr"))]
      (.mkdirs adr-tmp)
      (spit (io/file (str adr-tmp "/9997-lowercase.md"))
            (str "# ADR 9997: Lowercase\n\n"
                 "## Acceptance criteria\n\n"
                 "- Enforced by test/abc/tools/code_as_spec_test.clj.\n"))
      (let [{:keys [exit out err]}
            (sh "bash" script
                :env (env {"ADR_DIR" (str adr-tmp)
                           "ALLOWLIST" (str adr-tmp "/.acceptance-legacy-allowlist")}))]
        (is (zero? exit)
            (str "lowercase heading with a valid path should pass (exit="
                 exit " out=" out " err=" err))))))
