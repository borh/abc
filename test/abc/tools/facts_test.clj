(ns abc.tools.facts-test
  "Layer C emitter tests. The emitter reads real corpus artifacts
  (manifests, drift events, person records + indexes) and writes
  byte-stable Prolog facts. These tests assert the facts are present and
  that string args are single-quoted SWI atoms — bare 000879 would parse
  as integer 879 in SWI, abc-... as an arithmetic expression, losing
  identity. (Design §7.3: identity computed in Clojure, Prolog only compares.)"
  (:require [abc.tools.facts :as facts]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def tmp-dir (atom nil))

(use-fixtures :each
  (fn [f]
    (reset! tmp-dir (str (Files/createTempDirectory
                            "abc-facts" (make-array FileAttribute 0))))
    (f)
    (run! #(.delete (java.io.File. (str @tmp-dir "/" %)))
          ["manifest_identity.pl" "drift.pl" "person_records.pl"])
    (.delete (java.io.File. @tmp-dir))))

(deftest emit-prolog-writes-manifest-identity-facts-test
  (facts/emit-prolog! @tmp-dir)
  (let [content (slurp (str @tmp-dir "/manifest_identity.pl"))
        ;; Both args single-quoted atoms: '...','sha256:...' (emitter emits ", " with a space)
        facts (re-seq #"manifest_identity\('[^']+',\s*'sha256:[0-9a-f]+'\)\." content)]
    (is (seq facts) "must emit at least one manifest_identity/2 fact"))
  (is (.exists (java.io.File. (str @tmp-dir "/drift.pl")))
      "must create drift.pl even if corpus is empty")
  (is (.exists (java.io.File. (str @tmp-dir "/person_records.pl")))
      "must create person_records.pl even if corpus is empty"))

(deftest emit-prolog-uses-real-artifact-id-test
  (facts/emit-prolog! @tmp-dir)
  ;; The emitted hash must equal manifest/artifact-id on the real identity
  ;; object. Guards against hidden hand-translation in Prolog (design §7.3).
  (let [content (slurp (str @tmp-dir "/manifest_identity.pl"))
        m (files/read-json "examples/v0/example-work/manifest.json")
        expected-hash (manifest/artifact-id (get m "manifest_identity_object"))]
    (is (re-find (re-pattern (str "sha256:[0-9a-f]{64}")) content)
        "emitted hash is the real artifact-id shape")
    (is (str/includes? content (str "'" expected-hash "'"))
        "emitted hash must equal manifest/artifact-id on the real identity object")))

(deftest emit-prolog-quotes-person-ids-test
  (facts/emit-prolog! @tmp-dir)
  ;; All args single-quoted — bare 000879 would corrupt to 879 in SWI,
  ;; abc-000000000001 to an arithmetic expression. Both forms appear in the
  ;; real corpus (top-level record 000879; index abc-000000000001).
  (let [content (slurp (str @tmp-dir "/person_records.pl"))]
    (is (re-find #"person_record\('000879'\)\." content)
        "person_id '000879' must be single-quoted to survive SWI parsing intact")
    (is (re-find #"person_record\('abc-000000000001'\)\." content)
        "person_id 'abc-000000000001' must be single-quoted (bare abc-1 would parse as arithmetic)")))

(deftest emit-prolog-writes-drift-successor-facts-test
  (facts/emit-prolog! @tmp-dir)
  ;; Task 7 depends on drift_successor/2 — guard that it is actually emitted
  ;; (Blocker: the plan draft promised it but the original emitter only wrote
  ;; person_record/1). Successors resolved from prov.used (predecessors) and
  ;; prov.was_generated_by (successors) through participants[].snapshot_id.
  (let [content (slurp (str @tmp-dir "/person_records.pl"))]
    (is (re-find #"drift_successor\('[^']+'[^)]*\)\." content)
        "must emit drift_successor/2 facts resolved from prov.used/was_generated_by")))

(deftest emit-prolog-facts-are-byte-stable-test
  ;; Re-emitting twice to different dirs must produce identical bytes —
  ;; guards the byte-compare committed-facts check (Task 6 Step 3) against
  ;; .listFiles ordering nondeterminism.
  (let [tmp2 (str (Files/createTempDirectory
                    "abc-facts-stab" (make-array FileAttribute 0)))]
    (facts/emit-prolog! @tmp-dir)
    (facts/emit-prolog! tmp2)
    (doseq [f ["manifest_identity.pl" "drift.pl" "person_records.pl"]]
      (is (= (slurp (str @tmp-dir "/" f))
             (slurp (str tmp2 "/" f)))
          (str "two emits of " f " must be byte-identical")))
    (run! #(.delete (java.io.File. (str tmp2 "/" %)))
          ["manifest_identity.pl" "drift.pl" "person_records.pl"])
    (.delete (java.io.File. tmp2))))

(deftest committed-facts-match-real-corpus-test
  ;; Layer C CI guard (Task 6 Step 3): the committed fixtures/v0/facts/prolog/*.pl
  ;; files must equal a fresh emit. Regression: corpus changed and committed
  ;; .pl files were not regenerated. Runs under clj-nix-focused-tests (which has
  ;; the offline Clojure deps); the SWI Nix gate (Task 6 Step 5) is swipl-only.
  (let [tmp (str (Files/createTempDirectory
                    "abc-facts-committed" (make-array FileAttribute 0)))]
    (facts/emit-prolog! tmp)
    (doseq [f ["manifest_identity.pl" "drift.pl" "person_records.pl"]]
      (is (= (slurp (str "fixtures/v0/facts/prolog/" f))
             (slurp (str tmp "/" f)))
          (str "committed " f " diverges from real corpus; regenerate via "
               "abc.tools.facts/emit-prolog! on fixtures/v0/facts/prolog")))
    (run! #(.delete (java.io.File. (str tmp "/" %)))
          ["manifest_identity.pl" "drift.pl" "person_records.pl"])
    (.delete (java.io.File. tmp))))
