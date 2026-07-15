# Parser RQ — Foundation & Gate Migration (Focused Plan P0)

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development or superpowers:executing-plans. Steps use `- [ ]` tracking. This is P0 of the program roadmap `2026-07-15-parser-release-qualification-campaign.md`; read that roadmap's Cross-Cutting Contracts 1 & 2 first — this plan makes them real.

**Goal:** Land the data contracts and gate change that all release instruments
depend on, without producing any new predicate measurement yet. After P0 the gate
still reports `not-qualified` (predicates 2/3/6/8 remain `:instrument-missing`),
but for *coherent* reasons: observations are envelope-shaped and identity-bound,
admission is derived from the registry by projection, and `gate-status` fails
closed on an unadmitted or incoherent bundle.

**Files (authoritative paths):**
- `abc/src/abc/tools/parser_release_qualification.clj` — gate (migrate).
- `abc/test/abc/tools/parser_release_qualification_test.clj` — gate tests.
- `abc/src/abc/tools/aat_parser_ir_compat.clj` — reuse `compatible?`, `match-keys` (no change).
- `abc/docs/reports/parser-release-qualification-measurements.edn` — migrate to envelope form.
- `abc/docs/reports/parser-release-qualification-report.json` — regenerate.
- New: `abc/src/abc/tools/parser_rq_capture.clj` + `abc/test/abc/tools/parser_rq_capture_test.clj` — manifest/envelope Malli contracts + store verifier.
- New: `abc/data/parser-rq-store.edn` — external-store config template (committed; holds no machine paths, only the schema of config the runtime supplies).

**Test command (all steps):** `bin/kaocha --focus abc.tools.parser-release-qualification-test`
and `bin/kaocha --focus abc.tools.parser-rq-capture-test` (run from `abc/`). Full
focused Nix suite before P0's final commit: `just validate-migration` from repo root.

---

## Task 0a: Disposable diagnostic-plumbing probe; defer the candidate pin

**Interfaces:** produces a throwaway plumbing result. The release candidate is
not pinned in P0: `ab-validator/flake.nix` bakes `self.rev` into `ab-aozora`, so
every later instrument implementation commit would invalidate an earlier pin.
P5 pins the final implementation commit before any authoritative capture.

- [ ] **Step 1 (disposable probe):** on hinoki, run `ab-aozora --mode diagnostics`
  over a small diagnostic-triggering fixture; confirm it can emit and a capture can
  record `{code,severity,span}`. Label the probe disposable (not committed as
  production code). It validates *plumbing only* — it does not decide predicate 4
  (settled: envelope-completeness).
- [ ] **Step 2:** record the observed plumbing behavior. Do not cite the probe as
  release evidence and do not freeze its build as the candidate.

## Task 0b: Capture-manifest Malli contract (Contract 2)

**Interfaces:** produces `manifest-schema` +
`manifest-errors` in `parser_rq_capture.clj`.

- [ ] **Step 1 — failing test** `parser_rq_capture_test.clj`:

```clojure
(deftest manifest-requires-logical-blob-identity-not-a-path
  ;; A committed manifest addresses blobs by content identity, never a filesystem path.
  (let [ok  {:blob {:sha256 "sha256:aa" :bytes 12 :media_type "application/json"}}
        bad {:blob {:artifact_root "/db/hinoki/run-7/out.json"}}] ; machine path -> reject
    (is (nil? (seq (capture/manifest-errors ok))))
    (is (seq (capture/manifest-errors bad)))
    (is (some #(re-find #"sha256|logical blob identity" %) (capture/manifest-errors bad)))))

(deftest manifest-denominator-carries-explicit-units
  (let [bad {:blob {:sha256 "sha256:aa" :bytes 1 :media_type "x"}
             :denominator {:value 17886}}]                     ; unit missing -> reject
    (is (seq (capture/manifest-errors bad)))))
```

- [ ] **Step 2 — run → FAIL:** `bin/kaocha --focus abc.tools.parser-rq-capture-test`
  → `Unable to resolve: capture/manifest-errors` (ns/fn absent).
- [ ] **Step 3 — minimal impl:** define `manifest-schema` (Malli): each blob ref is
  `{:sha256 :string :bytes :int :media_type :string}` (no `:artifact_root` / path
  keys permitted in a *committed* manifest); denominators are `{:value :int :unit
  :string}`. `manifest-errors` returns humanized Malli errors.
- [ ] **Step 4 — run → PASS.** Commit `feat(parser-rq): capture-manifest contract`.

## Task 0c: Observation-envelope contract

**Interfaces:** produces `envelope-schema` + `observation-value` (reads `.value`).

- [ ] **Step 1 — failing test:**

```clojure
(deftest observation-is-an-identity-bound-envelope
  (let [env {:value 1.0 :identity_ref "sha256:deadbeef"}]
    (is (= 1.0 (capture/observation-value env)))
    (is (nil? (seq (capture/envelope-errors env))))
    (is (seq (capture/envelope-errors {:value 1.0})))          ; missing identity_ref -> reject
    (is (seq (capture/envelope-errors {:identity_ref "sha256:x"}))))) ; missing value -> reject
```

- [ ] **Step 2 — run → FAIL** (`observation-value` unresolved).
- [ ] **Step 3 — minimal impl:** `envelope-schema` = `{:value :any :identity_ref
  :string}`; `observation-value` returns `:value`; sentinels `:unavailable`/
  `:instrument-missing` are permitted as `:value` (they still mean unavailable).
- [ ] **Step 4 — run → PASS.** Commit `feat(parser-rq): observation-envelope contract`.

## Task 0d: External-store config + rebinding verifier

**Interfaces:** produces `verify-blob` / `verify-manifest` (stream + re-hash),
modeled on `ab-validator/reports/parser-study/freeze_run_evidence.py`
`verify_external`.

- [ ] **Step 1 — failing test** (uses a tmp file as a fake store blob):

```clojure
(deftest verifier-streams-and-rehashes-never-trusts-metadata
  (let [f (doto (java.io.File/createTempFile "blob" ".bin") (spit "hello"))
        real (hash/format-sha256 (hash/sha256-string "hello"))
        ref  {:sha256 real :bytes 5 :media_type "application/octet-stream"}]
    (is (= :ok (:status (capture/verify-blob {:root (.getParent f)} ref (.getName f)))))
    ;; hash mismatch -> :unavailable, NEVER a derived value
    (is (= :unavailable
           (:status (capture/verify-blob {:root (.getParent f)}
                                         (assoc ref :sha256 "sha256:0000") (.getName f)))))
    ;; absent blob -> :unavailable
    (is (= :unavailable
           (:status (capture/verify-blob {:root (.getParent f)} ref "missing.bin"))))))
```

- [ ] **Step 2 — run → FAIL** (`verify-blob` unresolved).
- [ ] **Step 3 — minimal impl:** `verify-blob` resolves `root` from the passed
  config (runtime, not committed), **streams the bytes and recomputes sha256 +
  byte length**, returns `{:status :ok}` on exact match else `{:status
  :unavailable :reason …}`. Never returns the blob contents as an observation.
  `parser-rq-store.edn` documents the config *shape* (`{:root "<runtime>"}`) with a
  placeholder, no real path.
- [ ] **Step 4 — run → PASS.** Commit `feat(parser-rq): external-store rebinding verifier`.

## Task 0e: Gate coherence precondition (the crux — fixes the admission-blind gate)

**Interfaces:** adds to `parser_release_qualification.clj`:
`qualification-identity-ref`, `admission-query`, `admitted?`,
`coherent-observations?`, `coherence-errors`; migrates `evaluate-predicate` and
`gate-status`; bumps the report schema version.

- [ ] **Step 1 — failing tests** in `parser_release_qualification_test.clj`:

```clojure
(def ADMITTED-IDENTITY
  ;; decomposed match-key projection of a known admitted fixture
  {:aat_version 2 :aat_adapter "ab-aozora"
   :aat_adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
   :mapping_id "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe"
   :mapping_version "0.4.0"
   :mapping_hash "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
   :mapping_schema_hash "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"})

(deftest admission-query-projects-exactly-the-nine-match-keys
  (is (= compat/match-keys (vec (keys (rq/admission-query ADMITTED-IDENTITY))))))

(deftest gate-not-qualified-when-tuple-unadmitted-even-if-all-pass
  ;; The current code gap: gate-status ignores admission. A fully-passing run of an
  ;; UNADMITTED build must be :not-qualified.
  (let [registry (compat/load-registry)
        unadmitted (assoc ADMITTED-IDENTITY :mapping_version "9.9.9")
        all-pass (all-pass-envelopes unadmitted)] ; helper: nine :pass envelopes bound to this identity
    (is (false? (rq/admitted? registry unadmitted)))
    (is (= :not-qualified (:gate_status (rq/build-report (bundle unadmitted all-pass)))))))

(deftest gate-not-qualified-when-observations-incoherent
  (let [envs (assoc-in (all-pass-envelopes ADMITTED-IDENTITY)
                       [:fatal_failures :identity_ref] "sha256:someone-elses-run")]
    (is (seq (rq/coherence-errors ADMITTED-IDENTITY envs)))
    (is (= :not-qualified (:gate_status (rq/build-report (bundle ADMITTED-IDENTITY envs)))))))

(deftest gate-release-qualified-requires-precondition-and-nine-pass
  (let [envs (all-pass-envelopes ADMITTED-IDENTITY)]     ; coherent + admitted + nine :pass
    (is (= :release-qualified (:gate_status (rq/build-report (bundle ADMITTED-IDENTITY envs)))))))
```

- [ ] **Step 2 — run → FAIL:** `Unable to resolve: rq/admission-query` /
  `rq/coherent-observations?`; the admission tests fail because `gate-status`
  currently ignores admission.
- [ ] **Step 3 — minimal impl:**
  - `(admission-query identity)` → `(select-keys identity compat/match-keys)`, with
    a doc-comment naming the nine fields (do not implicitly reuse `match-keys`
    without documenting the projection).
  - `(admitted? registry identity)` → `(compat/compatible? registry (admission-query
    identity))` — match-key membership, **not** `admission-report` (evidence
    equality is R6's job, not the release gate's).
  - `(qualification-identity-ref identity)` → canonical sha256 over the full
    identity's contract fields; `(coherent-observations? identity observations)` /
    `coherence-errors` → every envelope's `:identity_ref` equals it.
  - `evaluate-predicate` reads `(capture/observation-value (get measurements
    observed_key))`.
  - `gate-status` → `:release-qualified` iff `(and (empty? (coherence-errors …))
    (admitted? registry identity) (seq results) (every? #(= :pass (:verdict %))
    results))`, else `:not-qualified`. Record `:coherence` and `:admission` blocks
    in the report; bump report-schema version.
- [ ] **Step 4 — migrate the committed bundle** to envelope form (each measurement
  → `{:value … :identity_ref <candidate ref>}`; predicates 2/3/6/8 keep value
  `:instrument-missing`). Regenerate the report:
  `clojure -M -m abc.tools.parser-release-qualification docs/reports/parser-release-qualification-measurements.edn docs/reports/parser-release-qualification-report.json`.
  Expected: `gate_status not-qualified` (coherent, admitted, but four unavailable).
- [ ] **Step 5 — run → PASS**; then `just validate-migration` from repo root. If the
  report/bundle is in an ADR evidence closure, recapture on hinoki first (see
  `.superpowers/sdd/task-6-report.md`) so governance stays green.
- [ ] **Step 6 — commit** `feat(parser-rq): gate coherence precondition (derive admission by projection)`.

---

## Self-review

- Every task is independently testable with a named test, exact failing command,
  and expected failure message; the crux (0e) pins the two relations as separate
  functions — `admission-query` (nine-field projection) checked via `compatible?`,
  and `coherent-observations?` over the full identity — so the roadmap's Blocker-1
  fix is enforced by tests, not prose.
- 0e's `gate-not-qualified-when-tuple-unadmitted-even-if-all-pass` is the
  regression test for the live gap (`gate-status:156` ignores admission); it fails
  before the migration and passes after, proving ADR-0039-C5 is now enforced in code.
- No predicate is added or weakened; P0 produces zero new measurements (four
  predicates stay `:instrument-missing`). The migrated historical bundle is
  coherent but unadmitted, so the gate honestly stays `not-qualified` after P0.
- Contract 2 is enforced by 0b/0d tests: committed manifests carry logical blob
  identities (paths rejected), and the verifier streams + re-hashes, returning
  `:unavailable` on absence/mismatch — never a derived value.
- The bundle/report migration carries the governance-recapture step; heavy probe
  runs are on hinoki.
