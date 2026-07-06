# Code-as-Spec Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the three vacuous hand-transcribed SMT invariant files (R1/R2/R3) with gates that read real artifacts (schemas, validators, manifest corpus), and add an ADR-acceptance lint that prevents prose-only invariants from landing.

**Architecture:** Five layered instances of one design rule ("every formal model must read a real artifact; no hand-transcribed input"):
- **A** — condemned fixtures under `fixtures/v0/invalid/` + a fixture-battery test asserting the real validator rejects them (the existing `validate-drift-fixtures!` pattern, applied to manifest identity).
- **F** — a ratcheted ADR-acceptance lint (`docs/adr/.acceptance-legacy-allowlist` + Nix gate) that enforces executable condemnations on new/changed ADRs.
- **D** — `test.check` property tests in-process, generators call the real `manifest-index/reproducibility-conflicts` oracle (replaces R1).
- **C** — Clojure fact emitter writing `fixtures/v0/facts/prolog/*.pl`, SWI-Prolog gate queries cross-artifact invariants (Position L, person_id referential integrity).
- **G** — delete R1/R2/R3 `.smt2` + their Nix gates + README + the `adr-invariants-vacuity` gate once A/D/C are green.

**Tech Stack:** Clojure (deps.edn, kaocha 1.91.1392, test.check 1.1.3), JSON Schema (existing), SWI-Prolog (nixpkgs `swi-prolog`, pinned via flake input), Nix flakes.

## Global Constraints

- **Design rule:** no model may take a hand-transcribed restatement of a prose spec as input. Every gate reads a real artifact.
- **Repo term:** `manifest_identity_object` (NOT `artifact_identity_object`).
- **Facts location:** `fixtures/v0/facts/prolog/` (checked-in, governed by ADR 0011). NOT `out/`.
- **Prolog dialect:** SWI-Prolog (`swipl`) is the CI dialect, pinned to a concrete nixpkgs revision. `chiasmus_verify` prolog is exploratory/manual only.
- **TDD:** every new behavior has a failing test watched first. The R3 test (`test/abc/tools/person_drift_test.clj` commit `48c4c65`) is the exemplar; mutation-test each new gate to prove it bites before claiming done.
- **R-place retirement:** delete R1/R2/R3 `.smt2`, `docs/adr/0001-invariants.README.md`, the `adr0001-invariants` + `adr0020-drift-cardinality` + `adr-invariants-vacuity` Nix gates — only after their replacements (Tasks 1, 4, 6) are green in CI.
- **Existing forward-reference compile hazard in `validate_design_bundle.clj` (`check-errors!` used line 237, defined line 252) is being fixed by other agents — do NOT touch `validate_design_bundle.clj` unless a task explicitly modifies drift-fixture registration.**
- Run focused tests via: `nix build .#checks.x86_64-linux.clj-nix-focused-tests` (offline Nix sandbox; `ABC_TEI_SCHEMA_SKIP=1` is set by the derivation).
- Each task commits its own green work; the final task does the sweep delete.

---

## File Structure

**Create:**
- `fixtures/v0/invalid/manifest-nested-artifact-id/` — condemned R2 fixture (manifest whose identity object nests artifact_id).
- `test/abc/tools/code_as_spec_test.clj` — Layer A fixture-battery test (R2) + Layer D property tests (R1).
- `src/abc/tools/facts.clj` — new namespace: `emit-prolog!` reads real corpus, writes `fixtures/v0/facts/prolog/`.
- `fixtures/v0/facts/prolog/manifest_identity.pl` — emitted-by-emitter fact file (generated, checked in, byte-stable).
- `fixtures/v0/facts/prolog/drift.pl` — emitted-by-emitter fact file.
- `fixtures/v0/facts/prolog/person_records.pl` — emitted-by-emitter fact file.
- `docs/adr/manifest-identity-emitter-sanity.pl` — committed SWI-Prolog query (emitter sanity, NOT Position L — see Task 6 header).
- `docs/adr/person-id-referential-integrity.pl` — committed query: no dangling contributor.
- `docs/adr/.acceptance-legacy-allowlist` — ratchet allowlist (17 existing ADRs).
- `nix/check-acceptance-criteria.sh` — the F-layer lint.
- `test/abc/tools/facts_test.clj` — emitter + Prolog-fact tests.
- `test/abc/tools/acceptance_criteria_lint_test.clj` — lint self-test.

**Modify:**
- `flake.nix` — add `checks.adr-acceptance-criteria`, `checks.prolog-cross-artifact`; (Task 9) remove `adr0001-invariants`, `adr0020-drift-cardinality`, `adr-invariants-vacuity`.
- `test/abc/tools/validate_design_bundle_test.clj` — register the new R2 fixture in the manifest invalid-fixture set (mirror the drift-fixture registration pattern at lines 760–810).
- `deps.edn` — add SWI-Prolog invocation path if a `:abc/prolog-facts` alias is needed (probably not — emitter is a normal Clojure test; SWI runs in Nix).

**Delete (Task 9):**
- `docs/adr/r1-reproducibility-conflict.smt2`
- `docs/adr/r2-non-circularity.smt2`
- `docs/adr/r3-drift-cardinality.smt2`
- `docs/adr/0001-invariants.README.md`

---

## Task 1: R2 condemned fixture + fixture-battery test (Layer A)

**Files:**
- Create: `fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json`
- Create: `test/abc/tools/code_as_spec_test.clj`
- Modify: `src/abc/tools/manifest.clj:88-93` — NO. Read-only: confirm `manifest/artifact-manifest` puts `artifact_id` outside `manifest_identity_object` (it does — line ~84). The R2 invariant is structural; the fixture violates it by hand-embedding.

**Interfaces:**
- Consumes: existing `manifest/artifact-id` (`[identity-object] -> "sha256:..."`) and `manifest/identity-keys` (the 13-key list at `src/abc/tools/manifest.clj:40`). Note: the spec §4.3 says "12 identity fields" but the real `identity-keys` has 13 (includes `aat_parser_ir_mapping_hash`); use the real list. The fixture must embed `artifact_id` as a 14th key.
- Produces: a passing fixture-battery test that `schema/validation-errors` (`src/abc/tools/schema.clj:28`) flags the nested `artifact_id`. If the JSON Schema's `additionalProperties: false` on `manifest_identity_object` already rejects it, the test asserts that. If the schema doesn't currently express the constraint, this task surfaces the gap — do NOT hand-add a SMT; instead note in the test docstring which schema rule (or its absence) is the real carrier.

**Background:** `schemas/manifest.schema.json` defines `manifest_identity_object` with `additionalProperties: false` and a fixed `properties` set that does NOT include `artifact_id`. So an extra `artifact_id` key inside the identity object should already fail schema validation. This task makes that real rejection an executable condemnation rather than a latent fact.

- [ ] **Step 1: Write the failing test**

Create `test/abc/tools/code_as_spec_test.clj`:

```clojure
(ns abc.tools.code-as-spec-test
  "Code-as-spec gates (design: docs/superpowers/specs/2026-07-04-code-as-spec-formal-models-design.md).
  Layer A: condemned fixtures assert the REAL validator rejects invariant
  violations. Reads schemas/manifest.schema.json — no hand-transcribed model."
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is]]))

(def nested-id-fixture-path "fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json")

(deftest r2-manifest-schema-rejects-nested-artifact-id-test
  "ADR 0001 R2: artifact_id must not be nested inside manifest_identity_object.
  The real carrier is the JSON Schema's additionalProperties:false + fixed
  property set. This fixture violates that; the real validator must reject it.
  (Replaces vacuous r2-non-circularity.smt2 — see
  docs/handoffs/formal-verification-assessment-critique.md §2.)"
  (let [schema (files/read-json manifest/manifest-schema-path)
        fixture (files/read-json nested-id-fixture-path)
        errors (schema/validation-errors schema fixture)]
    (is (seq errors) "schema must reject a manifest whose identity object nests artifact_id")
    ;; The error must be specifically about the nested artifact_id, not an
    ;; unrelated field — otherwise the fixture passes for the wrong reason
    ;; (Medium finding: fixture must be otherwise valid).
    (is (some #(re-find #"manifest_identity_object.*artifact_id|additionalProperties"
                       (str %))
              (map str errors))
        (str "error path must target manifest_identity_object/artifact_id; got: "
             (pr-str errors)))))
```

- [ ] **Step 2: Run test to verify it fails (fixture does not exist yet)**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: FAIL — `nested-id-fixture-path` file not found (FileNotFoundException on `files/read-json`).

- [ ] **Step 3: Create the condemned fixture**

Create `fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json` — a minimal manifest whose `manifest_identity_object` has an extra `artifact_id` key:

```json
{
  "manifest_schema_id": "https://w3id.org/abc/schemas/manifest.schema.json",
  "artifact_id": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "artifact_kind": "tei",
  "validation_status": "passed",
  "manifest_identity_object": {
    "manifest_schema_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "corpus_snapshot_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "work_content_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "metadata_record_hash": null,
    "parser_build_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parser_config_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "aat_parser_ir_mapping_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parser_ir_schema_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "tei_profile_hash": null,
    "tokenizer_build_hash": null,
    "tokenizer_dictionary_hash": null,
    "analysis_recipe_hash": null,
    "output_format_spec_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "artifact_id": "sha256:0000000000000000000000000000000000000000000000000000000000000000"
  },
  "content": {"content_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
              "media_type": "text/plain", "byte_length": 0, "path_hint": "work.txt"},
  "sidecars": [],
  "provenance": {"generated_at": "2026-04-26T00:00:00Z",
                 "activity_id": "https://w3id.org/abc/activities/x",
                 "agent": "https://w3id.org/abc/agents/test",
                 "plan_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                 "used": [], "was_derived_from": []},
  "license": null, "signatures": [], "superseded_by": null
}
```

The `artifact_id` line inside `manifest_identity_object` is the violation. The
fixture is otherwise schema-valid: top-level keys are exactly the schema's
defined set (verified against `examples/v0/example-work/manifest.json`),
`validation_status` is in the real enum
`schemas/manifest.schema.json` `["not-run","passed","warning","failed"]`, and
`artifact_kind` is a real enum value. This ensures the schema rejects the
fixture *for the nested artifact_id*, not for an unrelated extra field.
(`manifest_schema_hash` and `generated_at` were removed from the top level —
the real manifest has neither.)

- [ ] **Step 4: Run test to verify it passes**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: PASS — schema rejects the extra `artifact_id` key.

- [ ] **Step 5: Mutation-test that the test bites (Box's usefulness test)**

Temporarily edit `schemas/manifest.schema.json` to add `"artifact_id": {"type": "string"}` to `manifest_identity_object.properties` (widening the schema so the violation becomes legal). Re-run the test. Expected: FAIL ("schema must reject..."). Restore the schema. Re-run. Expected: PASS. This proves the test's verdict changes on a real schema regression — it is not vacuous.

- [ ] **Step 6: Register the fixture in the manifest invalid-fixture set**

Find how manifest-side invalid fixtures register (mirror the drift pattern at `test/abc/tools/validate_design_bundle_test.clj:760-810` and `src/abc/tools/validate_design_bundle.clj:527-552`). If there is a manifest-side fixture battery, add an entry pointing at the new fixture with its expected error. If manifest invalid fixtures are not wired into a smoke test, this step is to confirm that and add a TODO note — do not invent scaffolding not in the plan.

- [ ] **Step 7: Commit**

```bash
git add fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json \
        test/abc/tools/code_as_spec_test.clj
git commit -m "test(manifest): bind ADR 0001 R2 non-circularity to real schema (Layer A)"
```

---

## Task 2: ADR-acceptance lint with ratchet allowlist (Layer F)

**Files:**
- Create: `docs/adr/.acceptance-legacy-allowlist`
- Create: `nix/check-acceptance-criteria.sh`
- Modify: `flake.nix` — add `checks.adr-acceptance-criteria`

**Interfaces:**
- Consumes: the existing ADR files in `docs/adr/*.md`.
- Produces: a Nix check `adr-acceptance-criteria` that exits 0 if every ADR not in the allowlist has, in its "Acceptance Criteria" section, at least one path matching `fixtures/|test/|fixtures/v0/facts/prolog/`.

- [ ] **Step 1: Write the allowlist**

Create `docs/adr/.acceptance-legacy-allowlist` (one ADR basename per line):

```
0001-manifest-identity.md
0002-parser-evaluation.md
0003-nix-materialization.md
0004-supply-chain-release-security.md
0005-operational-runtime.md
0006-v0-design-bundle-validation.md
0007-external-parser-validation-boundary.md
0008-abc-tools-runtime.md
0009-imported-output-materialization.md
0010-manifest-identity-hardening.md
0011-generated-fixture-policy.md
0012-tei-odd-schematron-validation.md
0013-cultural-heritage-lod-profile.md
0014-iiif-applicability.md
0017-vocabulary-review.md
0018-predicate-rename-batch-1.md
0020-person-identity-drift-data-model.md
0021-person-identity-drift-harness.md
0022-upstream-ingest-drift-awareness.md
```

- [ ] **Step 2: Write the lint script**

Create `nix/check-acceptance-criteria.sh`:

```bash
#!/usr/bin/env bash
# Layer F: every ADR with an "Acceptance Criteria" section must name an
# executable condemnation (fixtures/|test/|facts/) per invariant clause —
# unless the ADR is on the legacy allowlist. Ratcheted: forward rule for
# new/changed ADRs; allowlist exempts the 19 pre-existing ADRs (0001-0010,
# 0011-0014, 0017-0018, 0020-0022) whose Acceptance Criteria pre-date this
# gate. Verified 2026-07-04 against docs/adr/0*.md.
set -euo pipefail

adr_dir="docs/adr"
allowlist="$adr_dir/.acceptance-legacy-allowlist"
status=0

for f in "$adr_dir"/0*.md; do
  basename=$(basename "$f")
  if grep -qF "$basename" "$allowlist"; then
    continue
  fi
  if ! grep -qi '^## Acceptance Criteria' "$f"; then
    continue  # ADRs without an Acceptance Criteria section are out of scope
  fi
  # Extract the Acceptance Criteria section and check for executable paths.
  section=$(sed -n '/^## Acceptance Criteria/,/^## /p' "$f")
  if ! printf '%s\n' "$section" | grep -qE 'fixtures/|test/|facts/prolog/'; then
    echo "ADR $basename has an Acceptance Criteria section with no executable path" >&2
    echo "(expected a fixtures/|test/|facts/prolog/ reference). Add a condemned fixture," >&2
    echo "property test, or Prolog query; or, if legacy, add to .acceptance-legacy-allowlist." >&2
    status=1
  fi
done

exit $status
```

- [ ] **Step 3: Write a failing self-test for the lint**

Create `test/abc/tools/acceptance_criteria_lint_test.clj`:

```clojure
(ns abc.tools.acceptance-criteria-lint-test
  "Layer F self-test: the lint script must (a) pass on the current repo state,
  (b) fail when a non-allowlisted ADR loses its executable path. Mutation-tested."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [abc.tools.files :as files]))

(defn lint-runs-clean []
  (let [{:keys [exit out err]} (sh "bash" "nix/check-acceptance-criteria.sh")]
    [exit (str out err)]))

(deftest acceptance-lint-passes-on-current-repo-test
  (let [[exit msg] (lint-runs-clean)]
    (is (zero? exit) (str "lint failed on current repo:\n" msg))))

(deftest acceptance-lint-fails-when-path-removed-test
  (testing "a non-allowlisted ADR with an Acceptance Criteria section but no
            executable path fails the lint — using a TEMP ADR corpus so the
            real repo is untouched"
    ;; Mutation test against a temp corpus: copy docs/adr to a tmp dir,
    ;; write a synthetic ADR with prose-only Acceptance Criteria, run the
    ;; lint pointed at the tmp dir, assert non-zero exit. This is the
    ;; non-vacuous form of the self-test.
    (let [tmp (str (java.nio.file.Files/createTempDirectory
                     "abc-adr-lint" (make-array java.nio.file.attribute.FileAttribute 0)))
          adr-tmp (str tmp "/adr")]
      (doseq [f (.listFiles (java.io.File. "docs/adr"))
              :when (.isFile f)]
        (clojure.java.io/copy f (java.io.File. (str adr-tmp "/" (.getName f)))))
      (spit (str adr-tmp "/9999-synthetic.md")
            "# ADR 9999: Synthetic\n\n## Acceptance Criteria\n\n- The system MUST do X.\n")
      (let [{:keys [exit out err]}
            (clojure.java.shell/sh "bash" "-c"
                                   (str "adr_dir=" adr-tmp " allowlist=" adr-tmp
                                        "/.acceptance-legacy-allowlist; "
                                        "for f in \"$adr_dir\"/0*.md; do "
                                        "b=$(basename \"$f\"); "
                                        "grep -qF \"$b\" \"$allowlist\" && continue; "
                                        "grep -qi '^## Acceptance Criteria' \"$f\" || continue; "
                                        "section=$(sed -n '/^## Acceptance Criteria/,/^## /p' \"$f\"); "
                                        "printf '%s\n' \"$section\" | grep -qE 'fixtures/|test/|facts/' || exit 1; "
                                        "done; exit 0"))]
        ;; The synthetic 9999 ADR has no executable path and is not allowlisted
        ;; → the inline loop should exit 1 (via `exit 1`).
        (is (not (zero? exit))
            (str "lint should have failed on the synthetic prose-only ADR; out=" out " err=" err))))))
```

- [ ] **Step 4: Run the lint — verify it passes on current repo**

Run: `bash nix/check-acceptance-criteria.sh; echo "exit: $?"`
Expected: exit 0 (all ADRs with Acceptance Criteria are either allowlisted or — if any non-allowlisted ADR has Acceptance Criteria — this surfaces them; fix by allowlisting or adding a path).

- [ ] **Step 5: Add the Nix check**

In `flake.nix`, inside the `checks = forAllSystems (system: ... { ... })` block, after the existing `adr0001-invariants` entry:

```nix
          adr-acceptance-criteria =
            pkgs.runCommand "abc-adr-acceptance-criteria" { }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                bash nix/check-acceptance-criteria.sh
                mkdir -p "$out"
                echo "ADR acceptance-criteria lint passed (ratcheted)." > "$out/result.txt"
              '';
```

- [ ] **Step 6: Run the Nix check**

Run: `nix build .#checks.x86_64-linux.adr-acceptance-criteria --no-link 2>&1 | tail -10`
Expected: PASS.

- [ ] **Step 7: Mutation-test that the lint bites**

Temporarily create `docs/adr/9999-test-fixture.md` with an "Acceptance Criteria" section containing prose only (no `fixtures/|test/|facts/` path), do NOT add it to the allowlist. Run `bash nix/check-acceptance-criteria.sh`. Expected: non-zero exit with the error message. Delete the temp ADR. Re-run. Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add docs/adr/.acceptance-legacy-allowlist nix/check-acceptance-criteria.sh \
        test/abc/tools/acceptance_criteria_lint_test.clj flake.nix
git commit -m "feat(ci): ratcheted ADR-acceptance-criteria lint (Layer F)"
```

---

## Task 3: SWI-Prolog flake input pin (Layer C foundation)

**Files:**
- Modify: `flake.nix` — add `swi-prolog` to `nativeBuildInputs` for the forthcoming Prolog check; pin via the existing nixpkgs input.

**Interfaces:**
- Produces: `pkgs.swi-prolog` available to Nix checks in Tasks 6–7.

- [ ] **Step 1: Confirm swi-prolog is in the flake's nixpkgs**

Run: `nix eval nixpkgs#swi-prolog.meta.description 2>&1 | head -3`
(If nixpkgs isn't directly addressable, confirm via the flake's pinned nixpkgs: `nix flake metadata` and check the nixpkgs rev, then `nix build nixpkgs#swi-prolog --no-link --dry-run`.)
Expected: a description string or successful dry-run confirming `swi-prolog` is available.

- [ ] **Step 2: Add a smoke Nix check that swipl runs**

In `flake.nix`, add a minimal check (to be expanded in Task 6):

```nix
          swi-prolog-smoke =
            pkgs.runCommand "abc-swi-prolog-smoke" { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                # SWI-Prolog is the pinned CI dialect for Layer C (design spec §7.5).
                cat > "$TMPDIR/smoke.pl" <<'PL'
                :- initialization(main).
                main :- write('swipl ok'), nl, halt.
                PL
                swipl --quiet -t main -f "$TMPDIR/smoke.pl" > "$out"
              '';
```

- [ ] **Step 3: Run the smoke check**

Run: `nix build .#checks.x86_64-linux.swi-prolog-smoke --no-link 2>&1 | tail -10`
Expected: PASS (outputs `swipl ok`).

- [ ] **Step 4: Commit**

```bash
git add flake.nix
git commit -m "ci: pin SWI-Prolog for Layer C cross-artifact queries (spec §7.5)"
```

---

## Task 4: R1 reproducibility property test with test.check (Layer D)

**Files:**
- Modify: `test/abc/tools/code_as_spec_test.clj` — add the property test.
- Read-only (oracle): `src/abc/tools/manifest_index.clj:32` `reproducibility-conflicts` (`[entries] -> vector of conflict maps`) and `validate-no-reproducibility-conflicts!` (`[entries] -> true | throws`).

**Interfaces:**
- Consumes: `manifest-index/reproducibility-conflicts` is the REAL oracle. Its
  input is a vector of "entries" (maps) each with keys `validation_status`,
  `content_hash`, `artifact_id`, `manifest_path`. The real `successful-entry?`
  (`src/abc/tools/manifest_index.clj:6-7`) treats a status as successful iff it
  is in `#{"passed" "warning"}` — NOT `"success"`. The manifest schema enum
  (`schemas/manifest.schema.json`) is `["not-run","passed","warning","failed"]`.
  The generator MUST draw statuses from this real enum and compute success via
  the real predicate, or the property characterizes the wrong oracle.
- Produces: a `test.check` property that, for generated `(m1, m2)` entry pairs, asserts the oracle detects a conflict iff `artifact_id` matches and `content_hash` differs.

- [ ] **Step 1: Write the failing property test**

Append to `test/abc/tools/code_as_spec_test.clj`:

```clojure
(ns abc.tools.code-as-spec-test
  ;; require block extended:
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; (existing nested-id fixture test stays)

;; Layer D — R1 reproducibility conflict, bound to the real oracle
;; (manifest-index/reproducibility-conflicts). Replaces vacuous
;; r1-reproducibility-conflict.smt2 — see
;; docs/handoffs/formal-verification-assessment-critique.md §2.

(def ^:private success-statuses #{"passed" "warning"})

(defn- successful? [entry]
  ;; Mirrors manifest-index/successful-entry? verbatim — do not recompute.
  (contains? success-statuses (get entry "validation_status")))

(def gen-conflict-tuple
  "Generate a [m1 m2 expected-conflict?] tuple. m1, m2 are manifest-index
   entries. A conflict exists iff both are successful, share artifact_id,
   and differ on content_hash. expected? is computed from the ACTUAL generated
   hash equality (not a separate same-hash? flag) so the generator and the
   oracle see the same inputs."
  (gen/bind
    (gen/tuple gen/string-alphanumeric                  ; artifact_id
               gen/string-alphanumeric                  ; content_hash_1
               gen/string-alphanumeric                  ; content_hash_2 (independent)
               (gen/elements ["passed" "warning"])      ; m1 status (always successful)
               (gen/elements ["passed" "warning" "failed" "not-run"])) ; m2 status
    (fn [[id h1 h2 m1-status m2-status]]
      (let [m1 {"validation_status" m1-status
                "content_hash" h1
                "artifact_id" id
                "manifest_path" "/m1"}
            m2 {"validation_status" m2-status
                "content_hash" h2
                "artifact_id" id
                "manifest_path" "/m2"}
            ;; expected? derived from the actual generated values, using the
            ;; REAL success predicate and REAL string equality on hashes.
            expected? (and (successful? m1) (successful? m2)
                           (not= h1 h2))]
        (gen/return [m1 m2 expected?])))))

(def reproducibility-property
  (prop/for-all [[m1 m2 expected?] gen-conflict-tuple]
    (let [entries [m1 m2]
          conflicts (manifest-index/reproducibility-conflicts entries)
          ;; boolean() — (seq conflicts) is a seq or nil, not a boolean, so
          ;; (= expected? (seq conflicts)) would compare true to a seq and be
          ;; wrong on the satisfying case. Coerce to boolean.
          detected? (boolean (seq conflicts))]
      (= expected? detected?))))

(deftest r1-reproducibility-conflict-property-test
  (testing "real oracle detects conflict iff both success, same id, different hash"
    (let [result (tc/quick-check 200 reproducibility-property)]
      (is (:pass? result)
          (str "property failed:\n" (pr-str result))))))
```

Note: the generator above is aligned to the REAL `successful-entry?`
predicate (`src/abc/tools/manifest_index.clj:6-7`, statuses
#{"passed" "warning"}) and the REAL schema enum
(`schemas/manifest.schema.json`:
`["not-run","passed","warning","failed"]`). `expected?` is computed from actual
generated hash equality, not a separate flag. `detected?` is
`(boolean (seq conflicts))`. These three fixes (real statuses, boolean
coercion, expected-from-equality) are the Blocker R1 remediation — without them
the property characterizes a non-existent oracle.

- [ ] **Step 2: Run the test — verify it passes (no failure expected; this is characterization of a real oracle)**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: PASS (200 trials). Because the oracle is real and the generator matches its contract, this should pass.

- [ ] **Step 3: Mutation-test that the property bites (Box's test)**

In `src/abc/tools/manifest_index.clj:32`, the `reproducibility-conflicts` fn groups by `artifact_id` and reports if `distinct` content-hashes > 1. Temporarily change `(< 1 (count content-hashes))` to `(< 2 (count content-hashes))` (so a 2-way conflict is no longer detected). Re-run the property test. Expected: FAIL — `:pass?` is false, shrinking surfaces `[m1 m2 true]` where the oracle missed a real conflict. Restore the predicate. Re-run. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add test/abc/tools/code_as_spec_test.clj
git commit -m "test(manifest): R1 reproducibility property test against real oracle (Layer D)"
```

---

## Task 5: Prolog fact emitter (Layer C — Clojure side)

**Files:**
- Create: `src/abc/tools/facts.clj`
- Create: `test/abc/tools/facts_test.clj`

**Interfaces:**
- Consumes (read-only): `manifest/identity-object`, `manifest/artifact-id`, the real drift-event files under `examples/v0/example-persons/_events/` and person records under `examples/v0/example-persons/`, and the real manifest(s) under `examples/v0/example-work/manifest.json`.
- Produces: `facts/emit-prolog! [dir] -> nil` writes three files to `dir`:
  - `manifest_identity.pl` with facts `manifest_identity(ManifestPath, IdentityHash).` where `IdentityHash` is `manifest/artifact-id` applied to the real `manifest_identity_object` (NOT recomputed in Prolog — emits the real hash).
  - `drift.pl` with facts `drift_event(EventId).`
  - `person_records.pl` with facts `person_record(PersonId).` and `drift_successor(PersonId, SuccessorId).` read from real person records and drift logs.

- [ ] **Step 1: Write the failing emitter test**

Create `test/abc/tools/facts_test.clj`:

```clojure
(ns abc.tools.facts-test
  (:require [abc.tools.facts :as facts]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str])
  (:import [java.nio.file Files]))

(def tmp-dir (atom nil))

(use-fixtures :each
  (fn [f]
    (reset! tmp-dir (str (Files/createTempDirectory "abc-facts" (make-array java.nio.file.attribute.FileAttribute 0))))
    (f)
    (run! #(.delete (java.io.File. (str @tmp-dir "/" %)))
          ["manifest_identity.pl" "drift.pl" "person_records.pl"])
    (.delete (java.io.File. @tmp-dir))))

(deftest emit-prolog-writes-manifest-identity-facts-test
  (facts/emit-prolog! @tmp-dir)
  (let [content (slurp (str @tmp-dir "/manifest_identity.pl"))
        ;; Args are single-quoted atoms: '...'
        facts (re-seq #"manifest_identity\('[^']+',[^)]+sha256:[0-9a-f]+'\)\." content)]
    (is (seq facts) "must emit at least one manifest_identity/2 fact")))

(deftest emit-prolog-uses-real-artifact-id-test
  (facts/emit-prolog! @tmp-dir)
  ;; The emitted hash must equal manifest/artifact-id on the real identity object.
  ;; This guards against hidden hand-translation in Prolog (design §7.3).
  (let [content (slurp (str @tmp-dir "/manifest_identity.pl"))]
    (is (re-find #"sha256:[0-9a-f]{64}" content)
        "emitted hash is the real artifact-id shape")))

(deftest emit-prolog-quotes-person-ids-test
  (facts/emit-prolog! @tmp-dir)
  ;; All args single-quoted — bare 000879 would corrupt to 879 in SWI.
  (let [content (slurp (str @tmp-dir "/person_records.pl"))]
    (is (re-find #"person_record\('000879'\)\." content)
        "person_id must be quoted to survive SWI parsing intact")))

(deftest emit-prolog-writes-drift-successor-facts-test
  (facts/emit-prolog! @tmp-dir)
  ;; Task 7 depends on drift_successor/2 — guard that it's actually emitted
  ;; (Blocker: Task 5 used to promise it but only wrote person_record/1).
  (let [content (slurp (str @tmp-dir "/person_records.pl"))]
    (is (re-find #"drift_successor\('[^']+'[^)]*\)\." content)
        "must emit drift_successor/2 facts resolved from prov.used/was_generated_by")))
```

- [ ] **Step 2: Run test — verify it fails (`facts` namespace does not exist)**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: FAIL — `Unable to resolve symbol: emit-prolog!`.

- [ ] **Step 3: Write the minimal emitter**

Create `src/abc/tools/facts.clj`:

```clojure
(ns abc.tools.facts
  "Layer C fact emitter. Reads REAL manifests / drift events / person records
  and writes Prolog facts to a target directory. Identity hashes are computed
  here via the real manifest/artifact-id function — Prolog only compares
  emitted facts, never recomputes identity. (Design spec §7.3: avoid hidden
  hand-translation.) Generates fixtures/v0/facts/prolog/ via the Nix gate."
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^:private example-manifests
  ["examples/v0/example-work/manifest.json"])

(def ^:private example-drift-events-dir
  "examples/v0/example-persons/_events")

(def ^:private example-persons-dir
  "examples/v0/example-persons")

;; All Prolog string args are emitted as single-quoted atoms with `'` doubled
;; to `''`. This is CRITICAL: bare `000879` parses as the integer 879, and
;; `abc-000000000001` parses as the arithmetic expression abc-1. Real IDs
;; (person_id, artifact_id, drift_event_id, manifest_path) are strings — quote
;; them or the facts lose identity.
(defn- prolog-atom [s]
  (str "'" (.replace (str s) "'" "''") "'"))

(defn- ensure-dir! [out-dir]
  (.mkdirs (io/file out-dir)))

(defn- write-lines! [out-dir filename lines]
  ;; Sort for byte-stability across .listFiles orderings (Blocker: medium).
  ;; Parent dir created up-front; empty corpus writes an empty file (not none).
  (spit (io/file out-dir filename) (str/join "\n" (sort lines)) :append false))

(defn- emit-manifest-identity-facts! [out-dir]
  (let [lines (for [path example-manifests
                    :let [m (files/read-json path)
                          id-obj (get m "manifest_identity_object")
                          hash (manifest/artifact-id id-obj)]]
                (format "manifest_identity(%s, %s)."
                        (prolog-atom path) (prolog-atom hash)))]
    (write-lines! out-dir "manifest_identity.pl" lines)))

(defn- emit-drift-facts! [out-dir]
  (let [dir (io/file example-drift-events-dir)
        files (->> (.listFiles dir)
                   (filter #(.isFile %))
                   (sort-by #(.getName %)))
        lines (for [f files
                    :let [ev (files/read-json f)
                          id (get ev "drift_event_id")]]
                (format "drift_event(%s)." (prolog-atom id)))]
    (write-lines! out-dir "drift.pl" lines)))

(defn- participant-by-snapshot [event]
  ;; snapshot_id -> person_id, for resolving prov.used / was_generated_by
  ;; references back to participants[].person_id.
  (into {} (for [p (get event "participants")]
             [(get p "snapshot_id") (get p "person_id")])))

(defn- emit-person-record-facts! [out-dir]
  ;; person_record/1 from person-record files.
  (let [persons (io/file example-persons-dir)
        record-files (->> (.listFiles persons)
                          (filter #(and (.isFile %)
                                        (str/ends-with? (.getName %) ".json")))
                          (sort-by #(.getName %)))
        record-lines (for [f record-files
                           :let [r (files/read-json f)
                                 pid (get r "person_id")]]
                       (format "person_record(%s)." (prolog-atom pid)))]
    (write-lines! out-dir "person_records.pl" record-lines)
    ;; drift_successor/2: for each drift event, every post- participant
    ;; (successor) is a successor of every pre- participant (predecessor).
    ;; Resolved by mapping prov.used (pre snapshot_ids) and
    ;; prov.was_generated_by (post snapshot_ids) through participants to
    ;; person_ids. Real corpus is the source — no hand-transcription.
    (let [event-files (->> (.listFiles (io/file example-drift-events-dir))
                           (filter #(.isFile %))
                           (sort-by #(.getName %)))
          successor-lines (for [f event-files
                                :let [ev (files/read-json f)
                                      snap->pid (participant-by-snapshot ev)
                                      pre-pids (for [s (get-in ev ["prov" "used"])]
                                                 (get snap->pid s))
                                      post-pids (for [s (get-in ev ["prov" "was_generated_by"])]
                                                  (get snap->pid s))
                                      post (set (remove nil? post-pids))
                                      pre (set (remove nil? pre-pids))]
                                succ post
                                pred pre]
                            (format "drift_successor(%s, %s)."
                                    (prolog-atom pred) (prolog-atom succ)))]
      ;; Append drift_successor/2 facts to the same file (separated by a comment).
      (let [existing (slurp (io/file out-dir "person_records.pl"))
            all-lines (str existing (when (seq successor-lines)
                                      (str "\n\n% drift_successor/2 emitted from prov.used / was_generated_by\n"
                                           (str/join "\n" (sort successor-lines)))))]
        (spit (io/file out-dir "person_records.pl") all-lines)))))

(defn emit-prolog! [out-dir]
  (ensure-dir! out-dir)
  (emit-manifest-identity-facts! out-dir)
  (emit-drift-facts! out-dir)
  (emit-person-record-facts! out-dir))
```

Note: the emitter now writes BOTH `person_record/1` AND `drift_successor/2`
(needed by Task 7), quotes every string arg as a single-quoted SWI atom
(`'` doubled — bare `000879` would parse as integer 879, `abc-...` as an
arithmetic expr), sorts files by name for byte-stability, and creates the
output directory up-front. Verify the real person-record file shape
(`examples/v0/example-persons/000879.json` has `person_id: "000879"`) and
the real drift-event `prov` shape (`used`, `was_generated_by` arrays of
snapshot_ids resolvable through `participants[].snapshot_id`) at execution
time before finalizing the successor resolution.

- [ ] **Step 4: Run the tests — verify they pass**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/facts.clj test/abc/tools/facts_test.clj
git commit -m "feat(facts): Prolog fact emitter reading real corpus (Layer C, Clojure side)"
```

---

## Task 6: Manifest-identity-emitter sanity Prolog gate (Layer C — SWI side)

> **Honesty note (Blocker Position-L remediation):** the gate in this task is
> an **emitter sanity check** — it asserts no duplicate `manifest_identity/2`
> facts exist for the same manifest path. It does **NOT** prove ADR 0020
> Position L ("drift events do not rotate `manifest_identity_object`").
> Position L is enforced **structurally** today: drift events live in
> separate files (`_events/*.json`) that the manifest schema never references
> as identity inputs, so a drift event cannot rotate identity by construction.
> A real before/after Prolog proof of Position L would require the emitter to
> model a hypothetical post-drift identity (defining what identity *would be*
> if drift entered the identity object), which ADR 0020 forbids — that modeling
> is future work, not this task. **Do not claim this gate replaces Position L**
> in Task 8/9; claim only emitter sanity.

**Files:**
- Create: `fixtures/v0/facts/prolog/manifest_identity.pl` (emitted by a Nix check invoking the emitter, then committed; byte-stable per ADR 0011)
- Create: `fixtures/v0/facts/prolog/drift.pl`
- Create: `fixtures/v0/facts/prolog/person_records.pl`
- Create: `docs/adr/manifest-identity-emitter-sanity.pl` — committed query (renamed honestly from `position-l-rotation.pl`).
- Modify: `flake.nix` — `checks.prolog-facts` (emits + commits facts) and `checks.prolog-cross-artifact` (runs the query)

**Interfaces:**
- Consumes: `facts/emit-prolog!` (Task 5); `pkgs.swi-prolog` (Task 3).
- Produces: a Nix check `prolog-cross-artifact` that runs SWI-Prolog against the committed fact files + the query. The Clojure-side byte-compare (committed facts vs freshly-emitted) runs under the existing `clj-nix-focused-tests` derivation (which has the `cljDepsCache` + `deps.edn` replacement setup) — NOT in this Nix gate. This gate shells to `swipl` only; it does not shell to `clojure`, so it avoids the Nix-sandbox offline-dependency failure the plain `clojure -e` would hit.

- [ ] **Step 1: Write the emitter-sanity query**

Create `docs/adr/manifest-identity-emitter-sanity.pl`:

```prolog
% Emitter sanity: no duplicate manifest_identity/2 facts for the same manifest
% path. This is NOT a proof of ADR 0020 Position L — see the task header. It
% guards that the emitter (Task 5) didn't write two different identity hashes
% for one manifest, which would make downstream Prolog comparisons meaningless.
%
% Design §7.3: identity facts are EMITTED by the Clojure emitter using the real
% manifest/artifact-id. Prolog only compares emitted facts.

:- use_module(library(lists)).

% A violation: two different hashes claimed for the same manifest path.
violating_dup_identity(Manifest) :-
    manifest_identity(Manifest, H1),
    manifest_identity(Manifest, H2),
    H1 \= H2.

% Run: swipl -q -t "halt(\+ violating_dup_identity(_))" manifest_identity.pl
% Exit 0 (halt(true)) = no violation; exit 1 (halt(false)) = violation found.
```

Note: this is the **minimal honest** version — emitter sanity only. A fuller
Position-L query (pre-drift-vs-post-drift identity comparison) would require
the emitter to model a hypothetical post-drift identity, which ADR 0020
forbids; that is future work, NOT claimed by this task.

- [ ] **Step 2: Emit the committed fact files (developer step, OUTSIDE Nix)**

The committed `fixtures/v0/facts/prolog/*.pl` files are regenerated by a
developer running the emitter locally, then committed. They are NOT regenerated
inside a Nix check — the Nix sandbox's offline dependency setup
(`cljDepsCache` + `deps.edn` replacement, flake.nix:312-345) lives only in the
`clj-nix-focused-tests` derivation. To regenerate, from the repo root:

```bash
mkdir -p fixtures/v0/facts/prolog
clojure -M:abc/dev -e "(require '[abc.tools.facts :as f]) (f/emit-prolog! \"fixtures/v0/facts/prolog\")"
```

(Use whichever alias provides a classpath with `org.clojure/test.check`; if
none exists yet, add a `:abc/dev` alias to `deps.edn`, or invoke via
`clojure -M:test` if that alias exists. The point is: this is a local
reproducible Clojure invocation, not a Nix-shell invocation.)

Verify the three `.pl` files exist, contain real `sha256:...` hashes, and that
IDs are single-quoted (`'000879'`, not `000879`).

The CI guard against stale committed facts is Step 3's byte-compare test,
which runs under `clj-nix-focused-tests` (which HAS the offline-deps setup)
and re-emits to a temp dir, diffing against committed. So: developer regenerates
+ commits; CI verifies committed matches a fresh emit.

- [ ] **Step 3: Write the byte-compare test (runs under clj-nix-focused-tests)**

The `clj-nix-focused-tests` derivation already has the `cljDepsCache` +
`deps.edn` replacement + `ABC_TEI_SCHEMA_SKIP=1` setup (flake.nix:312-345),
so the Clojure emitter runs correctly there. Add to `test/abc/tools/facts_test.clj`:

```clojure
(deftest committed-facts-match-real-corpus-test
  (let [tmp (str (Files/createTempDirectory "abc-facts2" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (facts/emit-prolog! tmp)
    (doseq [f ["manifest_identity.pl" "drift.pl" "person_records.pl"]]
      (let [committed (slurp (str "fixtures/v0/facts/prolog/" f))
            emitted (slurp (str tmp "/" f))]
        (is (= committed emitted)
            (str "committed " f " diverges from real corpus; regenerate via the emitter"))))))
```

This is the CI guard that committed facts aren't stale (regression: corpus
changed, committed `.pl` not regenerated). It runs in the derivation that has
the offline Clojure deps — NOT in the swipl-only Nix gate.

- [ ] **Step 4: Run the test — verify it passes**

Run: `nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link 2>&1 | tail -20`
Expected: PASS (the committed facts match freshly-emitted facts from the real corpus).

- [ ] **Step 5: Add the Nix gate (swipl-only, no Clojure)**

In `flake.nix`. Note: `nativeBuildInputs` has ONLY `swi-prolog` (no `clojure`)
— the Clojure byte-compare is in Step 3's test under `clj-nix-focused-tests`,
which already has the offline-deps setup. This gate only runs swipl against
committed facts. (Blocker High remediation: a plain `clojure -e` here would
fail in the Nix sandbox without the `cljDepsCache` pattern.)

```nix
          prolog-cross-artifact =
            pkgs.runCommand "abc-prolog-cross-artifact"
              { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                set -euo pipefail
                # Reads COMMITTED fact files (regenerated & byte-checked by the
                # clj-nix-focused-tests derivation via the emitter). swipl only.
                swipl --quiet -t "halt(\\+ violating_dup_identity(_))" \
                      -c fixtures/v0/facts/prolog/manifest_identity.pl \
                      -c docs/adr/manifest-identity-emitter-sanity.pl
                mkdir -p "$out"
                echo "Manifest-identity-emitter sanity holds (SWI-Prolog)." > "$out/result.txt"
              '';
```

(Step 7 (Task 7) extends this gate to also load `person_records.pl` + `drift.pl`
and run the referential-integrity query.)

- [ ] **Step 6: Run the Nix gate**

Run: `nix build .#checks.x86_64-linux.prolog-cross-artifact --no-link 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 7: Mutation-test that the gate bites**

Edit `fixtures/v0/facts/prolog/manifest_identity.pl` and add a second
`manifest_identity('examples/v0/example-work/manifest.json', 'sha256:deadbeef...').`
line with a different hash for the same manifest. Re-run the Nix gate. Expected:
FAIL — `violating_dup_identity` succeeds, halt(false) → exit 1. Restore the
file. Re-run. Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add fixtures/v0/facts/prolog/*.pl docs/adr/manifest-identity-emitter-sanity.pl \
        test/abc/tools/facts_test.clj flake.nix
git commit -m "feat(ci): SWI-Prolog manifest-identity-emitter sanity gate (Layer C)"
```

(Commit message says "emitter sanity", NOT "Position L" — honesty per the task
header. Position L remains enforced structurally + via the manifest schema; a
real Prolog proof is future work.)

---

## Task 7: Person-id referential integrity query (Layer C cont.)

**Files:**
- Create: `docs/adr/person-id-referential-integrity.pl`
- Modify: `flake.nix` — extend `prolog-cross-artifact` to run both queries.

**Interfaces:**
- Consumes: `person_record/1` and `drift_successor/2` facts emitted in Task 5/6.

- [ ] **Step 1: Write the query**

Create `docs/adr/person-id-referential-integrity.pl`:

```prolog
% Referential integrity: every person_id appearing in a drift_successor/2
% fact (as either predecessor or successor) must resolve to a committed
% person_record/1. Task 5's emitter resolves successors from prov.used /
% was_generated_by through participants[].snapshot_id. A dangling person_id
% means a drift event references a person not present in the corpus.
:- use_module(library(lists)).

% A person_id mentioned by drift_successor/2 in either argument but with no
% committed person_record/1 fact.
dangling_person(Pid) :-
    ( drift_successor(Pid, _) ; drift_successor(_, Pid) ),
    \+ person_record(Pid).

% Run: swipl -q -t "halt(\+ dangling_person(_))" -c person_records.pl -c drift.pl -c person-id-referential-integrity.pl
```

Note: `drift_successor/2` is emitted by Task 5 (Blocker remediation: it was
promised but originally absent). Person_ids are single-quoted atoms (`'000879'`)
so `\=` and unification compare string atoms, not corrupted integers.

- [ ] **Step 2: Extend the Nix gate to run both queries**

In `flake.nix` `prolog-cross-artifact` (Task 6 Step 5), after the
emitter-sanity swipl call, add a second swipl call that loads
`person_records.pl` (which now contains both `person_record/1` and
`drift_successor/2`) and `drift.pl`:

```nix
                swipl --quiet -t "halt(\\+ dangling_person(_))" \
                      -c fixtures/v0/facts/prolog/person_records.pl \
                      -c fixtures/v0/facts/prolog/drift.pl \
                      -c docs/adr/person-id-referential-integrity.pl
```

- [ ] **Step 3: Run the gate**

Run: `nix build .#checks.x86_64-linux.prolog-cross-artifact --no-link 2>&1 | tail -20`
Expected: PASS (real corpus has no dangling successors).

- [ ] **Step 4: Mutation-test the gate**

Temporarily edit `fixtures/v0/facts/prolog/person_records.pl` and delete one `person_record(...).` line that a `drift_successor/2` references. Re-run. Expected: FAIL. Restore. Re-run. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add docs/adr/person-id-referential-integrity.pl flake.nix
git commit -m "feat(ci): person-id referential integrity Prolog gate (Layer C)"
```

---

## Task 8: Update the critiques/handoff to reflect replacements

**Files:**
- Modify: `docs/handoffs/formal-verification-assessment-critique.md` — add a "Resolution" section pointing to the new gates.

- [ ] **Step 1: Add a Resolution section**

Append to `docs/handoffs/formal-verification-assessment-critique.md`:

```markdown
## 9. Resolution (2026-07-04)

The mode-B vacuity identified in §2 is resolved by replacing each vacuous SMT
file with a gate that reads a real artifact (design:
docs/superpowers/specs/2026-07-04-code-as-spec-formal-models-design.md):

| Old vacuous file | Replacement (reads real artifact) |
|---|---|
| r1-reproducibility-conflict.smt2 | test/abc/tools/code_as_spec_test.clj — test.check property against manifest-index/reproducibility-conflicts (Layer D) |
| r2-non-circularity.smt2 | fixtures/v0/invalid/manifest-nested-artifact-id/ + fixture-battery test (Layer A) |
| r3-drift-cardinality.smt2 | test/abc/tools/person_drift_test.clj — six-mode schema cardinality test (already shipped 48c4c65) |
| (cross-artifact, new) | docs/adr/manifest-identity-emitter-sanity.pl — SWI-Prolog emitter sanity (NO dup manifest_identity facts); docs/adr/person-id-referential-integrity.pl — person_id referential integrity over emitted facts (Layer C). **NOT** a proof of ADR 0020 Position L — see note below. |
| (process gate, new) | nix/check-acceptance-criteria.sh — ratcheted ADR-acceptance lint (Layer F) |

**Honesty note:** the Prolog gate is an emitter-sanity + referential-integrity
check, NOT a proof of ADR 0020 Position L ("drift events do not rotate
`manifest_identity_object`"). Position L is enforced structurally: drift
events live in separate files the manifest schema never references as identity
inputs. A real before/after Prolog proof of Position L would require modeling a
hypothetical post-drift identity that ADR 0020 forbids — future work, not claimed
by this resolution.

Each replacement is mutation-tested to confirm its verdict changes on a real
regression. The three .smt2 files, their Nix gates, and 0001-invariants.README.md
are deleted in Task 9 once the replacements are green.
```

- [ ] **Step 2: Commit**

```bash
git add docs/handoffs/formal-verification-assessment-critique.md
git commit -m "docs(handoffs): record formal-model replacement resolution in critique"
```

---

## Task 9: Delete the vacuous SMT files + gates (Layer G sweep)

**Prerequisite:** Tasks 1, 4, 6, 7 are green in CI; the replacements cover R1 (Task 4), R2 (Task 1), R3 (already shipped), and the cross-artifact invariants (Tasks 6–7).

**Files:**
- Delete: `docs/adr/r1-reproducibility-conflict.smt2`
- Delete: `docs/adr/r2-non-circularity.smt2`
- Delete: `docs/adr/r3-drift-cardinality.smt2`
- Delete: `docs/adr/0001-invariants.README.md`
- Modify: `flake.nix` — remove the `adr0001-invariants`, `adr0020-drift-cardinality`, and `adr-invariants-vacuity` checks (G: delete, not dormant).

- [ ] **Step 1: Verify replacements are green**

Run:

```bash
nix build .#checks.x86_64-linux.clj-nix-focused-tests \
          .#checks.x86_64-linux.adr-invariants-vacuity \
          .#checks.x86_64-linux.prolog-cross-artifact --no-link 2>&1 | tail -5
```

Expected: all PASS. If any fails, STOP — do not delete until green.

- [ ] **Step 2: Delete the SMT files and README**

```bash
git rm docs/adr/r1-reproducibility-conflict.smt2 \
       docs/adr/r2-non-circularity.smt2 \
       docs/adr/r3-drift-cardinality.smt2 \
       docs/adr/0001-invariants.README.md
```

- [ ] **Step 3: Remove the three Nix checks from flake.nix**

Edit `flake.nix`: delete the `adr0001-invariants`, `adr0020-drift-cardinality`, and `adr-invariants-vacuity` `pkgs.runCommand` blocks. Run `nix fmt flake.nix`.

- [ ] **Step 4: Verify the flake is clean after deleting checks**

Selected-check builds aren't enough after deleting Nix outputs — a stale
reference elsewhere could survive. Run a full flake check + the design-bundle
runtime (which the deleted SMT gates used to shadow):

```bash
# Full flake check catches stale references to deleted checks.
nix flake check --print-build-logs 2>&1 | tail -20
# The design-bundle runtime is the real invariant carrier for manifest
# identity (ADR 0001). Confirm it still runs end-to-end after the sweep.
nix run .#validate-design-bundle 2>&1 | tail -20
```

Expected: `nix flake check` exits 0; `validate-design-bundle` succeeds. If
either fails, the sweep left a dangling reference — fix before committing.

- [ ] **Step 5: Commit**

```bash
git add -A flake.nix docs/adr/
git commit -m "chore(adr): retire vacuous R1/R2/R3 SMT + gates (Layer G sweep)

Replaced by real-artifact-reading gates (Tasks 1, 4, 6–7). Deletes r1/r2/r3
.smt2, 0001-invariants.README.md, and the adr0001-invariants /
adr0020-drift-cardinality / adr-invariants-vacuity Nix checks. No model
remains that takes a hand-transcribed restatement of a prose spec as input."
```

---

## Self-Review

**1. Spec coverage:**
- Layer A (schemas-as-spec + fixtures) → Task 1 (R2). R3 already shipped (`48c4c65`). ✅
- Layer F (ADR-acceptance lint, ratcheted) → Task 2. Allowlist has 19 ADRs (0001-0010, 0011-0014, 0017-0018, 0020-0022). ✅
- Layer D (test.check, R1) → Task 4. Generator aligned to real `successful-entry?` (`#{passed warning}`), real schema enum; `expected?` from actual hash equality; `detected?` = `(boolean (seq conflicts))`. ✅
- Layer C (Prolog over emitted facts) → Tasks 5, 6, 7. Task 6 is **emitter sanity** (honestly named, NOT a Position L proof); Task 7 is person-id referential integrity. ✅
- Layer G (delete SMT on replacement) → Task 9. ✅
- SWI-Prolog pin (spec §7.5) → Task 3. ✅
- Facts location `fixtures/v0/facts/prolog/` → Tasks 5, 6. ✅
- "Delete not dormant" vacuity gate → Task 9 deletes it. ✅
- Ratchet allowlist (19 ADRs, incl. 0011/0021) → Task 2. ✅
- Position L — NOT claimed by any Prolog gate (Task 6 header + Task 8 honesty note). Enforced structurally; real proof is future work. ✅
- Errata `manifest_identity_object` (not `artifact_identity_object`) → reflected in Global Constraints and Task 5. ✅
- Nix Clojure offline-deps pattern → Task 6 Step 3 byte-compare runs under `clj-nix-focused-tests`; Task 6 Step 5 Nix gate is swipl-only (no `clojure -e`). ✅
- Prolog ID quoting (single-quoted atoms) → Task 5 `prolog-atom` helper; Task 5 `emit-prolog-quotes-person-ids-test`. ✅
- `drift_successor/2` actually emitted (not just promised) → Task 5 emitter + `emit-prolog-writes-drift-successor-facts-test`. ✅
- Byte-stability (sort files, mkdir parents) → Task 5 `write-lines!` + `ensure-dir!`. ✅
- Task 9 broadened verification (`nix flake check` + `validate-design-bundle`) → Task 9 Step 4. ✅

**2. Placeholder scan:** No "TBD"/"implement later"/"similar to Task N". The
former `(is true "TODO...")` in Task 2 Step 3 is REPLACED by a non-vacuous
temp-corpus mutation test. Task 4's generator is fully specified against the
real predicate (no "verify at execution time" hedge — the exact enum and
statuses are given). Task 6 Step 1 honestly labels itself emitter-sanity, not
Position L.

**3. Type consistency:** `facts/emit-prolog!` arity-1 throughout (Task 5 def,
Task 6 Step 2 developer invocation, Task 6 Step 3 byte-compare test).
`reproducibility-conflicts` takes `entries` (vector of maps) — Task 4 generator
produces that shape. `manifest/artifact-id` takes `identity-object` (map) —
Task 5 calls it on `(get m "manifest_identity_object")`. Prolog fact shapes:
`manifest_identity/2`, `drift_event/1`, `person_record/1`, `drift_successor/2`
— consistent across Task 5, Task 6, Task 7. All Prolog string args
single-quoted via `prolog-atom`.

---

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-07-04-code-as-spec-formal-models.md`. Two execution options:

**1. Subagent-Driven (recommended)** — I dispatch a fresh subagent per task, review between tasks, fast iteration.

**2. Inline Execution** — Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
