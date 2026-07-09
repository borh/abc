# Review Follow-ups Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the accumulated final-review follow-ups from the ruby-annotation-view and annotation-input-view branches: consolidate the duplicate schema-contracts generators, fix the acceptance-lint case-sensitivity bug, bring the annotation release-guardrail test to parity with its tokenized twin, harden `annotation-join` (documented preconditions + zero-width guard), and extend fixture hash pinning family-wide.

**Architecture:** Five small independent hardening tasks. The generator consolidation makes `abc/tools/schema_contracts.py` a thin wrapper over the monorepo-root `scripts/abc_schema_contracts.py` `abc` profile (exactly how ab-validator's wrapper already works), eliminating the dual-`SCHEMA_FILES` failure mode that produced a Critical in the last review. Everything else is tests plus one guarded line in `annotation_join.clj` and one `sed` flag in the lint script.

**Tech Stack:** Clojure (abc tools), Python, bash, kaocha.

## Global Constraints

- All work in a git worktree on a feature branch under `.worktrees/`; merge to main when done (many concurrent sessions).
- Test runner: `bin/kaocha` from `abc/` (NOT `clojure -M:test`). Focused: `bin/kaocha --focus <ns>`.
- Design bundle gate: `clojure -M:abc/validate-design-bundle` from `abc/`.
- Baseline: 457 tests, 1647 assertions, 0 failures. The suite must be green at every task's commit; the acceptance lint on ADR 0028 must stay green throughout.
- Schema-contracts check gates that must be exit 0 at every commit: `python tools/schema_contracts.py` from `abc/`, and `python scripts/schema_contracts.py` from `ab-validator/`.
- The `join` contract keeps exactly four classifications (`aligned-single|aligned-multi|stem-prefix|conflict`) — no fifth class; zero-width behavior is documented, not enum-extended.
- Working dir for all paths below: `abc/` unless prefixed `ab-validator/` or `scripts/` (monorepo root).

---

### Task 1: Consolidate the schema-contracts generators

**Files:**
- Rewrite: `tools/schema_contracts.py` (abc) — becomes a wrapper; its duplicate `SCHEMA_FILES`/logic is deleted
- Reference (do not modify): `scripts/abc_schema_contracts.py` (root, the single source of truth — it already has an `abc` profile in `PROFILE_DEFAULTS` and stricter semver validation), `ab-validator/scripts/schema_contracts.py` (the wrapper shape to copy)

**Interfaces:**
- Produces: `python tools/schema_contracts.py [--write]` behaves identically (same check/write CLI, same manifest), so the nix drift derivation (`flake.nix` ~line 464 runs `python tools/schema_contracts.py`) is untouched. One `SCHEMA_FILES` list remains, in the root script.

- [ ] **Step 1: Prove the root script's abc profile reproduces the committed manifest** (pre-flight; a mismatch here means the two `SCHEMA_FILES` lists have already drifted — STOP and report BLOCKED with the diff)

```bash
cd abc && python ../scripts/abc_schema_contracts.py --profile abc && echo "abc-profile-check-exit=$?"
```

Expected: exit 0 (check mode against the committed `schemas/schema-contracts.json`). If the root script's CLI differs (no `--profile` check mode), read its `parse_args` and use the equivalent invocation.

- [ ] **Step 2: Replace `tools/schema_contracts.py` with the wrapper** (byte-for-byte the ab-validator wrapper pattern, profile swapped; note `parents[2]` is correct for both — `abc/tools/x.py` → repo root, same depth as `ab-validator/scripts/x.py`)

```python
#!/usr/bin/env python
"""Compatibility wrapper for the monorepo schema contract generator."""

from __future__ import annotations

import runpy
import sys
from pathlib import Path


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[2]
    sys.argv = [
        str(root / "scripts" / "abc_schema_contracts.py"),
        "--profile",
        "abc",
        *sys.argv[1:],
    ]
    runpy.run_path(sys.argv[0], run_name="__main__")
```

- [ ] **Step 3: Verify all four gates**

```bash
cd abc && python tools/schema_contracts.py; echo "abc-check=$?"
python tools/schema_contracts.py --write && git diff --exit-code schemas/schema-contracts.json; echo "abc-write-stable=$?"
cd ../ab-validator && python scripts/schema_contracts.py; echo "abv-check=$?"
```

Expected: all exit 0 — check clean, `--write` byte-stable (no diff), ab-validator side untouched and clean. (The root script's semver `VERSION_RE` now validates abc's schemas; all registered versions are `X.Y.Z`, so this tightening should be a no-op — if any schema fails it, STOP and report BLOCKED.)

- [ ] **Step 4: Run the suite and commit**

Run: `bin/kaocha` (457/0 — no Clojure changes) from abc/.

```bash
git add abc/tools/schema_contracts.py
git commit -m "refactor(schema-contracts): abc generator becomes a wrapper over the root script's abc profile (single SCHEMA_FILES)"
```

(Run the `git` commands from the monorepo root, or adjust the path accordingly.)

---

### Task 2: Acceptance-lint case-insensitive section extraction

**Files:**
- Modify: `nix/check-acceptance-criteria.sh` (the `section=$(sed -n ...)` line)
- Test: `test/abc/tools/acceptance_criteria_lint_test.clj` (add one deftest)

**Interfaces:**
- Produces: the lint's section extraction matches its presence grep's case-insensitivity, so a lowercase `## Acceptance criteria` heading with a valid executable path passes instead of false-failing on an empty extracted section (the exact bug that bit ADR 0028).

- [ ] **Step 1: Write the failing test** (append to `acceptance_criteria_lint_test.clj`; it mirrors the file's existing `acceptance-lint-passes-when-path-present-on-new-adr-test` temp-corpus pattern — same `sh`/`env` helpers already defined at the top of the file)

```clojure
(deftest acceptance-lint-handles-lowercase-heading-test
  (testing "a lowercase '## Acceptance criteria' heading WITH a valid
            executable path passes — the presence grep is case-insensitive,
            so section extraction must be too (regression: ADR 0028's
            original lowercase heading extracted an empty section and
            false-failed the lint)"
    (let [tmp (str (Files/createTempDirectory
                    "abc-adr-lint-case" (make-array FileAttribute 0)))
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.acceptance-criteria-lint-test`
Expected: the new deftest FAILS (exit 1 — the case-sensitive `sed` extracts an empty section, so the path grep finds nothing); the three pre-existing lint tests still pass.

- [ ] **Step 3: Fix the extraction** — in `nix/check-acceptance-criteria.sh`, replace

```bash
  section=$(sed -n '/^## Acceptance Criteria/,/^## /p' "$f")
```

with (GNU sed `I` address flag — this environment is nix/GNU):

```bash
  section=$(sed -n '/^## acceptance criteria/I,/^## /p' "$f")
```

- [ ] **Step 4: Run tests to verify all four lint tests pass**

Run: `bin/kaocha --focus abc.tools.acceptance-criteria-lint-test` then `bin/kaocha`
Expected: focused 4 tests PASS; full suite 458 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add nix/check-acceptance-criteria.sh test/abc/tools/acceptance_criteria_lint_test.clj
git commit -m "fix(lint): acceptance-criteria section extraction is case-insensitive, matching its presence grep"
```

---

### Task 3: Annotation release-guardrail test parity with the tokenized twin

**Files:**
- Modify: `test/abc/tools/manifest_index_test.clj` (extend the existing `annotation-release-guardrail-test`, ~line 529)

**Interfaces:**
- Consumes: the file's existing `annotation-manifest-from-producer` builder (~line 143) and `manifest-index/annotation-release-guardrail-errors`.
- Produces: the annotation guardrail test covers the same variants its tokenized twin (`tokenized-release-guardrail-test`, ~line 387) covers: a `failed`-status manifest proven EXCLUDED by the `successful-entry?` filter, and a missing-key (absent `annotation_policy_hash`) variant proven reported alongside the explicit-null one.

- [ ] **Step 1: Extend the test** — rework `annotation-release-guardrail-test` to add two entries, mirroring the tokenized twin's structure. Reuse `annotation-manifest-from-producer` exactly as the existing entries do (its current calls in this test show the arity: `(annotation-manifest-from-producer artifact-id producer-id policy-hash)` with optional override maps); for the missing-key variant, build with any hash then remove the key post-hoc — `(update-in built ["manifest_identity_object"] dissoc "annotation_policy_hash")` — IF the identity-overrides argument cannot express key removal (check the builder first; reuse, don't invent). Shape:

```clojure
(deftest annotation-release-guardrail-test
  (let [producer-id (files/example-hash "95")
        passed-annotation (annotation-manifest-from-producer
                           (files/example-hash "96") producer-id (files/example-hash "97"))
        null-policy-annotation (annotation-manifest-from-producer
                                (files/example-hash "98") producer-id nil
                                {} {"used" [producer-id]})
        failed-annotation (annotation-manifest-from-producer
                           (files/example-hash "93") producer-id nil
                           {} {"used" [producer-id]})
        failed-annotation (assoc failed-annotation "validation_status" "failed")
        missing-key-annotation (-> (annotation-manifest-from-producer
                                    (files/example-hash "94") producer-id
                                    (files/example-hash "97"))
                                   (update "manifest_identity_object"
                                           dissoc "annotation_policy_hash"))
        entries (manifest-index/index-entries
                 {"passed-annotation.manifest.json" passed-annotation
                  "null-policy-annotation.manifest.json" null-policy-annotation
                  "failed-annotation.manifest.json" failed-annotation
                  "missing-key-annotation.manifest.json" missing-key-annotation})]
    (is (= [{:artifact_id (files/example-hash "94")
             :manifest_path "missing-key-annotation.manifest.json"
             :validation_status "passed"}
            {:artifact_id (files/example-hash "98")
             :manifest_path "null-policy-annotation.manifest.json"
             :validation_status "passed"}]
           (manifest-index/annotation-release-guardrail-errors entries))
        "explicit-null AND missing-key both reported; failed-status excluded")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Successful annotation manifests require annotation_policy_hash"
                          (manifest-index/validate-annotation-release-guardrail! entries)))))
```

Adapt mechanics to the actual builder (how `validation_status` is set — if the builder's override maps set it directly, prefer that over `assoc`; check how the tokenized twin's `failed-tokenized` entry does it and mirror). The expected-errors vector's ORDER must match what `annotation-release-guardrail-errors` actually returns (likely index/insertion order over the entries map — mirror how the tokenized twin pins its two-error expectation; adjust entry keys or expectation order accordingly, but keep BOTH error entries and the failed-exclusion assertion).

- [ ] **Step 2: Run to verify the extended test passes against the CURRENT validator** (this is characterization of already-correct behavior — the `successful-entry?` filter and nil-or-missing check exist; if either new case FAILS, that is a real validator bug: STOP and report it, do not adjust the validator in this task)

Run: `bin/kaocha --focus abc.tools.manifest-index-test`
Expected: PASS (all cases).

- [ ] **Step 3: Run the full suite and commit**

Run: `bin/kaocha`
Expected: 458 tests, 0 failures.

```bash
git add test/abc/tools/manifest_index_test.clj
git commit -m "test(abc): annotation release-guardrail parity with tokenized twin — failed-status exclusion and missing-key variants"
```

---

### Task 4: `annotation-join` hardening — documented preconditions + zero-width guard

**Files:**
- Modify: `src/abc/tools/annotation_join.clj` (docstring + one guard in `join`)
- Test: `test/abc/tools/annotation_join_test.clj` (add one deftest)

**Interfaces:**
- Produces: zero-width annotation spans yield `{"token_indexes" [] "classification" "conflict"}` (no spurious indexes from point-containment); the docstring records the sorted-tokens precondition and the deliberate four-class contract. Public shape otherwise unchanged.

- [ ] **Step 1: Write the failing test** (append to `annotation_join_test.clj`; `tok`/`ruby-ann` helpers already exist at the top of the file)

```clojure
(deftest join-zero-width-span-test
  (testing "zero-width spans (D3 allows empty gaiji spans) cover no text:
            no spurious token_indexes from point-containment, classified
            conflict under the deliberate four-class contract"
    (let [tokens [(tok 0 0 2 "吾輩") (tok 1 2 3 "が")]
          result (join/join tokens [(ruby-ann 1 1 "" "")])]
      (is (= [] (get (first result) "token_indexes")))
      (is (= "conflict" (get (first result) "classification"))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.annotation-join-test`
Expected: FAIL — span `[1,1)` sits strictly inside token 0's `[0,2)`, so `overlapping`'s half-open check (`0 < 1 && 1 < 2`) includes token 0: `token_indexes` is `[0]`, not `[]`. (Classification is already "conflict".)

- [ ] **Step 3: Guard and document** — in `annotation_join.clj`, change `join`'s `cover` binding and replace the docstring:

```clojure
(defn join
  "Join annotations to tokens by span intersection in the shared plaintext
  unicode-scalar coordinate system.

  Preconditions (relied on, not checked): `tokens` is sorted by input_span
  start and non-overlapping, as tokenizer output streams are — classification
  reads the first/last covering token positionally. Zero-width annotation
  spans (D3 allows empty gaiji spans) cover no text: they yield
  token_indexes [] and classification \"conflict\"; a fifth classification
  for invisible-vs-misaligned is deliberately deferred until a consumer
  needs the distinction (ADR 0028 records four probe classifications).

  Returns a generated view; never a canonical artifact (ADR 0028)."
  [tokens annotations]
  (mapv (fn [ann]
          (let [{:strs [start end]} (get ann "span")
                cover (if (= start end)
                        []
                        (overlapping tokens start end))]
            {"annotation" ann
             "token_indexes" (mapv #(get % "token_index") cover)
             "classification" (if (empty? cover)
                                "conflict"
                                (classify cover start end))}))
        annotations))
```

(Only the docstring and the `cover` binding change; `overlapping`/`classify` are untouched.)

- [ ] **Step 4: Run tests to verify pass**

Run: `bin/kaocha --focus abc.tools.annotation-join-test` then `bin/kaocha`
Expected: focused 2 tests PASS (original four-classification test untouched); full suite 459 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/annotation_join.clj test/abc/tools/annotation_join_test.clj
git commit -m "fix(abc): annotation-join zero-width guard + documented preconditions (final-review follow-up)"
```

---

### Task 5: Family-wide fixture hash pinning

**Files:**
- Create: `test/abc/tools/fixture_hash_pinning_test.clj`

**Interfaces:**
- Consumes: `manifest/schema-hash`, `analysis-identity/analysis-recipe-hash`, `analysis-identity/identity-normalization-policy-hash`.
- Produces: recompute-pins for every embedded hash in `examples/v0/example-work/token-stream.json` and `analysis-result.json` that corresponds to a committed source — extending the annotation pin (`example-work-fixture-embedded-hashes-pinned-test` in `annotation_identity_test.clj`, which stays where it is) family-wide, closing the silent-drift path the first slice's final review named.

**Known pinnable vs placeholder values (verified 2026-07-10 on main):** token-stream's `schema_hash` recomputes from `schemas/token-output.schema.json`; its `tokenizer_profile_hash` (`sha256:e4ecbc7a…`) does NOT match the committed `data/tokenizer-profiles/fixture-tokenizer-ja-v1.json` (`sha256:ebda1b25…`) — it is a fixture placeholder, so it is NOT pinned (document with a comment). analysis-result's `schema_hash` recomputes from `schemas/analysis-result.schema.json`; its `analysis_recipe_hash` (`sha256:4ab6e5b3…`) recomputes from `data/analysis-recipes/literary-basic-ja-v1.json`; its `input_normalization_policy_hash` is the `identity-normalization-policy-hash` constant. Verify each key's actual location in the fixture (top-level vs nested) with a Read before writing the `get`/`get-in` paths — mirror the fixture, don't guess.

- [ ] **Step 1: Write the test** (adjust access paths to the fixtures' real shapes after reading them)

```clojure
(ns abc.tools.fixture-hash-pinning-test
  "Committed example-work fixtures embed hashes of other committed sources;
  these pins recompute them so silent drift fails loudly. Family-wide
  extension of the annotation pin in annotation_identity_test.clj
  (example-work-fixture-embedded-hashes-pinned-test). Values that are
  fixture placeholders (matching no committed source) are deliberately
  not pinned — see comments."
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is testing]]))

(deftest token-stream-fixture-embedded-hashes-pinned-test
  (let [fixture (files/read-json "examples/v0/example-work/token-stream.json")]
    (testing "schema_hash recomputes from the committed token-output schema"
      (is (= (manifest/schema-hash "schemas/token-output.schema.json")
             (get fixture "schema_hash"))))
    ;; tokenizer_profile_hash is a fixture placeholder (does not match the
    ;; committed fixture-tokenizer-ja-v1 profile) — deliberately not pinned.
    ))

(deftest analysis-result-fixture-embedded-hashes-pinned-test
  (let [fixture (files/read-json "examples/v0/example-work/analysis-result.json")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")]
    (testing "schema_hash recomputes from the committed analysis-result schema"
      (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
             (get fixture "schema_hash"))))
    (testing "analysis_recipe_hash recomputes from the committed recipe"
      (is (= (analysis-identity/analysis-recipe-hash recipe)
             (get fixture "analysis_recipe_hash"))))
    (testing "input normalization is the identity constant"
      (is (= analysis-identity/identity-normalization-policy-hash
             (get fixture "input_normalization_policy_hash"))))))
```

- [ ] **Step 2: Run the test** — these pin CURRENT correct values, so they pass immediately if the fixtures are drift-free. If any assertion FAILS, that is real, pre-existing drift: STOP and report BLOCKED with the mismatch (do not "fix" the fixture).

Run: `bin/kaocha --focus abc.tools.fixture-hash-pinning-test`
Expected: PASS (2 tests). To confirm the pins bite, temporarily perturb one embedded hash in a scratch copy ONLY if in doubt — never commit a perturbation.

- [ ] **Step 3: Run the full suite and commit**

Run: `bin/kaocha`
Expected: 461 tests, 0 failures.

```bash
git add test/abc/tools/fixture_hash_pinning_test.clj
git commit -m "test(abc): family-wide fixture hash pinning for token-stream and analysis-result examples"
```

---

### Final gates (after Task 5, before merge)

Run from abc/: `bin/kaocha` (461/0) and `clojure -M:abc/validate-design-bundle` (PASS); from ab-validator/: `python scripts/schema_contracts.py` (exit 0); from abc/: `python tools/schema_contracts.py` (exit 0). Merge the worktree branch to main once green (repo convention).
