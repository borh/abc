# Parser, IR, and Publication ADR Evidence Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct historical parser-selection authority, accept bounded custom-parser ownership for development, and migrate every supportable parser/IR/publication claim to registered artifact-backed evidence without implying neutral-comparison completion or release qualification.

**Architecture:** Plan 1 owns the evidence protocol, deterministic registrar, checked family-audit predicate, and monorepo-root component capture profile. This plan first adds narrow assertions and an exact historical Phase-5 tuple checker, then performs one clean Stage A claim/descriptor/template commit, captures every bundle into external staging from that revision, and performs one Stage B registrar/report commit. Historical comparison, exact-tuple admission, ownership, neutral comparison, and release qualification remain separate authorities.

**Tech Stack:** Clojure/Kaocha, EDN registration templates, deterministic JSON evidence bundles, Nix-provisioned TEI validation, JQ family audit, Markdown ADRs.

## Global Constraints

- Do not start until plans 1–3 have completed in this worktree. Plan 1 must
  provide `abc.tools.adr-evidence-register`, `abc/nix/adr-family-clean.jq`, the
  `component-clojure-test-v1`/`--repo-root` capture interface, registrar/
  governance `--workspace-root` plumbing, and root
  `monorepo-adr-governance` check in its Task 2B; plans 2–3 must have committed
  their shared ledger, registry, report, and graph updates.
- Use the immutable migration baseline and record dispositions/resulting IDs for all 36 old parser-family rows.
- Existing Accepted Decision/Hard Rule bodies remain byte-identical under plan 1's normative baseline. ADR 0038 supplies normative correction; “move” applies only to duplicated criteria/status prose.
- Historical July comparison authorizes no admission or release transition and is not the preregistered neutral comparison.
- Exact-tuple admission is derived only by ADR 0023's registry/admission boundary; a citation class alone never admits a tuple.
- ADR 0038 grants development ownership only. ADR 0039 remains Proposed until all exact-tuple release predicates pass.
- Preserve Phase 5 only under the complete frozen tuple and evidence scope specified below. Never join by adapter name, “latest,” or a floating coordinate.
- Preserve `work_content_hash = bundle_hash` separately from independently checked `primary_text_hash` for new source-schema generations. Legacy parser-IR remains historical and is not reinterpreted.
- Machine-local `/db/...` and `/home/...` strings in historical reports are locators, never evidence identities or freshness inputs.
- Every checked-in descriptor is an explicit input to its own bundle. Every runtime-read schema, fixture, report, registry, ADR, assessment, flake file, and lock file is explicit.
- Every B1–B5 and B7–B9 Clojure descriptor uses capture schema v2 and names its checked
  `docs/evidence/adr-inputs/<stem>.edn` manifest. Boundary tests trace repository
  reads, including transitive production reads, through Plan 1's instrumented
  shared file/hash helpers and call `assert-runtime-input-closure!`; descriptor
  runtime-data inputs equal manifest paths plus descriptor and manifest
  self-bindings. Plan 1's default-deny lint scans the resolved reachable-Var graph,
  not only the evidence-test namespace. Their derived closures contain no
  subprocess API. B6 is a version-1 `repo-files-v1` hermetic Nix boundary;
  B10 retains Plan 1's
  aggregate contract unchanged.
- All captures use the same clean Stage A revision and write only to one external staging directory. No run bundle is written into the repository until all captures validate.
- All joins use `abc.tools.adr-evidence-register` and the checked-in entry template. Never hand-edit `:artifact-hash`.
- Audit mode remains active; this plan does not switch the Nix governance gate to enforcement.

---

## Final claim and evidence ownership matrix

Boundary identifiers below are normative plan interfaces.

| Boundary | Descriptor → run bundle | Exact command/observation | Profile and owner |
| --- | --- | --- | --- |
| B1 citation identity | `parser-citation-identity.edn` → `parser-citation-identity.json` | exact focus `abc.tools.parser-evidence-test/historical-parser-citations-contract`; `historical-parser-citations-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B2 import boundary | `parser-import-boundary.edn` → `parser-import-boundary.json` | exact focus `abc.tools.parser-import-boundary-evidence-test/parser-import-boundary-contract`; `parser-import-boundary-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B3 mapping/admission | `parser-mapping-admission.edn` → `parser-mapping-admission.json` | exact focus `abc.tools.parser-mapping-admission-evidence-test/parser-mapping-admission-contract`; `parser-mapping-admission-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B4 identity relations | `parser-identity-relations.edn` → `parser-identity-relations.json` | exact focus `abc.tools.parser-identity-relations-evidence-test/parser-identity-relations-contract`; `parser-identity-relations-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B5 parser-IR schema | `parser-ir-schema-regression.edn` → `parser-ir-schema-regression.json` | exact focus `abc.tools.parser-ir-schema-evidence-test/parser-ir-schema-regression-contract`; `parser-ir-schema-regression-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B6 publication | `parser-publication-rendering.edn` → `parser-publication-rendering.json` | `bash -lc "system=$(nix eval --impure --raw --expr builtins.currentSystem); nix build --no-link ./abc#checks.${system}.parser-publication-evidence --print-build-logs"`; `parser-publication-rendering-pass` | version-1 `repo-files-v1`, exact hermetic Nix determinants, monorepo root, this plan |
| B7 relations/provenance | `parser-relations-provenance.edn` → `parser-relations-provenance.json` | exact focus `abc.tools.parser-relations-provenance-evidence-test/parser-relations-provenance-contract`; `parser-relations-provenance-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B8 ownership assessment | `parser-ownership-assessment.edn` → `parser-ownership-assessment.json` | exact focus `abc.tools.parser-ownership-assessment-evidence-test/custom-parser-ownership-assessment-contract`; `custom-parser-ownership-assessment-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B9 frozen Phase 5 | `parser-phase5-frozen-tuple.edn` → `parser-phase5-frozen-tuple.json` | exact focus `abc.tools.parser-phase5-frozen-tuple-test/phase5-frozen-tuple-contract`; `phase5-frozen-tuple-pass` | `component-clojure-test-v1`, monorepo root, this plan |
| B10 design-bundle operation | plan 1's `design-bundle-operational.edn` → `design-bundle-operational.json` | plan 1 observation `design-bundle-exits-zero` | read-only shared artifact; never recaptured here |

Every row below becomes one entry in
`abc/docs/evidence/adr-entries/parser-ir-publication.edn`. All predicates are
`{:operator := :value true}`.

| Claim | Final claim text (abridged only for this matrix; Task 5 supplies exact Markdown) | Claim kind / evidence kind | Boundary |
| --- | --- | --- | --- |
| ADR-0002-C1 | exact historical citations; explicitly not neutral comparison | structural-invariant / structural-test | B1 |
| ADR-0002-C2 | citation classes do not admit/release; exact registry gates admission | structural-invariant / structural-test | B3 |
| ADR-0002-C3 | committed producer fixture carries accepted IR/diagnostic/run/mapping/schema contract | fixture-behavior / fixture-conformance | B2 |
| ADR-0002-C4 | parser tuple reversal preserves source-bundle/primary-text relations | structural-invariant / structural-test | B4 |
| ADR-0007-C1 | imported positive bundle validates | fixture-behavior / fixture-conformance | B2 |
| ADR-0007-C2 | ABC validator consumes files and does not execute parser candidates | structural-invariant / structural-test | B2 |
| ADR-0007-C3 | malformed warning/run-summary fixtures are rejected | fixture-behavior / fixture-conformance | B2 |
| ADR-0007-C4 | supported design-bundle app exits zero | operational-behavior / operational-observation | B10 |
| ADR-0023-C1 | three mapping/divergence schemas meta-validate | structural-invariant / structural-test | B3 |
| ADR-0023-C2 | committed registry enforces complete exact keys/no wildcard/evidence scope | structural-invariant / structural-test | B3 |
| ADR-0023-C3 | admission reports admitted/missing/conflict | fixture-behavior / fixture-conformance | B3 |
| ADR-0023-C4 | mapping hash propagates into materialized identity | fixture-behavior / fixture-conformance | B2 |
| ADR-0023-C5 | supported design-bundle app exits zero over mapping boundary | operational-behavior / operational-observation | B10 |
| ADR-0024-C1 | current span coordinate/ruby direction schema cases validate | fixture-behavior / fixture-conformance | B5 |
| ADR-0024-C2 | legacy span object shapes remain accepted | fixture-behavior / fixture-conformance | B5 |
| ADR-0024-C3 | decoded-UTF-8 spans are consumed by current publication fixtures | fixture-behavior / fixture-conformance | B5 |
| ADR-0024-C4 | ruby direction renders to profile-valid TEI | fixture-behavior / fixture-conformance | B6 |
| ADR-0024-C5 | parser-IR schema JCS hash propagates to manifests | structural-invariant / structural-test | B5 |
| ADR-0024-C6 | supported design-bundle app exits zero | operational-behavior / operational-observation | B10 |
| ADR-0025-C1 | committed parser IR renders a valid TEI body | fixture-behavior / fixture-conformance | B6 |
| ADR-0025-C2 | plaintext contains visible body/base only and excludes reading/apparatus/provenance | fixture-behavior / fixture-conformance | B6 |
| ADR-0025-C3 | materializer writes and validates content/manifests/sidecar | fixture-behavior / fixture-conformance | B6 |
| ADR-0025-C4 | renderer coverage is schema-derived and fails on missing/extra node policy | structural-invariant / structural-test | B6 |
| ADR-0025-C5 | Aozora ruby defaults to `type="furigana"` | fixture-behavior / fixture-conformance | B6 |
| ADR-0025-C6 | supported design-bundle app exits zero over publication outputs | operational-behavior / operational-observation | B10 |
| ADR-0030-C1 | exact three historical citations and byte hashes | structural-invariant / structural-test | B1 |
| ADR-0030-C2 | reciprocal amendment links among 0002/0030/0032/0038 | structural-invariant / structural-test | B7 |
| ADR-0030-C3 | real admission boundary rejects absent/conflicting tuple regardless of citations | structural-invariant / structural-test | B3 |
| ADR-0032-C1 | reciprocal ADR 0030/0038 links | structural-invariant / structural-test | B7 |
| ADR-0032-C2 | detach pin, handoff, crate provenance, and NOTICE agree | structural-invariant / structural-test | B7 |
| ADR-0038-C1 | bounded project-owner assessment supports development ownership only | domain-interpretation / expert-assessment | B8 |
| ADR-0038-C2 | comparison/citations cannot bypass exact admission and grant no release authority | structural-invariant / structural-test | B3 |
| ADR-0038-C3 | historical Phase-5 full tuple is internally exact and distinct from live schema/source roles | structural-invariant / structural-test | B9 |

---

### Task 0: Verify plan-1 interfaces and immutable starting point

**Files:**
- Read: `abc/docs/adr/adr-claim-migration-baseline.json`
- Read: `abc/docs/adr/adr-claim-migration.edn`
- Read: `abc/src/abc/tools/adr_evidence_register.clj`
- Read: `abc/nix/adr-family-clean.jq`
- Read: `abc/src/abc/tools/adr_evidence_capture.clj`

**Interfaces:**
- Consumes: completed plan-1 registrar and monorepo capture profile.
- Produces: an external `/tmp/adr-parser-family-before.json`; no tracked edit.

- [ ] **Step 1: Verify clean state and required interfaces**

Run from the monorepo root:

```bash
test -z "$(git status --porcelain --untracked-files=all)"
rg -n 'abc/adr-evidence-register' abc/deps.edn
rg -n 'component-clojure-test-v1|--repo-root|--workspace-root' \
  abc/src/abc/tools/adr_evidence_capture.clj \
  abc/src/abc/tools/adr_evidence_bundle.clj \
  abc/src/abc/tools/adr_evidence_register.clj \
  abc/src/abc/tools/adr_governance.clj \
  abc/schemas/adr-evidence-run.schema.json
test -f abc/nix/adr-family-clean.jq
rg -n 'monorepo-adr-governance' flake.nix \
  tests/root-flake-output-contract-smoke.sh
```

Expected: all commands exit 0. If the capture profile is absent, stop and
complete plan 1 Task 2B; do not recreate it in this plan.

- [ ] **Step 2: Record the pre-family report externally**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit \
  --report /tmp/adr-parser-family-before.json
jq -e '.ok == false' /tmp/adr-parser-family-before.json
```

Expected: audit exits 0, report is incomplete, and the worktree remains clean.
Do not append execution notes to this plan.

---

### Task 1: Bind historical citation files without granting transition authority

**Files:**
- Modify: `abc/src/abc/tools/parser_evidence.clj`
- Modify: `abc/test/abc/tools/parser_evidence_test.clj`
- Create: `abc/data/evidence-higher-order-calls/parser-evidence-duplicate-errors.edn`

**Interfaces:**
- Produces: `citation-file-problems [monorepo-root index] -> vector problem maps`.
- Does not produce an `eligible-for-transition?` function; citation labels are
  not state authority.

- [ ] **Step 1: Write failing contained-path and exact-row tests**

Add `parser-evidence-citation-files-are-content-addressed-test` using a
temporary monorepo, covering passing, missing, byte-drift, lexical escape, and
real-path escape cases. Add
`historical-parser-citation-rows-are-exact-and-not-neutral-study-test`, loading
the committed index and asserting these exact rows and current file bytes:

```clojure
{"ab-validator/aozora-parser-comparison-study-2026-07-08"
 ["ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md"
  "sha256:8f68178186c9ccad2514c833098f3ffbc725183699757d5dc8182678c5826a59"]
 "ab-validator/parser-fork-candidacy-faithful-comparison-2026-07-08"
 ["ab-validator/docs/superpowers/reports/2026-07-08-parser-fork-candidacy-faithful-comparison.md"
  "sha256:ec0e5b7cdef6ea0c14b6ba59af8a35eb7b742b12a3da74683f771873330933e8"]
 "ab-validator/parser-comparison-followups-2026-07-09"
 ["ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md"
  "sha256:e13c904f36ddc4180f1326cdc375e6939716487e55318349c86c64e5158d2aa0"]}
```

Before registering the reachable higher-order call, replace the keyword and
anonymous `juxt` callables passed to `duplicate-errors` with named pure
targets, preserving returned keys exactly:

```clojure
(defn evidence-id-key [entry] (:evidence_id entry))
(defn logical-file-key [entry] [(:logical_path entry) (:sha256 entry)])
```

Add unit assertions that both named functions equal the old keyword/`juxt`
results, and add them as the only allowed targets for `duplicate-errors`
parameter `key-fn` in the new caller-scoped contract. Only B1 and later graphs
that reach `duplicate-errors` bind this file; earlier bundles remain fresh.

Assert none has a `:study_contract` or neutral-comparison marker; this is a
historical classification fact, not scientific sufficiency.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-evidence-test
```

Expected: FAIL because `citation-file-problems` is absent.

- [ ] **Step 3: Implement only file identity validation**

Use `path-containment/path-state` and `hash/sha256-file`. Return sorted maps
with `:kind`, `:evidence-id`, and `:logical-path`; use kinds
`:citation-missing`, `:citation-hash-mismatch`,
`:citation-path-traversal`, and `:citation-real-path-escape`. Do not add these
checks to ABC-component `validate-index!`, because that pinned component source
does not contain `ab-validator`; B1 supplies the monorepo-context check.

- [ ] **Step 4: Run GREEN and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-evidence-test
cd ..
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.clj-kondo"
git add abc/src/abc/tools/parser_evidence.clj \
  abc/data/evidence-higher-order-calls/parser-evidence-duplicate-errors.edn \
  abc/test/abc/tools/parser_evidence_test.clj
git commit -m "feat(parser-evidence): bind historical citation bytes"
```

Expected: tests/check pass and the worktree is clean.

---

### Task 2: Add direct ABC boundary, admission, identity, and relation assertions

**Files:**
- Create: `abc/test/abc/tools/parser_import_boundary_evidence_test.clj`
- Create: `abc/test/abc/tools/parser_mapping_admission_evidence_test.clj`
- Create: `abc/test/abc/tools/parser_identity_relations_evidence_test.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces the exact named assertions consumed by B2, B3, B4, B7, and B8.

- [ ] **Step 1: Write failing boundary tests**

Add these exact tests to the responsibility-matching namespace:

- `imported-boundary-validates-parser-ir-diagnostics-and-run-summary-test`:
  validate the committed parser IR, every warning JSONL row, and run-summary
  event set against their real schemas.
- `imported-boundary-rejects-malformed-diagnostic-and-event-set-test`: mutate
  severity/code and remove the complete event; assert deterministic rejection.
- `abc-boundary-does-not-execute-parser-candidates-test`: run the imported-
  bundle validator with a temporary input tree and a sentinel executable path;
  assert the sentinel is never invoked. The claim is “outside ABC validation,”
  not the obsolete “outside this monorepo.”
- `mapping-contract-schemas-meta-validate-test`: call the real meta-schema
  validator for all three ADR 0023 schemas; do not stub `schema-valid!`.
- `parser-citations-cannot-bypass-exact-admission-test`: with a citable
  historical selection entry present, assert missing and conflicting mapping
  tuples still produce nonempty `compatibility-errors`; assert the exact C5
  registry candidate is admitted.
- `parser-tuple-change-preserves-source-role-relations-test`: vary only parser
  build/adapter/mapping coordinates, assert the tuple/artifact identity changes,
  and assert `work_content_hash=bundle_hash` plus `primary_text_hash` remain
  unchanged and distinct.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test
```

Expected: FAIL because the new namespace/negative fixtures do not exist.

- [ ] **Step 3: Add only the assertions**

Do not add a citation-class transition lookup. Exercise
`aat-parser-ir-compat/admission-report` and
`validate-design-bundle/compatibility-errors`, the real admission/publication
seams. Use the existing public validation helpers and committed fixture; this
task does not alter production validation code or the shared B10 runtime input
closure.

- [ ] **Step 4: Run GREEN and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test \
  --focus abc.tools.materialize-import-test
cd ..
git add abc/test/abc/tools/parser_import_boundary_evidence_test.clj \
  abc/test/abc/tools/parser_mapping_admission_evidence_test.clj \
  abc/test/abc/tools/parser_identity_relations_evidence_test.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "test(parser): assert real boundary and admission authority"
```

Expected: PASS.

---

### Task 3: Characterize parser-IR compatibility and publication behavior

**Files:**
- Create: `abc/test/abc/tools/parser_ir_schema_evidence_test.clj`
- Create: `abc/test/abc/tools/parser_publication_evidence_test.clj`
- Modify: `abc/test/abc/tools/parser_ir_tei_test.clj`
- Modify: `abc/test/abc/tools/parser_ir_plaintext_test.clj`
- Modify: `abc/test/abc/tools/parser_ir_publication_policy_test.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Create: `abc/fixtures/parser-ir/span-current.json`
- Create: `abc/fixtures/parser-ir/span-legacy-start-end.json`
- Create: `abc/fixtures/parser-ir/span-legacy-line-column.json`
- Create: `abc/fixtures/parser-ir/span-invalid-coordinate.json`
- Modify: `abc/flake.nix`

**Interfaces:**
- Produces B5/B6 assertions. These are characterization tests; pre-existing
  correct behavior may make a newly added test green immediately.

- [ ] **Step 1: Add complete bounded span fixtures and tests**

The four files are span-object fixtures, not whole historical parser-IR
documents. Test the parser-IR schema's span definition through a minimal
complete current parser-IR envelope so the claim is exactly “these span object
shapes remain accepted.” Assert current decoded UTF-8 plus non-null direction,
both legacy shapes, and unknown-coordinate rejection. Assert the committed
publication fixture actually contains and consumes `decoded_utf8` spans.
Add `parser-ir-schema-hash-propagates-to-materialized-manifest-test`: hash the
current schema, make a deterministic temporary schema mutation and assert its
JCS hash differs, prepare a temporary imported bundle whose parser IR and
`manifest-inputs.json` carry that mutated coordinate, materialize it, and
assert the generated manifest copies the mutated parser-IR schema hash. This
proves propagation rather than only generic hash sensitivity.

- [ ] **Step 2: Strengthen renderer/materializer assertions**

Directly assert:

- TEI body validates with pinned RNG/Schematron and ruby direction uses
  profile-valid `rend`;
- plaintext retains ruby base but excludes ruby reading, source apparatus,
  provenance, layout metadata, and source-note back matter;
- materialization writes plaintext, TEI, both manifests, preservation sidecar,
  and passing `tei-validation-result.json`;
- deleting one renderer node policy and adding one extra policy both make the
  schema-derived coverage check fail;
- Aozora ruby defaults to `type="furigana"`.

Do not add or infer a TEI Level-3 claim.

- [ ] **Step 3: Add the dedicated hermetic publication evidence check**

`parser_publication_evidence_test.clj` makes the combined B6 assertions by
calling the same renderer/materializer/schema APIs as the focused suites. Add
`checks.<system>.parser-publication-evidence` in `abc/flake.nix` using the
existing writable-source and Clojure sandbox helpers:

```nix
parser-publication-evidence =
  pkgs.runCommand "abc-parser-publication-evidence"
    {
      nativeBuildInputs = [ pkgs.clojure pkgs.libxml2 ];
    }
    ''
      ${copyWritableSource}
      ${cljSandboxEnv}
      export TEI_SCHEMA_PATH="${tei.teiAllSchema}"
      bin/kaocha --focus \
        abc.tools.parser-publication-evidence-test/parser-publication-rendering-contract
      touch "$out"
    '';
```

The focused contract invokes the renderer/materializer/schema APIs directly
and contains every B6 assertion listed above; it does not call other `deftest`
Vars or run their namespaces. Its checked Nix/Clojure closure therefore starts
from this one exact Var.

This check is the B6 execution boundary. It uses the TEI artifact imported by
`abc/nix/tei-profile-artifacts.nix`; it does not enter an ambient development
shell.

- [ ] **Step 4: Run the dedicated check using the host system**

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.parser-publication-evidence" \
  --print-build-logs
```

Expected: PASS with nonempty `TEI_SCHEMA_PATH` supplied by the pinned flake.

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/tools/parser_ir_schema_evidence_test.clj \
  abc/test/abc/tools/parser_publication_evidence_test.clj \
  abc/test/abc/tools/parser_ir_tei_test.clj \
  abc/test/abc/tools/parser_ir_plaintext_test.clj \
  abc/test/abc/tools/parser_ir_publication_policy_test.clj \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/fixtures/parser-ir abc/flake.nix
git commit -m "test(parser-ir): pin schema and publication behavior"
```

---

### Task 4: Implement the exact historical Phase-5 tuple checker

**Files:**
- Create: `abc/src/abc/tools/parser_phase5_frozen_tuple.clj`
- Create: `abc/test/abc/tools/parser_phase5_frozen_tuple_test.clj`

**Interfaces:**
- Produces:
  - `frozen-tuple-problems [monorepo-root] -> vector problem maps`;
  - `assert-frozen-tuple! [monorepo-root] -> :ok` or throws with all problems.
- Reads only checked-in values; it never opens `/db/...` locators or replays a
  corpus.

The complete expected value is:

```clojure
{:aat-version 2
 :aat-adapter "ab-aozora"
 :aat-adapter-version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
 :candidate-commit "004deaf548f34a36abbc17d0f7a162df010a6292"
 :candidate-bin-sha256 "00066926b8cb035c2b2a55691cfcb99d26c8450b87d960d058855a69afedd24d"
 :converter-bin-sha256 "74f507726f352cd5749b056e46fcdbfb6801b85ffa280ebc8b11a627528b7481"
 :mapping-id "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe"
 :mapping-version "0.4.0"
 :mapping-hash "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
 :mapping-byte-sha256 "8ebd74dcd0f29973e375a9c89654ffbccc9f15fb2f5c5b7a5c97a0bef6206a56"
 :mapping-schema-hash "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
 :parser-ir-schema-id "https://w3id.org/abc/schemas/parser-ir.schema.json"
 :parser-ir-schema-hash "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"
 :compatibility "lossy"
 :evidence-scope {:evidence-type :conversion-audit
                  :adapter "ab-aozora"
                  :adapter-version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
                  :corpus "ab-aozora"
                  :files-scanned 17886 :files-succeeded 17886 :files-failed 0
                  :parser-ir-nodes 18803174
                  :divergence-records 231683
                  :divergence-occurrences 4938800
                  :rules-total 689 :rules-emitted 54 :rules-missing 635
                  :unsupported-occurrences 195437}}
```

- [ ] **Step 1: Write failing real-file join tests**

The positive test cross-compares:

- `ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json` parsed canonical
  hash and exact byte hash;
- `ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase5-c5-compat.edn`;
- the byte-equal whole matching row in
  `abc/data/aat-parser-ir-compatibility.edn`;
- C5 delta, conformance-gate, perf, and conversion-gate summaries, each with
  `stage=c5`, expected gate, `verdict=PASS`, and identical candidate tuple;
- conversion audit/gate mapping values and exact counts;
- `ab-validator/docs/superpowers/reports/2026-07-12-phase5-checkpoint.txt`
  beginning with `CHECKPOINT OK`.

Also compute the live `abc/schemas/parser-ir.schema.json` JCS hash and assert it
is **not** the historical
`sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2`
hash; assert the live schema exposes both
`work_content_hash` and `primary_text_hash` alternatives without rewriting the
historical report. Negative tests mutate every full-key field in temporary
copies, substitute the live mapping/schema, change the registry evidence
scope, and change one gate candidate; each must yield an exact
`:phase5-coordinate-mismatch` problem naming the path/key.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-phase5-frozen-tuple-test
```

Expected: FAIL because the checker namespace is absent.

- [ ] **Step 3: Implement the pure real-file checker**

Use ABC's EDN/JSON readers and JCS hash implementation. Select the ABC registry
row by the complete match key, require exactly one, and compare the whole row
to the producer compat entry. Ignore locator fields after confirming they are
not used as input paths. Return all deterministic problems; do not shell out to
the old checkpoint or claim fresh corpus measurement.

- [ ] **Step 4: Run GREEN plus existing verifier tests and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-phase5-frozen-tuple-test
cd ..
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./ab-validator#checks.${system}.phase5-checkpoint"
git add abc/src/abc/tools/parser_phase5_frozen_tuple.clj \
  abc/test/abc/tools/parser_phase5_frozen_tuple_test.clj
git commit -m "test(parser): verify frozen Phase 5 full tuple"
```

Expected: both checks pass. The new checker, not the synthetic pytest
derivation, owns B9 evidence.

---

### Task 5: Finalize claims, ADR 0038 assessment, ledger, descriptors, and entry template (Stage A)

**Files:**
- Modify: `abc/docs/adr/0002-parser-evaluation.md`
- Modify: `abc/docs/adr/0007-external-parser-validation-boundary.md`
- Modify: `abc/docs/adr/0023-owned-aat-parser-ir-mapping.md`
- Modify: `abc/docs/adr/0024-parser-ir-span-and-ruby-direction.md`
- Modify: `abc/docs/adr/0025-parser-ir-publication-rendering.md`
- Modify: `abc/docs/adr/0030-aozora-parser-selection.md`
- Modify: `abc/docs/adr/0032-parser-fork-hard-detach.md`
- Create: `abc/docs/adr/0038-custom-parser-ownership-and-neutral-comparison.md`
- Create: `abc/docs/evidence/external/custom-parser-ownership-assessment.md`
- Create: `abc/test/abc/tools/parser_relations_provenance_evidence_test.clj`
- Create: `abc/test/abc/tools/parser_ownership_assessment_evidence_test.clj`
- Modify: `abc/docs/adr/adr-claim-migration.edn`
- Modify: `abc/src/abc/tools/adr_evidence_inventory.clj`
- Modify: `abc/test/abc/tools/adr_evidence_inventory_test.clj`
- Create: the nine B1–B9 descriptors under `abc/docs/evidence/adr-capture/`
- Create: the eight matching B1–B5/B7–B9 manifests under
  `abc/docs/evidence/adr-inputs/`
- Create: `abc/docs/evidence/adr-inputs/parser-publication-rendering-nix-clojure-closure.edn`
- Create: `abc/docs/evidence/adr-entries/parser-ir-publication.edn`
- Regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`
- Regenerate: `abc/docs/adr/adr-graph.mmd`
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Modify: every B1–B5/B7–B9 `*-evidence-test.clj` namespace named in the boundary
  table to add its Plan 1 runtime-input closure assertion

**Interfaces:**
- Produces: the single clean Stage A revision consumed by every capture.

- [ ] **Step 1: Replace Acceptance Criteria with exact final text**

Use the following exact criteria and lifecycle values:

```text
ADR 0002: Validation scope: smoke-corpus / Release authority: development
ADR 0007: Validation scope: fixture      / Release authority: development
ADR 0023: Validation scope: fixture      / Release authority: publication
ADR 0024: Validation scope: fixture      / Release authority: publication
ADR 0025: Validation scope: fixture      / Release authority: publication
ADR 0030: Validation scope: full-corpus  / Release authority: development
ADR 0032: Validation scope: full-corpus  / Release authority: development
ADR 0038: Validation scope: structural   / Release authority: development
```

ADR 0038's accepted claims are an expert interpretation plus structural
guards, not an operational parser qualification. Add ADR 0038 to the closed
`parser-ir-publication` family set in `adr_evidence_inventory.clj`; extend the
inventory test to distinguish the immutable 26-ADR baseline from the new live
Accepted ADR and reject every unclassified live ADR.

```markdown
ADR 0002
- **ADR-0002-C1 — structural-invariant:** The parser evidence index binds the three explicitly named July 2026 historical reports to their workspace-relative paths and current byte hashes, and does not identify them as the preregistered neutral comparison.
- **ADR-0002-C2 — structural-invariant:** Parser citation classes do not admit or release a parser; exact adapter, mapping, parser-IR schema, and evidence-scope equality through ADR 0023 is required for admission.
- **ADR-0002-C3 — fixture-behavior:** The committed imported producer fixture carries parser IR, warning diagnostics, run-summary aggregation, mapping provenance, and schema identities accepted by the ABC file boundary.
- **ADR-0002-C4 — structural-invariant:** Changing the parser/adapter/mapping tuple changes parser derivation identity without changing the source relations `work_content_hash = bundle_hash` and independently checked `primary_text_hash`.

ADR 0007
- **ADR-0007-C1 — fixture-behavior:** The committed imported parser IR, warning JSON Lines, and run-summary event set conform to their ABC schemas and cross-file boundary checks.
- **ADR-0007-C2 — structural-invariant:** ABC validation consumes the imported file bundle and does not execute parser candidates; parser execution remains in the `ab-validator` component.
- **ADR-0007-C3 — fixture-behavior:** ABC rejects a malformed diagnostic row and a run-summary set missing its required completion event.
- **ADR-0007-C4 — operational-behavior:** The supported `nix run ./abc#validate-design-bundle` application exits zero over the committed imported boundary.

ADR 0023
- **ADR-0023-C1 — structural-invariant:** The AAT mapping, divergence, and divergence-bundle schemas are valid Draft 2020-12 schemas under the real meta-schema validator.
- **ADR-0023-C2 — structural-invariant:** The committed compatibility registry requires the complete adapter/version/mapping/parser-IR match key, matching evidence scope, and no wildcard adapter entries.
- **ADR-0023-C3 — fixture-behavior:** The admission boundary reports admitted, missing, and conflict states and treats changed evidence under an existing match key as conflict.
- **ADR-0023-C4 — fixture-behavior:** Import materialization copies `mapping_hash` into `manifest_identity_object.aat_parser_ir_mapping_hash`.
- **ADR-0023-C5 — operational-behavior:** The supported design-bundle application exits zero over the committed mapping schemas, registry, and imported fixture.

ADR 0024
- **ADR-0024-C1 — fixture-behavior:** The current parser-IR schema accepts `decoded_utf8` spans and ruby direction values `left`, `right`, and null, and rejects an unknown coordinate system.
- **ADR-0024-C2 — fixture-behavior:** The parser-IR span definition continues to accept the legacy `{start,end}` and `{start,end,line,column}` object shapes.
- **ADR-0024-C3 — fixture-behavior:** Current publication fixtures carry and consume `decoded_utf8` spans.
- **ADR-0024-C4 — fixture-behavior:** TEI rendering preserves ruby direction using a profile-valid `rend` value.
- **ADR-0024-C5 — structural-invariant:** Parser-IR schema JCS hash changes propagate to the parser-IR schema coordinate copied into materialized manifests.
- **ADR-0024-C6 — operational-behavior:** The supported design-bundle application exits zero over the current parser-IR schema and fixture.

ADR 0025
- **ADR-0025-C1 — fixture-behavior:** The committed parser-IR fixture renders a TEI body that passes the pinned Relax NG and project Schematron gates.
- **ADR-0025-C2 — fixture-behavior:** Plaintext retains visible body text and ruby base text while excluding ruby readings, source apparatus, provenance, layout metadata, and source-note back matter.
- **ADR-0025-C3 — fixture-behavior:** Publication materialization writes plaintext, TEI, both manifests, the preservation sidecar, and a passing `tei-validation-result.json`.
- **ADR-0025-C4 — structural-invariant:** Renderer node coverage is derived from the parser-IR schema and rejects both missing and unexpected node policies.
- **ADR-0025-C5 — fixture-behavior:** Generated Aozora ruby defaults to TEI `type="furigana"`.
- **ADR-0025-C6 — operational-behavior:** The supported design-bundle application exits zero over the committed publication outputs and TEI gates.

ADR 0030
- **ADR-0030-C1 — structural-invariant:** The three historical study citations have exact workspace-relative paths and byte hashes in the parser evidence index.
- **ADR-0030-C2 — structural-invariant:** ADRs 0002, 0030, 0032, and 0038 carry the reciprocal amendment links declared by the corrective decision chain.
- **ADR-0030-C3 — structural-invariant:** Historical parser-selection citations cannot satisfy the exact-registry admission boundary; absent or conflicting tuples remain rejected.

ADR 0032
- **ADR-0032-C1 — structural-invariant:** ADRs 0030, 0032, and 0038 carry reciprocal amendment links.
- **ADR-0032-C2 — structural-invariant:** The fork-provenance handoff, lifted-crate provenance headers, upstream NOTICE, and ADR record the same detach repository, revision, and licence boundary.

ADR 0038
- **ADR-0038-C1 — domain-interpretation:** Given the enumerated owned parser/publication contract, current maintainer commitment, and bounded five-parser/build-fresh survey, the project-owner assessment supports Soranoha ownership of the custom-parser contract for development only; it grants neither tuple admission nor publication authority.
- **ADR-0038-C2 — structural-invariant:** Comparison and citation records cannot bypass ADR 0023 exact-tuple admission, and this ADR has development rather than publication release authority.
- **ADR-0038-C3 — structural-invariant:** The historical Phase-5 evidence remains bound to its complete frozen C5 tuple and is distinct from the live parser-IR schema and current source-role relations.
```

Move all July winner/neutral-study/release-viability prose to exact
`## Historical Evidence` or `## Future Verification` sections without claim
headers. Move ADR 0025's obsolete Level-3 condition to Historical Evidence;
do not replace it with a current Level-3 claim. Move test-inventory prose to
Implementation Status. Move ADR 0032 maintenance economics to Future
Verification.

- [ ] **Step 2: Author the bounded ownership assessment exactly**

The assessment contains these metadata lines:

```markdown
Assessment date: 2026-07-12
Review after: 2026-10-12
Decision owner: Soranoha project owner
Technical reviewer: Soranoha parser maintainer
Authority: development-only
```

Its observed-facts section enumerates: the hard-detached implementation and
exact detach pin; ABC-owned parser-IR/publication contracts; the bounded July
survey's `aozora-pipeline`, `aozora-core`, `aozora-rs-core`, `aozora2html`,
and `aozora-epub3` parser families plus the build-fresh option; and that the survey did
not evaluate a preregistered neutral contract. It must say only that none of
the **surveyed** external implementations supplied the complete owned contract
without project adaptation. Its assumptions name continued maintainer
availability and project control of the publication contract. Its falsifiers
are: retirement of the custom parser in favor of an implementation that
assumes the complete owned contract, or a superseding owner decision after
maintainer capacity becomes inadequate. Its non-authority section denies
admission, neutral-comparison completion, release qualification, and
publication. It must not say Phase 5 proves current quality.

Add `parser-family-reciprocal-amendments-test` and
`fork-provenance-records-detach-test` to
`parser_relations_provenance_evidence_test.clj`. The former parses only ADRs
0002/0030/0032/0038 and asserts the exact reciprocal edges; the latter asserts
the ADR 0032 pin, provenance handoff, lifted crate headers, and upstream NOTICE
agree on repository/revision/licence. Add
`custom-parser-ownership-assessment-contract-test` to
`parser_ownership_assessment_evidence_test.clj`, asserting every exact metadata,
scope, assumption, falsifier, and non-authority requirement above. It also
reads `docs/adr/governance-as-of.edn`, parses both real calendar dates, and
requires the coordinated governance date to be on or before `Review after`;
advancing the epoch therefore stales the bundle and prevents an expired
assessment from being recaptured as passing.

- [ ] **Step 3: Record every old-row disposition**

Populate ledger entries by baseline content hash according to this complete
mapping:

| ADR | Original indices → disposition/resulting IDs |
| --- | --- |
| 0002 | 0→correct/C1; 1→correct/C1; 2→correct/C2; 3→correct/C2; 4→correct/C3; 5→correct/C3; 6→move-out-of-acceptance/[]; 7→correct/C4 |
| 0007 | 0→correct/C4; 1→correct/C2; 2→correct/C1,C3; 3→correct/C1,C3; 4→correct/C2 |
| 0023 | 0→correct/C1; 1→retain/C2; 2→retain/C3; 3→retain/C4; 4→correct/C5 |
| 0024 | 0→correct/C1; 1→correct/C2; 2→correct/C3; 3→retain/C4; 4→correct/C5; 5→correct/C6 |
| 0025 | 0→correct/C1; 1→correct/C2; 2→correct/C3; 3→correct/C4; 4→correct/C5; 5→correct/C6; 6→move-out-of-acceptance/[] |
| 0030 | 0→correct/C1; 1→correct/C2; 2→correct/C3 |
| 0032 | 0→correct/C1; 1→correct/C2 |

Every `:correct` row states which narrower observable boundary replaces the
compound/historical wording. Every `:move-out-of-acceptance` row states that
the old text is an unsatisfied neutral-comparison/re-measurement guard or a
test-inventory statement rather than an accepted observation. Every resulting
ID uses the full `ADR-NNNN-CN` string.

- [ ] **Step 4: Author exact descriptors and the registration template**

Use the boundary table verbatim. B1–B5 and B7–B9 descriptors use schema version
`abc-adr-evidence-capture-v2`, `component-clojure-test-v1`,
`:component-root "abc"`, and
`:runtime-input-manifest "abc/docs/evidence/adr-inputs/<stem>.edn"`. Each
manifest has the exact closed shape
`{:schema-version :abc-adr-runtime-inputs-v1 :paths [...]}` with sorted unique
monorepo-relative paths. Each descriptor's explicit runtime-data set is
exactly its manifest `:paths` plus descriptor and manifest self-bindings; the
component profile separately derives Clojure namespace closure.

For every version-2 boundary, add a test that executes the supported evidence operation
inside `abc.tools.evidence-io/with-read-trace`, so production helpers invoked
by the operation contribute their transitive reads, and calls:

```clojure
(runtime-inputs/assert-runtime-input-closure!
 {:repo-root abc-root
  :workspace-root workspace-root
  :descriptor {:path descriptor-path :value descriptor}
  :repository-paths repository-paths})
```

All repository reads in the resolved reachable-Var graph use Plan 1's
traceable shared physical adapters; Plan 1's closure-wide default-deny lint must
remain green. A test that traces only its wrapper while invoked production code
reads files directly is invalid. The manifest contents are:

Each descriptor focuses the exact dedicated wrapper Var named in the boundary
table. That wrapper scopes every freshly generated output directory with
`with-ephemeral-root`, invokes operations directly rather than calling another
`deftest`, and owns the descriptor-keyed closure assertion. Default-deny
analysis follows its reachable-Var graph and fails on unresolved/dynamic I/O,
network, or process edges.

B6 instead uses capture schema version 1 with `repo-files-v1`, no runtime
manifest, and the exact command in the boundary table. Its explicit inputs
are the B6 determinant list below plus its descriptor. Generate its checked
Nix/Clojure closure with Plan 1's
`derive-nix-clojure-source-closure` for exact focus
`abc.tools.parser-publication-evidence-test/parser-publication-rendering-contract`;
descriptor tests require
`explicit` to contain the closure manifest and every expanded path, reject any
missing/extra path, and prohibit a v2 runtime manifest.

- B1 contains exactly four runtime paths:
  `abc/data/parser-evidence-citations.edn` and these three July
  files only:
  `ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`,
  `ab-validator/docs/superpowers/reports/2026-07-08-parser-fork-candidacy-faithful-comparison.md`,
  and
  `ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md`.
  It must not bind older provisional performance/coverage reports or unrelated
  parser citations.
- B2 binds all eight files under `abc/examples/ab-validator-output/`:
  `README.md`, `comparison-report.json`, `divergence.json`,
  `manifest-inputs.json`, `parser-ir.json`, `run-summary.jsonl`,
  `source-region-coverage.json`, and `warnings.jsonl`; it also binds
  `abc/schemas/{parser-ir,diagnostic,run-summary,manifest-inputs,manifest}.schema.json`,
  the three AAT mapping/divergence schemas, and
  `abc/data/aat-parser-ir-compatibility.edn`.
- B3 binds the three AAT mapping/divergence schemas,
  `abc/schemas/parser-ir.schema.json`,
  `abc/data/aat-parser-ir-compatibility.edn`,
  `abc/data/parser-evidence-citations.edn`, and the exact imported provenance/
  manifest files used by its positive and negative queries.
- B4 binds source-bundle schema/known answer, ADR 0033, current parser-IR
  schema, and identity fixtures.
- B5 binds the four new span fixtures, current parser-IR schema, imported
  parser IR/manifest inputs, and manifest schema.
- B6 contains `abc/flake.nix`, `abc/flake.lock`, `abc/deps.edn`,
  `abc/deps-lock.json`, `abc/tests.edn`,
  `abc/nix/tei-profile-artifacts.nix`,
  `abc/docs/evidence/adr-inputs/parser-publication-rendering-nix-clojure-closure.edn`
  plus every exact source/test path generated in that checked closure for
  `abc.tools.parser-publication-evidence-test/parser-publication-rendering-contract`,
  `abc/examples/v0/example-work/{parser-ir.json,source.manifest.json,metadata-record.json,manifest.json}`,
  all five files below `abc/examples/v0/example-persons/`,
  `abc/data/{parser-ir-publication-policy-v0.json,publication-policy.edn}`,
  `abc/schemas/{parser-ir,manifest,parser-ir-publication-preservation,tei-validation-result,metadata-record,person-record}.schema.json`,
  `abc/schemas/workflow-run.schema.json`, and
  `abc/schemas/tei-profile.{odd,rng,sch}`.
  The descriptor contract asserts the dedicated check imports the TEI
  expression and exposes `parser-publication-evidence`. The ODD remains a real
  determinant because publication reads it for profile identity and rule
  coverage; committed RNG/Schematron determine project validation, while the
  lock-pinned `teiAllSchema` separately determines upstream validation.
- B7 binds ADRs 0002/0030/0032/0038, the fork handoff, Cargo manifests and
  NOTICE files for `ab-aozora-{corpus,encoding,facade,pipeline,proptest,render,scan,spec,syntax,veb}`
  plus `ab-aozora/Cargo.toml`, provenance-bearing source files
  `corpus/src/lib.rs`, `encoding/src/lib.rs`, `facade/{README.md,src/lib.rs}`,
  `pipeline/src/lib.rs`, `proptest/src/lib.rs`, `render/src/lib.rs`,
  `scan/src/lib.rs`, `spec/src/{lib.rs,diagnostic.rs}`, `syntax/src/lib.rs`, and
  `veb/src/lib.rs` under `ab-validator/crates/ab-aozora-*`.
- B8 binds the assessment, ADR 0038, the three historical reports, fork
  handoff, owned parser-IR/publication schemas/policy, and maintainer boundary
  facts asserted by the test, plus `abc/docs/adr/governance-as-of.edn`.
- B9 binds its descriptor, `abc/schemas/parser-ir.schema.json`,
  `abc/data/aat-parser-ir-compatibility.edn`,
  `ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json`, and these exact
  `ab-validator/docs/superpowers/reports/` files:
  `2026-07-12-ab-aozora-phase5-c5-compat.edn`,
  `2026-07-12-phase5-c5-delta-audit.summary.json`,
  `2026-07-12-phase5-c5-conformance-gate.summary.json`,
  `2026-07-12-phase5-c5-perf.summary.json`,
  `2026-07-12-phase5-c5-conversion-gate.summary.json`,
  `2026-07-12-ab-aozora-phase5-c5-conversion-audit.summary.json`, and
  `2026-07-12-phase5-checkpoint.txt`. It does not bind `/db/...` locator
  targets.

Author `parser-ir-publication.edn` as a closed registration map with
`:schema-version :abc-adr-evidence-registration-v1` and an `:entries` vector
containing one literal map for every claim-matrix row in matrix order. Each map uses
the row's exact claim/evidence kinds, the boundary table's
`docs/evidence/adr-runs/<stem>.json` path and observation key, and
`{:operator := :value true}`; B10 rows use
`docs/evidence/adr-runs/design-bundle-operational.json` and
`design-bundle-exits-zero`. The file contains exactly 33 entries, no
`:artifact-hash`, and no comment/generated placeholder. Add a test in
`adr_evidence_capture_test.clj` that loads all nine descriptors and the entry
template, checks their closed key sets, exact observation names, self-inputs,
33 distinct claim IDs, and exact equality with the matrix claim set. It also
loads all eight manifests, calls Plan 1's static descriptor/manifest closure
for their version-2 descriptors, and separately proves B6's closed
version-1 Nix determinant set
validator; changing a descriptor input or a manifest path must fail before
capture.

- [ ] **Step 5: Regenerate inventory/graph and run Stage A audit**

```bash
cd abc
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/presentation-diagrams adr-graph
bin/kaocha --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.parser-evidence-test \
  --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test \
  --focus abc.tools.parser-ir-schema-evidence-test \
  --focus abc.tools.parser-relations-provenance-evidence-test \
  --focus abc.tools.parser-ownership-assessment-evidence-test \
  --focus abc.tools.parser-phase5-frozen-tuple-test
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.parser-publication-evidence" \
  --print-build-logs
clojure -M:abc/adr-governance --mode audit \
  --report /tmp/adr-parser-stage-a.json
if jq -e --arg family '^ADR-(0002|0007|0023|0024|0025|0030|0032|0038)-' \
  -f nix/adr-family-clean.jq /tmp/adr-parser-stage-a.json; then
  echo "expected Stage A missing-evidence debt" >&2
  exit 1
fi
jq -e --arg family '^ADR-(0002|0007|0023|0024|0025|0030|0032|0038)-' '
  [.problems[]
   | select(((.["claim-id"] // "") | test($family)))
   | select(.kind == "missing-claim-evidence")]
  | length == 33
' /tmp/adr-parser-stage-a.json
```

Expected: tests pass; audit exits 0 and reports only expected missing evidence
for the new family claims, not lifecycle/header/dependency/ledger problems.

- [ ] **Step 6: Commit the complete clean Stage A boundary**

```bash
git add abc/docs/adr abc/docs/evidence/external \
  abc/docs/evidence/adr-capture \
  abc/docs/evidence/adr-inputs \
  abc/docs/evidence/adr-entries/parser-ir-publication.edn \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/src/abc/tools/adr_evidence_inventory.clj \
  abc/test/abc/tools/adr_evidence_inventory_test.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj \
  abc/test/abc/tools/parser_evidence_test.clj \
  abc/test/abc/tools/parser_import_boundary_evidence_test.clj \
  abc/test/abc/tools/parser_mapping_admission_evidence_test.clj \
  abc/test/abc/tools/parser_identity_relations_evidence_test.clj \
  abc/test/abc/tools/parser_ir_schema_evidence_test.clj \
  abc/test/abc/tools/parser_publication_evidence_test.clj \
  abc/test/abc/tools/parser_relations_provenance_evidence_test.clj \
  abc/test/abc/tools/parser_ownership_assessment_evidence_test.clj \
  abc/test/abc/tools/parser_phase5_frozen_tuple_test.clj
git commit -m "docs(parser): bind corrected claims and capture contracts"
test -z "$(git status --porcelain --untracked-files=all)"
```

Expected: clean tree. This commit is the producer revision for B1–B9.

---

### Task 6: Capture B1–B9 into external staging and validate

**Files:**
- Create externally first, then materialize:
  `abc/docs/evidence/adr-runs/parser-{citation-identity,import-boundary,mapping-admission,identity-relations,ir-schema-regression,publication-rendering,relations-provenance,ownership-assessment,phase5-frozen-tuple}.json`

**Interfaces:**
- Consumes: the clean Stage A revision and plan-1 `--repo-root` contract.
- Produces: nine deterministic run bundles, all with the same producer revision.

- [ ] **Step 1: Capture every descriptor from monorepo root**

Run from `abc/`; `--repo-root ..` is required and validated by plan 1:

```bash
stage="$(mktemp -d)"
for stem in \
  parser-citation-identity parser-import-boundary parser-mapping-admission \
  parser-identity-relations parser-ir-schema-regression \
  parser-publication-rendering parser-relations-provenance \
  parser-ownership-assessment parser-phase5-frozen-tuple; do
  clojure -M:abc/adr-evidence-capture -- \
    --repo-root .. \
    --descriptor "abc/docs/evidence/adr-capture/${stem}.edn" \
    --output "$stage/${stem}.json"
done
test -z "$(git -C .. status --porcelain --untracked-files=all)"
```

Expected: every command exits 0, every observation is true, all producer
revisions are identical to the Stage A commit, and the worktree remains clean.

- [ ] **Step 2: Fully validate staged bundles before copying**

For each staged JSON call `validate-bundle-value`, then copy it temporarily to
its final repo-relative path and call `validate-bundle` with
`{:artifact-root <monorepo>/abc :workspace-root <monorepo>}` and the canonical
hash returned by `load-bundle`. Because copying dirties the tree only after capture
is complete, this is valid. Any schema/input/profile/hash problem aborts the
entire materialization; remove all copied bundles and repair via a new clean
Stage A commit.

```bash
mkdir -p docs/evidence/adr-runs
for stem in \
  parser-citation-identity parser-import-boundary parser-mapping-admission \
  parser-identity-relations parser-ir-schema-regression \
  parser-publication-rendering parser-relations-provenance \
  parser-ownership-assessment parser-phase5-frozen-tuple; do
  install -m 0644 "$stage/${stem}.json" "docs/evidence/adr-runs/${stem}.json"
done
```

Expected: only the nine run bundles are dirty.

---

### Task 7: Register bundles, regenerate reports, and close the family (Stage B)

**Files:**
- Modify: `abc/docs/adr/adr-evidence.edn` through registrar only
- Regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`
- Regenerate: `abc/docs/reports/adr-evidence-migration.json`
- Create: the nine run bundles copied in Task 6

**Interfaces:**
- Produces: zero parser-family governance problems without changing global
  enforcement mode.

- [ ] **Step 1: Materialize all claim joins through the registrar**

```bash
cd abc
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/parser-ir-publication.edn \
  --registry docs/adr/adr-evidence.edn \
  --workspace-root ..
cp docs/adr/adr-evidence.edn /tmp/parser-registry-first.edn
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/parser-ir-publication.edn \
  --registry docs/adr/adr-evidence.edn \
  --workspace-root ..
cmp /tmp/parser-registry-first.edn docs/adr/adr-evidence.edn
```

Expected: registrar reports 33 distinct owned claims, derives every canonical
artifact hash, validates observation/kind/predicate/input freshness, preserves
unrelated entries, and is byte-idempotent.

- [ ] **Step 2: Regenerate reports and use the checked family predicate**

```bash
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
nix run .#adr-governance -- \
  --repo-root "$PWD" \
  --workspace-root "$PWD/.." \
  --mode audit \
  --report "$PWD/docs/reports/adr-evidence-migration.json"
jq -e --arg family '^ADR-(0002|0007|0023|0024|0025|0030|0032|0038)-' \
  -f nix/adr-family-clean.jq \
  docs/reports/adr-evidence-migration.json
```

Expected: exit 0. The checked filter covers `.file`, `"claim-id"`, and every
`"affected-claim-ids"[]`, so shared artifact-root failures cannot be hidden.

- [ ] **Step 3: Run proportional verification**

Stage the exact Stage B files before Nix evaluation so root `self` contains the
new bundles/registry/report, then run:

```bash
cd ..
git add abc/docs/evidence/adr-runs \
  abc/docs/adr/adr-evidence.edn \
  abc/docs/reports/adr-claim-migration-inventory.json \
  abc/docs/reports/adr-evidence-migration.json
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link ".#checks.${system}.monorepo-adr-governance" \
  "./abc#checks.${system}.clj-kondo" \
  "./abc#checks.${system}.clj-nix-focused-tests" \
  "./abc#checks.${system}.parser-publication-evidence" \
  "./ab-validator#checks.${system}.phase5-checkpoint"
nix run ./abc#validate-design-bundle
```

Expected: every command exits 0 while ADR governance remains audit-mode.

- [ ] **Step 4: Commit Stage B atomically**

```bash
git commit -m "feat(adr): register parser and publication evidence"
test -z "$(git status --porcelain --untracked-files=all)"
```

Expected: clean tree; neutral comparison and ADR 0039 release qualification
remain unsatisfied future work; no enforcement switch occurs.

## Plan self-review

- All 36 baseline rows have an explicit disposition and resulting-ID mapping.
- All 33 final claims have one exact claim/evidence/boundary owner.
- No citation label alone authorizes selection, admission, or release.
- ADR 0038's central assessment states the bounded development interpretation,
  not merely that a document exists.
- Phase 5 checks the complete real-file tuple and evidence scope; synthetic
  checkpoint tests are corroboration only.
- Cross-component inputs use plan 1's contained monorepo profile; no `../` key
  enters a bundle.
- Offline registration and governance receive separate ABC artifact and
  monorepo workspace roots; the root-flake check built from root `self` is the
  full-registry authority.
- B1–B9 capture from one clean Stage A commit into external staging; B10 is
  shared read-only from plan 1.
- The registrar owns hashes and atomic registry replacement.
- Family closure uses the checked JQ predicate and covers file-local,
  claim-local, and artifact-root affected-claim IDs.
- No step relies on audit exit zero as evidence of clean report content.
