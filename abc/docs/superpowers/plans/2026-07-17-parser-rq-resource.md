# P3 Parser Resource Qualification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace predicate 8's missing RSS instrument with a governance-approved, fail-closed cgroup-v2 process-tree memory instrument and prove it on bounded fixtures without performing P5's authoritative corpus capture.

**Architecture:** A Python wrapper executes one production Parser-IR command inside one transient systemd user service and reads that service's kernel `memory.peak` before exiting. A capture orchestrator owns corpus membership and lifecycle; a pure Clojure analyzer authenticates the resulting closed index and derives the predicate observation. Predicate-set identity rotates through ADR 0040, so old envelopes are never relabeled and P5 must later recapture all nine predicates.

**Tech Stack:** Python 3, pytest, Clojure/Kaocha, systemd user services, cgroup v2, JSON Schema, Nix flakes, ADR evidence tooling.

## Global Constraints

- Follow `abc/docs/superpowers/specs/2026-07-17-parser-rq-resource-design.md` exactly.
- Measure `peak_cgroup_memory_bytes <= 2147483648`; safety ceiling is `3221225472` bytes.
- Every unit sets `MemoryAccounting=yes`, `MemoryMax=3221225472`, `MemorySwapMax=0`, `OOMPolicy=continue`, and `Delegate=no`.
- Run one work per transient service and run works serially on `hinoki.hyakutake-barbel.ts.net`.
- A missing/malformed counter, identity mismatch, lingering descendant, unexpected OOM, or incomplete index yields `:unavailable`; never impute a value.
- Ceiling-clipped results are right-censored available failures only when exact peak, OOM evidence, wrapper survival, and process closure are all established.
- Keep volatile host allocation tests separate from byte-identical pure derivation tests.
- Store only small JSON witnesses in Git; do not add corpus-scale artifacts.
- Do not modify registry admission or ADR 0039 status. Do not reuse any pre-rotation observation envelope.
- Run comment hygiene and all language checks required by `/home/bor/Projects/soranoha/AGENTS.md`.

## File Structure

- `abc/docs/adr/0040-process-tree-memory-qualification.md` owns the predicate amendment and recapture consequence.
- `abc/data/parser-release-qualification-predicates.edn` owns the renamed predicate and rotated set hash.
- `abc/schemas/parser-rq-resource-{policy,work,index,aggregate}.schema.json` own closed machine contracts.
- `ab-validator/reports/parser-ir/parser-rq-resource-identity.py` generates the wrapper semantic/runtime identity.
- `ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py` owns execute, closure, and kernel-counter capture only.
- `ab-validator/reports/parser-ir/parser-rq-resource-capture.py` owns membership, transient-unit launch, and closed-index emission.
- `abc/src/abc/tools/parser_rq_resource.clj` owns pure authentication, folding, and observation-envelope production.
- `ab-validator/tests/parser-rq-resource-*.py` and `abc/test/abc/tools/parser_rq_resource_test.clj` prove the layers independently.
- `ab-validator/flake.nix` exposes pure checks plus a host-controlled Hinoki smoke app; the live systemd test is not a sandboxed Nix check.

---

### Task 1: Govern and Rotate Predicate-Set Identity

**Files:**
- Create: `abc/docs/adr/0040-process-tree-memory-qualification.md`
- Modify: `abc/data/parser-release-qualification-predicates.edn`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`

**Interfaces:**
- Consumes: `abc.tools.parser-release-qualification/predicate-set-hash` and `qualification-identity-ref`.
- Produces: predicate key `:peak_cgroup_memory_bytes`, instrument `"parser-rq-resource-v1"`, unchanged threshold `2147483648`, and a newly computed `:predicate_set_hash`.

- [ ] **Step 1: Add failing identity-rotation tests**

Assert the loaded predicate set has exactly one memory predicate with the new key, threshold, and instrument; assert its declared hash equals `(rq/predicate-set-hash predicates)`; assert replacing the new predicate with the former RSS fields changes both predicate-set and qualification identity refs. Update the shared `admitted-identity`, `all-pass-values`, predicate-evaluation, gate-status, and report fixtures from the former hash/key to the recomputed hash and `:peak_cgroup_memory_bytes`; do not leave that migration to an `rg` failure.

- [ ] **Step 2: Run the focused test and confirm red**

Run: `nix develop ./abc#clj --command bin/kaocha --focus abc.tools.parser-release-qualification-test`

Expected: FAIL because the checked-in predicate still uses `:peak_rss_bytes` and `:instrument-missing`.

- [ ] **Step 3: Write ADR 0040 and rotate the predicate**

ADR 0040 must state: the quantity changes from per-process RSS wording to complete process-tree cgroup memory; the 2 GiB threshold is unchanged; ADR 0039 remains Proposed; all nine observation envelopes require fresh P5 capture; registry admission is untouched. It must also mark the existing measurements EDN and ADR-0039-cited qualification report as `superseded-pending-P5`: historically valid for the former predicate set, intentionally incoherent with the new live set, and forbidden as current gate evidence. Update the EDN predicate, compute the hash with repository code rather than hand calculation, and write that exact hash.

- [ ] **Step 4: Verify green and no stale key remains in active contracts**

Run:

```bash
nix develop ./abc#clj --command bin/kaocha --focus abc.tools.parser-release-qualification-test
rg -n "peak_rss_bytes" abc/data abc/src abc/test
```

Expected: focused tests PASS; `rg` finds only explicit legacy/rotation characterization, not an active predicate.

- [ ] **Step 5: Commit**

```bash
git add abc/docs/adr/0040-process-tree-memory-qualification.md \
  abc/data/parser-release-qualification-predicates.edn \
  abc/test/abc/tools/parser_release_qualification_test.clj
git commit -m "docs(parser-rq): govern process-tree memory predicate"
```

### Task 2: Define Closed Resource Contracts

**Files:**
- Create: `abc/schemas/parser-rq-resource-policy.schema.json`
- Create: `abc/schemas/parser-rq-resource-work.schema.json`
- Create: `abc/schemas/parser-rq-resource-index.schema.json`
- Create: `abc/schemas/parser-rq-resource-aggregate.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces: policy identity fields, per-work status union, closed record index, and aggregate status union.
- Statuses: `measured`, `ceiling_clipped`, or `unavailable`; unavailable reasons are a closed enum including `counter_unavailable`, `counter_invalid`, `identity_mismatch`, `lingering_descendant`, `unexpected_oom`, `command_failed`, and `index_incomplete`.

- [ ] **Step 1: Add failing schema-registration and union tests**

Use minimal valid fixtures for all four schemas. Add negative cases for an extra work, duplicate work, missing `memory_swap_max_bytes: 0`, measured work with OOM evidence, ceiling-clipped work at or below 2 GiB, and unavailable work carrying a fabricated peak.

- [ ] **Step 2: Run the schema focus and confirm red**

Run: `nix develop ./abc#clj --command bin/kaocha --focus abc.tools.validate-design-bundle-test`

Expected: FAIL because the four schemas are absent.

- [ ] **Step 3: Implement schemas and register them**

Make every object closed with `additionalProperties: false`. Require byte counts to be non-negative integers. Bind policy to predicate-set hash, wrapper closure hash, Python executable hash/runtime tuple, production command hash, cgroup capability id, all five unit properties, threshold, and ceiling. Require the index's ordered work IDs to equal policy membership; the analyzer will enforce equality beyond schema shape.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./abc#clj --command bin/kaocha --focus abc.tools.validate-design-bundle-test
git add abc/schemas/parser-rq-resource-*.schema.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-rq): define resource evidence contracts"
```

### Task 3: Generate Wrapper Semantic and Runtime Identity

**Files:**
- Create: `ab-validator/reports/parser-ir/parser-rq-resource-identity.py`
- Create: `ab-validator/tests/test_parser_rq_resource_identity.py`

**Interfaces:**
- Produces: `discover_local_import_closure(entry: Path, roots: tuple[Path, ...]) -> tuple[Path, ...]` and `build_identity(entry: Path, python: Path, nix_derivation: str) -> dict[str, object]`.
- Identity hashes exact source bytes for the mechanically discovered local import closure and records resolved interpreter SHA-256, implementation, cache tag, version, and Nix derivation.

- [ ] **Step 1: Add failing identity tests**

Create temporary modules `entry.py -> helper.py`; assert both enter a stable sorted closure, changing helper bytes changes the semantic hash, an unlisted new import changes the closure, and changing interpreter metadata changes identity. Assert imports outside reviewed repository roots fail closed rather than disappear.

- [ ] **Step 2: Run and confirm red**

Run: `nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_identity.py`

Expected: FAIL because the module is absent.

- [ ] **Step 3: Implement AST closure and canonical identity**

Resolve `import` and `from ... import ...` without importing the modules. Reject dynamic local imports and unresolved relative imports. Hash a canonical JSON array of repository-relative path plus SHA-256 pairs. Resolve `sys.executable` and stream-hash its bytes. Serialize with sorted keys and compact separators.

- [ ] **Step 4: Verify and commit the generator**

```bash
nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_identity.py
git add ab-validator/reports/parser-ir/parser-rq-resource-identity.py \
  ab-validator/tests/test_parser_rq_resource_identity.py
git commit -m "feat(parser-rq): derive resource semantic identity"
```

### Task 4: Capture One Service Cgroup Honestly

**Files:**
- Create: `ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py`
- Create: `ab-validator/tests/test_parser_rq_resource_wrapper.py`

**Interfaces:**
- Produces: `capture_work(command: list[str], closure_timeout_s: float, *, cgroup_dir: Path | None = None) -> dict[str, object]`.
- Reads `memory.peak`, optional `memory.swap.peak`, `memory.events`, and `cgroup.procs` directly; never parses localized `systemctl` output.

- [ ] **Step 1: Add failing filesystem-level tests**

Use temporary fake cgroup files to prove: an exact measured peak; malformed/missing peak becomes unavailable; nonzero swap contradicts policy; a remaining PID becomes `lingering_descendant`; OOM below/equal threshold is unavailable; OOM with an exact peak above threshold becomes `ceiling_clipped`; command failure without qualifying ceiling evidence is unavailable.

- [ ] **Step 2: Run and confirm red**

Run: `nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_wrapper.py`

Expected: FAIL because the wrapper is absent.

- [ ] **Step 3: Implement the minimal wrapper**

Self-discover the live cgroup from `/proc/self/cgroup`; resolve that relative path beneath the mounted cgroup-v2 root. The keyword-only `cgroup_dir` exists solely for fake-filesystem tests and must be rejected in the production CLI. Launch the command as the wrapper's direct child, wait for it, then poll `cgroup.procs` until it contains only `os.getpid()` or times out. Stream-read counters, parse base-10 integers strictly, and emit one canonical JSON record. Label clipped values with `right_censored: true`; measured values require `right_censored: false`.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_wrapper.py
git add ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py \
  ab-validator/tests/test_parser_rq_resource_wrapper.py
git commit -m "feat(parser-rq): capture cgroup memory peak"
```

### Task 5: Materialize the Wrapper Identity and Qualification Policy

**Files:**
- Create generated: `ab-validator/data/parser-rq-resource-identity-v1.json`
- Create: `abc/data/parser-rq-resource-policy-v1.json`
- Modify: `ab-validator/tests/test_parser_rq_resource_identity.py`

**Interfaces:**
- Consumes: the real wrapper created by Task 4, the predicate-set hash from Task 1, and the already-pinned qualification corpus membership.
- Produces: the exact wrapper semantic/runtime identity and closed policy consumed by capture. Bounded test fixtures use separate synthetic policies; they are not qualification membership.

- [ ] **Step 1: Add the failing real-wrapper closure test**

Assert the generated closure for `parser-rq-resource-wrapper.py` equals the committed manifest exactly and currently contains only the wrapper plus any repository-local modules it actually imports. The stdlib is bound through the interpreter/runtime tuple, not listed as repository source. Assert the policy's copied identity fields equal the manifest and its ordered work IDs equal the existing pinned qualification corpus list.

- [ ] **Step 2: Run and confirm red**

Run: `nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_identity.py`

Expected: FAIL because the committed manifest and policy are absent.

- [ ] **Step 3: Generate the identity twice and write the policy**

```bash
nix develop ./ab-validator#checks --command python \
  ab-validator/reports/parser-ir/parser-rq-resource-identity.py \
  --entry ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py \
  --out /tmp/resource-identity-1.json
nix develop ./ab-validator#checks --command python \
  ab-validator/reports/parser-ir/parser-rq-resource-identity.py \
  --entry ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py \
  --out /tmp/resource-identity-2.json
cmp /tmp/resource-identity-1.json /tmp/resource-identity-2.json
```

Expected: both generators exit 0 and `cmp` exits 0. Install that byte-identical manifest at `ab-validator/data/parser-rq-resource-identity-v1.json`. Create the policy by copying its generated identity fields exactly and adding the predicate-set hash, pinned qualification membership, production command hash, capability id `linux-cgroup-v2-memory-peak-v1`, threshold `2147483648`, ceiling `3221225472`, and all five systemd properties.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_identity.py
git add ab-validator/data/parser-rq-resource-identity-v1.json \
  abc/data/parser-rq-resource-policy-v1.json \
  ab-validator/tests/test_parser_rq_resource_identity.py
git commit -m "feat(parser-rq): bind resource wrapper semantics"
```

### Task 6: Orchestrate Closed, Serial Transient Services

**Files:**
- Create: `ab-validator/reports/parser-ir/parser-rq-resource-capture.py`
- Create: `ab-validator/tests/test_parser_rq_resource_capture.py`

**Interfaces:**
- Produces: `build_systemd_run(work_id: str, wrapper_argv: list[str]) -> list[str]` and `capture_index(policy: dict, runner: Callable) -> dict`.
- Unit names are random attempt context, never observation identity.

- [ ] **Step 1: Add failing orchestration tests**

Assert argv contains `--user`, `--service-type=exec`, all five exact properties, and no `--scope`; assert calls are serial and policy-ordered; assert one missing/duplicate/extra result makes the index unavailable; assert unit names differ across attempts without changing logical output after attempt context is removed.

- [ ] **Step 2: Run and confirm red**

Run: `nix develop ./ab-validator#checks --command pytest -q ab-validator/tests/test_parser_rq_resource_capture.py`

Expected: FAIL because the orchestrator is absent.

- [ ] **Step 3: Implement lifecycle ownership**

The orchestrator does not resolve or pass `ControlGroup`; the already-running wrapper self-discovers its live cgroup through `/proc/self/cgroup` before systemd can collect the transient unit. Record exact kernel/systemd/NixOS/boot/swap/load values under `attempt_context`, not policy identity. Stop after any unavailable work only if configured fail-fast; either mode must emit closed membership and unavailable aggregate state.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./ab-validator#checks --command pytest -q \
  ab-validator/tests/test_parser_rq_resource_capture.py \
  ab-validator/tests/test_parser_rq_resource_wrapper.py
git add ab-validator/reports/parser-ir/parser-rq-resource-capture.py \
  ab-validator/tests/test_parser_rq_resource_capture.py
git commit -m "feat(parser-rq): orchestrate serial resource capture"
```

### Task 7: Add a Hinoki-Controlled Live Integration Smoke

**Files:**
- Create: `ab-validator/tests/parser-rq-resource-cgroup-live-smoke.sh`
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Produces flake app: `.#parser-rq-resource-cgroup-smoke`.
- This is a volatile host integration test, not a Nix `checks` derivation and not drift evidence.

- [ ] **Step 1: Write the failing smoke**

The script runs one small child allocation and one threshold-crossing fixture through the real transient-service path. Assert a positive exact peak for the small case, zero swap, and an available right-censored failure for the ceiling case. Skip is forbidden: absence of user systemd, cgroup v2, or required properties exits nonzero with a named prerequisite error.

- [ ] **Step 2: Run on Hinoki and confirm red**

Run on `hinoki.hyakutake-barbel.ts.net` from the same Git revision:

```bash
nix run ./ab-validator#parser-rq-resource-cgroup-smoke
```

Expected: FAIL because the app is not exposed.

- [ ] **Step 3: Expose the app without adding a sandbox check**

Use `pkgs.writeShellApplication` with runtime inputs `systemd`, `python3`, `coreutils`, and `jq`. The app invokes the committed smoke script. Do not add it under `checks`, because a Nix build sandbox cannot exercise the user service manager faithfully.

- [ ] **Step 4: Re-run on Hinoki and commit**

Expected: PASS with two named cases and emitted peak/censoring summaries.

```bash
git add ab-validator/tests/parser-rq-resource-cgroup-live-smoke.sh ab-validator/flake.nix
git commit -m "test(parser-rq): exercise live cgroup resource capture"
```

### Task 8: Derive the Aggregate and Observation Purely

**Files:**
- Create: `abc/src/abc/tools/parser_rq_resource.clj`
- Create: `abc/test/abc/tools/parser_rq_resource_test.clj`
- Create: `abc/test/fixtures/parser-rq-resource/`

**Interfaces:**
- Produces: `analyze` returning `{:status :measured|:unavailable :value <max-or-nil> :works [...]}` and `observation-envelope` keyed by `:peak_cgroup_memory_bytes`.
- Consumes only policy, qualification identity, capture index, and addressed work records; no filesystem discovery.

- [ ] **Step 1: Add failing pure analyzer tests**

Cover: max over exact measured works; any exact peak above 2 GiB fails; ceiling-clipped work fails; any unavailable/missing/extra/duplicate work makes aggregate unavailable; policy/identity/runtime/command/host-capability mismatch is unavailable; attempt-context changes do not change derivation; input order does not change canonical output; old `:peak_rss_bytes` envelope cannot cohere.

- [ ] **Step 2: Run and confirm red**

Run: `nix develop ./abc#clj --command bin/kaocha --focus abc.tools.parser-rq-resource-test`

Expected: FAIL loading `abc.tools.parser-rq-resource`.

- [ ] **Step 3: Implement authenticate then fold**

Validate closed membership and logical hashes first. Fold only authenticated work records. Preserve right-censoring in witnesses, but predicate result is `:fail` whenever authenticated peak exceeds the threshold. Build the envelope using `rq/qualification-identity-ref`; do not copy or rewrite an old envelope.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./abc#clj --command bin/kaocha --focus abc.tools.parser-rq-resource-test
git add abc/src/abc/tools/parser_rq_resource.clj \
  abc/test/abc/tools/parser_rq_resource_test.clj \
  abc/test/fixtures/parser-rq-resource
git commit -m "feat(parser-rq): derive process-tree memory observation"
```

### Task 9: Prove Deterministic Bounded Capture

**Files:**
- Create: `ab-validator/tests/parser-rq-resource-capture-smoke.sh`
- Modify: `ab-validator/flake.nix`
- Modify: `abc/test/abc/tools/parser_rq_resource_test.clj`

**Interfaces:**
- Produces Nix check: `parser-rq-resource-capture-smoke` using deterministic fake cgroup inputs.
- The check regenerates byte-identical index, records, aggregate, and envelope twice.

- [ ] **Step 1: Add a failing end-to-end fixture test**

Run wrapper filesystem simulation, orchestrator with a fake runner, and the Clojure analyzer twice. Compare canonical artifacts byte-for-byte. Mutate one counter, policy hash, command hash, wrapper dependency, and membership entry separately; each must either change authenticated output or become unavailable.

- [ ] **Step 2: Run and confirm red**

```bash
bash ab-validator/tests/parser-rq-resource-capture-smoke.sh
nix build ./ab-validator#checks.x86_64-linux.parser-rq-resource-capture-smoke
```

Expected: FAIL before the script/check is wired.

- [ ] **Step 3: Implement the pure smoke check**

Use only checked-in tiny fixtures and fake cgroup files. Normalize no volatile values after capture; volatile attempt context belongs outside canonical evidence by contract. Wire the script under `checks` because this path is pure and sandbox-safe.

- [ ] **Step 4: Verify and commit**

```bash
bash ab-validator/tests/parser-rq-resource-capture-smoke.sh
nix build ./ab-validator#checks.x86_64-linux.parser-rq-resource-capture-smoke
nix develop ./abc#clj --command bin/kaocha --focus abc.tools.parser-rq-resource-test
git add ab-validator/tests/parser-rq-resource-capture-smoke.sh \
  ab-validator/flake.nix abc/test/abc/tools/parser_rq_resource_test.clj
git commit -m "test(parser-rq): drift-test resource derivation"
```

### Task 10: Register Governance Evidence and Verify the Campaign Boundary

**Files:**
- Create: `abc/data/adr-evidence/parser-rq-resource-observation-catalog.edn`
- Create: `abc/docs/evidence/adr-descriptors/0040-parser-rq-resource-pure.edn`
- Create: `abc/docs/evidence/adr-descriptors/0040-parser-rq-resource-hinoki.edn`
- Create: `abc/docs/evidence/adr-entries/parser-rq-resource.edn`
- Create: `abc/docs/evidence/adr-runs/0040/`
- Modify generated: `abc/docs/adr/adr-evidence.edn`
- Modify: `abc/docs/adr/0040-process-tree-memory-qualification.md`

**Interfaces:**
- Produces separately validated ADR 0040 evidence for bounded instrument proof only.
- Does not produce an authoritative full-corpus predicate-8 envelope or modify ADR 0039.

- [ ] **Step 1: Add the bounded observation catalog**

Register the deterministic pure drift artifact and the dated Hinoki integration result. Bind both to commit, host/capability disclosure, policy hash, predicate-set hash, and exact commands. Mark the live result as an integration smoke, never as the byte-identical drift authority.

- [ ] **Step 2: Capture, register, and validate ADR evidence**

Run the checked-in ADR capture descriptors into an external staging directory,
install their validated bundles, register the resulting claim joins, and run the
governance audit. The descriptor filenames created in Step 1 are
`docs/evidence/adr-descriptors/0040-parser-rq-resource-pure.edn` and
`docs/evidence/adr-descriptors/0040-parser-rq-resource-hinoki.edn`.

```bash
cd abc
rm -rf /tmp/parser-rq-resource-0040-stage
mkdir -p /tmp/parser-rq-resource-0040-stage
clojure -M:abc/adr-evidence-capture -- \
  --repo-root . --workspace-root .. \
  --staging-root /tmp/parser-rq-resource-0040-stage \
  --descriptor docs/evidence/adr-descriptors/0040-parser-rq-resource-pure.edn \
  --output /tmp/parser-rq-resource-0040-stage/0040-parser-rq-resource-pure.json
clojure -M:abc/adr-evidence-capture -- \
  --repo-root . --workspace-root .. \
  --staging-root /tmp/parser-rq-resource-0040-stage \
  --descriptor docs/evidence/adr-descriptors/0040-parser-rq-resource-hinoki.edn \
  --output /tmp/parser-rq-resource-0040-stage/0040-parser-rq-resource-hinoki.json
mkdir -p docs/evidence/adr-runs/0040
install -m 0644 \
  /tmp/parser-rq-resource-0040-stage/0040-parser-rq-resource-pure.json \
  docs/evidence/adr-runs/0040/0040-parser-rq-resource-pure.json
install -m 0644 \
  /tmp/parser-rq-resource-0040-stage/0040-parser-rq-resource-hinoki.json \
  docs/evidence/adr-runs/0040/0040-parser-rq-resource-hinoki.json
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/parser-rq-resource.edn \
  --registry docs/adr/adr-evidence.edn
clojure -M:abc/adr-governance -- \
  --workspace-root .. --mode audit \
  --report docs/reports/adr-evidence-migration.json
```

Expected: each command exits 0; the registered ADR 0040 claims reference only
the two checked-in small witnesses.

- [ ] **Step 3: Run focused and repository-wide verification**

```bash
nix develop ./ab-validator#checks --command pytest -q \
  ab-validator/tests/test_parser_rq_resource_identity.py \
  ab-validator/tests/test_parser_rq_resource_wrapper.py \
  ab-validator/tests/test_parser_rq_resource_capture.py
nix develop ./abc#clj --command bin/kaocha \
  --focus abc.tools.parser-rq-resource-test \
  --focus abc.tools.parser-release-qualification-test
nix build ./ab-validator#checks.x86_64-linux.parser-rq-resource-capture-smoke
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
just validate-migration
```

Expected: every command exits 0. Separately rerun `nix run ./ab-validator#parser-rq-resource-cgroup-smoke` on Hinoki at the committed revision and attach its small witness before the ADR evidence validation.

- [ ] **Step 4: Perform boundary audit**

Run:

```bash
git diff origin/main...HEAD -- abc/data/compatibility-registry.edn abc/docs/adr/0039-custom-parser-release-qualification.md
rg -n "peak_rss_bytes|instrument-missing" abc/data/parser-release-qualification-predicates.edn
git status --short
```

Expected: no registry or ADR-0039 status change; no stale active memory predicate; only intended files are modified.

- [ ] **Step 5: Commit governance evidence**

```bash
git add abc/data/adr-evidence/parser-rq-resource-observation-catalog.edn \
  abc/docs/evidence/adr-descriptors/0040-parser-rq-resource-*.edn \
  abc/docs/evidence/adr-entries/parser-rq-resource.edn \
  abc/docs/evidence/adr-runs/0040 abc/docs/adr/adr-evidence.edn \
  abc/docs/adr/0040-process-tree-memory-qualification.md
git commit -m "docs(parser-rq): evidence process-tree memory amendment"
```

## Self-Review Checklist

- Predicate semantics, governance rotation, all-nine-envelope recapture consequence and interim evidence supersession: Task 1.
- Closed schemas, no imputation, right-censoring: Tasks 2, 4, and 7.
- Mechanized semantic/runtime closure: Tasks 3–5.
- Swap prohibition, service properties, lifecycle, serial execution: Tasks 4, 6, and 7.
- Capability identity versus volatile attempt context: Tasks 2 and 6.
- Separate live integration smoke and pure byte drift: Tasks 7 and 9.
- Pure analyzer, closed denominator, fresh envelope: Task 8.
- No registry admission, ADR-0039 promotion, or P5 corpus capture: Task 10 boundary audit.
