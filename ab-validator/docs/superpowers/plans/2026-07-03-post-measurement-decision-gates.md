# Post-Measurement Decision Gates Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the current aozora2html full-corpus AAT run from a useful measurement into a decision-quality measurement by classifying completeness gaps, extracting warigaki/kunten evidence, and publishing the gate result that determines whether ab-validator can start the owned AAT-to-parser-IR CLI.

**Architecture:** Keep the boundary file-based and measurement-first. The existing full run remains immutable evidence under `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`; new scripts read that run plus any targeted retry run and emit small, committed Markdown reports. Source feature detection, check reports, persisted AAT JSON, and DuckDB triage tables are reconciled into one trust table. The plan deliberately does not add AAT v2 vocabulary, does not change ABC manifests, and does not start compatibility-registry hardening.

**Tech Stack:** Bash, Python 3, DuckDB, `just`, existing `ab-index` JSON, existing `ab-check` reports, AAT JSON, Rust workspace tests for adjacent smoke coverage.

## Global Constraints

- Do not change AAT schema or parser-IR schema in this plan.
- Do not implement `crates/ab-aat-to-parser-ir` in this plan; produce the go/no-go evidence for a follow-up CLI plan.
- Do not edit `../abc` directly. Emit ABC-sync material in this repo, then let the ABC session consume it.
- Treat `/db/ab-validator/aat-corpus/...` as generated measurement output. Do not commit full-run AAT, check reports, DuckDB files, or workset JSON.
- Keep committed reports small. Do not paste long source excerpts from Aozora works; use work IDs, paths, node previews, and short marker strings only.
- Use the current full run as the baseline: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`.
- Keep corpus reruns targeted unless the audit finds a systemic adapter change that invalidates the full run.
- Keep taxonomy drift as a verification gate, not a design task. Current `generate_taxonomy` already reads `annotation/*.html` plus real `cards/` corpus and does not read `chuki_tag.txt`.

---

## Hammock Synthesis

Current settled facts and pre-audit measurements:

- The aozora-rs AAT-to-parser-IR mapping is now generated from measured folded buckets, validates against ABC's mapping schema, has 25 observed rules, and has zero measured `UNSUPPORTED`.
- The current aozora2html full run exists and is current-adapter evidence: 17,886 reports, 17,689 persisted AAT files, 197 schema-invalid or missing reports, 302 parse-incomplete or missing reports, 4,050 warigaki nodes across 243 works, and 22,504 kunten observations across 472 works.
- A quick source-index scan of the same run shows 359 source warigaki works and 635 source kunten works. Here `kunten` means the deduplicated union of `kaeriten` and `okurigana`, not the sum of those feature counts. Among those source-feature works, the current reports include 34 warigaki works with no persisted AAT/schema-invalid status and 39 parse-incomplete warigaki works; for kunten, 37 have no persisted AAT/schema-invalid status and 47 are parse-incomplete. These are pre-audit numbers; Task 1 is the reproducible authority and must either confirm or replace them before policy conclusions are treated as durable.
- The adapter boundary decision is locally recorded: AAT JSON is the normative adapter contract; `ab-ir` is an optional in-workspace helper until someone publishes or advertises it as an external SDK.

Main crux:

The question is no longer "does aozora2html ever emit warigaki or kunten?" It does. The question is whether the observed counts are complete enough to support policy, or whether the remaining failures hide enough source-feature works that the run can only support lower-bound claims.

Chosen direction:

Build a source-vs-AAT audit and sample extractor over the existing run. If the audit accounts for every warigaki/kunten source-feature work and any residual gaps are classified, publish the ABC sync and unblock the owned-mapping CLI follow-up. If adapter failures or parse incompleteness cover meaningful warigaki/kunten worksets, run a targeted retry at a longer timeout, then publish the merged trust verdict.

---

## Task 1: Add Aozora2html Measurement Trust Audit

**Files:**
- Create: `reports/aat-fidelity/audit-aozora2html-measurement.py`
- Create: `tests/aozora2html-measurement-audit-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/index.json`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/fidelity.duckdb`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/check-reports/`
- Read: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/`

**Interfaces:**
- Input CLI:
  `python3 reports/aat-fidelity/audit-aozora2html-measurement.py --run-dir "$RUN_DIR" --out-md "$REPORT_MD" --summary-json "$SUMMARY_JSON" --worksets-dir "$WORKSETS_DIR"`
- Optional input:
  `--retry-run-dir "$RETRY_RUN_DIR"` lets the audit compare a targeted retry against the baseline.
- Output Markdown sections:
  source-feature counts, AAT-observed counts, source-vs-AAT overlap, completeness buckets, failure-property overlap, and generated retry workset paths.
- Output generated worksets under `/db/.../worksets/`, not in git:
  `warigaki-incomplete.json`, `kunten-incomplete.json`, `policy-incomplete-union.json`.
- Workset files are flat JSON arrays of work-ID strings, matching `ab-check --work-ids`'s `Vec<String>` contract. Example: `["000005_53194", "000006_1868"]`.
- Output summary JSON keys consumed by Task 4:
  `run_dir`, `report_id`, `source_sets`, `source_counts`, `aat_observed_counts`, `bucket_counts`, `failure_overlap`, `worksets`, `retry`, `verdict_inputs`.
  `source_sets` must contain sorted arrays named `warigaki`, `kaeriten`, `okurigana`, `kunten`, and `policy_union`.

- [ ] **Step 1:** Implement the audit reader.

  It must load:

  - `index.json` `by_feature.warigaki`, `by_feature.kaeriten`, and `by_feature.okurigana`.
  - DuckDB tables `aat_batch_reports`, `aat_batch_property_results`, `aat_batch_source_derived_nodes`, and `aat_batch_semantic_summary`.
  - AAT JSON files to count `kind == "warigaki"` nodes and kunten representations (`style_type == "kaeriten"` plus `meta.semantic_summary.syntax` keys beginning with `kunten.`).

  Build these source-feature sets:

  - `warigaki = set(index.by_feature["warigaki"])`
  - `kunten = set(index.by_feature["kaeriten"]) ∪ set(index.by_feature["okurigana"])`
  - `policy_union = warigaki ∪ kunten`

  Count each work once per family. A work that has both `kaeriten` and `okurigana` counts once in `kunten`. A work that has both `warigaki` and `kunten` counts once in `policy_union`.

- [ ] **Step 2:** Emit deterministic classification buckets.

  For each source-feature family (`warigaki`, `kunten`), classify every source-feature work ID into exactly one primary bucket:

  - `observed_in_aat`
  - `schema_invalid_or_no_aat`
  - `parse_incomplete`
  - `adapter_timeout_or_protocol_error`
  - `report_failed_other_property`
  - `source_feature_without_aat_observation`

  Also emit cross-cutting counts for `visible_text_body_order`, `gaiji_resolution`, `ruby_completeness`, and `parse_completeness` so the report shows which existing failures overlap with policy evidence. The Markdown report and summary JSON must use the same bucket names listed above.

- [ ] **Step 3:** Make the audit fail on impossible accounting.

  The script exits non-zero if:

  - a source-feature work appears in no bucket,
  - for either family, the bucket count sum does not equal that family's deduplicated source-feature set size,
  - the generated `policy-incomplete-union.json` is not a flat JSON array of unique strings sorted ascending,
  - the run has no `metadata.json`,
  - the run has no persisted AAT directory,
  - the run report ID cannot be found in DuckDB.

- [ ] **Step 4:** Add a smoke test with a tiny synthetic run.

  `tests/aozora2html-measurement-audit-smoke.sh` must be a shell script that:

  - builds a baseline fixture run directory under `${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-measurement-audit-smoke/base`,
  - writes check reports under `check-reports/fixture-adapter/`,
  - writes AAT JSON under `aat/fixture-adapter/`,
  - writes `metadata.json`,
  - writes an `index.json` whose `by_feature.kaeriten` and `by_feature.okurigana` overlap so the kunten union is tested,
  - invokes `python3 reports/aat-fidelity/build-aat-batch-triage.py --reports-dir "$base/check-reports" --aat-dir "$base/aat" --db "$base/fidelity.duckdb" --report-id smoke-base --out-dir "$base/triage"`,
  - invokes `python3 reports/aat-fidelity/audit-aozora2html-measurement.py --run-dir "$base" --out-md "$out/audit.md" --summary-json "$out/audit.summary.json" --worksets-dir "$out/worksets"`,
  - asserts with `jq` that `source_counts.kunten` equals the deduplicated union size, not the sum of `kaeriten` and `okurigana`,
  - asserts with `jq -e 'type == "array" and all(.[]; type == "string")' "$out/worksets/policy-incomplete-union.json"`.

  The same smoke test must also build a retry fixture run under `${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-measurement-audit-smoke/retry`, invoke `build-aat-batch-triage.py` for it, then invoke the audit with `--retry-run-dir "$retry"`. Assert that a work classified as incomplete in the baseline moves to `observed_in_aat` when the retry AAT contains the relevant observation. This covers the Task 3 merge branch before any live retry is run.

- [ ] **Step 5:** Run the audit on the current full run.

  ```bash
  python3 reports/aat-fidelity/audit-aozora2html-measurement.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets
  ```

- [ ] **Step 6:** Verify Task 1.

  ```bash
  bash tests/aozora2html-measurement-audit-smoke.sh
  rg -n "source warigaki works|source kunten works|schema_invalid_or_no_aat|source_feature_without_aat_observation" docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md
  jq -e '.source_counts.kunten == (.source_sets.kaeriten + .source_sets.okurigana | unique | length)' docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json
  jq -e 'type == "array" and all(.[]; type == "string")' /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json
  ```

- [ ] **Step 7:** Commit Task 1.

  ```bash
  git add reports/aat-fidelity/audit-aozora2html-measurement.py \
    tests/aozora2html-measurement-audit-smoke.sh \
    docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json
  git commit -m "test: audit aozora2html measurement trust"
  ```

---

## Task 2: Extract Warigaki And Kunten Policy Samples

**Files:**
- Create: `reports/aat-fidelity/extract-aozora2html-policy-samples.py`
- Create: `tests/aozora2html-policy-samples-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`
- Create: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json`
- Read: Task 1 report and generated worksets
- Read: baseline AAT JSON and check reports

**Interfaces:**
- Input CLI:
  `python3 reports/aat-fidelity/extract-aozora2html-policy-samples.py --run-dir "$RUN_DIR" --audit-md "$AUDIT_MD" --audit-summary-json "$AUDIT_SUMMARY_JSON" --out-md "$REPORT_MD" --summary-json "$SUMMARY_JSON" --limit-per-bucket 10`
- Output Markdown:
  deterministic samples for observed warigaki, source-warigaki-without-AAT-observation, observed kunten, source-kunten-without-AAT-observation, adapter-failed policy works, and parse-incomplete policy works.
- Output summary JSON keys consumed by Task 4:
  `run_dir`, `sample_counts`, `families`, `buckets_sampled`, `limit_per_bucket`.

- [ ] **Step 1:** Implement sample extraction without long source excerpts.

  Each sample row must contain:

  - work ID,
  - report path relative to the run directory,
  - AAT path relative to the run directory if present,
  - node path if observed,
  - node kind or semantic syntax ID,
  - compact node JSON preview with child arrays removed,
  - failure properties for the work.

- [ ] **Step 2:** Include both positive and negative evidence.

  The report must include:

  - at least 10 observed warigaki examples, or all examples if fewer exist,
  - at least 10 observed kunten examples, or all examples if fewer exist,
  - examples from `source_feature_without_aat_observation` for each family if that bucket is non-empty,
  - examples from schema-invalid/no-AAT and parse-incomplete buckets if non-empty.

- [ ] **Step 3:** Add the smoke test.

  The smoke test should create a tiny synthetic run with one observed warigaki node, one observed kunten style, and one source-feature work without AAT observation, then assert the report contains all three sample categories.

- [ ] **Step 4:** Run the extractor on the current full run.

  ```bash
  python3 reports/aat-fidelity/extract-aozora2html-policy-samples.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --audit-md docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    --audit-summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json \
    --limit-per-bucket 10
  ```

- [ ] **Step 5:** Verify Task 2.

  ```bash
  bash tests/aozora2html-policy-samples-smoke.sh
  rg -n "Observed warigaki|Observed kunten|Source feature without AAT observation" docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md
  jq -e '.limit_per_bucket == 10 and (.sample_counts | type == "object")' docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json
  ```

- [ ] **Step 6:** Commit Task 2.

  ```bash
  git add reports/aat-fidelity/extract-aozora2html-policy-samples.py \
    tests/aozora2html-policy-samples-smoke.sh \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json
  git commit -m "docs: sample aozora2html policy evidence"
  ```

---

## Task 3: Run Targeted Retry Only If The Audit Needs It

**Files:**
- Generated only: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json`
- Optional generated run: a timestamped directory under `/db/ab-validator/aat-corpus/` named with the `aozora2html-policy-retry-` prefix.
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- Modify: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`

**Gate:**
- Skip this task if Task 1 reports zero works in `schema_invalid_or_no_aat`, `parse_incomplete`, and `adapter_timeout_or_protocol_error` for both source-feature families.
- Run this task if any of those buckets are non-empty.

- [ ] **Step 1:** Inspect the generated retry workset.

  ```bash
  jq length /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json
  jq -r '.[:20][]' /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json
  ```

- [ ] **Step 2:** Run a targeted retry with 24 jobs and a longer timeout.

  ```bash
  workset="/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-incomplete-union.json"
  retry_dir="/db/ab-validator/aat-corpus/aozora2html-policy-retry-$(date -u +%Y%m%dT%H%M%SZ)"

  # just recipe parameters are positional here. Do not use
  # `just aozora2html-aat-full DIR=... JOBS=...`; those strings become literal
  # recipe arguments in this justfile.
  just --dry-run aozora2html-aat-full \
    "$retry_dir" \
    24 \
    600s \
    aozora2html-policy-retry-2026-07-03 \
    "$workset" \
    "" \
    | tee /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dry-run.txt
  rg -F 'jobs="24"' /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dry-run.txt
  rg -F -- '--timeout "600s"' /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dry-run.txt
  rg -F -- "--work-ids \"$workset\"" /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dry-run.txt

  just aozora2html-aat-full \
    "$retry_dir" \
    24 \
    600s \
    aozora2html-policy-retry-2026-07-03 \
    "$workset" \
    ""
  printf '%s\n' "$retry_dir" > /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dir.txt
  ```

- [ ] **Step 3:** Re-run the audit with the retry run attached.

  ```bash
  retry_dir="$(cat /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets/policy-retry-dir.txt)"
  python3 reports/aat-fidelity/audit-aozora2html-measurement.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --retry-run-dir "$retry_dir" \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --worksets-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/worksets
  ```

- [ ] **Step 4:** Re-run the sample extractor against the updated audit.

  ```bash
  python3 reports/aat-fidelity/extract-aozora2html-policy-samples.py \
    --run-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z \
    --audit-md docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    --audit-summary-json docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    --out-md docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    --summary-json docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json \
    --limit-per-bucket 10
  ```

- [ ] **Step 5:** Commit Task 3 only if report content changed.

  ```bash
  git add docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md \
    docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md \
    docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json
  git commit -m "docs: classify aozora2html policy measurement retry"
  ```

---

## Task 4: Publish The ABC Sync And CLI Readiness Verdict

**Files:**
- Create: `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`
- Read: `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.md`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-policy-samples.summary.json`
- Read: `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`

**Verdict values:**
- `CLI_READY`: generated mapping exists, aozora-rs has zero `UNSUPPORTED`, aozora2html warigaki/kunten policy evidence is classified, and there are no residual policy-relevant adapter failures that could change the measured policy conclusion.
- `CLI_READY_WITH_LOWER_BOUND_CAVEAT`: same as `CLI_READY`, but residual adapter failures remain after targeted retry and a human reviewer explicitly accepts the known-gap list as sufficient to start the v1 CLI. The executor must not choose this verdict automatically.
- `MEASUREMENT_BLOCKED`: residual unclassified policy worksets remain and policy conclusions would be misleading.

- [ ] **Step 1:** Write the ABC sync report.

  It must contain:

  - mapping artifact path: `data/aat-to-parser-ir-mapping-v1.json`,
  - mapping schema hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`,
  - target parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`,
  - aozora-rs zero-`UNSUPPORTED` result and 25-rule generated mapping result,
  - aozora2html full-run metrics,
  - Task 1 trust buckets from `measurement-trust.summary.json.bucket_counts`,
  - Task 1 failure overlap from `measurement-trust.summary.json.failure_overlap`,
  - Task 1 workset paths from `measurement-trust.summary.json.worksets`,
  - Task 2 sample report link and `policy-samples.summary.json.sample_counts`,
  - one verdict from the list above,
  - if the verdict is `CLI_READY_WITH_LOWER_BOUND_CAVEAT`, a `Reviewer decision:` line that names the accepted known-gap list,
  - explicit "Do not hand-copy the historical 27-rule table" note.

- [ ] **Step 2:** State the next allowed implementation.

  If the verdict is `CLI_READY`, the report should say the next implementation plan may be `crates/ab-aat-to-parser-ir` with default drop-sidecar behavior for critical unsupported constructs and release-smoke enforcement in ABC.

  If the verdict is `CLI_READY_WITH_LOWER_BOUND_CAVEAT`, the report must include the residual work IDs by bucket, state that policy counts are lower bounds, and include a human-owned `Reviewer decision:` line before saying the next implementation plan may start.

  If the verdict is `MEASUREMENT_BLOCKED`, the report should list the exact blocking buckets and the retry/fix needed before any CLI plan starts.

- [ ] **Step 3:** Verify Task 4.

  ```bash
  rg -n "CLI_READY|CLI_READY_WITH_LOWER_BOUND_CAVEAT|MEASUREMENT_BLOCKED" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  rg -n "38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  rg -n "Do not hand-copy" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  if rg -q "CLI_READY_WITH_LOWER_BOUND_CAVEAT" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md; then rg -n "Reviewer decision:" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md; fi
  ```

- [ ] **Step 4:** Commit Task 4.

  ```bash
  git add docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
  git commit -m "docs: publish post-measurement ABC sync"
  ```

---

## Task 5: Keep Existing Gates Green

**Files:**
- Read: `flake.nix`
- Read: `data/generated-feature-taxonomy.md`
- Read: `data/aat-to-parser-ir-mapping-v1.json`
- Read: `tests/aat-parser-ir-mapping-smoke.sh`
- Read: `tests/aozora2html-aat-full-smoke.sh`

- [ ] **Step 1:** Run the cheap measurement/gate smoke tests.

  ```bash
  bash tests/aat-parser-ir-mapping-smoke.sh
  bash tests/aozora2html-aat-full-smoke.sh
  bash tests/aozora2html-measurement-audit-smoke.sh
  bash tests/aozora2html-policy-samples-smoke.sh
  ```

- [ ] **Step 2:** Confirm and run the taxonomy drift gate.

  This gate protects `data/generated-feature-taxonomy.md` only. It does not validate the new audit scripts or reports; those are protected by the smoke tests in Step 1 and `cargo fmt` in Step 3.

  ```bash
  system="$(nix eval --impure --raw --expr builtins.currentSystem)"
  nix flake show --json | jq -e --arg system "$system" '.checks[$system]["taxonomy-drift"]'
  nix build ".#checks.$system.taxonomy-drift"
  ```

- [ ] **Step 3:** Run formatting for changed files.

  ```bash
  cargo fmt --all -- --check
  ```

- [ ] **Step 4:** Confirm no generated run artifacts are staged.

  ```bash
  git status --short
  git diff --stat
  ```

- [ ] **Step 5:** If Step 4 shows unexpected source changes, stop and inspect them before committing. This task has no required commit when the gate commands are clean.

---

## Follow-Up Plan Trigger

Create a separate implementation plan for `crates/ab-aat-to-parser-ir` only after Task 4 records `CLI_READY` or `CLI_READY_WITH_LOWER_BOUND_CAVEAT`.

That follow-up plan should implement:

- Rust crate `ab-aat-to-parser-ir`,
- CLI command that consumes AAT JSON plus `data/aat-to-parser-ir-mapping-v1.json`,
- parser-IR output plus aggregated divergence sidecar,
- default drop-sidecar behavior for unsupported constructs,
- release-smoke integration on the ABC side, not a user-set `--strict` flag,
- tests that assert `style -> emphasis`, `ruby.direction` direct projection, `windows-31j-lossy -> Shift_JIS + AMBIGUITY`, and warigaki/kunten critical divergence behavior.

Do not start manifest identity hardening, ABC compatibility registry hardening, or AAT v2 vocabulary work before that follow-up plan exists.

---

## Plan Document Self-Review

- [x] This plan requires measurement and gates before production CLI work.
- [x] This plan names concrete files and commands for each task.
- [x] This plan preserves the AAT JSON adapter boundary decision.
- [x] This plan forbids new AAT vocabulary and parser-IR schema changes.
- [x] This plan forbids committing generated corpus artifacts.
- [x] This plan has an explicit ABC sync point.
- [x] This plan has an explicit go/no-go gate for the owned mapping CLI.
