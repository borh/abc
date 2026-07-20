# Parser Study Census and Diagnostic Scoring Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce the closed all-axis completeness census and finish the bounded
S2 diagnostics axis from authenticated four-case captures, then publish the
first version-2 study result without changing the frozen study. The available
structured comparison is specifically `ab-aozora` against its upstream
`aozora` lineage; the remaining executable third-party lanes disclose
non-comparability rather than implying a five-parser diagnostic comparison.

**Architecture:** The existing `ab-parser-study-report` crate remains the single
pure owner of the 108-row matrix, evidence-index authentication, census, and
report projection. The existing Python conformance projector is deepened into
one reusable envelope-projection module; S2 adds its own pure aggregate scorer
beside that projector without changing conformance scoring semantics. A
separate bounded capture tool only executes the frozen lanes and records raw
stdout/stderr. Captures live under a
caller-chosen bundle root, while committed axis records and indexes contain only
logical content identity and relative locators.

**Tech Stack:** Rust 2024 (`serde`, `serde_json`, `jsonschema`, existing
`ab-parser-study-report` crate), Python 3.13 standard library + pytest, JSON
Schema 2020-12, Nix flakes, shell only for the bounded historical-revision
capture command.

## Global Constraints

- Execute development on `main` in the sole active session; commit after each
  reviewable task and push only after the final gate.
- Follow
  `abc/docs/superpowers/specs/2026-07-20-parser-study-completion-design.md`
  exactly for this slice. S1, S3, S4, final publication closure, production
  migration, and third-party retirement are separate plans.
- Do not mutate
  `ab-validator/docs/studies/aozora-parser-comparison-preregistration.{json,md,sha256}`
  or either frozen fixture. Version result/evidence contracts under new schema
  identities.
- A result may be `measured`, `non_comparable`, `unavailable`, or `failed`.
  Never impute a count, shrink a denominator, or translate an absent capture
  into `non_comparable`.
- Capture debt is derived only from absent required inputs whose role is
  `third_party_capture`; no reason string or hand-authored summary may clear it.
- Raw parser bytes are written before projection and stay under the supplied
  `--bundle-root`. Commit only compact axis records, their closed index, and the
  generated publication.
- Runtime paths and hostnames are not evidence identity. Every consumed raw blob
  is authenticated by `sha256:<lowercase hex>`, byte count, and media type, and
  is streaming re-hashed when read.
- The frozen third-party lanes are built from their historical revisions. Do not
  re-add retired `aozora` sources or adapters to the active `main` flake.
- Native and adapter-normalized lanes remain distinct. Adapter-derived fields
  are never credited to a native lane.
- `aozora-parser.js` remains the preregistered build failure. It has one native
  row per axis but no executable diagnostic-capture requirement.
- The four-case diagnostics denominator is exactly four in manifest order.
  Structured fields are scored only when a documented channel emits code,
  severity, and UTF-8 span. Parser wording and regexes are never semantic
  authority.
- Run full-corpus or historical-parser realization on a runtime with the
  declared build capabilities when the local machine cannot realize the pinned
  Nix outputs. The runtime's name is not evidence identity. The four-case
  scoring and all pure checks remain locally reproducible.
- All tracked Python must pass `just python-quality`; Rust must pass the
  `cargo-check`, `cargo-clippy`, `cargo-fmt`, and relevant test Nix checks.
- Comments in active source must pass `scripts/comment-hygiene-check.sh` and may
  not cite this plan.
- Run bare `cargo`, `python`, and `nix build .#...` commands from
  `ab-validator/`. Run commands that name `./ab-validator`, `just`, `scripts/`,
  or repository paths from the monorepo root. Run every `git` command from the
  monorepo root.

---

## File Structure

- `ab-validator/schemas/parser-study-axis-policy.schema.json` — closed metric and
  required-input vocabulary for all nine frozen axes.
- `ab-validator/data/parser-study-axis-policy-v1.json` — the small policy value
  from which the census derives required inputs and metric membership.
- `ab-validator/schemas/parser-study-axis-evidence.schema.json` — one immutable
  candidate/mode/axis record with typed metric outcomes and authenticated inputs.
- `ab-validator/schemas/parser-study-evidence-index.schema.json` — closed list of
  logical axis-record identities and relative locators.
- `ab-validator/schemas/parser-comparison-result-v2.schema.json` — version-2
  publication rows with typed metrics; the v1 schema stays untouched.
- `ab-validator/crates/ab-parser-study-report/src/evidence.rs` — Rust contract
  types and content authentication for policies, records, and indexes.
- `ab-validator/crates/ab-parser-study-report/src/census.rs` — pure 108-row
  required-input census and mechanically derived capture debt.
- `ab-validator/crates/ab-parser-study-report/src/lib.rs` — version-2 result IR
  and deterministic axis-summary reduction.
- `ab-validator/crates/ab-parser-study-report/src/generate.rs` — pure projection
  of legacy manifests plus authenticated axis evidence into v2 reports.
- `ab-validator/reports/parser-conformance/diagnostics_scoring.py` — single pure
  envelope-projection authority shared by conformance and S2, plus the S2-only
  case/aggregate scoring functions.
- `ab-validator/reports/parser-conformance/run-aozora-notation-spec.py` — imports
  the shared scorer without changing existing conformance outcomes.
- `ab-validator/reports/parser-study/diagnostic_capture.py` — bounded executor;
  writes raw bytes and a closed capture manifest under `--bundle-root`.
- `ab-validator/reports/parser-study/diagnostic_axis.py` — authenticates a
  capture, invokes the shared scorer, and emits axis records plus an index.
- `ab-validator/reports/parser-study/tests/` — capture, derivation, and
  cross-language integration tests.
- `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/`
  — committed compact S2 axis records, policy, index, and census; never raw
  parser output.
- `ab-validator/reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/`
  — regenerated v2 machine result and narrative report.
- `ab-validator/flake.nix` — focused Python/Nix checks and a bounded capture app
  that receives already-realized historical programs through explicit arguments.

### Contract vocabulary used by every task

The JSON and Rust spellings are fixed here so later tasks cannot invent near
duplicates:

```rust
#[serde(rename_all = "snake_case")]
enum InputRole { SourceMarkup, IndependentReference, ThirdPartyCapture }

#[serde(tag = "state", rename_all = "snake_case")]
enum RequiredInputState {
    Present { content_ref: ContentRef, locator: String },
    Absent { reason: String },
}

struct ContentRef {
    sha256: String,
    bytes: u64,
    media_type: String,
}

#[serde(rename_all = "snake_case")]
enum MetricDisposition { Measured, NonComparable, Unavailable, Failed }

#[serde(tag = "kind", rename_all = "snake_case")]
enum MetricValue {
    Count { value: u64 },
    Ratio { numerator: u64, denominator: u64 },
    Bytes { value: u64 },
    Seconds { value: f64 },
    Boolean { value: bool },
    Text { value: String },
    Interval { lower: f64, upper: Option<f64>, unit: String },
}

struct MetricObservation {
    metric: String,
    disposition: MetricDisposition,
    value: Option<MetricValue>,
    reason: Option<String>,
}
```

The summary reducer is also fixed:

1. any `unavailable` metric -> row `unavailable`;
2. otherwise any `failed` metric -> row `failed`;
3. otherwise at least one `measured` and all remaining metrics
   `non_comparable` -> row `measured`;
4. all metrics `non_comparable` -> row `non_comparable`.

Measured metrics require `value` and forbid `reason`; every other disposition
forbids `value` and requires a non-empty `reason`.

---

### Task 1: Add the versioned study policy, evidence, index, and result contracts

**Files:**

- Create: `ab-validator/schemas/parser-study-axis-policy.schema.json`
- Create: `ab-validator/schemas/parser-study-axis-evidence.schema.json`
- Create: `ab-validator/schemas/parser-study-evidence-index.schema.json`
- Create: `ab-validator/schemas/parser-comparison-result-v2.schema.json`
- Create: `ab-validator/data/parser-study-axis-policy-v1.json`
- Modify: `ab-validator/tests/parser-comparison-preregistration-smoke.sh`

**Interfaces:**

- Consumes: the frozen candidates, modes, axes, fixture hashes, and denominators
  in the preregistration.
- Produces: schema IDs ending in `/parser-study-axis-policy-v1`,
  `/parser-study-axis-evidence-v1`, `/parser-study-evidence-index-v1`, and
  `/parser-comparison-result-v2`; policy ID
  `aozora-parser-neutral-comparison-axis-policy-v1`.

- [ ] **Step 1: Extend the preregistration smoke with failing contract tests.**

Add schema loading and these assertions inside the existing Python block:

```python
policy = json.load(open(repo / "data/parser-study-axis-policy-v1.json", encoding="utf-8"))
policy_schema = json.load(open(repo / "schemas/parser-study-axis-policy.schema.json", encoding="utf-8"))
Draft202012Validator.check_schema(policy_schema)
Draft202012Validator(policy_schema).validate(policy)
assert [axis["id"] for axis in policy["axes"]] == [axis["id"] for axis in contract["axes"]]
diagnostics = next(axis for axis in policy["axes"] if axis["id"] == "diagnostics")
assert diagnostics["metrics"] == [
    "diagnostic_presence",
    "stable_code",
    "severity",
    "relevant_span",
    "false_positive",
    "false_negative",
]
assert diagnostics["required_inputs"] == [
    {"role": "source_markup", "artifact": "diagnostic_fixture"},
    {"role": "third_party_capture", "artifact": "diagnostic_fixture_capture"},
]
for name in (
    "parser-study-axis-evidence.schema.json",
    "parser-study-evidence-index.schema.json",
    "parser-comparison-result-v2.schema.json",
):
    Draft202012Validator.check_schema(json.load(open(repo / "schemas" / name)))
```

- [ ] **Step 2: Run the smoke and verify RED.**

Run from `ab-validator/`:

```sh
bash tests/parser-comparison-preregistration-smoke.sh
```

Expected: failure opening `data/parser-study-axis-policy-v1.json`.

- [ ] **Step 3: Create the closed policy.**

The policy must encode `axes` as an array in preregistered order; each array
member contains `id`, `metrics`, and `required_inputs`. Use these exact metric
lists:

| Axis | Metrics, in order |
| --- | --- |
| `construct_coverage` | `type_coverage`, `occurrence_coverage`, `work_prevalence` |
| `fidelity` | `visible_text_byte_agreement`, `structure_agreement`, `ruby_exact`, `gaiji_exact`, `note_exact` |
| `robustness` | `corpus_parse_completion`, `malformed_fixture_outcomes` |
| `diagnostics` | `diagnostic_presence`, `stable_code`, `severity`, `relevant_span`, `false_positive`, `false_negative` |
| `spans` | `exact_spans`, `overlapping_spans`, `covered_source_bytes`, `invalid_spans` |
| `performance` | `wall_time`, `peak_rss`, `throughput`, `timeouts`, `median`, `p95`, `bootstrap_interval` |
| `maintenance` | `last_release_or_commit`, `bus_factor`, `issue_release_cadence` |
| `packaging` | `reproducible_build`, `locked_dependencies`, `runtime_requirements`, `artifact_size` |
| `license` | `project_license`, `bundled_dependency_licenses`, `redistribution_constraints`, `unknown_or_conflict` |

For each axis, encode the required roles from the design. Diagnostics uses the
two records shown in Step 1. Construct coverage and fidelity require
`source_markup` plus `independent_reference`; spans requires `source_markup` and
`independent_reference`; robustness requires its corpus/fixture
`source_markup` and `third_party_capture`; performance requires
`source_markup` and `third_party_capture`; maintenance, packaging, and license
require `independent_reference`. The schema uses `additionalProperties: false`
at every object and a `prefixItems`/`items: false` tuple for the nine-axis order.

- [ ] **Step 4: Create the evidence and index schemas.**

Pin these invariants in JSON Schema:

- candidate/mode enums and native-only candidates match v1;
- exactly one record key `(candidate, axis, measurement_mode)`;
- `required_inputs` is non-empty, unique by `(role, artifact)`, and each input
  has exactly one `present` or `absent` state;
- present inputs carry `sha256`, `bytes`, `media_type`, and a safe relative
  locator; absent inputs carry one non-empty reason;
- metric IDs are unique; disposition/value/reason constraints match the shared
  vocabulary above;
- the index contains unique `record_ref`/locator pairs and no absolute or `..`
  locators.

- [ ] **Step 5: Create the v2 result schema.**

Keep the v1 candidate/mode/provenance rules, set `schema_version` to `2`, keep
exactly 108 rows, replace the scalar status/count/missingness fields with
`summary_disposition` and a non-empty unique `metrics` array, and retain
`caveats` and `provenance`. Do not modify the v1 schema.

- [ ] **Step 6: Run the contract smoke and schema drift checks.**

```sh
bash tests/parser-comparison-preregistration-smoke.sh
nix build .#checks.x86_64-linux.abc-schema-contract-drift
```

Expected: both pass.

- [ ] **Step 7: Commit.**

```sh
git add ab-validator/data/parser-study-axis-policy-v1.json \
  ab-validator/schemas/parser-study-axis-policy.schema.json \
  ab-validator/schemas/parser-study-axis-evidence.schema.json \
  ab-validator/schemas/parser-study-evidence-index.schema.json \
  ab-validator/schemas/parser-comparison-result-v2.schema.json \
  ab-validator/tests/parser-comparison-preregistration-smoke.sh
git commit -m "feat(parser-study): define v2 axis evidence contracts"
```

---

### Task 2: Implement the pure evidence loader and all-axis census

**Files:**

- Create: `ab-validator/crates/ab-parser-study-report/src/evidence.rs`
- Create: `ab-validator/crates/ab-parser-study-report/src/census.rs`
- Modify: `ab-validator/crates/ab-parser-study-report/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-study-report/tests/evidence_contract.rs`
- Create: `ab-validator/crates/ab-parser-study-report/tests/census.rs`

**Interfaces:**

- Produces:
  `load_index(root: &Path, index_json: &str) -> Result<AxisEvidenceSet, EvidenceError>`,
  `derive_census(preregistration_json: &str, policy_json: &str,
  evidence: &AxisEvidenceSet) -> Result<CompletenessCensus, CensusError>`, and
  `CompletenessCensus::capture_debt() -> &[RequiredInputKey]`.
- `load_index` streams and re-hashes each relative member; it never trusts file
  metadata or a hash stored inside the member alone.

- [ ] **Step 1: Write failing evidence-loader tests.**

```rust
#[test]
fn index_rehashes_members_and_rejects_missing_extra_and_escaping_paths() {
    let fixture = EvidenceFixture::one_record();
    assert!(load_index(fixture.root(), fixture.index_json()).is_ok());
    fixture.tamper_record_bytes();
    assert_matches!(load_index(fixture.root(), fixture.index_json()),
                    Err(EvidenceError::ContentMismatch { .. }));
    assert_matches!(load_index(fixture.root(), &fixture.index_with("../escape.json")),
                    Err(EvidenceError::UnsafeLocator(_)));
}

#[test]
fn duplicate_candidate_axis_mode_is_rejected_even_with_distinct_files() {
    let fixture = EvidenceFixture::duplicate_record_identity();
    assert_matches!(load_index(fixture.root(), fixture.index_json()),
                    Err(EvidenceError::DuplicateRecordIdentity { .. }));
}
```

- [ ] **Step 2: Write failing census tests.**

```rust
#[test]
fn census_is_the_exact_frozen_108_row_matrix() {
    let census = derive_census(PREREGISTRATION, POLICY, &AxisEvidenceSet::empty()).unwrap();
    assert_eq!(census.rows().len(), 108);
    assert_eq!(census.rows()[0].key().candidate, Candidate::Aozora);
    assert_eq!(census.rows()[0].key().axis, Axis::ConstructCoverage);
    assert_eq!(census.rows()[0].key().mode, MeasurementMode::Native);
}

#[test]
fn diagnostic_and_malformed_robustness_captures_are_mechanical_debt() {
    let census = derive_census(PREREGISTRATION, POLICY, &AxisEvidenceSet::empty()).unwrap();
    assert!(census.capture_debt().iter().any(|key|
        key.axis == Axis::Diagnostics && key.artifact == "diagnostic_fixture_capture"));
    assert!(census.capture_debt().iter().any(|key|
        key.axis == Axis::Robustness && key.artifact == "malformed_fixture_capture"));
}

#[test]
fn changing_an_absence_reason_cannot_clear_capture_debt() {
    let first = absent_capture("not run");
    let second = absent_capture("independent authority unavailable");
    assert_eq!(debt_keys(first), debt_keys(second));
}
```

- [ ] **Step 3: Run the tests and verify RED.**

```sh
cargo test -p ab-parser-study-report --test evidence_contract --test census
```

Expected: compile failure because `evidence` and `census` modules do not exist.

- [ ] **Step 4: Implement the evidence types and streaming loader.**

Use `File::open` plus `std::io::Read` in 64 KiB chunks, `sha2::Sha256`, and a
component-wise relative-path check rejecting root/prefix/parent components and
symlink escapes after canonicalization. Add `sha2.workspace = true` to the crate
dependencies. Validate the record's own content identity against the index
entry, then enforce the unique record key.

- [ ] **Step 5: Implement census derivation.**

Derive candidates/modes from the preregistration disposition rather than a
separate hand-maintained list. Join each row to the axis policy and evidence set.
For executable lanes, instantiate every `third_party_capture` role; for the
excluded `aozora-parser.js`, replace capture roles with its authenticated
build-failure evidence. Preserve `source_markup` and `independent_reference`
absences but exclude them from `capture_debt()`.

- [ ] **Step 6: Run focused tests and clippy.**

```sh
cargo test -p ab-parser-study-report --test evidence_contract --test census
cargo clippy -p ab-parser-study-report --all-targets -- -D warnings
```

Expected: pass.

- [ ] **Step 7: Commit.**

```sh
git add ab-validator/crates/ab-parser-study-report
git commit -m "feat(parser-study): derive closed completeness census"
```

---

### Task 3: Extract the shared diagnostic projector and add the S2 scorer

**Files:**

- Create: `ab-validator/reports/parser-conformance/diagnostics_scoring.py`
- Modify: `ab-validator/reports/parser-conformance/run-aozora-notation-spec.py`
- Modify: `ab-validator/reports/parser-conformance/tests/test_diagnostics_scoring.py`
- Create: `ab-validator/reports/parser-study/tests/test_diagnostic_scoring.py`

**Interfaces:**

- Produces:

```text
project_schema3_envelope(payload: bytes) -> list[ActualDiagnostic]
project_inspect_v2_envelope(payload: bytes) -> list[ActualDiagnostic]
score_case(expected: ExpectedDiagnostic,
           actual: list[ActualDiagnostic]) -> DiagnosticCaseScore
aggregate_cases(scores: list[DiagnosticCaseScore]) -> DiagnosticMetricCounts
```

`ActualDiagnostic` contains `code`, `severity`, and half-open UTF-8
`span_start`/`span_end`. A diagnostic is relevant only when its span overlaps the
fixture's expected span. For one fixture case:

- presence = 1 when at least one relevant diagnostic exists;
- stable_code = 1 when a relevant diagnostic has a non-empty code;
- severity = 1 when a relevant diagnostic's severity is in the fixture's
  allowed set (`warning_or_error` expands to `{warning,error}`);
- relevant_span = 1 when a relevant diagnostic exactly matches the expected
  span;
- false_negative = 1 only when no relevant diagnostic exists;
- false_positive = the count of emitted diagnostics that do not overlap the
  expected span.

This objective matching rule does not invent a cross-parser code vocabulary.
The conformance runner reuses only the envelope projectors and retains its
existing exact-list comparison. It must not call `score_case` or
`aggregate_cases`; S2's overlap relevance rule therefore cannot change any
conformance outcome.

- [ ] **Step 1: Characterize the current conformance behavior before extraction.**

Add a test that feeds schema-v3 bytes with one exact diagnostic through the
current `run_diagnostics` path and pins the returned `code`, `severity`, and
span. Add a second assertion that malformed envelopes still return
`unsupported diagnostics envelope`. Pin the current exact-list comparison with
a near-overlap fixture that S2's `score_case` would consider relevant but the
conformance runner must still reject as unequal; this is the regression guard
that keeps S2 relevance semantics out of conformance.

- [ ] **Step 2: Add failing S2 scorer tests.**

```python
def test_score_case_counts_exact_relevant_and_extra_diagnostic():
    expected = ExpectedDiagnostic("error", 9, 18)
    actual = [
        ActualDiagnostic("unclosed-bracket", "error", 9, 18),
        ActualDiagnostic("unrelated", "warning", 0, 3),
    ]
    assert score_case(expected, actual) == DiagnosticCaseScore(
        presence=1, stable_code=1, severity=1, relevant_span=1,
        false_positive=1, false_negative=0,
    )

def test_score_case_discloses_false_negative_without_imputing_fields():
    expected = ExpectedDiagnostic("warning_or_error", 18, 21)
    assert score_case(expected, []) == DiagnosticCaseScore(
        presence=0, stable_code=0, severity=0, relevant_span=0,
        false_positive=0, false_negative=1,
    )
```

- [ ] **Step 3: Run both suites and verify RED.**

```sh
python -m pytest reports/parser-conformance/tests/test_diagnostics_scoring.py \
  reports/parser-study/tests/test_diagnostic_scoring.py -q
```

Expected: import failure for `diagnostics_scoring`/missing `score_case`.

- [ ] **Step 4: Extract projection unchanged, then add pure scoring.**

Move only envelope parsing/projection from the hyphenated runner into the new
module; keep subprocess execution in the runner. Have the runner import the
projector and convert its dataclasses back to its existing dictionaries so all
old rows remain byte-identical. Then implement the pure case/aggregate functions
with integer addition in fixture order.

- [ ] **Step 5: Run regression and quality checks.**

```sh
python -m pytest reports/parser-conformance/tests \
  reports/parser-study/tests/test_diagnostic_scoring.py -q
ruff format --check reports/parser-conformance reports/parser-study
ruff check reports/parser-conformance reports/parser-study
mypy reports/parser-conformance/diagnostics_scoring.py
```

Expected: all pass; no existing conformance summary changes.

- [ ] **Step 6: Commit.**

```sh
git add ab-validator/reports/parser-conformance \
  ab-validator/reports/parser-study/tests/test_diagnostic_scoring.py
git commit -m "refactor(parser-study): share diagnostic scoring authority"
```

---

### Task 4: Build the bounded raw diagnostic capture protocol

**Files:**

- Create: `ab-validator/reports/parser-study/diagnostic_capture.py`
- Create: `ab-validator/reports/parser-study/tests/test_diagnostic_capture.py`
- Create: `ab-validator/data/parser-study-diagnostic-lanes-v1.json`
- Create: `ab-validator/schemas/parser-study-diagnostic-lanes.schema.json`
- Modify: `ab-validator/flake.nix`

**Interfaces:**

- CLI:

```text
diagnostic_capture.py capture
  --preregistration PATH --fixture PATH --lane-policy PATH
  --programs-json PATH --bundle-root PATH --manifest-out PATH
diagnostic_capture.py verify
  --fixture PATH --lane-policy PATH --bundle-root PATH --manifest PATH
```

- `programs-json` is runtime-only and maps a required basename to an absolute
  executable path plus fixed environment. It is not copied into evidence.
- The lane policy contains exactly these executable lanes: five included
  third-party candidates × two modes plus `ab-aozora` native. It binds program
  basename and argv suffix:

```text
aozora/native                  aozora                 inspect diagnostics -
aozora/adapter_normalized      aozora-adapter         --mode aat
aozora2/native                 aozora2                html --encoding utf-8
aozora2/adapter_normalized     aozora2-adapter        --mode aat
aozora-rs/native               aozora-rs-native       (none)
aozora-rs/adapter_normalized   aozora-rs-adapter      --mode aat
aozora2html/native             aozora2html-adapter    --mode html
aozora2html/adapter_normalized aozora2html-adapter    --mode aat
aozora-epub3/native            aozora-epub3-adapter   --mode html
aozora-epub3/adapter_normalized aozora-epub3-adapter  --mode aat
ab-aozora/native               ab-aozora              --mode diagnostics
```

Only `aozora/native` (`inspect_v2`) and `ab-aozora/native` (`schema3`) declare a
structured diagnostic projector. Every other lane declares `projector: none`;
its raw bytes are still captured, but its diagnostic fields become
`non_comparable` unless a later governed policy version identifies a documented
structured channel.

The derivation must nevertheless probe `projector:none` stdout with both known
envelope parsers. If either accepts it, the lane is `unavailable` because the
closed lane policy is stale, rather than silently suppressing structured
evidence.

The `aozora2html/native` and `aozora-epub3/native` rows invoke their adapters in
`--mode html` only to preserve raw native output for this diagnostic-specific
capture. S2 never credits those adapter processes with diagnostic capability.
No S1/S3 or other measured-axis plan may reuse these native mappings without a
separate test proving that `--mode html` is a byte-transparent passthrough of
the upstream parser output.

- [ ] **Step 1: Write failing capture tests.**

Use tiny executable scripts and assert:

```python
def test_capture_writes_raw_bytes_before_any_projection(tmp_path):
    result = run_capture(tmp_path, stdout=b'{"not":"normalized"}\n', stderr=b'raw warning\n')
    member = result.manifest["members"][0]
    assert (result.root / member["stdout"]["locator"]).read_bytes() == b'{"not":"normalized"}\n'
    assert (result.root / member["stderr"]["locator"]).read_bytes() == b"raw warning\n"

def test_verify_rejects_tampering_missing_cases_and_wrong_program_basename(tmp_path):
    result = run_capture(tmp_path)
    tamper(result.first_stdout)
    with pytest.raises(ValueError, match="content mismatch"):
        verify_capture(
            fixture_path=result.fixture_path,
            lane_policy_path=result.lane_policy_path,
            bundle_root=result.root,
            manifest_path=result.manifest_path,
        )
```

Also assert the exact 44 third-party/custom `(lane, case)` members and manifest
order, timeout status, non-zero exit status, and duplicate rejection.

- [ ] **Step 2: Run tests and verify RED.**

```sh
python -m pytest reports/parser-study/tests/test_diagnostic_capture.py -q
```

Expected: missing module.

- [ ] **Step 3: Implement capture and verification.**

Invoke each closed lane exactly this way (with `environment` built solely from
the lane's authenticated program record):

```python
completed = subprocess.run(
    [program, *lane["argv"]],
    input=case["source"].encode("utf-8"),
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    timeout=300,
    check=False,
    env=environment,
)
```

Write stdout/stderr atomically to
`raw/<candidate>/<mode>/<case>/<stream>.bin`, then hash the bytes. The manifest
records fixture hash, lane-policy hash, parser/adapter revisions, program byte
hash, argv suffix, exit/timeout status, and content refs. `verify` enforces the
closed lane/case product and streaming re-hashes every member.

- [ ] **Step 4: Add Nix checks, not active historical dependencies.**

Add `reports/parser-study/tests` to `reportsPytestCheck`. Add a focused
`parser-study-diagnostic-capture-python-tests` check. The flake app packages the
Python tool only; it accepts `--programs-json` and does not reintroduce the
retired `aozora` input or adapter into current outputs.

- [ ] **Step 5: Run focused checks.**

```sh
python -m pytest reports/parser-study/tests/test_diagnostic_capture.py -q
nix build .#checks.x86_64-linux.parser-study-diagnostic-capture-python-tests
nix build .#checks.x86_64-linux.reports-pytest
```

Expected: pass.

- [ ] **Step 6: Commit.**

```sh
git add ab-validator/data/parser-study-diagnostic-lanes-v1.json \
  ab-validator/schemas/parser-study-diagnostic-lanes.schema.json \
  ab-validator/reports/parser-study/diagnostic_capture.py \
  ab-validator/reports/parser-study/tests/test_diagnostic_capture.py \
  ab-validator/flake.nix
git commit -m "feat(parser-study): capture bounded raw diagnostics"
```

---

### Task 5: Derive S2 axis evidence and the closed evidence index

**Files:**

- Create: `ab-validator/reports/parser-study/diagnostic_axis.py`
- Create: `ab-validator/reports/parser-study/tests/test_diagnostic_axis.py`
- Create during authoritative run:
  `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/diagnostics/*.json`
- Create during authoritative run:
  `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/evidence-index.json`

**Interfaces:**

- CLI:

```text
diagnostic_axis.py derive
  --preregistration PATH --fixture PATH --lane-policy PATH
  --bundle-root PATH --capture-manifest PATH --output-root PATH
diagnostic_axis.py verify
  --bundle-root PATH --evidence-root PATH --index PATH
```

- Produces one axis-evidence record per executable diagnostic lane (11 records)
  and one closed index. Each record carries six metric observations and the
  two required inputs (`source_markup`, `third_party_capture`).

- [ ] **Step 1: Write failing derivation tests.**

Pin this diagnostics-specific precedence:

1. missing, unauthenticated, or hash-mismatched capture -> `unavailable`;
2. `projector:none` with no known structured envelope -> `non_comparable`,
   regardless of exit code or timeout status;
3. `projector:none` whose stdout parses as `schema3` or `inspect_v2` ->
   `unavailable` because the lane policy is stale;
4. a declared structured projector with a usable envelope -> `measured`,
   regardless of exit code; and
5. a declared structured projector with any case lacking a usable envelope ->
   `failed`, whether caused by crash, timeout, malformed output, or absence.

Process status remains a captured witness and may feed the robustness axis
later, but never decides a diagnostics disposition by itself. Cover these
states explicitly:

```python
def test_structured_lane_emits_six_measured_metrics_with_denominator_four():
    record = derive_lane(schema3_capture(exact_for_all_four_cases()))
    assert [m["metric"] for m in record["metrics"]] == DIAGNOSTIC_METRICS
    assert ratio(record, "diagnostic_presence") == (4, 4)
    assert ratio(record, "false_negative") == (0, 4)

def assert_noncomparable(record):
    assert all(m["disposition"] == "non_comparable" for m in record["metrics"])
    assert all(m["value"] is None for m in record["metrics"])


def test_projector_none_nonzero_exit_is_noncomparable_not_failed():
    assert_noncomparable(derive_lane(raw_capture(projector="none", returncode=2)))


def test_projector_none_timeout_is_noncomparable_not_failed():
    assert_noncomparable(derive_lane(raw_capture(projector="none", timed_out=True)))


def test_missing_or_hash_mismatched_capture_is_unavailable():
    for capture in (missing_capture(), tampered_capture()):
        record = derive_lane(capture)
        assert all(metric["disposition"] == "unavailable" for metric in record["metrics"])
        assert all(metric["value"] is None for metric in record["metrics"])


def test_structured_nonzero_exit_with_usable_envelope_is_measured():
    record = derive_lane(schema3_capture(exact_for_all_four_cases(), returncode=2))
    assert all(metric["disposition"] == "measured" for metric in record["metrics"])


def test_structured_lane_without_usable_envelope_is_failed():
    record = derive_lane(schema3_capture(malformed_case=2, returncode=2))
    assert all(metric["disposition"] == "failed" for metric in record["metrics"])
    assert all(metric["value"] is None for metric in record["metrics"])


def test_known_envelope_on_projector_none_rejects_stale_policy():
    record = derive_lane(raw_capture(projector="none", stdout=schema3_bytes()))
    assert all(metric["disposition"] == "unavailable" for metric in record["metrics"])
    assert all("stale lane policy" in metric["reason"] for metric in record["metrics"])
```

Add an index test that rejects an 11th-lane omission, an extra record, a changed
fixture hash, and a record whose `(candidate, mode)` does not match its capture.

- [ ] **Step 2: Run tests and verify RED.**

```sh
python -m pytest reports/parser-study/tests/test_diagnostic_axis.py -q
```

Expected: missing module.

- [ ] **Step 3: Implement pure derivation.**

Authenticate the complete capture first, then apply the precedence fixed in
Step 1. For declared structured lanes, call only their selected projector and
`score_case`/`aggregate_cases` from `diagnostics_scoring.py`; parse before
consulting the recorded return code. For `projector:none`, probe both known
projectors only as a stale-policy guard and never opportunistically score the
lane. Convert the six totals to ratios over four cases; for false positives use
`Ratio(total_extra_diagnostics, 4)` and for false negatives use
`Ratio(missed_cases, 4)`. Preserve per-case process status and diagnostic
witnesses in the axis record. Compute record refs from canonical compact JSON
`json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))`
plus a trailing newline, and write atomically.

- [ ] **Step 4: Run bounded fake-capture integration.**

```sh
python -m pytest reports/parser-study/tests/test_diagnostic_axis.py \
  reports/parser-study/tests/test_diagnostic_capture.py -q
```

Expected: pass with an 11-record closed index.

- [ ] **Step 5: Commit the derivation tool before authoritative execution.**

```sh
git add ab-validator/reports/parser-study/diagnostic_axis.py \
  ab-validator/reports/parser-study/tests/test_diagnostic_axis.py
git commit -m "feat(parser-study): derive diagnostic axis evidence"
```

---

### Task 6: Execute the frozen diagnostic lanes and freeze compact evidence

**Files:**

- Create in a caller-chosen untracked root: raw diagnostic capture bundle
- Create/commit:
  `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/diagnostics/*.json`
- Create/commit:
  `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/evidence-index.json`
- Create/commit:
  `ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/completeness-census.json`

**Interfaces:**

- Historical realization revisions are exact values, not mutable branches:
  `75057f147404ea0077b10c58f009510487c929de` for the frozen `aozora`
  parser/adapter lane, `36bf29005719b58fe744b7c96d3a45e451f6c943`
  for the other third-party adapters, and
  `ac2be926738f919faf44300e2999b3548d724297` for the custom baseline.

- [ ] **Step 1: Prove every required executable can be realized before consuming
  the capture attempt.**

Create three disposable detached worktrees outside the source tree:

```sh
git worktree add --detach /tmp/parser-study-75057f 75057f147404ea0077b10c58f009510487c929de
git worktree add --detach /tmp/parser-study-36bf29 36bf29005719b58fe744b7c96d3a45e451f6c943
git worktree add --detach /tmp/parser-study-ac2be9 ac2be926738f919faf44300e2999b3548d724297
```

Build, without modifying current `main`, the exact package outputs named by each
historical flake. Record `nix path-info` paths and executable SHA-256 values in
the untracked `programs.json`. Expected: all 11 lane mappings resolve. Record an
authenticated build failure only when the exact preregistered build invocation
itself fails. If a pin is merely unreachable or the environment cannot execute
the build, stop as `unavailable`; do not mislabel it as a parser build failure
and do not substitute the current custom parser for `aozora`.

- [ ] **Step 2: Run the real four-case capture.**

```sh
nix run ./ab-validator#parser-study-diagnostic-capture -- capture \
  --preregistration ab-validator/docs/studies/aozora-parser-comparison-preregistration.json \
  --fixture ab-validator/docs/studies/fixtures/parser-comparison-diagnostics-v1.json \
  --lane-policy ab-validator/data/parser-study-diagnostic-lanes-v1.json \
  --programs-json /tmp/parser-study-programs.json \
  --bundle-root /tmp/parser-study-diagnostics-bundle \
  --manifest-out /tmp/parser-study-diagnostics-bundle/capture-manifest.json
```

Expected: 44 terminal lane/case records, with raw stdout and stderr preserved.

- [ ] **Step 3: Verify, derive, and copy only compact evidence into the repo.**

```sh
nix run ./ab-validator#parser-study-diagnostic-capture -- verify \
  --fixture ab-validator/docs/studies/fixtures/parser-comparison-diagnostics-v1.json \
  --lane-policy ab-validator/data/parser-study-diagnostic-lanes-v1.json \
  --bundle-root /tmp/parser-study-diagnostics-bundle \
  --manifest /tmp/parser-study-diagnostics-bundle/capture-manifest.json
python ab-validator/reports/parser-study/diagnostic_axis.py derive \
  --preregistration ab-validator/docs/studies/aozora-parser-comparison-preregistration.json \
  --fixture ab-validator/docs/studies/fixtures/parser-comparison-diagnostics-v1.json \
  --lane-policy ab-validator/data/parser-study-diagnostic-lanes-v1.json \
  --bundle-root /tmp/parser-study-diagnostics-bundle \
  --capture-manifest /tmp/parser-study-diagnostics-bundle/capture-manifest.json \
  --output-root ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07
```

- [ ] **Step 4: Generate the completeness census from the real index.**

Add a small `generate-census` subcommand to the report binary that calls
`derive_census` and writes canonical JSON. Run it against the committed evidence
root. Assert diagnostic capture debt is empty, while malformed-robustness and
later S1/S3/S4 required inputs remain honestly visible.

- [ ] **Step 5: Prove copied-root verification.**

```sh
cp -a /tmp/parser-study-diagnostics-bundle /tmp/parser-study-diagnostics-copy
python ab-validator/reports/parser-study/diagnostic_axis.py verify \
  --bundle-root /tmp/parser-study-diagnostics-copy \
  --evidence-root ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07 \
  --index ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07/evidence-index.json
```

Expected: pass without hostname or original absolute path.

- [ ] **Step 6: Remove disposable worktrees and commit compact evidence.**

```sh
git worktree remove /tmp/parser-study-75057f
git worktree remove /tmp/parser-study-36bf29
git worktree remove /tmp/parser-study-ac2be9
git add ab-validator/reports/parser-study/evidence/aozora-parser-neutral-comparison-2026-07
git commit -m "data(parser-study): freeze diagnostic axis evidence"
```

---

### Task 7: Project authenticated evidence into result schema v2

**Files:**

- Modify: `ab-validator/crates/ab-parser-study-report/src/lib.rs`
- Modify: `ab-validator/crates/ab-parser-study-report/src/generate.rs`
- Modify:
  `ab-validator/crates/ab-parser-study-report/src/bin/generate-parser-study-report.rs`
- Modify: `ab-validator/crates/ab-parser-study-report/tests/report_contract.rs`
- Modify: `ab-validator/crates/ab-parser-study-report/tests/report_generation.rs`
- Regenerate:
  `ab-validator/reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-result.json`
- Regenerate:
  `ab-validator/reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07/comparison-report.md`

**Interfaces:**

- `generate_reports` gains `evidence_root: &Path` and `evidence_index_json: &str`.
- Produces `StudyReportV2`, exactly 108 rows in frozen order.
- V1 remains deserializable for the equivalence test but is no longer the final
  generated publication format.

- [ ] **Step 1: Write failing v2 contract and summary-reducer tests.**

```rust
#[test]
fn summary_reduction_is_total_and_fail_closed() {
    assert_eq!(summarize(&[measured(), non_comparable()]).unwrap(), Measured);
    assert_eq!(summarize(&[non_comparable(), non_comparable()]).unwrap(), NonComparable);
    assert_eq!(summarize(&[measured(), failed()]).unwrap(), Failed);
    assert_eq!(summarize(&[measured(), unavailable()]).unwrap(), Unavailable);
}

#[test]
fn v2_report_requires_the_exact_108_row_matrix_and_axis_metric_sets() {
    let mut document = serde_json::to_value(complete_v2_report()).unwrap();
    assert_eq!(document["rows"].as_array().unwrap().len(), 108);

    document["rows"][0]["metrics"].as_array_mut().unwrap().pop();
    let error = serde_json::from_value::<StudyReportV2>(document).unwrap_err();
    assert!(error.to_string().contains("metric"));
}
```

- [ ] **Step 2: Write the v1-to-v2 robustness equivalence test.**

Parse the committed pre-migration v1 report fixture and compare every robustness
row after projection:

```rust
assert_eq!(v2.candidate(), v1.candidate());
assert_eq!(v2.measurement_mode(), v1.measurement_mode());
assert_eq!(v2.parser_revision(), v1.parser_revision());
assert_eq!(v2.adapter_revision(), v1.adapter_revision());
assert_eq!(v2.corpus_hash(), v1.corpus_hash());
assert_eq!(ratio(v2, "corpus_parse_completion"),
           (v1.numerator().unwrap(), v1.denominator().unwrap()));
assert_eq!(v2.provenance(), v1.provenance());
```

Also assert `malformed_fixture_outcomes` remains `unavailable`; do not present
the corpus arm as the entire robustness axis.

- [ ] **Step 3: Write the failing S2 integration test.**

Generate from the committed evidence index and assert one structured diagnostic
lane has six evidence-derived metric values, one `projector:none` lane has six
`non_comparable` metrics with no values, and no diagnostics row retains the old
generic “No committed diagnostic capture” caveat.

- [ ] **Step 4: Run focused tests and verify RED.**

```sh
cargo test -p ab-parser-study-report --test report_contract --test report_generation
```

Expected: failures because the generator still emits schema version 1 and does
not consume the evidence index.

- [ ] **Step 5: Implement the v2 IR and report projection.**

For diagnostics, install the authenticated S2 record. For robustness, project
the existing exact counts plus the missing fixture metric. For every other axis,
emit the policy's full metric set with its current honest disposition from the
census; never synthesize a scalar. Validate the axis metric set before
constructing each row and apply the fixed summary reducer.

- [ ] **Step 6: Update the binary and narrative.**

The CLI reads the committed evidence index relative to an explicit
`--evidence-root` (defaulting to the study evidence directory). The narrative
renders the metric-level matrix, names non-comparable and unavailable metrics,
retains native/adapted separation, and still prohibits an overall winner. It
states that structured S2 comparison is limited to the custom fork and its
upstream origin; it does not describe S2 as five-parser diagnostic coverage.
Maintenance, packaging, license, and other later-axis absences are labeled
`pending evidence` in prose while retaining their honest machine disposition;
the interim report must not imply those absences are terminal conclusions.

- [ ] **Step 7: Regenerate and run drift/equivalence tests.**

```sh
cargo run --manifest-path ab-validator/Cargo.toml \
  -p ab-parser-study-report --bin generate-parser-study-report -- ab-validator
cargo test -p ab-parser-study-report
```

Expected: v2 output validates, regeneration is byte-identical, robustness
equivalence passes, and diagnostic rows derive from the index.

- [ ] **Step 8: Commit.**

```sh
git add ab-validator/crates/ab-parser-study-report \
  ab-validator/reports/parser-study/reports/aozora-parser-neutral-comparison-2026-07
git commit -m "feat(parser-study): publish diagnostic evidence in result v2"
```

---

### Task 8: Close verification, documentation, and roadmap status

**Files:**

- Modify: `ab-validator/reports/parser-study/README.md`
- Modify:
  `ab-validator/reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/README.md`
- Modify: `abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md`
- Modify: `abc/docs/superpowers/specs/2026-07-20-parser-study-completion-design.md`
- Modify: `ab-validator/flake.nix` only if a final tracked-set correction is
  required by the gates.

**Interfaces:**

- Documents one command for capture verification, one command for evidence/report
  regeneration, and the remaining census debt. It calls the raw artifact root a
  caller-controlled research bundle, not external storage.

- [ ] **Step 1: Add a cross-language end-to-end Nix check.**

The check must run a synthetic 11-lane capture, Python derivation, Rust evidence
loading/report generation, and JSON Schema v2 validation. It must then mutate
one raw byte and prove derivation fails before restoring the fixture. Expose it
as `checks.x86_64-linux.parser-study-diagnostics`.

- [ ] **Step 2: Update documentation and status precisely.**

Record S2 as complete only if all executable lanes are terminal and the
diagnostic capture-debt set is empty. State plainly that only the custom fork
and its upstream origin expose comparable structured diagnostic channels; the
other executable third-party lanes are evidence-backed `non_comparable`, and
`aozora-parser.js` retains its authenticated build failure. State remaining
Track S work as S1, S3, S4, publication closure, production migration, and
retirement. Describe not-yet-run axes as pending evidence and do not call the
whole study complete.

- [ ] **Step 3: Run focused quality and integrity checks.**

```sh
just python-quality
nix build ./ab-validator#checks.x86_64-linux.parser-study-diagnostics
nix build ./ab-validator#checks.x86_64-linux.parser-study-report
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
scripts/comment-hygiene-check.sh
```

Expected: all pass.

- [ ] **Step 4: Prove the non-goals.**

```sh
git diff --exit-code HEAD -- \
  abc/docs/adr/0038-custom-parser-ownership-and-neutral-comparison.md \
  abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/data/aat-parser-ir-compatibility.edn \
  abc/data/parser-release-qualification-predicates.edn
```

Expected: no changes. Confirm no active code contains a new absolute artifact
root or hostname.

- [ ] **Step 5: Run the full repository gate.**

```sh
just validate-migration
```

Expected: exit 0. This proves repository integration and governance only; the
scientific claims are proven by the capture/evidence/report checks above.

- [ ] **Step 6: Commit, inspect, and push.**

```sh
git add ab-validator/flake.nix \
  ab-validator/reports/parser-study/README.md \
  ab-validator/reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/README.md \
  abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md \
  abc/docs/superpowers/specs/2026-07-20-parser-study-completion-design.md
git commit -m "docs(parser-study): close census and diagnostic scoring slice"
git status --short --branch
git push origin main
git status --short --branch
```

Expected final state: clean `main`, `HEAD == origin/main`, S2 capture debt empty,
and all non-S2 census limitations still explicit.

---

## Self-Review Checklist

- [ ] Every frozen candidate/mode/axis row is derived once; no 109th row and no
  missing adapted lane.
- [ ] The policy, evidence index, and result schemas are closed and versioned;
  v1 remains historical.
- [ ] Capture writes raw bytes before projection and verification re-hashes them.
- [ ] Envelope projection has one owner shared with conformance; S2 scoring
  stays out of the conformance path and no second implementation exists in the
  report crate.
- [ ] Diagnostics precedence is pinned end to end: capture authentication
  failure yields `unavailable`; `projector:none` yields `non_comparable` unless
  a known envelope exposes stale policy; a usable structured envelope is scored
  despite non-zero exit; and only a structured lane without a usable envelope
  yields `failed`.
- [ ] All six diagnostic metrics disclose the four-case denominator and retain
  FP/FN witnesses.
- [ ] Capture debt depends on required-input role/state, never prose.
- [ ] Robustness v1→v2 equivalence pins identities, exact counts, disposition,
  and provenance without remeasurement.
- [ ] Historical `aozora` is realized from revision
  `75057f147404ea0077b10c58f009510487c929de`; current
  `ab-aozora` is never substituted for it and retired code is not re-added to
  active `main`.
- [ ] Raw bundle verification works after an ordinary directory copy.
- [ ] No overall score, ranking, admission effect, or Track R change is added.
