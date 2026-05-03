# AAT Oracle Correctness Provenance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make oracle correctness auditable by linking every oracle case to independent, reusable evidence and by reporting oracle review state separately from adapter match results.

**Architecture:** Evidence is top-level shared truth data in `data/aat-oracle-cases.toml`; oracle cases reference evidence by id. Review state is modeled as a succession of immutable review entries, and the current review state is derived from the last entry. `oracle_status` remains only the adapter-vs-oracle match result (`pass`/`fail`); oracle credibility is reported separately as `oracle_review_status` and derived `oracle_evidence_strength`.

**Tech Stack:** Rust 2024 workspace crates, `ab-oracle`, TOML `1.1.2`, JSON Schema 2020-12, existing shell smoke tests, `/db/ab-validator` Cargo target directories.

---

## Design Corrections From Review

- Evidence is top-level `[[evidence]]`, not `[[case.evidence]]`, so references have file-level identity and can be reused across cases.
- There is no input `confidence` field. Evidence strength is derived from linked evidence and reported as data, not maintained as another mutable place.
- `review_status` is derived from the last `[[case.review]]` entry. Review history is an explicit succession of values.
- `disputed` belongs only to review status. It never changes `oracle_status`; `oracle_status` remains `pass` or `fail`.
- Retired cases are skipped by report generation by default.
- Assertion `evidence_ids` are optional overrides. Empty assertion evidence means inherit the case-level `evidence_ids`.
- `curator_note` evidence can support draft cases only; it cannot justify reviewed status.
- Semantic validation is composable: `validate_case_quality(case, evidence_index)` validates one case, and the file-level validator composes over cases.

## File Structure

- `docs/aat-contract.md`: document evidence, review history, inherited evidence, retired-case behavior, and report axis separation.
- `data/aat-oracle-cases.schema.json`: shape validation for top-level evidence, case evidence links, review history, and optional assertion evidence overrides.
- `data/aat-oracle-cases.toml`: top-level reusable evidence records and review entries for current seed cases.
- `crates/ab-oracle/src/data.rs`: typed evidence, review, and evidence-link loading.
- `crates/ab-oracle/src/oracle_quality.rs`: semantic validation for evidence uniqueness, evidence references, independence, and reviewed-case criteria.
- `crates/ab-oracle/src/evaluate.rs`: include derived review status/evidence strength in `CaseEvaluation` while keeping `oracle_status` pass/fail.
- `crates/ab-oracle/src/report.rs`: include review/evidence fields in JSON and Markdown report rows.
- `crates/ab-oracle/src/main.rs`: validate oracle quality before reporting and skip retired cases by default.
- `reports/aat-fidelity/fidelity_explorer.py`: expose filters for review status and evidence strength.

## Data Contract

Top-level reusable evidence:

```toml
[[evidence]]
id = "jis-x-0213-2-13-47"
kind = "reference_table" # reference_table | unicode | curator_note
citation = "JIS X 0213 plane 2 row 13 cell 47"
locator = "2-13-47"
supports = "JIS code 2-13-47 resolves to 撑."
independent = true
```

Case-level links and review succession:

```toml
[[case]]
id = "gaiji.jis.2-13-47"
syntax_row_ids = ["gaiji.jis_code"]
category = "gaiji"
source_utf8 = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて"
evidence_ids = ["jis-x-0213-2-13-47"]
notes = "JIS X 0213 plane 2 row 13 cell 47 should resolve to 撑."

[[case.review]]
status = "draft" # draft | reviewed | disputed | retired
reviewer = "oracle-seed"
reviewed_at = "2026-05-03"
notes = "Seeded from current oracle fixture; needs human source audit before reviewed."

[case.oracle]
visible_text = "耳朶を撑えて"

[[case.oracle.gaiji]]
selector = "blocks.*.content.*"
description = "「てへん＋掌」、第4水準2-13-47"
resolved = "撑"
jis_code = "2-13-47"
unresolved_reason = ""
source = "JIS X 0213"
# evidence_ids not set: inherits case.evidence_ids
```

Assertion-level override when needed:

```toml
[[case.oracle.nodes]]
selector = "**"
kind = "raw"
absent = true
evidence_ids = ["aozora-ruby-gaiji-inline-base"]
```

Rules:

- `case.evidence_ids` is required and non-empty.
- Assertion `evidence_ids` are optional. Empty means inherit `case.evidence_ids`.
- Every referenced evidence id must exist in top-level `[[evidence]]`.
- Evidence ids must be unique across the file.
- Oracle evidence must be independent of adapter output.
- `current_review_status(case)` is the last `[[case.review]]` entry.
- Reviewed cases require at least one non-`curator_note` independent evidence record.
- Retired cases are skipped by report generation in this plan.
- `oracle_evidence_strength` is derived from linked evidence using strongest-wins precedence: `unicode` -> `normative`, `reference_table` -> `reference`, `curator_note` -> `curated`.
- `visible_text` assertions always inherit case-level `evidence_ids`. If visible text needs different evidence from the case-level claim, split the fixture into a separate oracle case.
- Evidence records allow `x-*` extension fields in the schema. Implementations must ignore unknown `x-*` fields.
- `reviewed_at` is a date-shaped string in this plan. The schema rejects obvious malformed shapes; semantic calendar-date validation is outside this implementation.

---

### Task 1: Document Oracle Correctness Contract

**Files:**
- Modify: `docs/aat-contract.md`

- [ ] **Step 1: Add oracle correctness section**

Add this section after `Result Axes`:

```markdown
## Oracle Correctness Provenance

Oracle cases are independent truth-data claims, not adapter consensus. Evidence
for oracle correctness lives in top-level `[[evidence]]` records in
`data/aat-oracle-cases.toml`; cases reference those records through
`evidence_ids`.

Review state is a succession of `[[case.review]]` values. The current review
status is the last review entry. This preserves review history instead of
mutating a single status field without context.

`oracle_status` reports only whether adapter output matches the oracle
assertions. It remains independent of oracle credibility. Reports carry
`oracle_review_status` and `oracle_evidence_strength` as separate fields.

Assertion-level `evidence_ids` are optional overrides. When omitted or empty,
the assertion inherits the case-level `evidence_ids`.

Retired oracle cases are not evaluated in reports by default.

Evidence strength is derived from linked evidence using strongest-wins
precedence: `unicode` evidence reports `normative`, `reference_table` reports
`reference`, and `curator_note` reports `curated`. If more than one evidence
kind is linked, the strongest linked kind wins.

`visible_text` assertions inherit the case-level `evidence_ids`; there is no
separate `visible_text_evidence_ids` field in this contract. If a visible-text
claim needs different evidence from the rest of the case, split it into a
separate oracle case.

Evidence records may contain extension fields whose names start with `x-`.
Consumers must ignore unknown `x-*` fields.

`reviewed_at` is stored as `YYYY-MM-DD` text. This contract validates the
shape; it does not require semantic calendar-date validation.
```

- [ ] **Step 2: Verify docs text exists**

Run:

```bash
rg -n "Oracle Correctness Provenance|oracle_review_status|evidence_ids|Retired oracle cases|strongest-wins|visible_text assertions|x-\\*" docs/aat-contract.md
```

Expected: exit 0 and show the inserted section.

- [ ] **Step 3: Commit**

Run:

```bash
git add docs/aat-contract.md
git commit -m "docs: define oracle correctness provenance"
```

Expected: commit succeeds.

---

### Task 2: Atomically Tighten Oracle Schema and Data

**Files:**
- Modify: `data/aat-oracle-cases.schema.json`
- Modify: `data/aat-oracle-cases.toml`
- Test: `tests/aat-oracle-data-schema-smoke.sh`

This task must be one atomic commit. Do not commit the stricter schema without the matching TOML backfill.

- [ ] **Step 1: Confirm current schema/data pass before tightening**

Run:

```bash
bash tests/aat-oracle-data-schema-smoke.sh
```

Expected: PASS before edits.

- [ ] **Step 2: Add schema fields**

In `data/aat-oracle-cases.schema.json`, change top-level `required` to:

```json
"required": ["aat_version", "evidence", "case"]
```

Add top-level property:

```json
"evidence": {
  "type": "array",
  "minItems": 1,
  "items": { "$ref": "#/$defs/Evidence" }
}
```

Update `Case.required` to include `evidence_ids` and `review`:

```json
"required": ["id", "syntax_row_ids", "category", "source_utf8", "evidence_ids", "review", "oracle"]
```

Add to `Case.properties`:

```json
"evidence_ids": {
  "type": "array",
  "minItems": 1,
  "uniqueItems": true,
  "items": { "type": "string", "minLength": 1 }
},
"review": {
  "type": "array",
  "minItems": 1,
  "items": { "$ref": "#/$defs/ReviewEntry" }
}
```

Add to `$defs`:

```json
"Evidence": {
  "type": "object",
  "required": ["id", "kind", "citation", "supports", "independent"],
  "additionalProperties": false,
  "patternProperties": { "^x-": true },
  "properties": {
    "id": { "type": "string", "minLength": 1 },
    "kind": {
      "type": "string",
      "enum": ["reference_table", "unicode", "curator_note"]
    },
    "citation": { "type": "string", "minLength": 1 },
    "locator": { "type": "string" },
    "url": { "type": "string" },
    "supports": { "type": "string", "minLength": 1 },
    "independent": { "type": "boolean" },
    "notes": { "type": "string" }
  }
},
"ReviewEntry": {
  "type": "object",
  "required": ["status", "reviewer", "reviewed_at"],
  "additionalProperties": false,
  "properties": {
    "status": {
      "type": "string",
      "enum": ["draft", "reviewed", "disputed", "retired"]
    },
    "reviewer": { "type": "string", "minLength": 1 },
    "reviewed_at": {
      "type": "string",
      "pattern": "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    },
    "notes": { "type": "string" }
  }
}
```

Add optional assertion override fields:

```json
"evidence_ids": {
  "type": "array",
  "uniqueItems": true,
  "items": { "type": "string", "minLength": 1 }
}
```

to `NodeAssertion.properties`, `SequenceAssertion.properties`, and `GaijiAssertion.properties`.

- [ ] **Step 3: Backfill top-level evidence**

At the top of `data/aat-oracle-cases.toml`, after `aat_version = 1`, add:

```toml
[[evidence]]
id = "jis-x-0213-2-13-47"
kind = "reference_table"
citation = "JIS X 0213 plane 2 row 13 cell 47"
locator = "2-13-47"
supports = "JIS code 2-13-47 resolves to 撑."
independent = true

[[evidence]]
id = "unicode-u546d"
kind = "unicode"
citation = "Unicode code point U+546D"
locator = "U+546D"
supports = "U+546D is 呭."
independent = true

[[evidence]]
id = "aozora-ruby-gaiji-inline-base"
kind = "curator_note"
citation = "Curated Aozora syntax fixture for gaiji marker followed by ruby annotation"
supports = "The gaiji marker is the ruby base and should not become a raw node."
independent = true
notes = "Draft evidence only; requires separate confirmation against Aozora rule text or corpus examples before reviewed status."
```

- [ ] **Step 4: Backfill case evidence and review entries**

For `gaiji.jis.2-13-47`, add:

```toml
evidence_ids = ["jis-x-0213-2-13-47"]

[[case.review]]
status = "draft"
reviewer = "oracle-seed"
reviewed_at = "2026-05-03"
notes = "Seeded from current oracle fixture; needs source audit before reviewed."
```

For `gaiji.unicode.u546d`, add:

```toml
evidence_ids = ["unicode-u546d"]

[[case.review]]
status = "draft"
reviewer = "oracle-seed"
reviewed_at = "2026-05-03"
notes = "Seeded from Unicode code point fixture; needs source audit before reviewed."
```

For `ruby.gaiji.inline_base`, add:

```toml
evidence_ids = ["aozora-ruby-gaiji-inline-base"]

[[case.review]]
status = "draft"
reviewer = "oracle-seed"
reviewed_at = "2026-05-03"
notes = "Curated structural fixture; not reviewed against independent Aozora rule evidence yet."
```

Do not add assertion-level `evidence_ids` unless an assertion needs to override the case-level evidence.

- [ ] **Step 5: Verify schema/data pass**

Run:

```bash
bash tests/aat-oracle-data-schema-smoke.sh
```

Expected: PASS and print:

```text
validated aat-oracle-cases
validated aat-upstream-observations
```

- [ ] **Step 6: Commit schema and data atomically**

Run:

```bash
git add data/aat-oracle-cases.schema.json data/aat-oracle-cases.toml
git commit -m "data: add oracle evidence provenance"
```

Expected: commit succeeds.

---

### Task 3: Load Evidence and Review Succession

**Files:**
- Modify: `crates/ab-oracle/src/data.rs`

- [ ] **Step 1: Add failing loader test**

Add this test to `crates/ab-oracle/src/data.rs`:

```rust
#[test]
fn loads_oracle_evidence_and_review_history() {
    let cases = load_oracle_cases(&data_path("aat-oracle-cases.toml")).unwrap();
    let evidence = cases
        .evidence
        .iter()
        .find(|evidence| evidence.id == "jis-x-0213-2-13-47")
        .unwrap();
    assert_eq!(evidence.kind, EvidenceKind::ReferenceTable);
    assert!(evidence.independent);

    let case = cases
        .case
        .iter()
        .find(|case| case.id == "gaiji.jis.2-13-47")
        .unwrap();
    assert_eq!(case.evidence_ids, vec!["jis-x-0213-2-13-47"]);
    assert_eq!(case.current_review_status(), ReviewStatus::Draft);
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml loads_oracle_evidence_and_review_history -- --nocapture
```

Expected: FAIL because the Rust types do not include evidence/review fields.

- [ ] **Step 2: Add data types**

Add to `crates/ab-oracle/src/data.rs`:

```rust
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Evidence {
    pub id: String,
    pub kind: EvidenceKind,
    pub citation: String,
    pub locator: Option<String>,
    pub url: Option<String>,
    pub supports: String,
    pub independent: bool,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum EvidenceKind {
    ReferenceTable,
    Unicode,
    CuratorNote,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct ReviewEntry {
    pub status: ReviewStatus,
    pub reviewer: String,
    pub reviewed_at: String,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ReviewStatus {
    Draft,
    Reviewed,
    Disputed,
    Retired,
}
```

Update `OracleCases`:

```rust
pub evidence: Vec<Evidence>,
```

Update `OracleCase`:

```rust
pub evidence_ids: Vec<String>,
pub review: Vec<ReviewEntry>,
```

Update `NodeAssertion`, `SequenceAssertion`, and `GaijiAssertion`:

```rust
#[serde(default)]
pub evidence_ids: Vec<String>,
```

Add:

```rust
impl OracleCase {
    pub fn current_review_status(&self) -> ReviewStatus {
        self.review
            .last()
            .map(|entry| entry.status)
            .unwrap_or(ReviewStatus::Draft)
    }
}

impl ReviewStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            ReviewStatus::Draft => "draft",
            ReviewStatus::Reviewed => "reviewed",
            ReviewStatus::Disputed => "disputed",
            ReviewStatus::Retired => "retired",
        }
    }
}
```

- [ ] **Step 3: Update existing struct-literal tests**

Find existing `OracleCase {` test literals:

```bash
rg -n "OracleCase \\{" crates/ab-oracle/src
```

In every test literal, add:

```rust
evidence_ids: vec!["fixture-evidence".to_owned()],
review: vec![crate::data::ReviewEntry {
    status: crate::data::ReviewStatus::Draft,
    reviewer: "test".to_owned(),
    reviewed_at: "2026-05-03".to_owned(),
    notes: None,
}],
```

Where an `OracleCases` literal is used, add:

```rust
evidence: vec![crate::data::Evidence {
    id: "fixture-evidence".to_owned(),
    kind: crate::data::EvidenceKind::ReferenceTable,
    citation: "fixture".to_owned(),
    locator: None,
    url: None,
    supports: "fixture".to_owned(),
    independent: true,
    notes: None,
}],
```

- [ ] **Step 4: Verify loader and all crate tests**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture
```

Expected: PASS.

- [ ] **Step 5: Commit data model**

Run:

```bash
git add crates/ab-oracle/src/data.rs crates/ab-oracle/src/evaluate.rs
git commit -m "feat: load oracle evidence metadata"
```

Expected: commit succeeds.

---

### Task 4: Add Composable Oracle Quality Validation

**Files:**
- Create: `crates/ab-oracle/src/oracle_quality.rs`
- Modify: `crates/ab-oracle/src/lib.rs`
- Modify: `crates/ab-oracle/src/main.rs`

- [ ] **Step 1: Add failing quality tests**

Create `crates/ab-oracle/src/oracle_quality.rs`:

```rust
use crate::data::{Evidence, OracleCase, OracleCases};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OracleQualityError {
    pub case_id: String,
    pub message: String,
}

pub struct EvidenceIndex<'a> {
    pub evidence: std::collections::BTreeMap<&'a str, &'a Evidence>,
    pub duplicate_ids: Vec<String>,
}

pub fn build_evidence_index(cases: &OracleCases) -> EvidenceIndex<'_> {
    let mut evidence = std::collections::BTreeMap::new();
    let mut duplicate_ids = Vec::new();
    for record in &cases.evidence {
        if evidence.insert(record.id.as_str(), record).is_some() {
            duplicate_ids.push(record.id.clone());
        }
    }
    EvidenceIndex { evidence, duplicate_ids }
}

pub fn validate_case_quality(
    _case: &OracleCase,
    _evidence_index: &EvidenceIndex<'_>,
) -> Vec<OracleQualityError> {
    Vec::new()
}

pub fn validate_oracle_quality(cases: &OracleCases) -> Vec<OracleQualityError> {
    let evidence_index = build_evidence_index(cases);
    let mut errors = Vec::new();
    for duplicate in evidence_index.duplicate_ids {
        errors.push(OracleQualityError {
            case_id: "<evidence>".to_owned(),
            message: format!("duplicate evidence id {duplicate:?}"),
        });
    }
    for case in &cases.case {
        errors.extend(validate_case_quality(case, &evidence_index));
    }
    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{
        EvidenceKind, OracleExpectations, ReviewEntry, ReviewStatus,
    };

    fn evidence(id: &str, kind: EvidenceKind, independent: bool) -> Evidence {
        Evidence {
            id: id.to_owned(),
            kind,
            citation: "fixture".to_owned(),
            locator: None,
            url: None,
            supports: "fixture".to_owned(),
            independent,
            notes: None,
        }
    }

    fn case_with(evidence_ids: Vec<&str>, status: ReviewStatus) -> OracleCase {
        OracleCase {
            id: "case".to_owned(),
            syntax_row_ids: vec!["fixture.syntax".to_owned()],
            category: "fixture".to_owned(),
            source_utf8: "本文".to_owned(),
            evidence_ids: evidence_ids.into_iter().map(str::to_owned).collect(),
            review: vec![ReviewEntry {
                status,
                reviewer: "test".to_owned(),
                reviewed_at: "2026-05-03".to_owned(),
                notes: None,
            }],
            notes: None,
            oracle: OracleExpectations::default(),
        }
    }

    #[test]
    fn rejects_case_evidence_ids_that_do_not_exist() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: Vec::new(),
            case: vec![case_with(vec!["missing"], ReviewStatus::Draft)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(errors
            .iter()
            .any(|error| error.message.contains("missing evidence id \"missing\"")));
    }

    #[test]
    fn rejects_non_independent_oracle_evidence() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![evidence("not-independent", EvidenceKind::ReferenceTable, false)],
            case: vec![case_with(vec!["not-independent"], ReviewStatus::Draft)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(errors
            .iter()
            .any(|error| error.message.contains("not independent")));
    }

    #[test]
    fn reviewed_cases_require_non_curator_evidence() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![evidence("curated", EvidenceKind::CuratorNote, true)],
            case: vec![case_with(vec!["curated"], ReviewStatus::Reviewed)],
        };

        let errors = validate_oracle_quality(&cases);

        assert!(errors
            .iter()
            .any(|error| error.message.contains("non-curator")));
    }

    #[test]
    fn duplicate_evidence_ids_are_reported() {
        let cases = OracleCases {
            aat_version: 1,
            evidence: vec![
                evidence("duplicate", EvidenceKind::ReferenceTable, true),
                evidence("duplicate", EvidenceKind::Unicode, true),
            ],
            case: Vec::new(),
        };

        let errors = validate_oracle_quality(&cases);

        assert!(errors
            .iter()
            .any(|error| error.message.contains("duplicate evidence id \"duplicate\"")));
    }
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml oracle_quality -- --nocapture
```

Expected: FAIL because `validate_case_quality` returns no errors.

- [ ] **Step 2: Export module**

Add to `crates/ab-oracle/src/lib.rs`:

```rust
pub mod oracle_quality;
```

- [ ] **Step 3: Implement case validation**

Replace the stub `validate_case_quality` with this implementation and helper:

```rust
use crate::data::{EvidenceKind, ReviewStatus};

pub fn validate_case_quality(
    case: &OracleCase,
    evidence_index: &EvidenceIndex<'_>,
) -> Vec<OracleQualityError> {
    let mut errors = Vec::new();
    check_ids(
        &case.id,
        "case.evidence_ids",
        &case.evidence_ids,
        evidence_index,
        &mut errors,
    );

    for assertion in &case.oracle.nodes {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.nodes.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }
    for assertion in &case.oracle.sequence {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.sequence.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }
    for assertion in &case.oracle.gaiji {
        if !assertion.evidence_ids.is_empty() {
            check_ids(
                &case.id,
                "case.oracle.gaiji.evidence_ids",
                &assertion.evidence_ids,
                evidence_index,
                &mut errors,
            );
        }
    }

    if case.current_review_status() == ReviewStatus::Reviewed {
        let has_non_curator_evidence = case.evidence_ids.iter().any(|id| {
            evidence_index
                .evidence
                .get(id.as_str())
                .is_some_and(|record| {
                    record.independent && !matches!(record.kind, EvidenceKind::CuratorNote)
                })
        });
        if !has_non_curator_evidence {
            errors.push(OracleQualityError {
                case_id: case.id.clone(),
                message: "reviewed cases require at least one independent non-curator evidence record"
                    .to_owned(),
            });
        }
    }

    errors
}

fn check_ids(
    case_id: &str,
    field_name: &str,
    ids: &[String],
    evidence_index: &EvidenceIndex<'_>,
    errors: &mut Vec<OracleQualityError>,
) {
    for id in ids {
        match evidence_index.evidence.get(id.as_str()) {
            Some(record) if record.independent => {}
            Some(_) => errors.push(OracleQualityError {
                case_id: case_id.to_owned(),
                message: format!("{field_name} references evidence id {id:?}, but it is not independent"),
            }),
            None => errors.push(OracleQualityError {
                case_id: case_id.to_owned(),
                message: format!("{field_name} references missing evidence id {id:?}"),
            }),
        }
    }
}
```

- [ ] **Step 4: Wire quality validation into CLI**

In `crates/ab-oracle/src/main.rs`, after loading oracle cases:

```rust
let quality_errors = ab_oracle::oracle_quality::validate_oracle_quality(&oracle);
if !quality_errors.is_empty() {
    for error in &quality_errors {
        eprintln!("oracle quality error in {}: {}", error.case_id, error.message);
    }
    bail!("oracle quality validation failed");
}
```

- [ ] **Step 5: Verify tests and CLI**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo run --manifest-path crates/ab-oracle/Cargo.toml -- \
  --oracle data/aat-oracle-cases.toml \
  --upstream data/aat-upstream-observations.toml
```

Expected: tests pass; CLI prints loaded case/observation counts.

- [ ] **Step 6: Commit**

Run:

```bash
git add crates/ab-oracle/src/oracle_quality.rs crates/ab-oracle/src/lib.rs crates/ab-oracle/src/main.rs
git commit -m "feat: validate oracle evidence quality"
```

Expected: commit succeeds.

---

### Task 5: Report Review Status and Evidence Strength

**Files:**
- Modify: `crates/ab-oracle/src/evaluate.rs`
- Modify: `crates/ab-oracle/src/report.rs`
- Modify: `crates/ab-oracle/src/main.rs`
- Modify: `reports/aat-fidelity/fidelity_explorer.py`
- Modify: `reports/aat-fidelity/fixtures/report.json`

- [ ] **Step 1: Add failing report test**

In `crates/ab-oracle/src/report.rs`, add:

```rust
#[test]
fn markdown_names_oracle_review_fields() {
    let markdown = render_markdown(&OracleReport {
        rows: vec![ReportRow {
            case_id: "case".to_owned(),
            adapter: "adapter".to_owned(),
            schema_status: "pass".to_owned(),
            upstream_status: "faithful".to_owned(),
            oracle_status: "pass".to_owned(),
            oracle_review_status: "draft".to_owned(),
            oracle_evidence_strength: "reference".to_owned(),
            failures: Vec::new(),
        }],
    });

    assert!(markdown.contains("oracle_review_status"));
    assert!(markdown.contains("oracle_evidence_strength"));
}
```

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml markdown_names_oracle_review_fields -- --nocapture
```

Expected: FAIL because `ReportRow` lacks these fields.

- [ ] **Step 2: Add evidence strength derivation**

In `crates/ab-oracle/src/evaluate.rs`, add:

```rust
use crate::data::EvidenceKind;

fn evidence_strength_rank(kind: EvidenceKind) -> u8 {
    match kind {
        EvidenceKind::Unicode => 3,
        EvidenceKind::ReferenceTable => 2,
        EvidenceKind::CuratorNote => 1,
    }
}

pub fn evidence_strength(case: &OracleCase, evidence: &[crate::data::Evidence]) -> &'static str {
    let linked = evidence
        .iter()
        .filter(|record| case.evidence_ids.iter().any(|id| id == &record.id))
        .collect::<Vec<_>>();

    match linked
        .iter()
        .map(|record| evidence_strength_rank(record.kind))
        .max()
        .unwrap_or(1)
    {
        3 => "normative",
        2 => "reference",
        _ => "curated",
    }
}
```

Add to `CaseEvaluation`:

```rust
pub oracle_review_status: String,
pub oracle_evidence_strength: String,
```

Change `evaluate_case` signature to accept evidence:

```rust
pub fn evaluate_case(
    case: &OracleCase,
    all_evidence: &[crate::data::Evidence],
    observations: &UpstreamObservations,
    adapter: &str,
    aat: Value,
) -> CaseEvaluation
```

Set:

```rust
oracle_review_status: case.current_review_status().as_str().to_owned(),
oracle_evidence_strength: evidence_strength(case, all_evidence).to_owned(),
```

Keep:

```rust
let oracle_status = if failures.is_empty() { "pass" } else { "fail" }.to_owned();
```

Do not introduce a disputed value for `oracle_status`.

- [ ] **Step 3: Update call sites and tests**

In `crates/ab-oracle/src/main.rs`, call:

```rust
let evaluation = evaluate_case(case, &oracle.evidence, &observations, &adapter.id, aat);
```

The `evaluate_case_reports_all_axes` test updated in Task 3 needs a second edit here because `evaluate_case` gains an evidence argument. In that test, pass this fixture evidence slice as the second argument:

```rust
&[crate::data::Evidence {
    id: "fixture-evidence".to_owned(),
    kind: crate::data::EvidenceKind::ReferenceTable,
    citation: "fixture".to_owned(),
    locator: None,
    url: None,
    supports: "fixture".to_owned(),
    independent: true,
    notes: None,
}]
```

- [ ] **Step 4: Update report row and Markdown**

Add to `ReportRow`:

```rust
pub oracle_review_status: String,
pub oracle_evidence_strength: String,
```

Update Markdown header:

```rust
"| case_id | adapter | schema_status | upstream_status | oracle_status | oracle_review_status | oracle_evidence_strength | failures |\n\
 | --- | --- | --- | --- | --- | --- | --- | --- |\n"
```

Update row formatting to include the two new fields.

- [ ] **Step 5: Update notebook fixture and filters**

In `reports/aat-fidelity/fixtures/report.json`, add:

```json
"oracle_review_status": "draft",
"oracle_evidence_strength": "reference"
```

In `reports/aat-fidelity/fidelity_explorer.py`, add dropdowns for `oracle_review_status` and `oracle_evidence_strength`, and include them in filtering. Use the existing `oracle_status` filtering pattern.

- [ ] **Step 6: Verify**

Run:

```bash
CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh
```

Expected: all pass; JSON/Markdown reports include `oracle_review_status` and `oracle_evidence_strength`.

- [ ] **Step 7: Commit**

Run:

```bash
git add crates/ab-oracle/src/evaluate.rs crates/ab-oracle/src/report.rs crates/ab-oracle/src/main.rs reports/aat-fidelity/fidelity_explorer.py reports/aat-fidelity/fixtures/report.json
git commit -m "feat: report oracle review evidence strength"
```

Expected: commit succeeds.

---

### Task 6: Skip Retired Cases in Reports

**Files:**
- Modify: `crates/ab-oracle/src/main.rs`
- Test: `crates/ab-oracle/src/main.rs` through CLI smoke

- [ ] **Step 1: Add a helper function in `main.rs`**

Add near the top-level functions in `crates/ab-oracle/src/main.rs`:

```rust
fn should_evaluate_case(case: &ab_oracle::data::OracleCase, requested_case_id: Option<&str>) -> bool {
    if matches!(
        case.current_review_status(),
        ab_oracle::data::ReviewStatus::Retired
    ) {
        return false;
    }
    requested_case_id.is_none_or(|case_id| case.id == case_id)
}
```

Change the report case filter to call this helper:

```rust
let cases = oracle
    .case
    .iter()
    .filter(|case| should_evaluate_case(case, args.case_id.as_deref()));
```

- [ ] **Step 2: Verify existing smoke still works**

Run:

```bash
bash tests/adapter-fidelity-smoke.sh
```

Expected: PASS, because current seed cases are draft, not retired.

- [ ] **Step 3: Commit**

Run:

```bash
git add crates/ab-oracle/src/main.rs
git commit -m "feat: skip retired oracle cases"
```

Expected: commit succeeds.

---

## Verification Bundle

Run all of these after the final task:

```bash
bash tests/aat-oracle-data-schema-smoke.sh
bash tests/adapter-fidelity-smoke.sh
bash tests/adapter-oracle-report-smoke.sh
bash tests/aat-fidelity-marimo-notebook-smoke.sh

CARGO_TARGET_DIR=/db/ab-validator/target-ab-oracle-provenance \
  nix develop .# --command cargo test --manifest-path crates/ab-oracle/Cargo.toml -- --nocapture

CARGO_TARGET_DIR=/db/ab-validator/target-ab-coverage-oracle \
  nix develop .# --command cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
```

Expected: every command exits 0.

## Self-Review

- **Spec coverage:** The plan answers “how do we know the oracle is correct?” with shared evidence, review history, independence checks, report visibility, and retired/disputed handling.
- **Simplicity:** One mutable-looking concept was removed: input `confidence`. Current review status is derived from review history; evidence strength is derived from evidence kind.
- **Composability:** `validate_case_quality(case, evidence_index)` validates one case and is composed by file-level validation.
- **Time:** Review transitions are stored as a succession of `[[case.review]]` values.
- **Protocol:** TOML shape, JSON Schema shape, and Rust semantic validation have separate responsibilities.
- **Constraints:** Evidence kinds start with only the kinds needed by current seed cases. `curator_note` cannot justify reviewed status.
