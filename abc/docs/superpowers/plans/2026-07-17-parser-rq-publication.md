# Parser RQ Publication Structure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace ADR 0039 predicate 6's `:instrument-missing` value with an
identity-bound, non-vacuous publication-structure observation derived from
authenticated per-work captures over the pinned qualification corpus.

**Architecture:** The existing Python validator remains the single reader of a
materialized publication bundle and emits raw structure-check candidates,
joinability, counts, and supporting-precondition blocks. ABC owns closed schemas, policy, fixture census,
and the pure Clojure aggregate/observation boundary; a Python capture command
materializes and validates explicit indexed inputs, writes immutable blobs, and
never decides the final ratio.

**Tech Stack:** Clojure 1.12, Malli, networknt JSON Schema, Kaocha/test.check;
Python 3.13, jsonschema, pytest; JSON Schema 2020-12; Nix flakes and `just`.

## Global Constraints

- Read the approved design first:
  `docs/superpowers/specs/2026-07-16-parser-rq-publication-design.md`.
- Predicate 6 remains “required publication structures present per successful
  work” with exact threshold `= 1.0`; this plan does not change the predicate.
- Predicate 6 never consumes R1 or predicate 5 verdicts. It owns only the
  narrower `publication_join_input_valid` structural precondition.
- A complete bundle that fails a policy check or required-construct census is
  an available failure. Missing, malformed, unauthenticated, incoherent, or
  incomplete evidence is unavailable.
- Every pinned corpus member has exactly one indexed disposition. Failed and
  timed-out works carry no fabricated publication evidence.
- The numeric denominator is parsed works. Zero parsed works is unavailable;
  denominator shrink is disclosed and remains predicates 1/9's gate authority.
- Policy identity binds check names, preservation schema identity, and the
  validator semantic closure. A validator change requires a new policy identity.
- Corpus-scale artifacts stay in the external store. Git receives schemas,
  policies, deterministic qualification inputs, manifests, bounded witnesses,
  and a small production-shaped drift fixture only.
- Runtime paths are locators, never evidence identity. Consumers use P0's
  authenticated single-read boundary and never reopen authenticated locators.
- P2 proves the instrument. P5 pins the final candidate and performs the
  authoritative hinoki capture, admission, and conditional ADR promotion.
- Follow `docs/comment-standards.md`; production comments must not mention this
  task, plan, review, or transient implementation history.

## File Map

- `abc/schemas/parser-rq-publication-{policy,work,index,aggregate}.schema.json`
  own the four closed immutable contracts.
- `abc/data/parser-rq-publication-policy-v1.json` owns predicate-6 check
  membership and all authority hashes.
- `abc/data/parser-rq-publication-fixtures-v1.json` owns the three-work minimum
  construct census and qualification-input identities.
- `ab-validator/reports/parser-ir/publication-bundle-validate.py` remains the
  detailed bundle reader and emits raw checks/counts without consuming policy.
- `ab-validator/reports/parser-ir/publication-validator-identity.py` generates
  the semantic-closure manifest.
- `ab-validator/reports/parser-ir/publication-rq-capture.py` orchestrates an
  explicit capture index, materialization, validation, and CAS writes.
- `abc/src/abc/tools/parser_rq_publication.clj` authenticates and derives the
  aggregate observation; it performs no processes or path discovery.
- `abc/test/fixtures/parser-rq/publication-capture/` is the committed
  production-shaped drift fixture.

---

### Task 1: Define the closed ABC policy, work, index, and aggregate protocols

**Files:**
- Create: `abc/schemas/parser-rq-publication-policy.schema.json`
- Create: `abc/schemas/parser-rq-publication-work.schema.json`
- Create: `abc/schemas/parser-rq-publication-index.schema.json`
- Create: `abc/schemas/parser-rq-publication-aggregate.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Test: `abc/test/abc/tools/parser_rq_publication_test.clj`

**Interfaces:**
- Consumes: P0 SHA-256 identity format and qualification identity references.
- Produces: four Draft 2020-12 schema ids under
  `https://w3id.org/abc/schemas/parser-rq-publication-*.schema.json`.

- [ ] **Step 1: Write failing closed-contract tests**

Create `parser_rq_publication_test.clj` with helpers that read each schema and
validate a minimal value. Pin these required fields:

```clojure
(deftest publication-contracts-are-closed
  (doseq [[schema value]
          [["schemas/parser-rq-publication-policy.schema.json" valid-policy]
           ["schemas/parser-rq-publication-work.schema.json" valid-work]
           ["schemas/parser-rq-publication-index.schema.json" valid-index]
           ["schemas/parser-rq-publication-aggregate.schema.json" valid-aggregate]]]
    (let [contract (files/read-json schema)]
      (is (nil? (schema/validation-errors contract value)))
      (is (seq (schema/validation-errors contract
                                         (assoc value "attacker_field" true)))))))

(deftest parsed-and-nonparsed-work-evidence-are-disjoint
  (is (nil? (schema/validation-errors work-schema valid-parsed-work)))
  (is (nil? (schema/validation-errors work-schema valid-failed-work)))
  (is (seq (schema/validation-errors
            work-schema
            (assoc valid-failed-work "publication" parsed-publication-block)))))

(deftest index-records-are-unique-and-closed
  (is (seq (schema/validation-errors
            index-schema
            (update valid-index "records" conj (first (get valid-index "records")))))))
```

The schemas must express these exact types:

```text
policy: schema_id/version, policy_id/hash, ordered unique structure_checks,
        ordered unique supporting_checks, preservation_schema{id,hash},
        validator{id,semantics_hash}
work: schema_id/version, work_id, source_sha256, qualification_identity_ref,
      parser_disposition, policy_hash, preservation_schema_hash,
      validator_semantics_hash, census_hash, and exactly one conditional branch:
      parsed -> raw publication evidence block; failed|timeout -> disposition witness only
index: schema_id/version, corpus_id/snapshot_hash/list_hash,
       qualification_identity_ref, policy/schema/validator/census hashes,
       expected_work_ids, records[{work_id,source_sha256,locator,ref}]
aggregate: schema_id/version, status, all authority hashes,
           expected/parsed/failed/timed_out/eligible/passed counts,
           ratio numerator/denominator/value, denominator_shrink_witnesses,
           failed_work_witnesses
```

- [ ] **Step 2: Run tests and verify schema absence fails**

Run from `abc/`:

```bash
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test
```

Expected: FAIL while reading
`schemas/parser-rq-publication-policy.schema.json`.

- [ ] **Step 3: Add the four complete closed schemas**

Use `additionalProperties: false` at every object level, `$defs` for
`sha256`, `blobRef`, and `indexRecord`, `uniqueItems: true` for named check and
work-id arrays, and `if/then/else` to prohibit publication evidence on
`failed|timeout` records. A parsed publication block requires:

```json
{
  "join_input_valid": true,
  "structure_check_candidates": {},
  "counts": {
    "preservation_records": 1,
    "tei_preservation_references": 1,
    "non_null_tei_pointers": 1,
    "non_null_source_pointers": 1,
    "by_construct": {"span_coordinates": 1}
  },
  "artifacts": []
}
```

The work schema contains no projected predicate verdict. Projection is solely
the Task 6 analyzer's responsibility; capture records contain raw authenticated
checks and counts.

- [ ] **Step 4: Wire all four schemas into design-bundle validation**

Add their paths to `design-schema-inputs`, load them beside the existing parser
RQ schemas in `validate-json-schemas!`, include them in the schema-validity
loop, and add a test that records all four paths passed to `validate-json!`.

- [ ] **Step 5: Run focused checks**

```bash
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test \
  --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-parser-rq-publication-contracts-test
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add abc/schemas/parser-rq-publication-*.schema.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/test/abc/tools/parser_rq_publication_test.clj
git commit -m "feat(parser-rq): define publication evidence protocols"
```

---

### Task 2: Bind validator semantics to a mechanically checked import closure

**Files:**
- Create: `ab-validator/reports/parser-ir/publication-validator-identity.py`
- Create: `ab-validator/reports/parser-ir/tests/test_publication_validator_identity.py`

**Interfaces:**
- Consumes: the validator's actual transitive local Python import graph and
  exact Python/jsonschema runtime versions.
- Produces: a generator/checker for canonical semantic-closure manifests. Task
  3 creates the authoritative manifest after validator v2 semantics are final.

- [ ] **Step 1: Write failing import-closure and identity tests**

```python
def test_reviewed_sources_equal_actual_transitive_local_imports(module):
    actual = module.transitive_local_imports(VALIDATOR, REPORTS_ROOT)
    assert actual == set(module.REVIEWED_SOURCE_PATHS)

def test_added_local_import_fails_closed(module, tmp_path):
    validator = copy_validator_tree(tmp_path)
    validator.write_text(validator.read_text() + "\nfrom reports.lib.freshness import x\n")
    with pytest.raises(ValueError, match="reviewed semantic closure differs"):
        module.build_manifest(validator.parents[2])

def test_manifest_hash_recomputes_and_check_detects_drift(module):
    manifest = module.build_manifest(REPO_ROOT)
    assert manifest["validator_semantics_hash"] == module.projected_hash(manifest)
    assert module.check_manifest(manifest, manifest) == []
```

- [ ] **Step 2: Run and verify module absence fails**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_validator_identity.py -q
```

Expected: FAIL importing `publication-validator-identity.py`.

- [ ] **Step 3: Implement static transitive import discovery and canonical identity**

Parse every visited module with `ast`. Follow only absolute
`reports.*` imports that resolve beneath the repository's `reports/` directory;
ignore standard-library and installed-package imports. Recurse to a fixed point,
reject cycles only if traversal cannot terminate, and compare the actual set in
both directions with this reviewed closed set:

```text
reports/parser-ir/publication-bundle-validate.py
reports/lib/evidence.py
reports/lib/hashing.py
reports/lib/io.py
reports/lib/paths.py
reports/lib/source_region.py
```

The manifest records validator id
`ab-validator/publication-bundle-validate/v2`, exact runtime coordinates from
`sys.version_info` and `importlib.metadata.version("jsonschema")`, and the
ordered full SHA-256 identity of every reviewed source. Compute
`validator_semantics_hash` over canonical JSON with its own field removed.
Support `--check`, `--write`, `--repo-root`, and `--out`.

- [ ] **Step 4: Generate twice to temporary files and prove determinism**

```bash
nix develop ./ab-validator# --command python \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  --repo-root ab-validator --write \
  --out /tmp/parser-rq-publication-validator-1.json
nix develop ./ab-validator# --command python \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  --repo-root ab-validator --write \
  --out /tmp/parser-rq-publication-validator-2.json
cmp /tmp/parser-rq-publication-validator-1.json \
    /tmp/parser-rq-publication-validator-2.json
```

Expected: both commands and `cmp` exit zero; the import-closure tests prove that
adding or removing a local transitive import fails before stale identity use.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/reports/parser-ir/publication-validator-identity.py \
  ab-validator/reports/parser-ir/tests/test_publication_validator_identity.py
git commit -m "feat(parser-rq): bind publication validator semantics"
```

---

### Task 3: Correct and deepen the single publication-bundle validator

**Files:**
- Modify: `ab-validator/reports/parser-ir/publication-bundle-validate.py`
- Create: `ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py`
- Modify: `ab-validator/tests/parser-ir-publication-bundle-smoke.sh`
- Modify: `ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh`
- Create: `ab-validator/data/parser-rq-publication-validator-v1.json`

**Interfaces:**
- Consumes: explicit authoritative parser-IR/preservation schemas and
  validator-identity path.
- Produces: detailed evidence v2 with raw `structure_check_candidates`,
  `supporting_preconditions`, `join_input`, `counts`, and validator/schema
  identities. It does not consume policy or emit a predicate-6 verdict.

- [ ] **Step 1: Write failing Python tests for the four review gaps**

Load the hyphenated CLI with `importlib.util.spec_from_file_location` and test
pure helpers before the CLI:

```python
def test_empty_schema_valid_preservation_discloses_zero_counts(module, fixture):
    result = module.validate_values(
        **fixture(preservation_records=[]),
    )
    assert result["structure_check_candidates"]["preservation_schema_valid"] is True
    assert result["counts"]["preservation_records"] == 0

def test_schema_invalid_but_join_valid_ir_keeps_independent_structure_evidence(module, fixture):
    result = module.validate_values(**fixture(parser_ir_extra_field=True))
    assert result["supporting_preconditions"]["parser_ir_schema_valid"] is False
    assert result["join_input"] == {"status": "valid", "errors": []}
    assert result["structure_check_candidates"]["plaintext_body_only"] is True

def test_join_invalid_ir_is_unavailable_not_structure_failure(module, fixture):
    result = module.validate_values(**fixture(node={"type": "text", "text": 7}))
    assert result["join_input"]["status"] == "invalid"
    assert result["structure_check_candidates"] is None

def test_supporting_source_status_does_not_change_structure(module, fixture):
    green = module.validate_values(**fixture(source_gate="pass"))
    red = module.validate_values(**fixture(source_gate="fail"))
    assert green["structure_check_candidates"] == red["structure_check_candidates"]
```

Also assert that a `0.2.0` sidecar and a `0.3.0` sidecar with an extra property
fail full JSON Schema validation. Policy membership is not present in Python and
is tested only at the Task 6 analyzer boundary.

- [ ] **Step 2: Run tests and verify current behavior fails**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py -q
```

Expected: FAIL because `validate_values`, joinability, raw split-check, and
count blocks do not exist and the live validator still expects preservation
`0.2.0`.

- [ ] **Step 3: Implement full schema validation and joinability**

Add required CLI options:

```python
parser.add_argument("--parser-ir-schema", required=True, type=pathlib.Path)
parser.add_argument("--preservation-schema", required=True, type=pathlib.Path)
parser.add_argument("--validator-identity", required=True, type=pathlib.Path)
```

Make `--source-region-summary` optional. Its absence or red status appears only
under `supporting_preconditions`.

Implement and call these exact pure interfaces from `validate_bundle`:
`publication_join_input_errors(parser_ir: object) -> list[str]`,
`validate_schema(instance: object, contract: dict[str, Any]) -> list[str]`, and
`publication_counts(parser_ir: dict[str, Any], preservation: dict[str, Any],
tei_root: ET.Element) -> dict[str, Any]`.

`publication_join_input_errors` rejects unknown node types and malformed fields
used by `node_visible_body_text`; it must not silently turn an unknown node into
empty text. `validate_schema` uses `Draft202012Validator` with a format checker.

- [ ] **Step 4: Migrate batch aggregation and Markdown as one v2 contract**

Replace `REQUIRED_CHECKS` with separate closed
`STRUCTURE_CHECK_CANDIDATES` and `SUPPORTING_PRECONDITIONS` tuples.
`validate_batch` must retain every row's nested blocks, aggregate each block by
name, aggregate join status separately, and preserve the legacy overall verdict
only as the conjunction of readable/join-valid/detailed results. It must never
look up the removed flat `row["checks"]`.

`render_markdown` renders distinct “Structure check candidates,” “Join input,”
“Supporting preconditions,” and “Counts” sections for single and batch output.
Add tests that fail if any section is absent or if batch aggregation reads a
flat `checks` key.

- [ ] **Step 5: Update both smoke fixtures to v2**

Use real schema `0.3.0` values including required `parser_ir`, `tei`, `source`,
`producer`, and `mapping` blocks. Pass all explicit authority paths. Assert:

```bash
jq -e '.structure_check_candidates.plaintext_body_only == true' "$summary_json"
jq -e '.supporting_preconditions.parser_ir_schema_valid == true' "$summary_json"
jq -e '.join_input.status == "valid"' "$summary_json"
jq -e '.counts.preservation_records > 0' "$summary_json"
jq -e '.rows[0].structure_check_candidates.plaintext_body_only == true' "$batch_summary_json"
```

Add one empty-record fixture that remains schema-valid and reports zero counts;
the analyzer task proves that policy+census projection turns it into a measured
failure.

- [ ] **Step 6: Generate and check the final v2 semantic manifest**

```bash
nix develop ./ab-validator# --command python \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  --repo-root ab-validator --write \
  --out ab-validator/data/parser-rq-publication-validator-v1.json
nix develop ./ab-validator# --command python \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  --repo-root ab-validator --check \
  --out ab-validator/data/parser-rq-publication-validator-v1.json
```

Expected: both exit zero against the final Task 3 validator bytes.

- [ ] **Step 7: Run focused and Nix checks**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py -q
bash ab-validator/tests/parser-ir-publication-bundle-smoke.sh
bash ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh
nix build ./ab-validator#checks.x86_64-linux.reports-pytest
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add ab-validator/reports/parser-ir/publication-bundle-validate.py \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py \
  ab-validator/tests/parser-ir-publication-bundle-smoke.sh \
  ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh \
  ab-validator/data/parser-rq-publication-validator-v1.json
git commit -m "feat(parser-rq): separate raw publication structure evidence"
```

---

### Task 4: Characterize then freeze policy, census, and qualification metadata

**Files:**
- Create: `abc/data/parser-rq-publication-policy-v1.json`
- Create: `abc/data/parser-rq-publication-fixtures-v1.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/persons/rq-fixture.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000001_1/metadata-record.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000002_2/metadata-record.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000003_3/metadata-record.json`
- Create: `abc/docs/superpowers/reports/2026-07-17-parser-rq-publication-census-characterization.json`
- Create: `abc/docs/superpowers/reports/2026-07-17-parser-rq-publication-census-characterization.md`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `abc/test/abc/tools/parser_rq_publication_test.clj`

**Interfaces:**
- Consumes: Task 1 schemas, Task 2 identity generator, Task 3 final validator
  manifest, and the pinned three-work corpus.
- Produces: immutable policy/census identities and deterministic metadata inputs.

- [ ] **Step 1: Add and validate qualification-only metadata inputs**

Use person id `abc-000000000001`; titles are `Parser RQ Fixture 000001_1`,
`Parser RQ Fixture 000002_2`, and `Parser RQ Fixture 000003_3`; contributor
relation is `著者`; provenance is deterministic. Metadata work ids are the
six-digit card prefixes (`000001`, `000002`, `000003`). The fixture contract
maps each prefix to the corpus work id and source hash and carries:

```json
{"authority": "qualification-fixture-only", "bibliographic_authority": false}
```

Add a test that all metadata/person records validate, every mapping is exact,
and the immutable work record identity source is always the corpus id plus
`source_sha256`; the bare metadata prefix is never accepted as record/index
identity.

- [ ] **Step 2: Run a disposable production-chain census characterization twice**

Create a temporary, uncommitted shell probe that, for each explicit corpus row:

1. runs `cargo run -p ab-aozora -- --mode aat` on the pinned source;
2. runs `cargo run -p ab-aat-to-parser-ir -- convert` with
   `data/aat-to-parser-ir-mapping-v1.json`, the exact source hash, and ABC root;
3. writes a source manifest with the pinned corpus and work hashes;
4. invokes `clojure -M:abc/materialize-publication` with the qualification
   metadata, person directory, and fixed `2026-07-17T00:00:00Z`; and
5. emits canonical per-work counts grouped by preservation `construct`, plus
   record/reference/pointer totals.

Run the probe twice into `/tmp/parser-rq-publication-census-1.json` and
`/tmp/parser-rq-publication-census-2.json`; `cmp` must exit zero. If either
materialization fails or the observations differ, stop and diagnose rather than
freezing an oracle. Also stop if any work has zero total preservation records or
no strictly positive candidate construct; never freeze an empty non-vacuity
census.

- [ ] **Step 3: Commit the characterization report, then freeze by rule**

Copy the canonical observation and a Markdown rendering to the two report paths
in this task. Derive each work's minimum census mechanically from candidate
constructs `source_identity`, `mapping_identity`, `span_coordinates`, and
`gaiji_resolution`: retain every strictly positive observed count and use the
observed count as the minimum. Do not require a zero-count construct merely
because the fixture name suggests it. Record the selection rule and selected
rows in Markdown; delete the disposable probe before commit.

- [ ] **Step 4: Write failing frozen-authority tests**

```clojure
(deftest publication-policy-binds-check-schema-and-validator-semantics
  (let [policy (read-json "data/parser-rq-publication-policy-v1.json")]
    (is (= expected-structure-checks (:structure_checks policy)))
    (is (= (hash/file-ref "schemas/parser-ir-publication-preservation.schema.json")
           (get-in policy [:preservation_schema :hash])))
    (is (= (validator-manifest-hash)
           (get-in policy [:validator :semantics_hash])))
    (is (= (:policy_hash policy) (projected-policy-hash policy)))))

(deftest every-pinned-work-has-an-exercise-census
  (is (= #{"000001_1" "000002_2" "000003_3"}
         (set (keys (:works fixture-contract)))))
  (is (= (derive-positive-candidate-minimums characterization)
         (project-required-censuses fixture-contract))))

(deftest work-record-identity-never-uses-metadata-prefix
  (is (= "000001_1" (get-in fixture-contract [:works "000001_1" :work_id])))
  (is (= "000001" (get-in fixture-contract [:works "000001_1" :metadata_work_id])))
  (is (not= (get-in fixture-contract [:works "000001_1" :work_id])
            (get-in fixture-contract [:works "000001_1" :metadata_work_id]))))
```

- [ ] **Step 5: Run and verify missing frozen data fails**

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test
```

Expected: FAIL reading `data/parser-rq-publication-policy-v1.json`.

- [ ] **Step 6: Create policy and census values**

The policy structure list is exactly the ten raw validator candidates in the
approved design plus analyzer-local `required_construct_census_valid`. The
latter is a predicate-6 structure check, not a supporting flag. Supporting names are exactly
`parser_ir_schema_valid`, `source_region_coverage_valid`, and
`source_region_sidecar_role_available`.

The census values must equal Step 3's characterized projection exactly. Compute
`policy_hash` and `census_hash` as JCS SHA-256 over each document with its own
hash field removed. Tests recompute rather than trust them.

- [ ] **Step 7: Prove deterministic three-work materialization**

Add a test around `materialize-publications-batch!` that stages three parser-IR
and source-manifest inputs, uses the committed batch metadata/person values,
runs with jobs 1 and 3, and compares SHA-256 for all six artifacts per work.
Assert each preservation sidecar satisfies its census.

Run:

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.materialize-publication-test/parser-rq-publication-inputs-materialize-deterministically-test \
  --focus abc.tools.parser-rq-publication-test
```

Expected: PASS and byte-identical outputs at both concurrency settings.

- [ ] **Step 8: Commit**

```bash
git add abc/data/parser-rq-publication-*.json \
  abc/test/fixtures/parser-rq/publication-inputs \
  abc/docs/superpowers/reports/2026-07-17-parser-rq-publication-census-characterization.* \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/test/abc/tools/parser_rq_publication_test.clj
git commit -m "test(parser-rq): freeze publication qualification inputs"
```

---

### Task 5: Build explicit capture-to-CAS and closed-index production

**Files:**
- Create: `ab-validator/reports/parser-ir/publication-rq-capture.py`
- Create: `ab-validator/reports/parser-ir/tests/test_publication_rq_capture.py`
- Create: `ab-validator/tests/parser-rq-publication-capture-smoke.sh`
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Consumes: explicit JSON capture input with one disposition per pinned member,
  ABC root, policy/census/validator identities, and external-store root.
- Produces: immutable work blobs, `publication-index.json`, and P0-compatible
  `manifest.json`; it does not produce the final ratio.

- [ ] **Step 1: Write failing capture tests**

```python
def test_capture_rejects_discovered_or_incomplete_members(tmp_path, capture_input):
    capture_input["works"].pop()
    with pytest.raises(CaptureError, match="exact pinned corpus membership"):
        capture(capture_input, tmp_path / "store")

def test_failed_work_has_no_publication_evidence(tmp_path, capture_input):
    capture_input["works"][0] = {
        "work_id": "000001_1", "source_sha256": SOURCE_1,
        "parser_disposition": "failed", "failure": {"kind": "exit", "code": 1},
    }
    result = capture(capture_input, tmp_path / "store")
    record = read_blob(result.index["records"][0])
    assert "publication" not in record

def test_capture_is_order_independent_and_index_is_canonical(tmp_path, capture_input):
    first = capture(capture_input, tmp_path / "a")
    second = capture({**capture_input, "works": list(reversed(capture_input["works"]))},
                     tmp_path / "b")
    assert first.index_bytes == second.index_bytes

def test_metadata_prefix_cannot_become_record_identity(tmp_path, capture_input):
    capture_input["works"][0]["work_id"] = "000001"
    with pytest.raises(CaptureError, match="exact pinned corpus membership"):
        capture(capture_input, tmp_path / "store")
```

Also test duplicate/extra work, source hash mismatch, qualification identity
mismatch, materializer failure, validator unavailable, wrong policy/schema/
validator/census hash, and two works attempting to reuse one record blob.

- [ ] **Step 2: Run and verify module absence fails**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_rq_capture.py -q
```

Expected: FAIL importing `publication-rq-capture.py`.

- [ ] **Step 3: Implement explicit capture orchestration**

Expose frozen `CaptureResult(manifest, index, manifest_bytes, index_bytes)` and
these exact interfaces: `validate_capture_input(value: object, corpus:
dict[str, Any]) -> list[str]`, `put_blob(store_root: Path, value: bytes,
media_type: str) -> dict[str, Any]`, `capture(value: dict[str, Any], store_root:
Path) -> CaptureResult`, and `main() -> int`.

For parsed works invoke one ABC batch materialization followed by one validator
call per explicit work. For failed/timeouts write disposition-only records.
Sort the final index by pinned corpus order, not input or filesystem order.
`put_blob` writes beneath `sha256/{first-two}/{digest}.{suffix}` with exclusive
create-or-byte-equality semantics; a pre-existing mismatch is fatal.

- [ ] **Step 4: Add a production-shaped smoke capture**

The shell test stages all three pinned sources, candidate parser-IR fixture
outputs, source manifests, qualification metadata, and identities; invokes the
CLI twice with reversed input ordering; and asserts byte-identical index and
manifest output. It then mutates one source hash and expects exit 2 with
`source identity mismatch`.

Wire `parser-rq-publication-capture-smoke` beside existing publication checks in
`ab-validator/flake.nix`.

- [ ] **Step 5: Run checks**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_rq_capture.py -q
bash ab-validator/tests/parser-rq-publication-capture-smoke.sh
nix build ./ab-validator#checks.x86_64-linux.parser-rq-publication-capture-smoke
just python-quality
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/parser-ir/publication-rq-capture.py \
  ab-validator/reports/parser-ir/tests/test_publication_rq_capture.py \
  ab-validator/tests/parser-rq-publication-capture-smoke.sh \
  ab-validator/flake.nix
git commit -m "feat(parser-rq): capture publication structure evidence"
```

---

### Task 6: Authenticate the closed fold and derive predicate 6 in ABC

**Files:**
- Create: `abc/src/abc/tools/parser_rq_publication.clj`
- Modify: `abc/test/abc/tools/parser_rq_publication_test.clj`
- Modify: `abc/src/abc/tools/parser_release_qualification.clj`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`

**Interfaces:**
- Consumes: P0 manifest/store, exact work index and work blobs, qualification
  identity, policy/census/schema/validator authority values.
- Produces:
  `(derive-publication-envelope store manifest index identity) -> envelope | unavailable`
  and `(install-publication-observation measurements envelope) -> measurements`.

- [ ] **Step 1: Write failing happy-path and honest-failure tests**

```clojure
(deftest publication-envelope-uses-exact-work-denominator
  (let [envelope (publication/derive-publication-envelope
                  store manifest index qualification-identity)]
    (is (= identity-ref (:identity_ref envelope)))
    (is (= 1.0M (:value envelope)))
    (is (= {:expected 3 :parsed 3 :eligible 3 :passed 3
            :failed 0 :timed_out 0}
           (:counts envelope)))))

(deftest complete-census-failure-is-an-available-ratio
  (let [result (derive-after-resealed-mutation
                #(assoc-in % [:publication :counts :by_construct
                              :span_coordinates] 0))]
    (is (= 0.6666M (:value result)))
    (is (= ["required_construct_census_valid"]
           (get-in result [:failed_work_witnesses 0 :failed_checks])))))

(deftest missing-successful-record-is-unavailable
  (is (= :unavailable
         (:status (derive-after-index-mutation #(update % :records pop))))))

(deftest zero-parsed-denominator-is-unavailable
  (is (= :unavailable
         (:status (derive-after-all-dispositions "failed")))))
```

Use integer equality for the release pass; decimal display uses DOWN rounding
and never supplies the truth condition.

- [ ] **Step 2: Write the adversarial identity matrix**

One table-driven test reseals each mutation so rejection proves semantic joins,
not a trivial hash mismatch:

```clojure
(doseq [[label mutate]
        [["qualification" #(assoc % :qualification_identity_ref other-hash)]
         ["policy" #(assoc % :policy_hash other-hash)]
         ["schema" #(assoc % :preservation_schema_hash other-hash)]
         ["validator semantics" #(assoc % :validator_semantics_hash other-hash)]
         ["census" #(assoc % :census_hash other-hash)]
         ["cross-work substitution" substitute-record-between-works]
         ["duplicate work" duplicate-index-work]
         ["extra work" append-attacker-work]
         ["unknown check" append-unknown-check]
         ["join invalid" make-join-invalid]
         ["counter mismatch" increment-passed-count]]]
  (testing label
    (is (= :unavailable (:status (derive-resealed mutate))))))
```

Add test.check properties: input-order invariance, `0 <= passed <= eligible`,
and duplicate injection always unavailable.

- [ ] **Step 3: Run tests and verify namespace absence fails**

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test
```

Expected: FAIL loading `abc.tools.parser-rq-publication`.

- [ ] **Step 4: Implement the authenticated pure fold**

The namespace exposes only:

Define `publication-instrument-version` as
`"parser-rq-publication-v1"` and expose
`(derive-publication-envelope store manifest index identity)`.

Implementation order:

1. `capture/verify-manifest` once and use returned authenticated bytes.
2. Validate identity and require the exact instrument version.
3. Select index, policy, census, validator, schema, and identity members by
   unique logical identity; reject ambiguous locators or hashes.
4. Validate closed schemas before reading semantic fields.
5. Recompute policy/census/validator/index identities.
6. Require exact pinned corpus-id plus source-hash membership and record pairing;
   metadata's six-digit work id is never a record identity.
7. Require raw structure-check candidate keys to equal the policy set excluding
   `required_construct_census_valid`, compute that census check locally from
   authenticated counts, and project each parsed work exactly once.
8. Return unavailable if parsed is zero or eligible differs from parsed.
9. Otherwise return an envelope containing `:value`, `:identity_ref`, `:counts`,
   `:denominator_shrink_witnesses`, and `:failed_work_witnesses`.

Never reopen a locator and never accept caller-supplied decoded maps that differ
from authenticated bytes.

- [ ] **Step 5: Install the envelope without accepting a scalar bypass**

Add to `parser_release_qualification.clj`:

```clojure
(defn install-publication-observation [measurements envelope]
  (assoc measurements :publication_structure envelope))
```

Test that the installed envelope evaluates predicate 6 and participates in
coherence; wrong identity remains `:not-qualified`. Do not add a CLI flag that
accepts `--publication-structure 1.0`.

- [ ] **Step 6: Run focused checks**

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test \
  --focus abc.tools.parser-release-qualification-test
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add abc/src/abc/tools/parser_rq_publication.clj \
  abc/src/abc/tools/parser_release_qualification.clj \
  abc/test/abc/tools/parser_rq_publication_test.clj \
  abc/test/abc/tools/parser_release_qualification_test.clj
git commit -m "feat(parser-rq): derive publication structure observation"
```

---

### Task 7: Commit the drift fixture and wire schema, Nix, and governance checks

**Files:**
- Create: `abc/test/fixtures/parser-rq/publication-capture/manifest.json`
- Create: `abc/test/fixtures/parser-rq/publication-capture/store/**`
- Modify: `abc/test/abc/tools/parser_rq_publication_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/data/adr-evidence/parser-ir-publication-observation-catalog.edn`
- Modify: `abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md`
- Modify as generated: `abc/docs/adr/adr-evidence.edn`
- Modify as generated: `abc/docs/evidence/adr-runs/**`

**Interfaces:**
- Consumes: Tasks 1-5 complete instrument and the three pinned fixture works.
- Produces: byte-identical production-shaped fixture, live-read governance
  closure, and roadmap status. It does not modify the historical measurement
  bundle or claim an authoritative P5 observation.

- [ ] **Step 1: Write the failing drift test**

```clojure
(deftest committed-publication-capture-regenerates-byte-identically
  (let [committed (load-production-fixture)
        regenerated (regenerate-publication-fixture! (temp-dir))]
    (is (= (slurp (:manifest committed))
           (slurp (:manifest regenerated))))
    (is (= (fixture-file-hashes committed)
           (fixture-file-hashes regenerated)))
    (is (= (:envelope committed)
           (publication/derive-publication-envelope
            (:store regenerated) (:manifest-value regenerated)
            (:index-value regenerated) (:identity-value regenerated))))))
```

Expected initial failure: committed fixture path is absent.

- [ ] **Step 2: Generate, inspect, and commit only bounded fixture evidence**

Run the Task 5 capture command against the three small tracked sources. The
fixture store may contain only the small JSON/XML/text artifacts for those three
works. Inspect:

```bash
du -ah abc/test/fixtures/parser-rq/publication-capture | sort -h | tail
find abc/test/fixtures/parser-rq/publication-capture -type l -print
```

Expected: no symlinks, no corpus-scale file, and all files are required by the
manifest. Run regeneration twice and `cmp` manifests plus recursive hashes.

- [ ] **Step 3: Register schema/live reads**

Add the four schemas and policy/census fixtures to
`validate_design_bundle.clj` fixture validation. Add every new runtime input
read by Accepted ADR evidence to the parser-IR publication observation catalog;
do not hand-edit captured hashes.

- [ ] **Step 4: Mark only P2 implementation status in the roadmap**

Record P2 as implemented and drift-checked. State explicitly that P3, P4B, and
P5 authoritative capture/admission/promotion remain unstarted. Do not change ADR
0039 status or the historical `:instrument-missing` bundle in this task.

- [ ] **Step 5: Run governance recapture on hinoki**

Push the implementation branch to `hinoki.hyakutake-barbel.ts.net`, run the real
ADR evidence capture workflow from `.superpowers/sdd/task-6-report.md`, copy
back only generated committed evidence, and run:

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test
scripts/comment-hygiene-check.sh
NIX_CONFIG='eval-cache = false' just validate-migration
```

Expected: fixture tests PASS, comment hygiene exits zero, ADR governance reports
zero problems, and `just validate-migration` exits zero. Cache post-hook warnings
are non-fatal only when the command exit code is zero.

- [ ] **Step 6: Review the complete P2 range before commit**

Review from the parent of Task 1 through the working tree for:

- vacuous passes;
- scalar/manual observation bypasses;
- policy/validator semantic drift;
- schema-invalid versus join-invalid confusion;
- missing/duplicate/extra/cross-work membership;
- denominator shrink concealment;
- authenticate-then-reopen races; and
- accidental corpus-scale or machine-local artifacts.

Fix every Critical/Important finding and rerun the focused and full gates.

- [ ] **Step 7: Commit the drift/governance closure**

```bash
git add abc/test/fixtures/parser-rq/publication-capture \
  abc/test/abc/tools/parser_rq_publication_test.clj \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/data/adr-evidence/parser-ir-publication-observation-catalog.edn \
  abc/docs/adr/adr-evidence.edn abc/docs/evidence/adr-runs \
  abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md
git commit -m "test(parser-rq): drift-check publication structure instrument"
```

## Plan Self-Review Checklist

- Every approved design requirement maps to a task: non-vacuity (Tasks 2/3/5),
  joinability separation (Tasks 3/6), semantic-closure identity (Tasks 2/4/6),
  denominator disclosure (Task 6), capture/derive/drift (Tasks 5/6/7).
- Task interfaces agree on the names `publication_structure`,
  `publication_join_input_valid`, `validator_semantics_hash`, `policy_hash`,
  `census_hash`, and `derive-publication-envelope`.
- P2 stops at an implemented, drift-tested instrument. It does not mutate the
  historical measurement or perform P5's release capture.
- No task permits a hand-keyed predicate value, filesystem-discovered
  denominator, missing-evidence failure ratio, or vacuous zero-structure pass.
