# Parser RQ Publication Structure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace ADR 0039 predicate 6's `:instrument-missing` value with an
identity-bound, non-vacuous publication-structure observation derived from
authenticated per-work captures over the pinned qualification corpus.

**Architecture:** The existing Python validator remains the single reader of a
materialized publication bundle and emits separate publication-structure and
supporting-precondition blocks. ABC owns closed schemas, policy, fixture census,
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
  detailed bundle reader and validator.
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
      parsed -> publication block; failed|timeout -> disposition witness only
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
  "detailed_checks": {},
  "publication_structure": {"status": "pass", "failed_checks": []},
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

`publication_structure.status` is exactly `pass|fail`; unavailable is an
aggregate/derivation state, never a fabricated parsed-work verdict.

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

### Task 2: Correct and deepen the single publication-bundle validator

**Files:**
- Modify: `ab-validator/reports/parser-ir/publication-bundle-validate.py`
- Create: `ab-validator/reports/parser-ir/publication-validator-identity.py`
- Create: `ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py`
- Modify: `ab-validator/tests/parser-ir-publication-bundle-smoke.sh`
- Modify: `ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh`
- Create: `ab-validator/data/parser-rq-publication-validator-v1.json`

**Interfaces:**
- Consumes: explicit authoritative preservation schema, policy, census, and
  validator-identity paths.
- Produces: detailed evidence v2 with
  `publication_structure`, `supporting_preconditions`, `join_input`, `counts`,
  and validator/schema/policy identities.

- [ ] **Step 1: Write failing Python tests for the four review gaps**

Load the hyphenated CLI with `importlib.util.spec_from_file_location` and test
pure helpers before the CLI:

```python
def test_empty_schema_valid_preservation_fails_required_census(module, fixture):
    result = module.validate_values(
        **fixture(preservation_records=[]),
        required_constructs={"span_coordinates": 1},
    )
    assert result["checks"]["preservation_schema_valid"] is True
    assert result["publication_structure"] == {
        "status": "fail",
        "failed_checks": ["required_construct_census_valid"],
    }
    assert result["counts"]["preservation_records"] == 0

def test_schema_invalid_but_join_valid_ir_keeps_independent_structure_verdict(module, fixture):
    result = module.validate_values(**fixture(parser_ir_extra_field=True))
    assert result["supporting_preconditions"]["parser_ir_schema_valid"] is False
    assert result["join_input"] == {"status": "valid", "errors": []}
    assert result["publication_structure"]["status"] == "pass"

def test_join_invalid_ir_is_unavailable_not_structure_failure(module, fixture):
    result = module.validate_values(**fixture(node={"type": "text", "text": 7}))
    assert result["join_input"]["status"] == "invalid"
    assert result["publication_structure"]["status"] == "unavailable"

def test_supporting_source_status_does_not_change_structure(module, fixture):
    green = module.validate_values(**fixture(source_gate="pass"))
    red = module.validate_values(**fixture(source_gate="fail"))
    assert green["publication_structure"] == red["publication_structure"]
```

Also assert that a `0.2.0` sidecar and a `0.3.0` sidecar with an extra property
fail full JSON Schema validation, and that unknown/missing/extra policy check
names yield `unavailable` rather than changing membership.

- [ ] **Step 2: Run tests and verify current behavior fails**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py -q
```

Expected: FAIL because `validate_values`, joinability, census, and split verdict
blocks do not exist and the live validator still expects preservation `0.2.0`.

- [ ] **Step 3: Implement full schema validation and joinability**

Add required CLI options:

```python
parser.add_argument("--parser-ir-schema", required=True, type=pathlib.Path)
parser.add_argument("--preservation-schema", required=True, type=pathlib.Path)
parser.add_argument("--publication-policy", required=True, type=pathlib.Path)
parser.add_argument("--fixture-census", required=True, type=pathlib.Path)
parser.add_argument("--validator-identity", required=True, type=pathlib.Path)
```

Make `--source-region-summary` optional. Its absence or red status appears only
under `supporting_preconditions`.

Implement and call these exact pure interfaces from `validate_bundle`:
`publication_join_input_errors(parser_ir: object) -> list[str]`,
`validate_schema(instance: object, contract: dict[str, Any]) -> list[str]`,
`publication_counts(parser_ir: dict[str, Any], preservation: dict[str, Any],
tei_root: ET.Element) -> dict[str, Any]`,
`required_census_passes(counts: dict[str, Any], minimums: dict[str, int]) -> bool`,
and `project_publication_structure(checks: dict[str, bool], policy:
dict[str, Any], join_errors: list[str]) -> dict[str, Any]`.

`publication_join_input_errors` rejects unknown node types and malformed fields
used by `node_visible_body_text`; it must not silently turn an unknown node into
empty text. `validate_schema` uses `Draft202012Validator` with a format checker.

- [ ] **Step 4: Bind validator semantics, not only its filename**

The identity generator writes canonical JSON with validator id
`ab-validator/publication-bundle-validate/v2`, exact runtime coordinates from
`sys.version_info` and `importlib.metadata.version("jsonschema")`, and ordered source rows for
`publication-bundle-validate.py`, `reports/lib/evidence.py`,
`reports/lib/hashing.py`, `reports/lib/io.py`, `reports/lib/paths.py`, and
`reports/lib/source_region.py`. Each row contains the computed full SHA-256
identity. `validator_semantics_hash` is computed over the whole value with that
field removed.

The generator discovers no imports dynamically: it owns this reviewed closed
path list, rejects missing/extra configured sources, hashes the canonical value
without `validator_semantics_hash`, and supports `--check` and `--write`.

Run:

```bash
nix develop ./ab-validator# --command python \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  --repo-root ab-validator --write \
  --out ab-validator/data/parser-rq-publication-validator-v1.json
```

Expected: one canonical manifest whose recomputed hash equals its pinned hash.

- [ ] **Step 5: Update both smoke fixtures to v2**

Use real schema `0.3.0` values including required `parser_ir`, `tei`, `source`,
`producer`, and `mapping` blocks. Pass all explicit authority paths. Assert:

```bash
jq -e '.publication_structure.status == "pass"' "$summary_json"
jq -e '.supporting_preconditions.parser_ir_schema_valid == true' "$summary_json"
jq -e '.counts.preservation_records > 0' "$summary_json"
```

Add one empty-record fixture that remains schema-valid but has
`publication_structure.status == "fail"` and
`required_construct_census_valid == false`.

- [ ] **Step 6: Run focused and Nix checks**

```bash
nix develop ./ab-validator# --command python -m pytest \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py -q
bash ab-validator/tests/parser-ir-publication-bundle-smoke.sh
bash ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh
nix build ./ab-validator#checks.x86_64-linux.reports-pytest
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add ab-validator/reports/parser-ir/publication-bundle-validate.py \
  ab-validator/reports/parser-ir/publication-validator-identity.py \
  ab-validator/reports/parser-ir/tests/test_publication_bundle_validate.py \
  ab-validator/tests/parser-ir-publication-bundle-smoke.sh \
  ab-validator/tests/parser-ir-publication-bundle-batch-smoke.sh \
  ab-validator/data/parser-rq-publication-validator-v1.json
git commit -m "feat(parser-rq): separate publication structure verdict"
```

---

### Task 3: Freeze policy, non-vacuity census, and qualification-only metadata

**Files:**
- Create: `abc/data/parser-rq-publication-policy-v1.json`
- Create: `abc/data/parser-rq-publication-fixtures-v1.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/persons/rq-fixture.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000001_1/metadata-record.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000002_2/metadata-record.json`
- Create: `abc/test/fixtures/parser-rq/publication-inputs/000003_3/metadata-record.json`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `abc/test/abc/tools/parser_rq_publication_test.clj`

**Interfaces:**
- Consumes: Task 1 schemas, Task 2 validator manifest, pinned three-work corpus.
- Produces: immutable policy/census identities and deterministic metadata inputs.

- [ ] **Step 1: Write failing authority and non-vacuity tests**

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
  (is (= 1 (get-in fixture-contract [:works "000002_2"
                                     :required_constructs :gaiji_resolution]))))
```

- [ ] **Step 2: Run and verify missing data fails**

```bash
cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-publication-test
```

Expected: FAIL reading `data/parser-rq-publication-policy-v1.json`.

- [ ] **Step 3: Create policy and census values**

The policy structure list is exactly the ten names in the approved design plus
`required_construct_census_valid`. The latter is a predicate-6 structure check,
not a supporting flag. Supporting names are exactly
`parser_ir_schema_valid`, `source_region_coverage_valid`, and
`source_region_sidecar_role_available`.

The census minimums are exactly:

```json
{
  "000001_1": {"source_identity": 1, "mapping_identity": 1, "span_coordinates": 1},
  "000002_2": {"source_identity": 1, "mapping_identity": 1, "span_coordinates": 1, "gaiji_resolution": 1},
  "000003_3": {"source_identity": 1, "mapping_identity": 1, "span_coordinates": 1, "gaiji_resolution": 1}
}
```

Compute `policy_hash` and `census_hash` as JCS SHA-256 over each document with
its own hash field removed. Tests recompute rather than trust them.

- [ ] **Step 4: Add qualification-only metadata and person records**

Use person id `abc-000000000001`; titles are `Parser RQ Fixture 000001_1`,
`Parser RQ Fixture 000002_2`, and `Parser RQ Fixture 000003_3`; contributor
relation `著者`, and deterministic source provenance. Each metadata work id must
equal the six-digit card prefix (`000001`, `000002`, `000003`); the fixture
contract explicitly maps it to corpus ids `000001_1`, `000002_2`, and
`000003_3`. Include this non-authority marker in the fixture contract, not in
production schema fields:

```json
{"authority": "qualification-fixture-only", "bibliographic_authority": false}
```

Generate schema hashes and `person_record_hash` using the existing ABC hash
functions; do not copy example-work hashes. The test and capture command create
the runtime batch value from three explicit fixture-contract rows, fixed
`generated_at: 2026-07-17T00:00:00Z`, and no discovered paths.

- [ ] **Step 5: Prove deterministic three-work materialization**

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

- [ ] **Step 6: Commit**

```bash
git add abc/data/parser-rq-publication-*.json \
  abc/test/fixtures/parser-rq/publication-inputs \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/test/abc/tools/parser_rq_publication_test.clj
git commit -m "test(parser-rq): freeze publication qualification inputs"
```

---

### Task 4: Build explicit capture-to-CAS and closed-index production

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

### Task 5: Authenticate the closed fold and derive predicate 6 in ABC

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
                #(assoc-in % [:publication :publication_structure]
                           {:status "fail"
                            :failed_checks ["required_construct_census_valid"]}))]
    (is (= 0.6666M (:value result)))
    (is (= 1 (count (:failed_work_witnesses result))))))

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
6. Require exact pinned corpus membership and record/source pairing.
7. Fold dispositions and parsed structure results.
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

### Task 6: Commit the drift fixture and wire schema, Nix, and governance checks

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

Run the Task 4 capture command against the three small tracked sources. The
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
  joinability separation (Tasks 2/5), semantic-closure identity (Tasks 2/3/5),
  denominator disclosure (Task 5), capture/derive/drift (Tasks 4/5/6).
- Task interfaces agree on the names `publication_structure`,
  `publication_join_input_valid`, `validator_semantics_hash`, `policy_hash`,
  `census_hash`, and `derive-publication-envelope`.
- P2 stops at an implemented, drift-tested instrument. It does not mutate the
  historical measurement or perform P5's release capture.
- No task permits a hand-keyed predicate value, filesystem-discovered
  denominator, missing-evidence failure ratio, or vacuous zero-structure pass.
