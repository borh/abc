# Parser IR Identity Compatibility Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make ab-validator AAT-to-parser-IR outputs carry the producer identity and mapping evidence that ABC's adapter-scoped compatibility registry can validate.

**Architecture:** ABC remains the only compatibility-registry owner. ab-validator regenerates the mapping policy so adapter identity is no longer recorded as lost, emits parser-IR `derived_from`, computes the mapping document hash with the pinned ABC legacy canonicalization, and publishes per-adapter compatibility evidence candidates from the conversion audit. ABC then admits those exact identity tuples into `data/aat-parser-ir-compatibility.edn`.

**Tech Stack:** Rust (`ab-aat-to-parser-ir`), Python mapping generator (`reports/aat-fidelity/aat_parser_ir_mapping`), JSON Schema, jq, Clojure/EDN in `../abc`, Nix/Just smoke gates.

## Global Constraints

- Do not create a second compatibility registry in ab-validator; ABC's `data/aat-parser-ir-compatibility.edn` remains the source of truth.
- `mapping_hash` means the ABC legacy canonical SHA-256 of the mapping document, not the mapping schema hash.
- `parser_ir.derived_from` must not include `mapping_hash`; ABC combines `derived_from` with manifest input `mapping_hash`.
- Registry entries must use actual AAT metadata values from `meta.adapter` and `meta.adapter_version` (`aozora-rs`, `aozora2html`), not input directory labels such as `aozora-rs-adapter`.
- Adapter-version matching is exact. Any new `meta.adapter_version` tuple requires a fresh measured compatibility entry; do not add wildcard or prefix matching.
- Keep full-corpus runs out of flake checks. Flake checks must remain deterministic and must not read `/db`.
- Use 24 jobs for heavyweight corpus runs unless the operator explicitly overrides it.
- Resolve the aozora2html full-corpus AAT directory through `AB_AOZORA2HTML_AAT_DIR`, defaulting to `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`. If the corpus is refreshed under a new timestamp, update that environment variable or the stable symlink it points at before running the plan.
- Before editing `../abc`, run `git -C ../abc status --short --branch` and preserve unrelated dirty files.
- Keep `data/abc-schemas/` byte-for-byte aligned with the ABC schema snapshot used by the mapping generator unless a separate ABC schema sync is explicitly planned.

---

## Evidence Ledger

| Claim | Type | Source | Confidence | Impact if wrong |
|---|---|---|---:|---|
| Current mapping `0.1.1` records `meta.adapter` and `meta.adapter_version` as LOSS. | Observation | `data/aat-to-parser-ir-mapping-v1.json` rules `L-49`, `L-50` | High | Converter would emit `derived_from` while the mapping artifact still says producer identity is lost. |
| ABC requires `derived_from` when manifest inputs carry `mapping_hash`. | Observation | `../abc/src/abc/tools/validate_design_bundle.clj` | High | Current ab-validator outputs cannot satisfy ABC compatibility checks. |
| Registry matching is exact over adapter, adapter version, mapping id/version/hash, mapping schema hash, parser-IR schema id/hash. | Observation | `../abc/docs/adr/0023-owned-aat-parser-ir-mapping.md` and `aat_parser_ir_compat.clj` | High | Any wildcard or label mismatch silently authorizes the wrong artifact. |
| Adapter metadata is stable in the measured corpora. | Observation | `jq` over full AAT corpora | High | If versions varied, registry entries would need to be generated per adapter-version tuple. |
| Existing audit summary lacks mapping document hash and per-identity compatibility candidates. | Observation | `crates/ab-aat-to-parser-ir/src/audit.rs` | High | ABC registry updates would require hand-copying evidence. |

## File Structure

- Modify `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`: remove stale adapter identity LOSS records.
- Modify `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`: default generated mapping version to `0.2.0`.
- Modify `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`: report identity projection policy and keep generated summaries explicit.
- Modify `tests/aat-parser-ir-mapping-smoke.sh` and `tests/aat-parser-ir-mapping-policy-smoke.sh`: assert mapping `0.2.0` policy and absence of adapter identity LOSS.
- Regenerate `data/aat-to-parser-ir-mapping-v1.json`: keep path stable, bump internal `mapping_version`.
- Modify `crates/ab-aat-to-parser-ir/src/schema.rs`: expose a document-hash helper using `abc_legacy_json_c14n_v0`.
- Modify `crates/ab-aat-to-parser-ir/src/mapping.rs`: compute and retain mapping document hash outside the serialized mapping document.
- Modify `crates/ab-aat-to-parser-ir/src/convert.rs`: emit top-level parser-IR `derived_from`.
- Modify `crates/ab-aat-to-parser-ir/src/audit.rs`: emit mapping hash and per-identity compatibility candidates.
- Modify `crates/ab-aat-to-parser-ir/src/main.rs`: add optional `--compat-edn-out` for audit output.
- Modify `justfile`: make the standard full-audit recipe emit the compatibility EDN and honor `AB_AOZORA2HTML_AAT_DIR`.
- Modify `flake.nix`: provide Clojure to the parser-IR smoke check so generated EDN is parsed by `clojure.edn/read-string`.
- Modify `tests/aat-to-parser-ir-cli-smoke.sh`: assert `derived_from`, mapping hash, and compatibility candidate shape.
- Modify `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md`: supersede the old "derived_from unset" decision.
- In `../abc`, modify `data/aat-parser-ir-compatibility.edn` and registry tests only after ab-validator emits measured candidates.

---

### Task 1: Regenerate Mapping Policy With Producer Identity Projected

**Files:**
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`
- Modify: `tests/aat-parser-ir-mapping-policy-smoke.sh`
- Modify: `tests/aat-parser-ir-mapping-smoke.sh`
- Regenerate: `data/aat-to-parser-ir-mapping-v1.json`

**Interfaces:**
- Consumes: full AAT corpora under `scratch/morph-full-corpus/aats/aozora-rs-adapter` and `$AB_AOZORA2HTML_AAT_DIR` (defaulting to the current dated `/db` full-corpus path).
- Produces: mapping artifact with `mapping_version == "0.2.0"` and no `meta.adapter` or `meta.adapter_version` LOSS rules.

- [ ] **Step 1: Write the failing mapping policy assertions**

In `tests/aat-parser-ir-mapping-policy-smoke.sh`, change both `--mapping-version 0.1.1` and the jq version assertion to `0.2.0`, then add these assertions after the current `jq -e '.mapping_version == ...'` line:

```bash
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.identity_projection.parser_ir_pointer == "derived_from"' "$out_dir/summary.json"
jq -e '.identity_projection.aat_pointers == ["version", "meta.adapter", "meta.adapter_version"]' "$out_dir/summary.json"
```

In `tests/aat-parser-ir-mapping-smoke.sh`, change both `--mapping-version 0.1.1` invocations and the jq version assertion to `0.2.0`, then add:

```bash
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
```

- [ ] **Step 2: Run the policy smoke and verify it fails for the old artifact policy**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
```

Expected: FAIL because `mapping_version` is still `0.1.1` or because `meta.adapter` / `meta.adapter_version` LOSS records still exist.

- [ ] **Step 3: Remove stale adapter identity LOSS records from the Python mapper**

In `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`, replace this block in `map_meta_source`:

```python
    # Producer identity / fidelity metadata has no parser-IR home.
    for f in ("adapter", "adapter_version", "parse_complete"):
        ledger_list.append(ledger("LOSS", f"meta.{f}", "(none)",
                                  f"{f}={meta.get(f)!r} dropped; parser-IR carries no producer identity / parse status"))
```

with:

```python
    # Producer identity now projects to parser-IR derived_from. parse_complete
    # remains adapter-fidelity metadata preserved in the divergence bundle.
    ledger_list.append(ledger("LOSS", "meta.parse_complete", "(none)",
                              f"parse_complete={meta.get('parse_complete')!r} preserved in divergence bundle; parser-IR derived_from does not model parse completeness"))
```

- [ ] **Step 4: Bump generator default mapping version**

In `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`, change both defaults from `0.1.1` to `0.2.0`:

```python
def build_mapping_document_from_counts(
    rule_counts,
    first_path_by_rule,
    first_note_by_rule,
    repo_root=REPO_ROOT,
    mapping_version="0.2.0",
):
```

```python
def build_mapping_document(ledger_entries, repo_root=REPO_ROOT, mapping_version="0.2.0"):
```

Also change the CLI default:

```python
    parser.add_argument("--mapping-version", default="0.2.0")
```

In `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`, change:

```python
    parser.add_argument("--mapping-version", default="0.1.1")
```

to:

```python
    parser.add_argument("--mapping-version", default="0.2.0")
```

- [ ] **Step 5: Add identity projection metadata to generator summaries**

In `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`, add this object to the `summary` dictionary:

```python
        "identity_projection": {
            "parser_ir_pointer": "derived_from",
            "aat_pointers": ["version", "meta.adapter", "meta.adapter_version"],
            "mapping_pointers": ["mapping_id", "mapping_version", "mapping_schema_hash"],
            "mapping_hash_source": "manifest-inputs.mapping_hash",
        },
```

In `write_report`, after the schema hashes section, add:

```python
            "## Identity Projection",
            "",
            "- `version`, `meta.adapter`, and `meta.adapter_version` project into parser-IR `derived_from`.",
            "- `mapping_id`, `mapping_version`, and `mapping_schema_hash` project from the mapping document into parser-IR `derived_from`.",
            "- `mapping_hash` remains an external manifest input and is not stored inside parser-IR `derived_from`.",
            "",
```

- [ ] **Step 6: Regenerate the checked-in mapping artifact**

Run:

```bash
aozora2html_dir="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
uv run --isolated --no-project --with 'jsonschema>=4.0' \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  --aat-dir scratch/morph-full-corpus/aats/aozora-rs-adapter \
  --aat-dir "$aozora2html_dir" \
  --abc-root ../abc \
  --mapping-version 0.2.0 \
  --out data/aat-to-parser-ir-mapping-v1.json \
  --summary-json docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.summary.json \
  --report-md docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.md
```

Expected: command exits 0 and `data/aat-to-parser-ir-mapping-v1.json` validates against ABC's mapping schema.

- [ ] **Step 7: Verify generator smokes**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-parser-ir-mapping-smoke.sh
```

Expected: both PASS.

- [ ] **Step 8: Commit**

```bash
git add reports/aat-fidelity/aat_parser_ir_mapping/mapper.py \
  reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  tests/aat-parser-ir-mapping-policy-smoke.sh \
  tests/aat-parser-ir-mapping-smoke.sh \
  data/aat-to-parser-ir-mapping-v1.json \
  docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.summary.json \
  docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.md
git commit -m "feat: project parser-ir provenance in mapping policy"
```

### Task 2: Compute Mapping Document Hash in Rust

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/schema.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/mapping.rs`

**Interfaces:**
- Consumes: `schema::abc_legacy_json_c14n_v0`.
- Produces: `schema::abc_legacy_json_hash(&Value) -> Result<String>` and `MappingDocument::document_hash: String`.

- [ ] **Step 1: Add a failing Rust unit test for mapping document hash**

Append this test module to `crates/ab-aat-to-parser-ir/src/mapping.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{abc_legacy_json_hash, read_json};
    use std::path::PathBuf;

    fn repo_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
    }

    #[test]
    fn from_path_computes_mapping_document_hash() {
        let path = repo_root().join("data/aat-to-parser-ir-mapping-v1.json");
        let mapping = MappingDocument::from_path(&path).expect("mapping loads");
        let value = read_json(&path).expect("mapping json reads");
        let expected = abc_legacy_json_hash(&value).expect("hash computes");
        assert_eq!(mapping.document_hash, expected);
        assert!(mapping.document_hash.starts_with("sha256:"));
        assert_eq!(mapping.document_hash.len(), "sha256:".len() + 64);
    }
}
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
cargo test -p ab-aat-to-parser-ir mapping::tests::from_path_computes_mapping_document_hash
```

Expected: FAIL because `abc_legacy_json_hash` and `MappingDocument::document_hash` do not exist.

- [ ] **Step 3: Add the hash helper**

In `crates/ab-aat-to-parser-ir/src/schema.rs`, replace `schema_hash` with a general helper plus alias:

```rust
pub fn abc_legacy_json_hash(value: &Value) -> Result<String> {
    let payload = abc_legacy_json_c14n_v0(value)?;
    let mut hasher = Sha256::new();
    hasher.update(payload);
    Ok(format!("sha256:{:x}", hasher.finalize()))
}

pub fn schema_hash(value: &Value) -> Result<String> {
    abc_legacy_json_hash(value)
}
```

- [ ] **Step 4: Store the document hash outside serialized mapping fields**

In `crates/ab-aat-to-parser-ir/src/mapping.rs`, import `abc_legacy_json_hash`:

```rust
use crate::schema::{SchemaSet, abc_legacy_json_hash, schema_hash, validate_value};
```

Add a skipped field to `MappingDocument`:

```rust
    #[serde(skip)]
    pub document_hash: String,
```

Replace `MappingDocument::from_path` with:

```rust
    pub fn from_path(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        let value: Value =
            serde_json::from_str(&text).with_context(|| format!("failed to parse {}", path.display()))?;
        let document_hash = abc_legacy_json_hash(&value)
            .with_context(|| format!("failed to hash {}", path.display()))?;
        let mut document: Self = serde_json::from_value(value)
            .with_context(|| format!("failed to decode {}", path.display()))?;
        document.document_hash = document_hash;
        Ok(document)
    }
```

- [ ] **Step 5: Verify the test passes**

Run:

```bash
cargo test -p ab-aat-to-parser-ir mapping::tests::from_path_computes_mapping_document_hash
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/schema.rs crates/ab-aat-to-parser-ir/src/mapping.rs
git commit -m "feat: compute parser-ir mapping document hash"
```

### Task 3: Emit Parser-IR `derived_from`

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `tests/aat-to-parser-ir-cli-smoke.sh`

**Interfaces:**
- Consumes: `MappingDocument` fields and AAT top-level `version`, `meta.adapter`, `meta.adapter_version`.
- Produces: parser-IR top-level `derived_from`.

- [ ] **Step 1: Add failing CLI smoke assertions**

In `tests/aat-to-parser-ir-cli-smoke.sh`, change:

```bash
jq -e '.mapping.mapping_version == "0.1.1"' "$out_dir/divergence.json"
```

to:

```bash
jq -e '.mapping.mapping_version == "0.2.0"' "$out_dir/divergence.json"
```

After the existing parser-IR `schema_id` jq assertion, add:

```bash
jq -e '.derived_from.aat_version == 1' "$out_dir/parser-ir.json"
jq -e '.derived_from.aat_adapter == "fixture"' "$out_dir/parser-ir.json"
jq -e '.derived_from.aat_adapter_version == "fixture 0.1.0"' "$out_dir/parser-ir.json"
jq -e '.derived_from.mapping_id == "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$out_dir/parser-ir.json"
jq -e '.derived_from.mapping_version == "0.2.0"' "$out_dir/parser-ir.json"
jq -e '.derived_from.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/parser-ir.json"
jq -e 'all(.records[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/divergence.json"
jq -e 'any(.records[]; .aat_pointer == "meta.parse_complete")' "$out_dir/divergence.json"
```

- [ ] **Step 2: Run the smoke and verify it fails**

Run:

```bash
bash tests/aat-to-parser-ir-cli-smoke.sh
```

Expected: FAIL because parser-IR has no `derived_from`.

- [ ] **Step 3: Stop recording adapter identity as source LOSS**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, replace:

```rust
    for field in ["adapter", "adapter_version", "parse_complete"] {
```

with:

```rust
    for field in ["parse_complete"] {
```

- [ ] **Step 4: Add the `derived_from` helper**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add this helper near `map_source`:

```rust
fn derived_from(aat: &Value, mapping: &MappingDocument) -> Result<Value> {
    let meta = &aat["meta"];
    let Some(aat_version) = aat["version"].as_u64() else {
        bail!("AAT version is required for parser-IR derived_from");
    };
    let Some(adapter) = meta["adapter"].as_str() else {
        bail!("AAT meta.adapter is required for parser-IR derived_from");
    };
    Ok(json!({
        "aat_version": aat_version,
        "aat_adapter": adapter,
        "aat_adapter_version": meta.get("adapter_version").and_then(Value::as_str),
        "mapping_id": mapping.mapping_id,
        "mapping_version": mapping.mapping_version,
        "mapping_schema_hash": mapping.mapping_schema_hash,
    }))
}
```

- [ ] **Step 5: Insert `derived_from` into parser-IR output**

In `convert_preflighted`, change parser-IR construction to include `derived_from` directly after `schema_hash`:

```rust
    let parser_ir = json!({
        "schema_id": mapping.target_parser_ir_schema_id,
        "schema_hash": mapping.target_parser_ir_schema_hash,
        "derived_from": derived_from(&aat, mapping)?,
        "source": source,
        "nodes": nodes,
        "warnings": warnings,
        "errors": [],
    });
```

- [ ] **Step 6: Verify CLI smoke passes**

Run:

```bash
bash tests/aat-to-parser-ir-cli-smoke.sh
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs tests/aat-to-parser-ir-cli-smoke.sh
git commit -m "feat: emit parser-ir mapping provenance"
```

### Task 4: Publish Compatibility Evidence From the Audit

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/audit.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs`
- Modify: `tests/aat-to-parser-ir-cli-smoke.sh`
- Modify: `flake.nix`
- Modify: `justfile`

**Interfaces:**
- Consumes: `ConversionOutput.parser_ir.derived_from`, `MappingDocument.document_hash`, divergence records.
- Produces: audit summary field `compatibility_candidates` and optional EDN output accepted by ABC registry syntax.

- [ ] **Step 1: Add failing smoke assertions for audit identity evidence**

In `tests/aat-to-parser-ir-cli-smoke.sh`, after the audit summary totals jq assertions, add:

```bash
jq -e '.mapping.mapping_hash | test("^sha256:[0-9a-f]{64}$")' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates | length == 1' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].aat_adapter == "fixture"' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].aat_adapter_version == "fixture 0.1.0"' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].mapping_hash == .mapping.mapping_hash' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].evidence_scope.evidence_type == "conversion-audit"' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].evidence_scope.files_scanned == 1' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].evidence_scope.files_succeeded == 1' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].evidence_scope.files_failed == 0' "$out_dir/audit-summary.json"
jq -e '.compatibility_candidates[0].evidence_scope.unsupported_occurrences == 0' "$out_dir/audit-summary.json"
```

Also extend `audit_args` with:

```bash
  --compat-edn-out "$out_dir/compatibility-candidates.edn"
```

and add:

```bash
grep -n ':evidence_type :conversion-audit' "$out_dir/compatibility-candidates.edn"
grep -n ':aat_adapter "fixture"' "$out_dir/compatibility-candidates.edn"
COMPAT_EDN="$out_dir/compatibility-candidates.edn" clojure -e "(require '[clojure.edn :as edn]) (edn/read-string (slurp (System/getenv \"COMPAT_EDN\")))"
```

- [ ] **Step 2: Run the smoke and verify it fails**

Run:

```bash
bash tests/aat-to-parser-ir-cli-smoke.sh
```

Expected: FAIL because `mapping.mapping_hash`, `compatibility_candidates`, and `--compat-edn-out` do not exist.

- [ ] **Step 3: Add audit CLI argument**

In `crates/ab-aat-to-parser-ir/src/main.rs`, add to `Command::AuditCorpus`:

```rust
        #[arg(long)]
        compat_edn_out: Option<PathBuf>,
```

Pass it into `CorpusAuditConfig`:

```rust
                compat_edn_out,
```

In `crates/ab-aat-to-parser-ir/src/audit.rs`, add to `CorpusAuditConfig`:

```rust
    pub compat_edn_out: Option<PathBuf>,
```

- [ ] **Step 4: Add Clojure to the flake smoke check**

In `flake.nix`, add `pkgs.clojure` to the `abAatToParserIrCheck.nativeBuildInputs` list:

```nix
              nativeBuildInputs = [
                pkgs.clojure
                pkgs.jq
                pythonWithAatSchemaDeps
              ];
```

This keeps `tests/aat-to-parser-ir-cli-smoke.sh` able to parse the generated EDN inside the sandbox-pure Nix check.

- [ ] **Step 5: Extend audit data types**

In `crates/ab-aat-to-parser-ir/src/audit.rs`, add `mapping_hash` to `MappingSummary`:

```rust
    mapping_hash: String,
```

Add these serializable structs:

```rust
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize)]
struct CompatibilityIdentity {
    aat_version: u64,
    aat_adapter: String,
    aat_adapter_version: Option<String>,
    mapping_id: String,
    mapping_version: String,
    mapping_hash: String,
    mapping_schema_hash: String,
    parser_ir_schema_id: String,
    parser_ir_schema_hash: String,
}

#[derive(Debug, Serialize)]
struct CompatibilityCandidate {
    aat_version: u64,
    aat_adapter: String,
    aat_adapter_version: Option<String>,
    mapping_id: String,
    mapping_version: String,
    mapping_hash: String,
    mapping_schema_hash: String,
    parser_ir_schema_id: String,
    parser_ir_schema_hash: String,
    evidence_scope: CompatibilityEvidenceScope,
    compatibility: String,
}

#[derive(Debug, Default, Serialize)]
struct CompatibilityEvidenceScope {
    evidence_type: String,
    adapter: String,
    adapter_version: Option<String>,
    corpus: String,
    files_scanned: u64,
    files_succeeded: u64,
    files_failed: u64,
    parser_ir_nodes: u64,
    divergence_records: u64,
    divergence_occurrences: u64,
    rules_total: u64,
    rules_emitted: u64,
    rules_missing: u64,
    unsupported_occurrences: u64,
}
```

Add `compatibility_candidates` to `AuditSummary`:

```rust
    compatibility_candidates: Vec<CompatibilityCandidate>,
```

Add to `FileSuccess`:

```rust
    compatibility_identity: CompatibilityIdentity,
```

- [ ] **Step 6: Extract compatibility identity from conversion output**

Change `audit_file` to accept the mapping hash and pass it through so `summarize_output` can attach it to `CompatibilityIdentity`:

```rust
fn audit_file(file: &AuditFile, converter: &PreparedConverter, mapping_hash: &str) -> AuditFileResult {
```

Before building `results` in `run_audit`, clone the mapping hash:

```rust
let mapping_hash = mapping.document_hash.clone();
```

Change the parallel call sites in `run_audit` from:

```rust
.map(|file| audit_file(file, &converter))
```

to:

```rust
.map(|file| audit_file(file, &converter, mapping_hash.as_str()))
```

Change `summarize_output` to accept the mapping hash:

```rust
fn summarize_output(
    output: &ab_aat_to_parser_ir::ConversionOutput,
    mapping_hash: &str,
) -> FileSuccess {
```

Then build `CompatibilityIdentity` from parser-IR plus the mapping hash:

```rust
    let derived_from = &output.parser_ir["derived_from"];
    let compatibility_identity = CompatibilityIdentity {
        aat_version: derived_from["aat_version"]
            .as_u64()
            .expect("validated parser-IR derived_from.aat_version"),
        aat_adapter: derived_from["aat_adapter"]
            .as_str()
            .expect("validated parser-IR derived_from.aat_adapter")
            .to_owned(),
        aat_adapter_version: derived_from
            .get("aat_adapter_version")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        mapping_id: derived_from["mapping_id"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_id")
            .to_owned(),
        mapping_version: derived_from["mapping_version"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_version")
            .to_owned(),
        mapping_hash: mapping_hash.to_owned(),
        mapping_schema_hash: derived_from["mapping_schema_hash"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_schema_hash")
            .to_owned(),
        parser_ir_schema_id: output.parser_ir["schema_id"]
            .as_str()
            .expect("validated parser-IR schema_id")
            .to_owned(),
        parser_ir_schema_hash: output.parser_ir["schema_hash"]
            .as_str()
            .expect("validated parser-IR schema_hash")
            .to_owned(),
    };
```

- [ ] **Step 7: Compute compatibility candidates in `summarize`**

In `summarize`, add a `BTreeMap<CompatibilityIdentity, CompatibilityEvidenceScope>` and update it only for successful files. Failed files continue to be counted in `totals`, `by_corpus`, `top_errors`, and `failure_samples`; do not emit compatibility candidates for failures because a failed conversion has no validated `derived_from` identity.

For each success, update:

```rust
let evidence = evidence_by_identity
    .entry(success.compatibility_identity.clone())
    .or_insert_with(|| CompatibilityEvidenceScope {
        evidence_type: "conversion-audit".to_owned(),
        adapter: success.compatibility_identity.aat_adapter.clone(),
        adapter_version: success.compatibility_identity.aat_adapter_version.clone(),
        corpus: result.corpus.clone(),
        ..CompatibilityEvidenceScope::default()
    });
evidence.files_scanned += 1;
evidence.files_succeeded += 1;
evidence.parser_ir_nodes += success.parser_ir_nodes;
evidence.divergence_records += success.divergence_records;
evidence.divergence_occurrences += success.divergence_occurrences;
evidence.rules_total = mapping.transform_rule_descriptions.len() as u64;
evidence.unsupported_occurrences += success.category_occurrences.get("UNSUPPORTED").copied().unwrap_or(0);
```

Track `BTreeSet<String>` emitted rule ids per identity while processing successes. After all results, compute per-identity emitted/missing rule counts from the union of emitted rule ids for that identity. The invariant is:

```rust
evidence.rules_emitted + evidence.rules_missing == evidence.rules_total
```

Then materialize:

```rust
let compatibility_candidates = evidence_by_identity
    .into_iter()
    .map(|(identity, evidence_scope)| CompatibilityCandidate {
        aat_version: identity.aat_version,
        aat_adapter: identity.aat_adapter,
        aat_adapter_version: identity.aat_adapter_version,
        mapping_id: identity.mapping_id,
        mapping_version: identity.mapping_version,
        mapping_hash: identity.mapping_hash,
        mapping_schema_hash: identity.mapping_schema_hash,
        parser_ir_schema_id: identity.parser_ir_schema_id,
        parser_ir_schema_hash: identity.parser_ir_schema_hash,
        evidence_scope,
        compatibility: "lossy".to_owned(),
    })
    .collect::<Vec<_>>();
```

- [ ] **Step 8: Add mapping hash to mapping summary**

In the `MappingSummary` construction, add:

```rust
            mapping_hash: mapping.document_hash,
```

Because `mapping` is consumed in `summarize`, clone the hash before moving fields if the compiler requires it:

```rust
let mapping_hash = mapping.document_hash.clone();
```

- [ ] **Step 9: Write optional EDN candidate output**

In `write_outputs`, if `config.compat_edn_out` is present, write an EDN map whose top-level key is `:entries` and whose value is the generated compatibility candidate vector.

Add these helpers in `crates/ab-aat-to-parser-ir/src/audit.rs`:

```rust
fn edn_quote(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            ch if ch.is_control() => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn edn_option_string(value: Option<&str>) -> String {
    value.map(edn_quote).unwrap_or_else(|| "nil".to_owned())
}

fn render_compatibility_candidates_edn(summary: &AuditSummary) -> String {
    let mut out = String::from("{:entries\n [");
    for (index, candidate) in summary.compatibility_candidates.iter().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        let scope = &candidate.evidence_scope;
        out.push_str(&format!(
            "{{:aat_version {}\n  :aat_adapter {}\n  :aat_adapter_version {}\n  :mapping_id {}\n  :mapping_version {}\n  :mapping_hash {}\n  :mapping_schema_hash {}\n  :parser_ir_schema_id {}\n  :parser_ir_schema_hash {}\n  :evidence_scope {{:evidence_type :conversion-audit\n                   :adapter {}\n                   :adapter_version {}\n                   :corpus {}\n                   :files_scanned {}\n                   :files_succeeded {}\n                   :files_failed {}\n                   :parser_ir_nodes {}\n                   :divergence_records {}\n                   :divergence_occurrences {}\n                   :rules_total {}\n                   :rules_emitted {}\n                   :rules_missing {}\n                   :unsupported_occurrences {}}}\n  :compatibility {}}}",
            candidate.aat_version,
            edn_quote(&candidate.aat_adapter),
            edn_option_string(candidate.aat_adapter_version.as_deref()),
            edn_quote(&candidate.mapping_id),
            edn_quote(&candidate.mapping_version),
            edn_quote(&candidate.mapping_hash),
            edn_quote(&candidate.mapping_schema_hash),
            edn_quote(&candidate.parser_ir_schema_id),
            edn_quote(&candidate.parser_ir_schema_hash),
            edn_quote(&scope.adapter),
            edn_option_string(scope.adapter_version.as_deref()),
            edn_quote(&scope.corpus),
            scope.files_scanned,
            scope.files_succeeded,
            scope.files_failed,
            scope.parser_ir_nodes,
            scope.divergence_records,
            scope.divergence_occurrences,
            scope.rules_total,
            scope.rules_emitted,
            scope.rules_missing,
            scope.unsupported_occurrences,
            edn_quote(&candidate.compatibility),
        ));
    }
    out.push_str("]}\n");
    out
}
```

Then extend `write_outputs`:

```rust
    if let Some(path) = &config.compat_edn_out {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::write(path, render_compatibility_candidates_edn(summary))
            .with_context(|| format!("failed to write {}", path.display()))?;
    }
```

- [ ] **Step 10: Render mapping hash and candidate table in Markdown**

In `render_report`, add:

```rust
    out.push_str(&format!(
        "- mapping_hash: `{}`\n",
        summary.mapping.mapping_hash
    ));
```

Add a "Compatibility Candidates" section with columns:

```text
adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences
```

- [ ] **Step 11: Update the standard full-audit recipe**

In `justfile`, add a stable aozora2html AAT directory variable near the existing path variables:

```make
aozora2html_full_aat_dir := env_var_or_default("AB_AOZORA2HTML_AAT_DIR", ab_db_root + "/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter")
```

Change the full-audit recipe signature to include the EDN output path:

```make
aat-to-parser-ir-full-audit JOBS="24" REPORT_MD="docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json" COMPAT_EDN="docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn":
```

In that recipe, replace the hardcoded aozora2html AAT path with:

```make
		--aat-dir "{{aozora2html_full_aat_dir}}" \
```

and add:

```make
		--compat-edn-out "{{repo_root}}/{{COMPAT_EDN}}" \
```

- [ ] **Step 12: Verify smoke passes**

Run:

```bash
bash tests/aat-to-parser-ir-cli-smoke.sh
cargo test -p ab-aat-to-parser-ir
just aat-to-parser-ir-flake-smoke
```

Expected: all PASS.

- [ ] **Step 13: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/audit.rs \
  crates/ab-aat-to-parser-ir/src/main.rs \
  tests/aat-to-parser-ir-cli-smoke.sh \
  flake.nix \
  justfile
git commit -m "feat: publish parser-ir compatibility evidence"
```

### Task 5: Rerun Full-Corpus Conversion Audit

**Files:**
- Regenerate: `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- Regenerate: `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`
- Create: `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`

**Interfaces:**
- Consumes: release build of `ab-aat-to-parser-ir`.
- Produces: measured compatibility evidence for ABC.

- [ ] **Step 1: Run full audit with 24 jobs**

Run:

```bash
export AB_AOZORA2HTML_AAT_DIR="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
just aat-to-parser-ir-full-audit JOBS=24
```

Expected: `audited 35583 AAT files: 35583 succeeded, 0 failed`.

- [ ] **Step 2: Verify identity candidates**

Run:

```bash
jq -e '.totals.files_attempted == 35583' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e '.totals.files_failed == 0' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e '.compatibility_candidates | length == 2' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e 'any(.compatibility_candidates[]; .aat_adapter == "aozora-rs" and .aat_adapter_version == "aozora-rs-adapter 0.1.0 2b4e8d1")' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e 'any(.compatibility_candidates[]; .aat_adapter == "aozora2html" and .aat_adapter_version == "aozora2html-adapter 0.1.0 gem-3.0.1")' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e 'all(.compatibility_candidates[]; .evidence_scope.files_failed == 0)' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e 'all(.compatibility_candidates[]; .evidence_scope.rules_emitted + .evidence_scope.rules_missing == .evidence_scope.rules_total)' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
grep -n ':aat_adapter "aozora-rs"' docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
grep -n ':aat_adapter "aozora2html"' docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
COMPAT_EDN="docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn" clojure -e "(require '[clojure.edn :as edn]) (edn/read-string (slurp (System/getenv \"COMPAT_EDN\")))"
```

Expected: all PASS.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md \
  docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json \
  docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
git commit -m "docs: refresh parser-ir compatibility evidence"
```

### Task 6: Admit Measured Compatibility Entries in ABC

**Files in `../abc`:**
- Modify: `data/aat-parser-ir-compatibility.edn`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`
- Usually unchanged: `src/abc/tools/aat_parser_ir_compat.clj`

**Interfaces:**
- Consumes: `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn` from ab-validator.
- Produces: ABC registry entries that accept current ab-validator mapping `0.2.0` outputs.

- [ ] **Step 1: Inspect and preserve ABC dirty state**

Run:

```bash
git -C ../abc status --short --branch
git -C ../abc diff --stat
```

Expected: identify existing unrelated changes before editing. Do not overwrite untracked files or unrelated diffs.

- [ ] **Step 2: Verify ABC registry validation already supports conversion-audit evidence**

Run:

```bash
rg -n ':conversion-audit|required-conversion-audit-evidence-keys|files_succeeded|rules_emitted|unsupported_occurrences' \
  src/abc/tools/aat_parser_ir_compat.clj \
  test/abc/tools/validate_design_bundle_test.clj \
  data/aat-parser-ir-compatibility.edn
```

Expected: current ABC already has `:conversion-audit` support, including `files_scanned == files_succeeded + files_failed` and `rules_total == rules_emitted + rules_missing` validation. If this command fails in a future checkout, add the validation rules below before admitting new entries:

```clojure
(= (:adapter scope) (:aat_adapter entry))
(= (:adapter_version scope) (:aat_adapter_version entry))
(= (:files_scanned scope) (+ (:files_succeeded scope) (:files_failed scope)))
(= (:rules_total scope) (+ (:rules_emitted scope) (:rules_missing scope)))
```

Keep existing `:mapping-generation` evidence support for old entries if old registry entries remain.

- [ ] **Step 3: Append measured entries**

From `../abc`, capture the measured mapping hash from ab-validator's full-audit summary:

```bash
expected_mapping_hash="$(jq -r '.mapping.mapping_hash' ../ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json)"
test -n "$expected_mapping_hash"
```

Open `../ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn` and copy its two entry maps into `data/aat-parser-ir-compatibility.edn`.

The final ABC registry should still be a single map:

```edn
{:entries
 [;; old entries may remain here
  ;; new aozora-rs 0.2.0 conversion-audit entry
  ;; new aozora2html 0.2.0 conversion-audit entry
  ]}
```

Do not add wildcard entries and do not use `aozora-rs-adapter` or `aozora2html-adapter` as `:aat_adapter` for the new entries.

Verify that the registry contains the measured mapping hash:

```bash
grep -n ":mapping_hash \"$expected_mapping_hash\"" data/aat-parser-ir-compatibility.edn
```

Expected: exactly the new `0.2.0` entries use this hash.

- [ ] **Step 4: Update ABC unit tests to assert current entries**

In `../abc/test/abc/tools/validate_design_bundle_test.clj`, add assertions to `aat-parser-ir-compatibility-test` that derive the mapping hash from the registry entry instead of hard-coding it:

```clojure
(let [entry-for (fn [adapter adapter-version]
                  (->> (:entries registry)
                       (filter #(and (= adapter (:aat_adapter %))
                                     (= adapter-version (:aat_adapter_version %))
                                     (= "0.2.0" (:mapping_version %))))
                       first))
      aozora-rs-entry (entry-for "aozora-rs" "aozora-rs-adapter 0.1.0 2b4e8d1")
      aozora2html-entry (entry-for "aozora2html" "aozora2html-adapter 0.1.0 gem-3.0.1")]
  (is (seq aozora-rs-entry) "missing current aozora-rs registry entry")
  (is (seq aozora2html-entry) "missing current aozora2html registry entry")
  (is (= (:mapping_hash aozora-rs-entry) (:mapping_hash aozora2html-entry))
      "both adapter entries must point at the same measured mapping document")
  (doseq [entry [aozora-rs-entry aozora2html-entry]]
    (is (true? (compat/compatible? registry (select-keys entry compat/match-keys))))
    (is (= :conversion-audit (get-in entry [:evidence_scope :evidence_type])))
    (is (= (get-in entry [:evidence_scope :files_scanned])
           (+ (get-in entry [:evidence_scope :files_succeeded])
              (get-in entry [:evidence_scope :files_failed]))))
    (is (= (get-in entry [:evidence_scope :rules_total])
           (+ (get-in entry [:evidence_scope :rules_emitted])
              (get-in entry [:evidence_scope :rules_missing]))))))
```

- [ ] **Step 5: Run focused ABC tests**

Run from `../abc`:

```bash
clojure -M:test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-registry-validation-test
clojure -M:test --focus abc.tools.validate-design-bundle-test/aat-parser-ir-compatibility-test
clojure -M:test --focus abc.tools.validate-design-bundle-test/compatibility-errors-test
```

Expected: all PASS.

- [ ] **Step 6: Run ABC design bundle validation**

Run from `../abc`:

```bash
nix run .#validate-design-bundle
```

Expected: PASS.

- [ ] **Step 7: Commit ABC changes separately**

Run from `../abc`:

```bash
git add data/aat-parser-ir-compatibility.edn \
  test/abc/tools/validate_design_bundle_test.clj
if ! git diff --quiet -- src/abc/tools/aat_parser_ir_compat.clj; then
  git add src/abc/tools/aat_parser_ir_compat.clj
fi
git commit -m "feat: accept measured parser-ir mapping compatibility"
```

### Task 7: Update ab-validator Docs and Gates

**Files:**
- Modify: `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md`
- Modify: `docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`
- Test: all local parser-IR smokes

**Interfaces:**
- Consumes: mapping version `0.2.0`, full audit evidence, ABC registry commit.
- Produces: documentation that no longer says `derived_from` is intentionally unset.

- [ ] **Step 1: Update crate design spec**

In `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md`, replace the old decision:

```text
To avoid silently changing the measured mapping, keep derived_from unset.
```

with:

```text
Mapping version 0.2.0 projects AAT producer identity into parser-IR `derived_from`. The mapping document hash remains external identity carried by ABC manifest inputs as `mapping_hash`; `derived_from` carries only AAT version, adapter, adapter version, mapping id, mapping version, and mapping schema hash.
```

Add this policy note in the same section:

```text
ABC compatibility registry matching is exact over `aat_adapter_version`. A new adapter version tuple is not covered by an older registry entry; it requires a fresh conversion audit and a new adapter-scoped compatibility entry. Wildcards, adapter-neutral entries, and prefix matches are intentionally invalid.
```

- [ ] **Step 2: Update ABC sync report**

Use this command to capture the measured mapping hash:

```bash
mapping_hash="$(jq -r '.mapping.mapping_hash' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json)"
printf '%s\n' "$mapping_hash"
```

Then append the identity update section with the measured hash:

```bash
cat >> docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md <<EOF

## Identity and Compatibility Update

- Mapping version: \`0.2.0\`
- Mapping document hash: \`$mapping_hash\`
- Parser-IR now emits \`derived_from\`.
- ABC registry entries are adapter-scoped over actual AAT metadata adapters: \`aozora-rs\` and \`aozora2html\`.
- Compatibility evidence source: \`docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json\`.
EOF
```

- [ ] **Step 3: Run local verification**

Run:

```bash
bash tests/aat-parser-ir-schema-hash-smoke.sh
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-parser-ir-mapping-smoke.sh
bash tests/aat-to-parser-ir-cli-smoke.sh
cargo test -p ab-aat-to-parser-ir
just aat-to-parser-ir-flake-smoke
```

Expected: all PASS.

- [ ] **Step 4: Check for placeholders and whitespace**

Run:

```bash
rg -n 'TBD|TODO|<copy|placeholder|sha256:\\.\\.\\.|0\\.1\\.1' docs/superpowers/plans/2026-07-04-parser-ir-identity-compatibility-hardening.md docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md tests crates/ab-aat-to-parser-ir reports/aat-fidelity/aat_parser_ir_mapping data/aat-to-parser-ir-mapping-v1.json
git diff --check
```

Expected: `rg` may find historical references outside touched sections only if they are explicitly labelled historical; no `sha256:...` or `<copy...>` tokens remain. `git diff --check` exits 0.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md \
  docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md
git commit -m "docs: record parser-ir compatibility identity policy"
```

## Final Verification

Run in ab-validator:

```bash
git status --short --branch
cargo test -p ab-aat-to-parser-ir
bash tests/aat-parser-ir-schema-hash-smoke.sh
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-parser-ir-mapping-smoke.sh
bash tests/aat-to-parser-ir-cli-smoke.sh
just aat-to-parser-ir-flake-smoke
jq -e '.mapping.mapping_version == "0.2.0"' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e '.totals.files_failed == 0' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
jq -e '.compatibility_candidates | length == 2' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
```

Run in ABC after Task 6:

```bash
git -C ../abc status --short --branch
(cd ../abc && nix run .#validate-design-bundle)
```

## Self-Review

- Spec coverage: The plan covers mapping policy regeneration, parser-IR `derived_from`, mapping document hash computation, audit evidence, adapter-scoped ABC registry entries, and deterministic gates.
- Placeholder scan: The plan has an explicit `rg` gate to prevent unresolved placeholders or example hashes from landing in touched files.
- Type consistency: Rust fields use `mapping_hash`, `document_hash`, `derived_from`, and `compatibility_candidates` consistently; ABC registry keys use EDN snake-case keywords matching current registry code.
- Protocol risk: The registry remains ABC-owned. ab-validator emits candidates and evidence, not acceptance decisions.
- Known sequencing risk: Adding `derived_from` changes mapping semantics, so mapping generation is deliberately first and the converter change is blocked on mapping `0.2.0`.
