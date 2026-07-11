# Consolidated Parser Phase 4 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** AAT schema v2 (Level-3 structures), two identity rotations (C3 in-body, C4 source_note), mapping v2 with version-tuple dispatch, ADR 0023 admission ceremony, atomic activation of `ab-aozora` as the publication lane, legacy-lane retirement.

**Architecture:** Same strangler discipline as Phase 3: every parser change lands under a candidate commit whose full-corpus effects are proven confined by a fail-closed delta audit before the identity is registered. The schema/mapping/converter rotate first (keeping v1 byte-frozen for history), the parser rotates twice (C3, C4), and only after both rotations' evidence is frozen does the ceremony run: registry row → admission-report capture → bundle validation → one atomic activation commit → wholesale acceptance gate → checkpoint → retirement.

**Tech Stack:** Rust (workspace crates `ab-aozora-aat`, `ab-aozora-facade`, `ab-aozora`, `ab-aat-to-parser-ir`), Python 3 (reports/ instruments, pytest), Clojure (abc registry/validation tooling), just, nix, hinoki for corpus-scale runs.

**Spec:** `docs/superpowers/specs/2026-07-11-consolidated-parser-phase4-level3-admission-activation-design.md` (all decisions and contracts in this plan trace to it).

## Global Constraints

Copied from the spec; every task implicitly includes these.

- Working root: the `ab-validator/` directory of the phase worktree (`.worktrees/parser-fork-phase4`, branch `feat/parser-fork-phase4`, created at execution start via superpowers:using-git-worktrees), except the abc-side tasks (11, 17, 18) which edit `abc/` in the same worktree.
- **Never** enable the facade `json` feature in any workspace member: it pulls `serde_json/preserve_order` into the workspace graph. `bash tests/workspace-no-preserve-order.sh` must stay green after every task ("OK: no preserve_order in the root workspace feature graph").
- **Never** edit frozen evidence reports (anything already under `docs/superpowers/reports/` dated 2026-07-11 or earlier). Phase 4 gate evidence files are frozen the moment their task completes.
- **Never** modify `adapters/aozora` or the pinned upstream lane before Task 21 (retirement). Building/running the frozen crate as the perf baseline (Tasks 10, 15) is fine; editing it is not. Task 19 (activation) changes defaults and contracts but deletes nothing.
- Version strings (exact — the cross-gate join keys):
  - C3: `ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git <C3>)`
  - C4: `ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git <C4>)`
  - The facade moves to 0.3.0 at Task 4 (ruby projection is a facade API change; the spec's escape clause "if facade code changes, the join key records it" applies). `wire-schema` stays 3; `aozora_json::SCHEMA_VERSION == 3` (const-asserted at `crates/ab-aozora-aat/src/lib.rs:302`) is untouched all phase.
- **Candidate identity (binding for tasks 10, 11, 15, 16, 17, 18, 19, 20):** `C3` and `C4` are the HEADs after Task 9 and Task 14 respectively, **persisted machine-readably** in `$(git rev-parse --show-toplevel)/.superpowers/sdd/phase4-identity.json` (`{"c3": "<40hex>", "c4": "<40hex>"}`) the moment each close task commits. **Gate tasks NEVER derive a candidate from current HEAD** — they read the identity file and cross-check the ledger. Gate builds happen on hinoki in a **detached clean checkout** of the candidate commit with `AB_AOZORA_GIT_REV=$CANDIDATE` exported; every gate asserts `./target/release/ab-aozora --version | grep -F $CANDIDATE` before running. A `(git unknown)` binary is never gate evidence. `reports/**`-only and `docs/**`-only commits do not move a candidate; any code commit after a stage's gate started moves that candidate forward and ALL of that stage's gates re-run. Gate instruments (the delta-audit modes) land BEFORE their identity close, so the candidate checkout contains them.
- **Local candidate-bound builds** (the conformance steps run on the local machine): before any local gate command, `export AB_AOZORA_GIT_REV=$CANDIDATE RUSTC_WRAPPER= SCCACHE_DISABLE=1`, assert code-identity `git diff --quiet $CANDIDATE HEAD -- crates Cargo.toml Cargo.lock` (STOP if dirty — HEAD has non-report changes vs the candidate), `cargo build --release -p ab-aozora`, assert `./target/release/ab-aozora --version | grep -F "$CANDIDATE"`, run the recipes in that same shell (the exported env survives any recipe-triggered rebuild), and re-assert the version line afterwards.
- Gate summary JSON schema (one file per gate, frozen once written):

  ```json
  {
    "stage": "c3 | c4",
    "gate": "<gate name>",
    "candidate": {
      "commit": "<full 40-hex>",
      "bin_sha256": "<sha256 of target/release/ab-aozora>",
      "version": "<verbatim --version line>"
    },
    "verdict": "PASS",
    "details": { }
  }
  ```

- hinoki: `hinoki.hyakutake-barbel.ts.net`, passwordless ssh, 32 cores, `AB_DB_ROOT=/db/ab-validator`. PrivateTmp — never use `/tmp` there. ~10-minute ssh command cap: long runs are `nohup … &` detached with a log file under `~`, then **polled synchronously from the controller session** (Phase 3 lesson: background monitor processes died silently twice — never rely on them).
- Never delete `/db/ab-validator/aat-corpus/{aozora-full-repin-1a4f864, aozora-fork-parity-2263b92a, ab-aozora-phase2-9cbb7b7b, ab-aozora-phase3-capability-a81edf0, ab-aozora-phase3-span-a3f91f5}`. Phase 4 retains `ab-aozora-phase4-c3-<C3:0:7>` and `ab-aozora-phase4-c4-<C4:0:7>`.
- Builds for gates and perf: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1`.
- Progress ledger: append task-completion lines and both candidate commits to `$(git rev-parse --show-toplevel)/.superpowers/sdd/progress.md` under a `## Phase 4` heading.
- Perf gate (both rotations): committed workset `data/perf-workset.json`, corpus `/db/ab-validator/perf-workset-corpus-v1` (pre-extracted on hinoki), `--runs 5` minimum, >10% median wall-time regression blocks the stage, new timeouts block unconditionally. Baseline lane = the pinned legacy `adapters/aozora` + nix upstream binary (exactly as Phase 3 measured, preserving comparability with the recorded −4.09% margin). **Activation floor:** if the C4 median is slower than the legacy-lane median, STOP before Task 18 and escalate — an optimization task must be inserted (spec decision 4).
- `just` 1.55 has **no `NAME=value` CLI syntax** — recipe parameters are positional. `aozora-notation-spec-comparison` takes `VECTORS REPORT_MD SUMMARY_JSON AOZORA_BIN` positionally (empty `""` VECTORS = full P4suta suite + span-deviation manifest); `official-docs-seed-comparison` takes `REPORT_MD SUMMARY_JSON`. All invocations below are written in working positional form.
- Dump layout: `run-aat-full.sh --out-dir <out>` writes AAT under `<out>/aat/ab-aozora/`. All compare/audit invocations below use the full `<out>/aat/ab-aozora` path (Phase 3 lesson: `<out>/aat` alone is wrong).
- Registry rows copy the audit tool's emitted `--compat-edn-out` values **verbatim** — field names, numbers, `:corpus` label. No prose paraphrase (admission is byte-exact row equality; this is the `:corpus`-trap rule).
- Conformance baselines at C2 (the comparison anchors): P4suta `ab-aozora` 118 pass / 9 warning / 0 fail / 0 skip with 25/25 `must`; seed 22/8/0/0. Expected at C3 and C4: **differing = 0 vs these baselines on both suites** (AAT-lane scoring compares diagnostics only, and nothing in Phase 4 touches the diagnostics wire). Any row diff blocks until classified; vectors are nix-built and never edited — the span-deviation manifest is the only expected-row override mechanism.

---

## File Structure

New files:
- `data/aat-schema-v1.json` — byte-frozen v1 schema copy (comparison lanes).
- `data/aat-to-parser-ir-mapping-v2.json` — mapping 0.3.0, `source_aat_version` 2.
- `crates/ab-aozora-facade` gains `ruby_entries` projection (json.rs).
- `reports/source-regions/terminal-provenance-split.py` + `reports/lib/terminal_provenance.py` — the measurement-split instrument and its shared classifier.
- `reports/aat-fidelity/verify-phase4-checkpoint.py` + tests.
- New delta-audit modes inside `reports/aat-fidelity/audit-aat-delta.py` (`v2-migration`, `source-note-append`) + tests.
- `crates/ab-aozora-aat/tests/data/terminal-provenance-utf8.txt` + golden (Task 14).
- `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json` — archival run-set holding the retired legacy entry (Task 19).
- Evidence reports under `docs/superpowers/reports/2026-07-12-phase4-*`.

Modified files (owners):
- `data/aat-schema.json` → v2 (Task 2 owns the schema; later tasks never edit it).
- `crates/ab-aat-to-parser-ir/{src/schema.rs, src/mapping.rs, src/main.rs, src/convert.rs, tests/integration.rs}` (Tasks 2–3).
- `crates/ab-aozora-aat/src/lib.rs` (Tasks 4–7, 9, 14 — emission), `crates/ab-aozora-facade/src/json.rs` (Task 4 only).
- `abc/data/aat-parser-ir-compatibility.edn` (Tasks 11, 16), `abc/data/source-region-publication-policy-v0.json` (Task 16), `abc/examples/ab-validator-output/*` (Tasks 17–18).
- `reports/aat-fidelity/run-sets/current.json`, `docs/handoffs/ir-publication-coverage-contract.md`, `justfile` (Task 19 — the atomic activation commit).
- `flake.nix`, `adapters/aozora` (deleted), `reports/aat-fidelity/run-aat-full.sh` (Task 21).

---

### Task 1: Housekeeping — pipeline version bump, stale README, handoff one-liner

**Files:**
- Modify: `crates/ab-aozora-pipeline/Cargo.toml:4` (`version = "0.1.0"` → `"0.2.0"`)
- Modify: `crates/ab-aat-to-parser-ir/README.md:75-77` (mapping coordinates)
- Modify: `docs/handoffs/2026-07-10-parser-fork-provenance.md` (append)

**Interfaces:**
- Consumes: nothing.
- Produces: nothing later tasks call; clears the three Phase 3 record-only Minors before any C3 evidence exists.

- [ ] **Step 1: Bump the pipeline crate**

In `crates/ab-aozora-pipeline/Cargo.toml` change `version = "0.1.0"` to `version = "0.2.0"`. This is the owed record of the Phase 3 `sanitize_mapped` API addition. Run `cargo build -p ab-aozora-pipeline` (Cargo.lock updates).

- [ ] **Step 2: Fix the stale converter README coordinates**

In `crates/ab-aat-to-parser-ir/README.md`, the mapping block still cites version `0.2.6` and hash `sha256:e36508c3…`. Replace with the shipped coordinates: version `0.2.8`, hash `sha256:952620ced4eb22f9771e6a10c3a1d4d93de604a8c33e360311f82b6e1eafc5b7`. Verify no other stale hash remains: `grep -rn "e36508c3\|0\.2\.6" crates/ab-aat-to-parser-ir/` → no matches.

- [ ] **Step 3: Append the omitted-items one-liner to the provenance handoff**

At the end of the "Phase 3 closure (2026-07-11)" section of `docs/handoffs/2026-07-10-parser-fork-provenance.md`, append:

```markdown
- **Also carried on the Phase 4 ledger** (recorded in the execution ledger,
  omitted from the list above): the bare-toggle marker forms
  （［＃横組み］…終わり ~3,188, ［＃罫囲み］ toggles ~25）as the classifier
  ceiling; the keigakomi 44-marker denominator residual (673 matrix vs 717
  frozen, UNRESOLVED); and the `:corpus` label normalization trap at
  admission (registry equality is byte-exact — rows must copy tool-emitted
  labels verbatim).
```

- [ ] **Step 4: Verify workspace + guard**

Run: `cargo test -p ab-aozora-pipeline` (expect all pass, 49+ sanitize tests included), `bash tests/workspace-no-preserve-order.sh` (expect `OK: no preserve_order in the root workspace feature graph`).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-pipeline/Cargo.toml Cargo.lock crates/ab-aat-to-parser-ir/README.md docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "chore(parser): phase 3 record-only minors — pipeline 0.2.0, README coordinates, handoff omissions"
```

---

### Task 2: AAT schema v2, frozen v1, converter version-tuple dispatch, x-field inventory

**Files:**
- Create: `data/aat-schema-v1.json` (byte copy of current `data/aat-schema.json`)
- Modify: `data/aat-schema.json` (becomes v2)
- Modify: `crates/ab-aat-to-parser-ir/src/schema.rs:17-31` (`SchemaSet::load` → `load_for_aat_version`)
- Modify: `crates/ab-aat-to-parser-ir/src/mapping.rs:96-98` (version-aware preflight)
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs` (all subcommands select the tuple)
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs:22-29` (`schemas_and_mapping` → v1 tuple)
- Create: `docs/superpowers/reports/2026-07-12-phase4-x-field-inventory.md`

**Interfaces:**
- Consumes: current v1 schema (`$id https://abc.local/schemas/aat-v1.json`, `version const 1`).
- Produces: `SchemaSet::load_for_aat_version(repo_root: &Path, abc_root: &Path, aat_version: u64) -> Result<SchemaSet>` (1 → `data/aat-schema-v1.json`, 2 → `data/aat-schema.json`, other → error). Preflight rule: `mapping.source_aat_version` must equal the loaded AAT schema's `properties.version.const`. **Input-validation rule: `SchemaValidators` gains an AAT validator compiled from `SchemaSet.aat_schema`, and `convert_preflighted` validates every input document against IT (the tuple's schema), never against a globally embedded current schema** — without this, validated v1 conversion fails the moment `data/aat-schema.json` becomes v2. Tasks 3, 10, 15 depend on all of this. Schema v2 field names: `jizume_block.width`, `jisage_block.indent`, `heading.indent` (optional), style `align`/`offset_from_end` (chitsuki), `indent_first`/`indent_rest` (burasage), warning `{code, severity, message, span?, path?}`, `source_note {kind, placement, region_class, content, span?}` (schema field names used by Tasks 3, 5, 6, 7, 9, 14).

- [ ] **Step 1: Freeze v1**

```bash
cp data/aat-schema.json data/aat-schema-v1.json
```

- [ ] **Step 2: Edit `data/aat-schema.json` to v2**

Apply exactly these changes (nothing else):

1. `"$id": "https://abc.local/schemas/aat-v1.json"` → `"https://abc.local/schemas/aat-v2.json"`; `"version": { "const": 1 }` → `{ "const": 2 }`.
2. `block` oneOf gains `{ "$ref": "#/$defs/source_note" }` as a fourth member.
3. `block_container`: `kind` enum gains `"jizume_block"`; properties gain `"indent": { "type": "integer", "minimum": 0 }` and `"width": { "type": "integer", "minimum": 1 }`; add after `properties`:

```json
"allOf": [
  { "if": { "properties": { "kind": { "const": "jisage_block" } } },
    "then": { "required": ["indent"] } },
  { "if": { "properties": { "kind": { "const": "jizume_block" } } },
    "then": { "required": ["width"] } }
]
```

4. `heading` properties gain `"indent": { "type": "integer", "minimum": 0 }` (optional — heading indentation is conditional).
5. `inline_container` properties gain `"align": { "const": "right" }`, `"offset_from_end": { "type": "integer", "minimum": 0 }`, `"indent_first": { "type": "integer", "minimum": 0 }`, `"indent_rest": { "type": "integer", "minimum": 0 }`; add:

```json
"allOf": [
  { "if": { "properties": { "kind": { "const": "style" }, "style_type": { "const": "chitsuki" } }, "required": ["style_type"] },
    "then": { "required": ["align", "offset_from_end"] } },
  { "if": { "properties": { "kind": { "const": "style" }, "style_type": { "const": "burasage" } }, "required": ["style_type"] },
    "then": { "required": ["indent_first", "indent_rest"] } }
]
```

6. Replace the `warning` def entirely:

```json
"warning": {
  "type": "object",
  "required": ["code", "severity", "message"],
  "additionalProperties": false,
  "properties": {
    "code": { "type": "string" },
    "severity": { "enum": ["error", "warning", "note"] },
    "message": { "type": "string" },
    "span": { "$ref": "#/$defs/span" },
    "path": { "type": "string" }
  }
}
```

(`line` is gone — subsumed by `span.line_start`. The severity enum is the façade's `severity_str` vocabulary verbatim: `error`, `warning`, `note`.)

7. Add the `source_note` def:

```json
"source_note": {
  "type": "object",
  "required": ["kind", "placement", "region_class", "content"],
  "additionalProperties": false,
  "patternProperties": { "^x-": true },
  "properties": {
    "kind": { "const": "source_note" },
    "placement": { "enum": ["front", "body", "back", "unknown"] },
    "region_class": { "enum": ["front_legend", "terminal_provenance", "colophon_metadata", "body_end_boundary", "malformed_residue"] },
    "content": { "type": "array", "items": { "$ref": "#/$defs/inline" } },
    "span": { "$ref": "#/$defs/span" }
  }
}
```

- [ ] **Step 3: Verify both schema files are valid JSON Schema and v1 is byte-frozen**

```bash
python3 - <<'EOF'
import json
for p in ("data/aat-schema.json", "data/aat-schema-v1.json"):
    s = json.load(open(p))
    print(p, s["$id"], s["properties"]["version"]["const"])
assert json.load(open("data/aat-schema.json"))["properties"]["version"]["const"] == 2
assert json.load(open("data/aat-schema-v1.json"))["properties"]["version"]["const"] == 1
EOF
git show HEAD:data/aat-schema.json | cmp - data/aat-schema-v1.json && echo V1-FROZEN
```

Expected: both print; `V1-FROZEN`.

- [ ] **Step 4: Write the failing converter tests (version-tuple dispatch)**

In `crates/ab-aat-to-parser-ir/tests/integration.rs`, change the shared helper to select the v1 tuple explicitly, and add the fail-closed tests:

```rust
fn schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let schemas =
        SchemaSet::load_for_aat_version(&repo_root, &abc_root, mapping.source_aat_version).unwrap();
    (schemas, mapping)
}

#[test]
fn schema_set_rejects_unknown_aat_version() {
    let (repo_root, abc_root) = roots();
    let err = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 3).unwrap_err();
    assert!(err.to_string().contains("unsupported AAT schema version 3"), "{err}");
}

#[test]
fn validated_v1_conversion_succeeds_under_v1_tuple() {
    // full pipeline proof, not just preflight: any existing v1 fixture through
    // convert() with the v1 tuple must succeed WITH input validation on.
    let (schemas, mapping) = schemas_and_mapping();
    let aat = include_fixture_json("nested-sentence-basic.aat.json");
    convert(ConversionRequest { aat, mapping, schemas, options: default_test_options() }).unwrap();
}

#[test]
fn v1_document_under_v2_tuple_fails_input_validation() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json")); // Task 3 file; until it exists, mark #[ignore] and un-ignore in Task 3
    // v1 fixture (version:1) must be REJECTED by the v2 tuple's input validation.
    // Symmetric test v2_document_under_v1_tuple_fails lives in Task 3 (needs a v2 doc).
}

#[test]
fn preflight_rejects_mismatched_mapping_schema_tuple() {
    let (repo_root, abc_root) = roots();
    // v1 mapping paired with the v2 schema file must fail preflight.
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    let err = mapping.preflight(&schemas).unwrap_err();
    assert!(err.to_string().contains("tuple mismatch"), "{err}");
}
```

(`roots()` — extract the existing repo_root/abc_root derivation from the current `schemas_and_mapping` body into a helper so both tests share it. `default_test_options()` — extract however the sibling integration tests build `ConversionOptions` into one helper and reuse it everywhere. The cross-version test is written now and `#[ignore]`d until Task 3 supplies the v2 mapping; Task 3 un-ignores it and adds the symmetric case.)

- [ ] **Step 5: Run to verify failure**

Run: `cargo test -p ab-aat-to-parser-ir --test integration schema_set_rejects -- --nocapture`
Expected: FAIL — `load_for_aat_version` not found.

- [ ] **Step 6: Implement the tuple machinery**

In `crates/ab-aat-to-parser-ir/src/schema.rs`, replace `SchemaSet::load` with:

```rust
impl SchemaSet {
    pub fn load_for_aat_version(repo_root: &Path, abc_root: &Path, aat_version: u64) -> Result<Self> {
        let aat_schema_path = match aat_version {
            1 => repo_root.join("data/aat-schema-v1.json"),
            2 => repo_root.join("data/aat-schema.json"),
            other => bail!("unsupported AAT schema version {other} (known: 1, 2)"),
        };
        Ok(Self {
            aat_schema: read_json(&aat_schema_path)?,
            mapping_schema: read_json(&abc_root.join("schemas/aat-parser-ir-mapping.schema.json"))?,
            parser_ir_schema: read_json(&abc_root.join("schemas/parser-ir.schema.json"))?,
            abc_divergence_record_schema: read_json(&abc_root.join("schemas/aat-parser-ir-divergence.schema.json"))?,
            bundle_schema: read_json(&repo_root.join("data/aat-parser-ir-divergence-bundle-v1.schema.json"))?,
        })
    }
}
```

Delete the old `load` (or keep it as `load_for_aat_version(repo_root, abc_root, 1)` only if a caller count makes removal churn-heavy — prefer removal; update every caller). In `src/mapping.rs` replace the hardcoded check at lines 96-98:

```rust
let schema_aat_version = schemas
    .aat_schema
    .pointer("/properties/version/const")
    .and_then(Value::as_u64)
    .context("AAT schema missing properties.version.const")?;
if self.source_aat_version != schema_aat_version {
    bail!(
        "mapping/schema tuple mismatch: mapping source_aat_version {} but loaded AAT schema is version {}",
        self.source_aat_version,
        schema_aat_version
    );
}
```

In `src/main.rs`, every subcommand that currently does `MappingDocument::from_path(&mapping)?` then `SchemaSet::load(&repo_root, &abc_root)?` now does:

```rust
let mapping = MappingDocument::from_path(&mapping)?;
let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, mapping.source_aat_version)?;
```

(The tuple is resolved from the mapping's declared version.) `audit.rs` threads config through `run_audit` — if it loads schemas itself, apply the same two-line change there.

Then make input validation tuple-aware (the reviewer-identified gap): in `SchemaValidators::compile(&schemas)` add a compiled validator for `schemas.aat_schema` (field name `aat`), and in `convert_preflighted` (convert.rs:89 region) replace whatever validates the input AAT today — if it references a globally embedded/current schema, delete that path — with `validators.aat.validate(&aat)`. Per-document enforcement is then genuinely tuple-bound: a v1 document under the v2 tuple fails on `version const` and vice versa, which is exactly what the paired tests pin.

- [ ] **Step 7: Run the full converter suite**

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: 89 existing + 2 new = 91 passed (57+2 integration, 32 unit). All v1 fixtures still convert (they now load the frozen v1 file — byte-identical to what they loaded before this task).

- [ ] **Step 8: Write the x-field inventory report**

Create `docs/superpowers/reports/2026-07-12-phase4-x-field-inventory.md` recording the **repository-wide** inventory (spec: "not a line-range read"). Run and embed the output of:

```bash
grep -n '"x-' crates/ab-aozora-aat/src/lib.rs | grep -v '^\s*//' | grep -v 'get("x-'
```

Then the classification table (every emitted `x-*` field, no third category):

| Field | Sites (lib.rs) | Classification | v2 disposition |
|---|---|---|---|
| `x-indent` | 496, 510 (jisage_block); 872 (heading) | layout | promoted → `indent` (Task 5) |
| `x-align` | 590 (chitsuki style) | layout | promoted → `align` (Task 5) |
| `x-offset` | 591 (chitsuki style) | layout | promoted → `offset_from_end` (Task 5) |
| `x-indent-first` | 621 (burasage style) | layout | promoted → `indent_first` (Task 5) |
| `x-indent-rest` | 622 (burasage style) | layout | promoted → `indent_rest` (Task 5) |
| `x-provenance` | 592, 623, 869, 967, 987, 1007, 1109 | provenance | retained verbatim |
| `x-source-marker-kind` | 968, 988, 1008, 1110 | provenance/marker | retained verbatim |
| `x-break-kind` | 969, 989 | provenance/marker (page-break metadata) | retained verbatim |
| `x-codepoint` | 1063 (gaiji) | provenance-of-resolution | retained verbatim |

Add the note: **line-jisage has no emission site** — standalone per-line 字下げ markers remain `raw` nodes (`x-source-marker-kind: "indent"`); the converter's `line-jisage` style path serves other-adapter v1 documents only. Nothing to promote; classified, not skipped.

- [ ] **Step 9: Guard + commit**

```bash
bash tests/workspace-no-preserve-order.sh
git add data/aat-schema.json data/aat-schema-v1.json crates/ab-aat-to-parser-ir docs/superpowers/reports/2026-07-12-phase4-x-field-inventory.md
git commit -m "feat(aat-schema): v2 contract + frozen v1 + converter version-tuple dispatch"
```

---

### Task 3: Mapping v2 document + converter v2 rules (source-note authority, jizume, typed layout, heading accounting)

**Files:**
- Create: `data/aat-to-parser-ir-mapping-v2.json`
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs` (v2 paths)
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs` (v2 tests + repoint the v2-artifact pin)

**Interfaces:**
- Consumes: Task 2's tuple machinery and v2 schema field names.
- Produces: the complete v2 mapping (0.3.0) whose hash is frozen for the rest of the phase — **Tasks 10 and 15 conversion audits and Tasks 11/17 registry rows all cite it; any later edit to this file invalidates C3 evidence**. Converter behaviors later tasks rely on: v2 documents never trigger the source-note heuristic; explicit `source_note` blocks convert per the spec contract; `jizume_block` converts like `jisage_block`; typed layout fields are read with x- fallback (`typed.or(x-)`) so one code path serves both versions.

- [ ] **Step 1: Author `data/aat-to-parser-ir-mapping-v2.json`**

Start from a copy of `data/aat-to-parser-ir-mapping-v1.json`, then apply:

1. `"mapping_id"`: `https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe`; `"mapping_version"`: `"0.3.0"`; `"source_aat_version"`: `2`. `mapping_schema_hash` and `target_parser_ir_schema_hash` stay verbatim (same mapping schema, same parser-IR schema — preflight recomputes and would catch drift).
2. Rename every rule `aat_pointer` that references an x- layout field to the typed name. Find them: `python3 -c "import json; m=json.load(open('data/aat-to-parser-ir-mapping-v2.json')); print([r['rule_id'] for r in m['transform_rule_descriptions'] if r.get('aat_pointer') and ('x-indent' in r['aat_pointer'] or 'x-align' in r['aat_pointer'] or 'x-offset' in r['aat_pointer'] or 'x-width' in r['aat_pointer'])])"`. Apply: `x-indent`→`indent`, `x-align`→`align`, `x-offset`→`offset_from_end`, `x-indent-first`→`indent_first`, `x-indent-rest`→`indent_rest`, `x-width`→`width`. Provenance pointers (`x-provenance`, `x-source-marker-kind`, `x-break-kind`, `x-codepoint`) stay.
3. Append the new rules (rule_id prefix continues each category's existing numbering; descriptions state provenance "Phase 4 v2 rotation" — observed counts are filled by convention from the C3/C4 audits later, write `Observed pending C3 audit` initially and NEVER edit after Task 10 freezes the hash — so instead: write descriptions WITHOUT observed counts; the generated-probe convention tolerates description-only rules):
   - STRUCTURAL `blocks[].jizume_block` → null (action `drop-sidecar`): "jizume_block container collapsed on mixed-children fallback; layout not projected" — mirrors jisage's S-08 (null pointer, fallback semantics). PLUS an INVENTION rule `blocks[].jizume_block` → `indentation` (action `invent`) mirroring I-32, so the fallback's invented indentation node is strictly accounted. Both are recorded strictly (`record`, not `record_if_measured`) on the fallback branch only; the success path fires neither. [Amended per Task 3 review: the original authoring put success-path semantics on a fallback-only rule and left the invented node unaccounted.]"
   - LOSS `blocks[].heading.indent` → null (action `drop-sidecar`): "heading indentation has no parser-IR heading layout field; recorded, not projected."
   - STRUCTURAL `blocks[].source_note` → `source-note` (action `project`): "explicit v2 source_note block → parser-IR source-note node, classification direct."
   - LOSS `blocks[].source_note.region_class` → `(source-note.note_type)` (action `project`): "region_class terminal_provenance → note_type source-attribution."
   - LOSS `meta.warnings[].code` → null, LOSS `meta.warnings[].severity` → null, LOSS `meta.warnings[].span` → null (action `drop-sidecar`): parser-IR carries warnings via its own diagnostics channel; the enriched fields are sidecar-recorded. (Mirror however v1 treats `meta.warnings[].message` — keep the same action verbatim.)
   - **No ruby.direction rule is added**: parser-IR already carries `ruby.direction` and the converter already projects it (convert.rs:829). Verify the v1 mapping's existing ruby rules carry over unchanged (grep the v2 copy for `ruby` pointers; direction needs no pointer rename). Adding a LOSS claim here would be false accounting.
4. Every new rule's category must already exist in `loss_taxonomy` (all five exist; no taxonomy edits needed).

- [ ] **Step 2: Preflight test for the v2 artifact**

Repoint `mapping_preflight_accepts_checked_in_v2_artifact` (integration.rs:426-470) at the new file and update its pinned constants:

```rust
#[test]
fn mapping_preflight_accepts_checked_in_v2_artifact() {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    assert_eq!(mapping.mapping_version, "0.3.0");
    assert_eq!(mapping.source_aat_version, 2);
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    mapping.preflight(&schemas).unwrap();
}
```

(Keep the v1 pin too — add `mapping_preflight_accepts_checked_in_v1_artifact` asserting `mapping_version == "0.2.8"` and `transform_rule_descriptions.len() == 680` against the frozen v1 file, preserving the old test's teeth.)

Run: `cargo test -p ab-aat-to-parser-ir --test integration mapping_preflight` — the v2 test fails until the file's pointers all exist in the v2 schema (preflight `aat_pointer_exists`); fix pointer spellings until green. This is the fail-closed loop that proves every new rule pointer is schema-real.

- [ ] **Step 3: Converter v2 paths — failing tests first**

Add to integration.rs (shapes follow the existing `projects_burasage_style_wrapper_to_paragraph_layout` pattern at integration.rs:1511 — copy its scaffolding for building an AAT doc and converting):

```rust
fn v2_schemas_and_mapping() -> (SchemaSet, MappingDocument) {
    let (repo_root, abc_root) = roots();
    let mapping =
        MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::load_for_aat_version(&repo_root, &abc_root, 2).unwrap();
    (schemas, mapping)
}

#[test]
fn v2_jizume_block_projects_paragraph_layout() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-jizume",
        "blocks": [{ "kind": "jizume_block", "width": 21, "children": [
            { "kind": "paragraph", "content": [{ "kind": "text", "value": "本文" }] } ] }],
        "meta": v2_test_meta()   // helper: build the required meta block (adapter,
                                 // adapter_version, source_encoding, source_hash,
                                 // parse_complete, warnings: []) — copy the meta the
                                 // sibling layout tests use and set warnings to []
    });
    let out = convert(ConversionRequest { aat, mapping, schemas, options: default_test_options() }).unwrap();
    // access paragraphs[] exactly as projects_burasage_style_wrapper_to_paragraph_layout does
    let layout = paragraph_layout_of(&out, 0);
    assert_eq!(layout, json!({ "kind": "jizume", "source": "aat-block", "width": 21 }));
}

#[test]
fn v2_typed_layout_fields_project_without_x_names() {
    let (schemas, mapping) = v2_schemas_and_mapping();
    let aat = json!({
        "version": 2, "work_id": "t-burasage",
        "blocks": [{ "kind": "paragraph", "content": [{
            "kind": "style", "style_type": "burasage",
            "indent_first": 6, "indent_rest": 7, "x-provenance": "source-derived",
            "content": [{ "kind": "text", "value": "本文" }] }] }],
        "meta": v2_test_meta()
    });
    let out = convert(ConversionRequest { aat, mapping, schemas, options: default_test_options() }).unwrap();
    let layout = paragraph_layout_of(&out, 0);
    assert_eq!(layout, json!({ "kind": "burasage", "source": "aat-style",
                               "first_line_indent": 6, "continuation_indent": 7 }));
}

#[test]
fn v2_never_applies_source_note_heuristic() {
    // version:2 doc whose FINAL top-level paragraph is （…から。） attribution text
    // expect: ordinary body paragraph nodes; NO node with "type":"source-note"
}

#[test]
fn v1_heuristic_still_fires_byte_identically() {
    // version:1 doc, same final paragraph → source-note node with classification "heuristic"
    // (pins blocker-3's v1-retention clause)
}

#[test]
fn v2_explicit_source_note_converts_direct() {
    // version:2 doc with trailing source_note{placement:"back",region_class:"terminal_provenance",
    //   content:[text "底本：…", …], span}
    // expect node {"type":"source-note","note_type":"source-attribution","placement":"back",
    //   "classification":"direct","source_pointer":"$.blocks[1].source_note","text":"底本：…",…}
    // and a paragraphs[] row with role "source-note", classification "direct",
    // node_range covering exactly that node
}

#[test]
fn v2_source_note_unknown_region_class_fails_closed() {
    // region_class "colophon_metadata" (schema-valid, unmapped) → convert returns Err
}
```

Fill each body concretely (build the AAT `json!` docs inline; the exact expected JSON values are in the spec's "Source-note authority" section and the layout shapes in convert.rs:516-556). Run: `cargo test -p ab-aat-to-parser-ir --test integration v2_` → all FAIL (no v2 paths yet).

- [ ] **Step 4: Implement the v2 convert paths**

In `crates/ab-aat-to-parser-ir/src/convert.rs`:

1. **Version accessor + heuristic gate.** At the top of the conversion (where `blocks` is first walked), read `let aat_version = aat.get("version").and_then(Value::as_u64).unwrap_or(1);` and thread a `heuristic_enabled: bool = aat_version == 1` flag to `map_block` (alongside `is_final_top_level`). In the paragraph arm change line 260's condition to `if is_final_top_level && heuristic_enabled`.
2. **Typed layout readers with x- fallback.** In `paragraph_layout_from_style` (convert.rs:516-556) and `paragraph_layout_from_jisage_block` (558-564), read the typed name first, then the x- name: e.g. for burasage `wrapper.get("indent_first").or_else(|| wrapper.get("x-indent-first"))?.as_u64()?`; chitsuki `align`/`x-align` + `offset_from_end`/`x-offset`; jisage/heading `indent`/`x-indent`; jizume `width`/`x-width`. One code path serves both versions; v1 behavior is bit-identical because v1 docs never carry the typed names.
3. **`jizume_block` arm** in `map_block`, mirroring the `jisage_block` arm (convert.rs:376-428) exactly, with `paragraph_layout_from_jizume_block`:

```rust
fn paragraph_layout_from_jizume_block(block: &Value) -> Value {
    let width = block
        .get("width")
        .or_else(|| block.get("x-width"))
        .and_then(Value::as_u64)
        .unwrap_or(1);
    json!({ "kind": "jizume", "source": "aat-block", "width": width })
}
```

   All-paragraph children → recurse each with `Some(layout.clone())` inherited; otherwise the structural fallback branch, recording via the new STRUCTURAL `blocks[].jizume_block` rule (use `record` — the rule exists, so it must not be silent).
4. **`source_note` arm** in `map_block` (new match arm before the unknown-kind `bail!`):

```rust
"source_note" => {
    let placement = block.get("placement").and_then(Value::as_str).unwrap_or("unknown");
    let region_class = block.get("region_class").and_then(Value::as_str).unwrap_or("");
    let note_type = match region_class {
        "terminal_provenance" => "source-attribution",
        other => bail!("unmapped source_note region_class {other:?} at {path}"),
    };
    let text = visible_content_text(block.get("content"), recorder, &format!("{path}.content"), Some("source-note.text"))?;
    let end = current + utf8_len(&text);
    let span = map_span(block.get("span"), current, end, recorder, path)?;
    let node_start = outputs.nodes.len();
    outputs.nodes.push(json!({
        "type": "source-note",
        "span": span,
        "text": text,
        "note_type": note_type,
        "placement": placement,
        "classification": "direct",
        "source_pointer": path,
    }));
    current = end;
    // paragraph row: role source-note, classification direct, node_range exactly this node
    // — reuse the paragraph-row emission the heuristic branch uses (convert.rs:300-331),
    // with role "source-note" and classification "direct".
    ...
    return Ok(current);
}
```

   Record divergences via the new STRUCTURAL `blocks[].source_note` and LOSS `blocks[].source_note.region_class` rules (`record`, not `record_if_measured` — fail if unmeasured). `placement: "unknown"` maps through as `"unknown"` on the node (parser-IR consumers must not body-render it — that is ABC's validated rule); it is never converted to body text here.
5. **Heading indent accounting**: in the heading arm (convert.rs:332-375), after the existing style LOSS record, add `recorder.record_if_measured("LOSS", &format!("{path}.heading.indent"), None, block.get("indent").or_else(|| block.get("x-indent")).cloned(), None);` guarded on the field being present (matches the new LOSS rule).

- [ ] **Step 5: Run the suite**

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: 91 prior + ~7 new all pass. The three real-corpus fixture tests and both golden-pinned expected.json comparisons must be untouched (v1 path bit-identical — the x- fallback ordering guarantees it).

- [ ] **Step 6: README v2 section + cross-version test completion**

Add a "Mapping v2 (AAT schema 2)" block to `crates/ab-aat-to-parser-ir/README.md` documenting: file `data/aat-to-parser-ir-mapping-v2.json`, version `0.3.0`, `source_aat_version 2`, hash computed (never hand-written; surfaced by `audit-corpus --summary-json`), tuple selection by `source_aat_version`. (Task 1's 0.2.8 correction now explicitly describes the frozen v1 coordinates — reword its block heading to "Mapping v1 (frozen)".) Un-`#[ignore]` Task 2's `v1_document_under_v2_tuple_fails_input_validation`, complete its body, and add the symmetric `v2_document_under_v1_tuple_fails_input_validation` (any v2 `json!` doc from this task's tests, converted under the v1 tuple → input-validation `Err`).

- [ ] **Step 7: Commit**

```bash
git add data/aat-to-parser-ir-mapping-v2.json crates/ab-aat-to-parser-ir
git commit -m "feat(converter): mapping 0.3.0 + v2 convert paths — explicit source-note authority, jizume, typed layout"
```

---

### Task 4: Ruby plumbing — facade `ruby_entries` projection, adapter structured emission, `direction: "left"`

**Files:**
- Modify: `crates/ab-aozora-facade/src/json.rs` (new `ruby_entries` projection, following the `gaiji_entries` pattern at json.rs:209+)
- Modify: `crates/ab-aozora-facade/Cargo.toml:4` (`0.2.0` → `0.3.0`)
- Modify: `crates/ab-aozora-aat/src/lib.rs` (`ruby_node` at 1034-1047, `RUBY_RE` at 26-27, tripwire literal at 1239-1244)
- Modify: `crates/ab-aozora-aat/tests/goldens/*.expected.json` (regenerate — version string + any ruby node changes)

**Interfaces:**
- Consumes: `ab-aozora-syntax` `RubySide` (`lib.rs:188`, variants `Right`/`Left`), `ast/payload.rs:107-125` `Ruby { base: ContentRange, reading: ContentRange, side: RubySide, .. }`; facade `Tree`, `node_entries`, and the `gaiji_entries` source-resolution pattern (json.rs:239-249).
- Produces: `aozora_json::ruby_entries(tree: &Tree<'_>) -> Vec<RubyEntry>` where `RubyEntry` serializes `{ span: Span, base: String, reading: String, side: "right" | "left" }`. The adapter's ruby emission consumes it; the delta-audit ruby class (Task 8) and the join key (`facade 0.3.0` from here on) depend on this task.

- [ ] **Step 1: Failing facade test**

In the facade's test module (same file or tests/ dir as the existing `gaiji_entries` tests — follow their location), add:

```rust
#[test]
fn ruby_entries_exposes_side_base_reading() {
    let src = "｜漢字《かんじ》と繹《・・》\n名［＃「名」の左に「な」のルビ］\n";
    let tree = parse_for_test(src); // reuse the existing test-parse helper next to gaiji_entries tests
    let entries = ruby_entries(&tree);
    assert!(entries.iter().any(|e| e.base_str() == "漢字" && e.reading_str() == "かんじ" && e.side_str() == "right"));
    assert!(entries.iter().any(|e| e.base_str() == "名" && e.reading_str() == "な" && e.side_str() == "left"));
}
```

(Accessor names: match however the `gaiji_entries` entry type exposes fields for tests — if fields are only reachable via `serde_json::to_value`, assert on the serialized JSON instead; keep the same convention.) Run `cargo test -p ab-aozora-facade ruby_entries` → FAIL (function absent).

- [ ] **Step 2: Implement `ruby_entries`**

In `crates/ab-aozora-facade/src/json.rs`, mirroring `gaiji_entries`: walk `tree.source_nodes()`, match the nodes whose payload is `Node::Ruby(ruby)`, resolve `ruby.base` and `ruby.reading` `ContentRange`s against the source exactly the way `gaiji_entries` resolves its ranges, and emit:

```rust
#[derive(Serialize)]
pub struct RubyEntry {
    span: Span,
    base: String,
    reading: String,
    side: &'static str, // "right" | "left"
}

pub fn ruby_entries(tree: &Tree<'_>) -> Vec<RubyEntry> {
    tree.source_nodes()
        .iter()
        .filter_map(|sn| {
            // Match the ruby payload the same way gaiji_entries matches its payload
            // (json.rs:209-249) — the payload accessor and range-resolution helper
            // names MUST be taken from that existing code, not invented here.
            let ruby = ruby_payload_of(sn)?; // seam: gaiji_entries' payload-match pattern
    let base = resolve_content_range(sn, &ruby.base);     // seam: gaiji_entries' resolver
            let reading = resolve_content_range(sn, &ruby.reading);
            Some(RubyEntry {
                span: sn.source_span.into(),
                base,
                reading,
                // RubySide is #[non_exhaustive]; default unknown variants to "right"
                // (same convention as severity_str's wildcard arm).
                side: match ruby.side { RubySide::Left => "left", _ => "right" },
            })
        })
        .collect()
}
```

`side`: `RubySide::Right => "right"`, `RubySide::Left => "left"`, wildcard arm `_ => "right"` with a comment citing the `#[non_exhaustive]` enum (same defensive convention as `severity_str`). Bump `crates/ab-aozora-facade/Cargo.toml` to `0.3.0` in this commit — the projection is new public API and the facade version is the compatibility coordinate in the join key. Run the facade suite: `cargo test -p ab-aozora-facade` → all pass.

- [ ] **Step 3: Failing adapter tests**

In `crates/ab-aozora-aat/src/lib.rs` tests:

```rust
#[test]
fn ruby_emission_uses_structured_entries_right_parity() {
    // ｜base《reading》 input: typed ruby node with the SAME base/reading bytes the
    // regex path produced, direction "right".
    let aat = aat_value_for("｜漢字《かんじ》\n"); // reuse the existing test helper that parses+returns Value
    let ruby = find_first_node(&aat, "ruby");
    assert_eq!(ruby["base"], "漢字");
    assert_eq!(ruby["reading"], "かんじ");
    assert_eq!(ruby["direction"], "right");
}

#[test]
fn left_ruby_emits_direction_left() {
    let aat = aat_value_for("名［＃「名」の左に「な」のルビ］\n");
    let ruby = find_first_node(&aat, "ruby");
    assert_eq!(ruby["direction"], "left");
    assert_eq!(ruby["base"], "名");
    assert_eq!(ruby["reading"], "な");
}
```

(Use/extend whatever parse-to-Value helper the existing lib.rs tests use; add `find_first_node` if absent — a depth-first search over blocks/content for the first node with the given kind.) Run → left test FAILS (today left ruby falls through `RUBY_RE` to a raw node).

- [ ] **Step 4: Rewire `ruby_node`**

In `crates/ab-aozora-aat/src/lib.rs`:

1. Where the tree is projected (`projections()`, lib.rs:274-288), also collect `aozora_json::ruby_entries(&tree)` into a `HashMap<(usize, usize), RubyEntryOwned>` keyed by `(span.start, span.end)` (deserialize the serialized entries the same lossy-local-struct way `AozoraDiagnostic` is obtained, adding a local `struct AozoraRubyEntry { span: Span, base: String, reading: String, side: String }`).
2. Replace `ruby_node`'s regex body: look up the node's span in the map; on hit emit

```rust
json!({
    "kind": "ruby",
    "base": entry.base,
    "reading": entry.reading,
    "direction": entry.side,
    "span": span_json(&node.span, &decoded.span_ctx)
})
```

   on miss fall back to `raw_node(decoded, node, "ruby")` (defensive; preserves the broken-ruby behavior). Delete `RUBY_RE` and the `regex` use if now unused (`cargo build` tells you; if `regex` drops out of the crate's dependencies, remove it from Cargo.toml too).
3. **Parity hazard, verify now, not at the gate:** run the adapter over `tests/data/broken-ruby-utf8.txt` and diff against its golden. If the structured path now resolves nodes the regex refused (raw → typed upgrades), STOP and escalate to the controller with the diff — that is a corpus-visible behavior change the delta-audit ruby class (byte-identical right-ruby) will reject, and the ruling (constrain emission vs. extend the class) is the human's.

- [ ] **Step 5: Re-baseline goldens + tripwire**

The join key now reads `facade 0.3.0`, so all 4 golden files and the tripwire literal change:

```bash
cargo test -p ab-aozora-aat 2>&1 | head -30   # observe the golden/tripwire failures
for f in plain-ascii.txt broken-ruby-utf8.txt full-markup-utf8.txt full-markup-shift_jis.txt; do
  cargo run -q --bin ab-aozora -- --mode aat < crates/ab-aozora-aat/tests/data/$f > crates/ab-aozora-aat/tests/goldens/$f.expected.json
done
python3 reports/aat-fidelity/verify-golden-spans.py   # all spans re-verified
```

Wait — the goldens are hand-verified: regeneration is allowed only because this task's diff to them is (a) the version string and (b) ruby nodes whose base/reading you just proved byte-identical in Step 3/4. Diff each regenerated golden against git and confirm the diff contains ONLY those two classes; anything else is a defect in this task. Re-paste the tripwire literal (`lib.rs:1241`) using the documented probe procedure (lib.rs:1226-1238): run the deliberately-wrong assert, paste the actual bytes from the panic verbatim.

- [ ] **Step 6: Full workspace + guard**

Run: `cargo test --workspace` (expect green), `cargo clippy --workspace -- -D warnings`, `bash tests/workspace-no-preserve-order.sh`, `just preserve-order-hazard-check`.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aozora-facade crates/ab-aozora-aat Cargo.lock
git commit -m "feat(parser): structured ruby via facade ruby_entries — direction left, regex reparse retired (facade 0.3.0)"
```

---

### Task 5: Typed layout emission — promote the five layout x- fields

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (emission sites 494-498, 508-512, 583-595, 614-626, 864-874; unit test at 1509)
- Modify: `crates/ab-aozora-aat/tests/goldens/*.expected.json` (regenerate affected)

**Interfaces:**
- Consumes: Task 2's schema v2 field names; the x-field inventory table (its "promoted" rows are this task's complete work list — all five, no more).
- Produces: v2 layout emission: `jisage_block.indent`, chitsuki style `align`/`offset_from_end`, burasage style `indent_first`/`indent_rest`, heading `indent` (conditional). Task 8's migration class encodes exactly these renames.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn jisage_block_emits_typed_indent() {
    let aat = aat_value_for("［＃ここから２字下げ］\n本文\n［＃ここで字下げ終わり］\n");
    let block = find_first_node(&aat, "jisage_block");
    assert_eq!(block["indent"], 2);
    assert!(block.get("x-indent").is_none());
}

#[test]
fn chitsuki_style_emits_typed_align_offset() {
    let aat = aat_value_for("本文［＃地から２字上げ］\n");
    let style = find_first_node(&aat, "style");
    assert_eq!(style["align"], "right");
    assert_eq!(style["offset_from_end"], 2);
    assert!(style.get("x-align").is_none() && style.get("x-offset").is_none());
    assert_eq!(style["x-provenance"], "source-derived");  // provenance retained
}

#[test]
fn burasage_style_emits_typed_first_rest() { /* pinned (6,7) compound input; indent_first==6, indent_rest==7, no x- names */ }

#[test]
fn heading_emits_typed_indent_when_indented() { /* indented-heading input; heading["indent"] present, x-indent absent */ }
```

(For the chitsuki/burasage/heading inputs, copy the source strings from the existing unit tests around those emission sites — they already exercise each path; assert the typed names.) Run → FAIL.

- [ ] **Step 2: Rename at the five sites**

Mechanical `json!` key edits, values untouched: 494-498 and 508-512 `"x-indent"` → `"indent"`; 590-591 `"x-align"` → `"align"`, `"x-offset"` → `"offset_from_end"`; 621-622 `"x-indent-first"` → `"indent_first"`, `"x-indent-rest"` → `"indent_rest"`; 872 `heading["x-indent"]` → `heading["indent"]`. `x-provenance` at 592/623/869 stays. Update the unit test at lib.rs:1509 (`x-indent` absence assert) to the typed name.

- [ ] **Step 3: Tests pass + golden refresh**

`cargo test -p ab-aozora-aat` — golden failures expected for `full-markup-*` (they contain jisage/heading). Regenerate those two exactly as Task 4 Step 5 (loop + verify-golden-spans.py), diff-audit: only key renames. `plain-ascii`/`broken-ruby` must be byte-unchanged this task. Tripwire: `"あ\n"` has no layout — literal unchanged; the tripwire test must stay green WITHOUT re-pasting (if it fails, this task leaked a change it must not make).

- [ ] **Step 4: Commit**

```bash
git add crates/ab-aozora-aat
git commit -m "feat(parser): typed layout fields replace x- layout emission (schema v2)"
```

---

### Task 6: `jizume_block` emission

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (container dispatch around 516-543; recognizers 688-704 already exist; compound test at 1567-1577)
- Modify: `crates/ab-aozora-aat/tests/goldens/full-markup-*.expected.json` if the full-markup inputs contain jizume (check; regenerate only if diff appears)

**Interfaces:**
- Consumes: `jizume_open_chars(source) -> Option<u64>` and `is_jizume_close(source)` (Phase 3 Task 8, emission-free until now); `find_matching_container_close(content, start, needle)`; the keigakomi/yokogumi arm shape (lib.rs:516-543).
- Produces: `jizume_block` emission with required `width`; compound jisage+jizume wraps the existing compound output in `jizume_block`. Task 8's jizume migration class mirrors exactly this logic.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn paired_jizume_emits_jizume_block() {
    let aat = aat_value_for("［＃ここから２１字詰め］\n本文\n［＃ここで字詰め終わり］\n");
    let block = find_first_node(&aat, "jizume_block");
    assert_eq!(block["width"], 21);
    assert_eq!(block["children"].as_array().unwrap().len(), 1);
}

#[test]
fn unpaired_jizume_open_stays_raw() {
    let aat = aat_value_for("［＃ここから２１字詰め］\n本文\n");
    assert!(find_node(&aat, "jizume_block").is_none());
    // the open survives as a raw containerOpen node — zero silent drops
}

#[test]
fn compound_jisage_jizume_nests_jizume_block() {
    let aat = aat_value_for("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここで字下げ終わり］\n");
    let jizume = find_first_node(&aat, "jizume_block");
    assert_eq!(jizume["width"], 21);
    // children carry the burasage classification exactly as before, now typed (6,7)
    let style = find_first_node(&jizume, "style");
    assert_eq!(style["indent_first"], 6);
    assert_eq!(style["indent_rest"], 7);
}
```

Replace `compound_jizume_still_classifies_burasage_and_emits_no_jizume_block` (lib.rs:1567-1577) with the third test above — its old assertion (no jizume_block) is the v1 behavior this task retires. Run → FAIL.

- [ ] **Step 2: Implement the arms**

In the container dispatch (alongside the keigakomi arm at lib.rs:516):

1. **Pure jizume**: when the node `is_container_open_raw` and `jizume_open_chars(source) == Some(width)` and the marker contains no `字下げ` segment, find the close with `find_matching_container_close(&content, index + 1, "字詰め")` (matches `［＃ここで字詰め終わり］` via the contains-needle rule); on hit emit

```rust
blocks.push(json!({
    "kind": "jizume_block",
    "width": width,
    "children": blocks_from_inline_content(inner)
}));
```

   with the same `push_paragraph_if_not_empty` / `strip_boundary_newlines` / `strip_next_leading_newline` / index-advance choreography as the keigakomi arm, verbatim. On no-close: fall through (raw preserved).
2. **Compound**: locate the existing compound-marker path (the one the old 1567 test pinned — where `［＃ここから…字下げ、折り返して…、N字詰め］` classifies as burasage). Where that path currently pushes its classified output, wrap: if `jizume_open_chars(marker) == Some(width)`, collect the same children and push them inside a `jizume_block {width}` instead of directly. Do not touch the close-matching logic — the compound container still closes on its existing close.

- [ ] **Step 3: Tests + goldens + workspace**

`cargo test -p ab-aozora-aat` → new tests pass. Check `grep -l 字詰め crates/ab-aozora-aat/tests/data/*`; regenerate any golden whose input contains jizume (diff-audit: only jizume container formation). `cargo clippy -p ab-aozora-aat -- -D warnings`.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-aozora-aat
git commit -m "feat(parser): jizume_block emission — paired and compound forms, width typed"
```

---

### Task 7: Warning enrichment — façade code/severity/span passthrough

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (`AozoraDiagnostic` at 127-132, `diagnostic_warning` at 1115-1127)
- Modify: `crates/ab-aozora-aat/tests/goldens/*.expected.json` (regenerate any whose input produces diagnostics — check broken-ruby)

**Interfaces:**
- Consumes: façade wire `Diagnostic {kind, code, severity, span, …}` (json.rs:411-421; severity vocabulary `error`/`warning`/`note` from `severity_str`); schema v2 `warning` def (Task 2).
- Produces: `meta.warnings[]` entries `{code, severity, message, span}` — same population (façade-diagnostic-derived only; there are zero adapter-origin warning sites today, and this task's evidence note records that classification), enriched shape. Task 8's warning-projection class depends on: `message` unchanged from v1, `code == message.replace('_', "-")` when code came from the façade, `span.line_start` equal to v1's `line`.

- [ ] **Step 1: Failing test**

```rust
#[test]
fn warnings_carry_facade_code_severity_span() {
    // any input that produces at least one diagnostic — reuse the input the
    // diagnostics_json_from_bytes_emits_schema3_envelope_with_codes test uses (lib.rs:1247)
    let aat = aat_value_for(DIAG_PRODUCING_INPUT);
    let w = &aat["meta"]["warnings"][0];
    assert!(w["code"].as_str().unwrap().chars().all(|c| c != '_'), "kebab code");
    assert!(matches!(w["severity"].as_str().unwrap(), "error" | "warning" | "note"));
    assert!(w.get("message").is_some());
    assert!(w.get("line").is_none(), "line dropped in v2");
    let span = &w["span"];
    assert!(span["line_start"].as_u64().unwrap() >= 1);
    assert!(span["byte_end"].as_u64().unwrap() >= span["byte_start"].as_u64().unwrap());
}
```

Run → FAIL.

- [ ] **Step 2: Implement**

1. `AozoraDiagnostic` (lib.rs:127-132) gains `code: Option<String>` (serde picks it off the wire entry that already carries it — the deserialize boundary stops dropping it).
2. Replace `diagnostic_warning`:

```rust
fn diagnostic_warning(diagnostic: &AozoraDiagnostic, ctx: &SpanContext) -> Value {
    let message = diagnostic.kind.clone().unwrap_or_else(|| "aozora diagnostic".to_owned());
    let code = diagnostic
        .code
        .clone()
        .unwrap_or_else(|| message.replace('_', "-"));
    // severity_str's non-exhaustive default arm is "error"; mirror that here so an
    // absent severity surfaces loudly rather than passing as benign.
    let severity = diagnostic.severity.clone().unwrap_or_else(|| "error".to_owned());
    let mut warning = json!({ "code": code, "severity": severity, "message": message });
    if let Some(span) = diagnostic.span.as_ref() {
        warning["span"] = span_json(span, ctx);
    }
    warning
}
```

   (`span_json` is the existing helper `ruby_node` uses; it applies `to_decoded`/`to_decoded_end`/`line_of` — the same composition, so `span.line_start` equals the old `line` value by construction.)
3. Evidence note: in this task's report file, record the emission-site classification the spec requires: one constructor (`diagnostic_warning`), one call site (`lib.rs:409-412`), **all façade-passthrough; adapter-origin sites: none**.

- [ ] **Step 3: Tests + goldens**

`cargo test -p ab-aozora-aat` — regenerate goldens whose inputs produce diagnostics (expect `broken-ruby-utf8`; diff-audit: warnings array reshape only). Tripwire: `"あ\n"` produces no diagnostics → literal unchanged, test stays green untouched.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-aozora-aat
git commit -m "feat(parser): meta.warnings carry facade code/severity/span verbatim (schema v2 shape)"
```

---

### Task 8: `audit-aat-delta.py` — `v2-migration` mode (lands BEFORE the C3 close so the candidate checkout contains it)

**Files:**
- Modify: `reports/aat-fidelity/audit-aat-delta.py`
- Test: `reports/aat-fidelity/tests/test_audit_aat_delta.py` (20 existing tests stay green)

**Interfaces:**
- Consumes: the existing mode-handler pattern (`handler(base_doc, cand_doc, name, summary)`, exit 0/2 only, fail-closed), `strip_identity`, the container-grammar helpers (`rewrite_blocks`, `scan_segment`, `CONSTRUCTS`), and Tasks 4–7's exact emission semantics.
- Produces: `audit-aat-delta.py v2-migration BASELINE_DIR CANDIDATE_DIR --summary-json PATH` — forward-rewrites each v1 baseline doc to its expected v2 form and requires deep equality with the candidate. Summary classes: `{"migrated": n, "jizume_rewritten": n, "ruby_left_rewritten": n}` (a doc counts in the most specific class it triggered; every doc changes at least mechanically, so there is no `identical` bucket in this mode). Task 10 runs it corpus-wide from the C3 checkout.

The forward rewrite, mirroring `lib.rs` semantics ONLY (never corpus-fitted — Phase 3 discipline; any grammar-fidelity gap found at the gate is a controller escalation, not a quiet patch):

1. **Root**: `version` 1 → 2; strip `/meta/adapter_version` both sides (existing `strip_identity`).
2. **Warnings projection** (not fully derivable forward — severity/span are new information): for `meta.warnings`, check by index: equal length; `cand.message == base.message`; `cand.code == base.message.replace("_", "-")`; `cand.severity in {"error","warning","note"}`; if `base` had `line` and `cand` has `span`: `cand.span.line_start == base.line`; `cand` has no `line` key. Then, for the deep-equality step, replace both arrays with a sentinel (`"__warnings_checked__"`) so already-verified fields don't double-fail.
3. **Layout renames** (recursive over all nodes): key renames with identical values — `x-indent`→`indent` (on `jisage_block` and `heading` nodes), `x-align`→`align` + `x-offset`→`offset_from_end` (style nodes with `style_type=="chitsuki"`), `x-indent-first`→`indent_first` + `x-indent-rest`→`indent_rest` (style `burasage`).
4. **Ruby**: baseline `ruby` nodes must be byte-identical in the candidate (base, reading, span) with `direction` still `"right"` — any candidate change to a right-ruby's base/reading is a FAIL. Baseline `raw` nodes with `x-source-marker-kind == "ruby"` whose `source` matches the left-ruby marker `［＃「{base}」の左に「{reading}」のルビ］` rewrite to `{"kind":"ruby","base":base,"reading":reading,"direction":"left","span":<same span>}`. A raw-ruby node whose source does NOT match the left form must survive unchanged (broken ruby stays raw).
5. **Jizume container formation**: extend the container grammar — a jizume entry whose open-matcher is a function: a raw containerOpen whose trimmed source satisfies the Python transcription of `jizume_open_chars` (`［＃ここから…N字詰め］`, jizume segment last), close-needle `"字詰め"`, producing `{"kind":"jizume_block","width":N,"children":[…]}` via the same `scan_segment`/`make_para` choreography the keigakomi entry uses. Compound form (marker also contains `字下げ`): the baseline's burasage-classified output gets wrapped in `jizume_block{width}` — mirror Task 6's wrap rule.

- [ ] **Step 1: Write failing tests** — add to `test_audit_aat_delta.py` (using its existing fixture-builder helpers): `test_v2_migration_mechanical_only_passes`, `test_v2_migration_rejects_changed_right_ruby`, `test_v2_migration_left_ruby_upgrade`, `test_v2_migration_rejects_unexpected_raw_to_ruby_upgrade`, `test_v2_migration_jizume_formation`, `test_v2_migration_rejects_leftover_x_layout`, `test_v2_migration_warning_code_mismatch`, `test_v2_migration_compound_jizume_wrap` — 8 tests, semantics per the numbered contract above. Run: `python -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -v` → 8 FAIL (unknown mode), 20 pass.

- [ ] **Step 2: Implement** — add `"v2-migration"` to `choices`, the three class buckets, `v2_migration_mode(base_doc, cand_doc, name, summary)`, dispatch wiring. New helpers: `migrate_warnings(base_meta, cand_meta, name)`, `rename_layout_keys(node)`, `rewrite_left_ruby(node)`, `jizume_open_width(source)`.

- [ ] **Step 3: Run** — 28/28 pass. Commit:

```bash
git add reports/aat-fidelity/audit-aat-delta.py reports/aat-fidelity/tests/test_audit_aat_delta.py
git commit -m "feat(instruments): audit-aat-delta v2-migration mode — schema v2 forward rewrite"
```

---

### Task 9: C3 identity close — 0.4.0, aat-schema 2, document version 2, full golden re-baseline, identity file

**Files:**
- Modify: `crates/ab-aozora-aat/Cargo.toml:4`, `crates/ab-aozora/Cargo.toml:4` (`0.3.0` → `0.4.0`)
- Modify: `crates/ab-aozora-aat/src/lib.rs` (format literal at 1158, doc `"version": 1` at 414, tripwire at 1239-1244)
- Modify: all 4 goldens (regenerate)
- Write (untracked, machine-readable): `$(git rev-parse --show-toplevel)/.superpowers/sdd/phase4-identity.json`

**Interfaces:**
- Consumes: Tasks 4–8 merged (emission complete AND the migration instrument already in-tree).
- Produces: `adapter_version()` = `ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git …)`; documents carry `"version": 2`. **HEAD after this task's commit is C3** — persisted to the identity file and the ledger; Tasks 10–11 read the file, never HEAD.

- [ ] **Step 1: Failing test**

```rust
#[test]
fn c3_identity_join_key_and_document_version() {
    assert!(ab_aozora_aat::adapter_version()
        .starts_with("ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3"));
    let aat = aat_value_for("あ\n");
    assert_eq!(aat["version"], 2);
}
```

- [ ] **Step 2: Flip the four literals** — both Cargo.toml versions → `0.4.0`; `lib.rs:1158` `"aat-schema 1"` → `"aat-schema 2"`; `lib.rs:414` `"version": 1` → `"version": 2`.

- [ ] **Step 3: Re-baseline everything version-bearing** — regenerate all 4 goldens (Task 4 Step 5 loop), run `python3 reports/aat-fidelity/verify-golden-spans.py`, diff-audit each (this task's diff is exactly `"version": 2` + the adapter_version string), re-paste the tripwire literal via the probe procedure. Run `cargo test --workspace && cargo clippy --workspace -- -D warnings && bash tests/workspace-no-preserve-order.sh && just preserve-order-hazard-check` → all green.

- [ ] **Step 4: Schema-validity spot check**

```bash
cargo run -q --bin ab-aozora -- --mode aat < crates/ab-aozora-aat/tests/data/full-markup-utf8.txt > target/c3-sample.json
python3 - <<'PYEOF'
import json
doc = json.load(open("target/c3-sample.json"))
assert doc["version"] == 2
assert "aat-schema 2" in doc["meta"]["adapter_version"]
for w in doc["meta"]["warnings"]:
    assert set(w) >= {"code", "severity", "message"} and "line" not in w
print("C3-SAMPLE OK")
PYEOF
```

- [ ] **Step 5: Commit + persist C3**

```bash
git add crates/ab-aozora-aat crates/ab-aozora Cargo.lock
git commit -m "feat(parser): C3 identity — ab-aozora 0.4.0, aat-schema 2, document version 2"
ROOT=$(git rev-parse --show-toplevel); C3=$(git rev-parse HEAD)
mkdir -p "$ROOT/.superpowers/sdd"
[ -f "$ROOT/.superpowers/sdd/phase4-identity.json" ] || echo '{}' > "$ROOT/.superpowers/sdd/phase4-identity.json"
python3 -c "import json,sys; p,k,v=sys.argv[1:]; d=json.load(open(p)); d[k]=v; json.dump(d,open(p,'w'),indent=1)" "$ROOT/.superpowers/sdd/phase4-identity.json" c3 "$C3"
```

Ledger: `Phase 4 C3 = $C3` (must equal the identity file — the file is authoritative for tooling, the ledger for humans).

---

### Task 10: C3 gate — migration delta, conformance, perf, conversion audit (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-12-phase4-c3-delta.{md,summary.json}`
- Create: `docs/superpowers/reports/2026-07-12-phase4-c3-conformance.{md,summary.json}` (+ `-seed` variants) and `…-c3-conformance-gate.summary.json`
- Create: `docs/superpowers/reports/2026-07-12-phase4-c3-perf.{md,summary.json,runner.json}`
- Create: `docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c3-conversion-audit.{md,summary.json}` + `…-c3-compat.edn`

**Interfaces:**
- Consumes: C3 from the identity file; baseline dump `/db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5`; `audit-aat-delta.py v2-migration` (in the C3 checkout — Task 8 < C3); mapping v2 (Task 3).
- Produces: three PASS gate summaries (`stage: "c3"`, gates `migration` / `conformance` / `perf`), the conversion-audit pair + compat.edn Task 11 copies verbatim, dump `/db/ab-validator/aat-corpus/ab-aozora-phase4-c3-<C3:0:7>` (retained; Task 15's baseline).

- [ ] **Step 1: Resolve C3 from the identity file** (never HEAD):

```bash
ROOT=$(git rev-parse --show-toplevel)
CANDIDATE=$(python3 -c "import json;print(json.load(open('$ROOT/.superpowers/sdd/phase4-identity.json'))['c3'])")
grep -F "Phase 4 C3 = $CANDIDATE" "$ROOT/.superpowers/sdd/progress.md"   # cross-check; STOP on mismatch
git push origin HEAD:refs/heads/feat/parser-fork-phase4
```

- [ ] **Step 2: Detached clean build on hinoki**

```bash
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha && git fetch origin && (git worktree add --detach ~/Projects/soranoha/.worktrees/parser-fork-phase4 $CANDIDATE 2>/dev/null || git -C ~/Projects/soranoha/.worktrees/parser-fork-phase4 checkout --detach $CANDIDATE)"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase4 && test -z \"\$(git status --porcelain)\" && git rev-parse HEAD"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=$CANDIDATE && cargo build --package ab-aozora --release && ./target/release/ab-aozora --version | grep -F $CANDIDATE && sha256sum ./target/release/ab-aozora"
```

Expected version line: `ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git $CANDIDATE)`. Record `bin_sha256`.

- [ ] **Step 3: Full-corpus dump**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh --adapter ab-aozora --adapter-bin ./target/release/ab-aozora --jobs 32 --report-id phase4-c3 --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase4-c3-'"${CANDIDATE:0:7}"' > ~/phase4-c3-run.log 2>&1 &'
# poll synchronously until 17886:
ssh hinoki.hyakutake-barbel.ts.net 'tail -3 ~/phase4-c3-run.log; ls /db/ab-validator/aat-corpus/ab-aozora-phase4-c3-'"${CANDIDATE:0:7}"'/aat/ab-aozora 2>/dev/null | wc -l'
```

- [ ] **Step 4: Migration delta audit vs the Phase 3 span dump**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && python3 reports/aat-fidelity/audit-aat-delta.py v2-migration /db/ab-validator/aat-corpus/ab-aozora-phase3-span-a3f91f5/aat/ab-aozora /db/ab-validator/aat-corpus/ab-aozora-phase4-c3-'"${CANDIDATE:0:7}"'/aat/ab-aozora --summary-json ~/phase4-c3-delta.json; echo "exit=$?"'
```

Expected: `exit=0`, `compared: 17886`, class sum == 17886; `jizume_rewritten` on the order of the paired-jizume work set (matrix universe 3,239 markers — record the exact count); `ruby_left_rewritten` recorded (first corpus-wide left-ruby measurement). **On exit 2:** collect example work IDs and STOP — controller escalation; instrument grammar-fidelity fixes are reports-only commits (C3 unchanged); parser bugs move C3 and re-run the stage.

- [ ] **Step 5: Conformance, both suites** (local, candidate-bound per the global rule):

```bash
export AB_AOZORA_GIT_REV=$CANDIDATE RUSTC_WRAPPER= SCCACHE_DISABLE=1
git diff --quiet $CANDIDATE HEAD -- crates Cargo.toml Cargo.lock || { echo "HEAD diverged from C3 in code paths"; exit 1; }
cargo build --release -p ab-aozora
./target/release/ab-aozora --version | grep -F "$CANDIDATE"
just aozora-notation-spec-comparison "" docs/superpowers/reports/2026-07-12-phase4-c3-conformance.md docs/superpowers/reports/2026-07-12-phase4-c3-conformance.summary.json
just official-docs-seed-comparison docs/superpowers/reports/2026-07-12-phase4-c3-conformance-seed.md docs/superpowers/reports/2026-07-12-phase4-c3-conformance-seed.summary.json
./target/release/ab-aozora --version | grep -F "$CANDIDATE"   # re-assert: no recipe rebuilt it unbound
python3 - <<'PYEOF'
import json
d = json.load(open('docs/superpowers/reports/2026-07-12-phase4-c3-conformance.summary.json'))
must = [r for r in d['rows'] if r['adapter'] == 'ab-aozora' and r['level'] == 'must']
fails = [r['vector'] for r in must if r['status'] == 'fail']
skips = [r['vector'] for r in must if r['status'] == 'skip']
print(f"must={len(must)} fail={fails} skip={skips}")
assert len(must) == 25 and not fails and not skips, "25/25 must gate FAILED"
PYEOF
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-11-phase3-span-conformance.summary.json docs/superpowers/reports/2026-07-12-phase4-c3-conformance.summary.json --adapter ab-aozora
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-11-phase3-span-conformance-seed.summary.json docs/superpowers/reports/2026-07-12-phase4-c3-conformance-seed.summary.json --adapter ab-aozora
```

Expected: 25/25 must; both comparators exit 0 (differing = 0 — AAT-lane scoring reads diagnostics only and Phase 4 does not touch the diagnostics wire). Any nonzero: classify every row diff in the gate report; a fork defect moves C3; a vector defect is span-deviation-manifest territory (fail-closed amendment rules), never a vector edit.

- [ ] **Step 6: Perf workset** (baseline = frozen legacy lane, candidate = the Step 2 binary):

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build --release --manifest-path adapters/aozora/Cargo.toml && AOZ=$(nix build --no-link --print-out-paths .#upstream-parser-aozora)/bin/aozora && nohup python3 reports/aat-fidelity/run-perf-workset.py --workset data/perf-workset.json --corpus /db/ab-validator/perf-workset-corpus-v1 --baseline-cmd "env AB_AOZORA_BIN=$AOZ adapters/aozora/target/release/aozora-adapter --mode aat" --baseline-id-bin adapters/aozora/target/release/aozora-adapter --candidate-cmd "./target/release/ab-aozora --mode aat" --candidate-id-bin ./target/release/ab-aozora --runs 5 --out ~/phase4-c3-perf.runner.json > ~/phase4-c3-perf.log 2>&1 &'
```

Expected: `new_timeouts: 0`, `verdict: PASS`. Record `median_delta_pct` against the −4.09% Phase 3 margin either way.

- [ ] **Step 7: Conversion audit — mapping v2** (doubles as corpus-wide v2 schema validation — every document validates against the v2 tuple during conversion):

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build -p ab-aat-to-parser-ir --release && nohup ./target/release/ab-aat-to-parser-ir audit-corpus --aat-dir /db/ab-validator/aat-corpus/ab-aozora-phase4-c3-'"${CANDIDATE:0:7}"'/aat/ab-aozora --mapping data/aat-to-parser-ir-mapping-v2.json --summary-json ~/phase4-c3-audit.summary.json --report-md ~/phase4-c3-audit.md --compat-edn-out ~/phase4-c3-compat.edn --jobs 32 --abc-root data/abc-schemas > ~/phase4-c3-audit.log 2>&1 &'
```

Expected: 17886/17886/0; summary `mapping.mapping_version == "0.3.0"`; its `mapping_hash` is THE frozen v2 hash all later citations repeat. Locally `cargo test -p ab-aat-to-parser-ir` green at C3.

- [ ] **Step 8: Evidence + freeze** — copy down the four hinoki artifacts; write the three gate summaries (`stage: "c3"`, shared candidate triple; `migration` details = the delta summary verbatim; `conformance` details include `must_fail: 0`, `must_skip: 0`, `differing_full: 0`, `differing_seed: 0`; `perf` details include `new_timeouts: 0`, `median_delta_pct`); write the narrative reports; commit (reports-only — C3 does not move):

```bash
git add docs/superpowers/reports/2026-07-12-phase4-c3-* docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c3-*
git commit -m "docs(reports): phase 4 C3 gate evidence — migration/conformance/perf/conversion PASS"
```

Ledger: `C3 gates PASS (C3=<sha>, bin=<sha256>, migrated=<n>/jizume=<n>/ruby_left=<n>)`.

---

### Task 11: C3 registry row (abc, inert)

**Files:**
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (append one row)
- Possibly modify: the abc Malli entry schema IF it pins `:aat_version` (Step 2)

**Interfaces:**
- Consumes: Task 10's `…-c3-compat.edn`.
- Produces: the C3 row (`:aat_version 2`, C3 join key, mapping 0.3.0 + v2 hash) — inert until activation; Task 20's checkpoint re-reads it.

- [ ] **Step 1: Append the row** — copy the emitted compat.edn entry **verbatim** (every key, every number, the `:corpus` label exactly as emitted). Verify `:aat_version 2` and `:mapping_version "0.3.0"` in the emitted values (if the tool emitted `:aat_version 1`, that is a converter bug: fix `CompatibilityCandidate.aat_version` to carry `mapping.source_aat_version` — a code change that moves C3 and re-runs Task 10; STOP and escalate first).

- [ ] **Step 2: Validate abc-side**

```bash
cd ../abc
grep -rn "aat_version" src/abc/ --include="*.clj" | grep -v "tools/aat_parser_ir_compat"   # find any := 1 Malli pin
clojure -M -m abc.tools.adr-governance          # expect: ADR governance valid
clojure -M:test:kaocha -m kaocha.runner          # expect: 0 failures
```

If Malli pins `:aat_version` to 1, widen to `[:enum 1 2]` in the same commit (the spec's registry-schema risk item, resolved here).

- [ ] **Step 3: Commit**

```bash
git add data/aat-parser-ir-compatibility.edn   # plus the Malli file if touched
git commit -m "feat(registry): ab-aozora 0.4.0 C3 row (aat v2, mapping 0.3.0) — inert pre-activation"
```

Ledger: `Task 11 complete (C3 row, kaocha green, governance valid)`.

---

### Task 12: Terminal-provenance / colophon measurement split — stateful classifier (instrument)

**Files:**
- Create: `reports/lib/terminal_provenance.py`
- Create: `reports/source-regions/terminal-provenance-split.py`
- Create: `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.{md,summary.json}`
- Test: `reports/source-regions/tests/test_terminal_provenance_split.py`

**Interfaces:**
- Consumes: `reports/lib/source_region.py` (read first — the split refines its terminal_provenance/colophon boundary and must not contradict its region model); the corpus on hinoki.
- Produces: **the normative boundary rule is a state machine, not a per-line predicate** (a lone date line is provenance after 底本： but colophon after 入力： — line-local classification cannot distinguish them):

```python
# reports/lib/terminal_provenance.py
State = str   # "provenance" | "colophon"
Class = str   # "terminal_provenance" | "colophon_metadata" | "blank"

# Ordered transition rules — THE normative rule; the report embeds this table and
# Task 14's Rust transcription mirrors it case-for-case.
def classify_tail(lines: list[str]) -> list[Class]:
    """Classify every tail line. Fail-closed: raises UnclassifiableTail (carrying
    the line and its index) if a non-blank line arrives before any state-setting
    head line — the caller exits 2."""
    state: State | None = None
    out: list[Class] = []
    for line in lines:
        stripped = line.strip()
        if not stripped:
            out.append("blank")            # blank: class blank, state unchanged
            continue
        if is_provenance_head(stripped):   # 底本：/底本の親本： (+ heads found by the corpus scan)
            state = "provenance"
            out.append("terminal_provenance")
        elif is_colophon_head(stripped):   # 入力：/校正：/青空文庫作成ファイル：/※… (+ heads found by the scan)
            state = "colophon"
            out.append("colophon_metadata")
        elif state == "provenance":
            out.append("terminal_provenance")   # continuation (edition/date lines) inherits
        elif state == "colophon":
            out.append("colophon_metadata")
        else:
            raise UnclassifiableTail(line)
    return out
```

  Summary JSON fields Tasks 13/15/20 read: `works_scanned`, `works_with_terminal_provenance`, `terminal_provenance_lines`, `colophon_lines`, `boundary_rule` (the head-marker lists + the transition rules, as data).

- [ ] **Step 1: Failing tests** — `classify_tail` cases pinning exactly the stateful behavior: `底本：…` → provenance; continuation date line AFTER 底本： → provenance; the SAME date-shaped line AFTER 入力： → colophon (the distinguishing case); `底本の親本：…` continuation → provenance; `入力：…`/`校正：…`/`青空文庫作成ファイル：`/`※…` → colophon; blank between 底本 block and 入力 block → blank with state preserved; non-blank line before any head → `UnclassifiableTail`; generator-level: unclassifiable tail → `SystemExit(2)` with ≤20 examples listed.

- [ ] **Step 2: Implement** classifier + generator. Generator CLI: `terminal-provenance-split.py --corpus-root DIR --summary-json PATH --report-md PATH [--jobs N]`; work discovery mirrors `reports/source-regions/source-region-disposition-samples.py` (read its corpus-iteration code and reuse the same mechanism); the head-marker lists start from the skeleton above and are extended ONLY by the corpus scan's residuals, each extension recorded in the report with examples; a non-empty unclassifiable residual FAILS the run (exit 2).

- [ ] **Step 3: Full-corpus run on hinoki** (nohup + synchronous poll, log `~/phase4-split.log`); copy down report + summary; cross-check `works_with_terminal_provenance` and the line counts against the 2026-07-06 coverage reference (609 terminal_provenance / 89,416 colophon occurrences — confirm or correct, explaining any delta).

- [ ] **Step 4: Commit**

```bash
git add reports/lib/terminal_provenance.py reports/source-regions/terminal-provenance-split.py reports/source-regions/tests/ docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.*
git commit -m "feat(instruments): terminal-provenance/colophon split — stateful normative boundary rule"
```

---

### Task 13: `audit-aat-delta.py` — `source-note-append` mode (lands BEFORE the C4 close)

**Files:**
- Modify: `reports/aat-fidelity/audit-aat-delta.py`
- Test: `reports/aat-fidelity/tests/test_audit_aat_delta.py`

**Interfaces:**
- Consumes: the mode-handler pattern; Task 14's emission contract (written here first — this mode IS the contract's executable form; Task 14 implements to it).
- Produces: `audit-aat-delta.py source-note-append BASELINE_DIR CANDIDATE_DIR --summary-json PATH`; classes `{"identical": n, "source_note_appended": m}`. Task 15 runs it C3-dump → C4-dump from the C4 checkout (Task 13 < C4).

The append-only confinement contract:

1. Strip `/meta/adapter_version` both sides; `version` 2 == 2.
2. `cand.blocks[:len(base.blocks)] == base.blocks` — deep equality, order preserved, zero body drift.
3. Every appended block is `kind == "source_note"`, `placement == "back"`, `region_class == "terminal_provenance"`, non-empty `content` of text inlines whose `value`s **preserve their line terminators** (each non-final value ends with the terminator it carried in source; values never concatenate lines), each span `byte_end > byte_start`, `line_start >= 1`; the block `span.byte_start` equals the first content span's `byte_start`.
4. `meta.warnings` byte-equal; any other differing `meta` key → exit 2.
5. Zero appended → `identical`; ≥1 → `source_note_appended`.

- [ ] **Step 1: Failing tests** — `test_append_mode_identical_pass`, `test_append_mode_valid_source_note_append`, `test_append_mode_rejects_body_drift`, `test_append_mode_rejects_non_source_note_append`, `test_append_mode_rejects_wrong_region_class`, `test_append_mode_rejects_inserted_not_appended`, `test_append_mode_rejects_warning_drift`, `test_append_mode_rejects_degenerate_span`, `test_append_mode_rejects_terminator_stripped_values` (two content values that would concatenate without a terminator → exit 2). 9 tests.

- [ ] **Step 2: Implement** `source_note_append_mode(...)`; wire mode + classes. Run: `python -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -v` → 37/37.

- [ ] **Step 3: Commit**

```bash
git add reports/aat-fidelity/audit-aat-delta.py reports/aat-fidelity/tests/test_audit_aat_delta.py
git commit -m "feat(instruments): audit-aat-delta source-note-append confinement mode"
```

---

### Task 14: source_note emission + C4 identity (0.5.0)

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (`sanitize_for_aat` 197-210, `aozora_body_range` 212-241, `DecodedSource` 31-48, `build_aat`, versions/tripwire)
- Modify: `crates/ab-aozora-aat/Cargo.toml`, `crates/ab-aozora/Cargo.toml` (`0.4.0` → `0.5.0`)
- Create: `crates/ab-aozora-aat/tests/data/terminal-provenance-utf8.txt` + `crates/ab-aozora-aat/tests/goldens/terminal-provenance-utf8.txt.expected.json`
- Modify: all goldens (version string regeneration)
- Write: identity file `c4` entry

**Interfaces:**
- Consumes: Task 12's `classify_tail` state machine (transcribed; the report cited in a code comment as the rule's authority); Task 13's confinement contract; `SanitizeMaps.to_source_offset/to_source_end`, `line_starts`/`line_of`.
- Produces: C4 emission — trailing `source_note` blocks (placement `back`, region_class `terminal_provenance`, terminator-preserving text inlines with real decoded-source spans); join key `ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3`. **HEAD after this task's commit is C4** (identity file + ledger).

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn terminal_provenance_tail_emits_source_note() {
    let src = "本文です。\n\n底本：「作品集」文庫社\n　1990（平成2）年5月10日発行\n入力：someone\n校正：other\n";
    let aat = aat_value_for(src);
    let notes: Vec<&Value> = top_level_blocks(&aat).iter().filter(|b| b["kind"] == "source_note").collect();
    assert_eq!(notes.len(), 1);
    let note = notes[0];
    assert_eq!(note["placement"], "back");
    assert_eq!(note["region_class"], "terminal_provenance");
    // one text inline per terminal-provenance line (底本 + its continuation date line),
    // colophon lines (入力/校正) EXCLUDED; values PRESERVE the line terminator
    let content = note["content"].as_array().unwrap();
    assert_eq!(content.len(), 2);
    assert_eq!(content[0]["value"], "底本：「作品集」文庫社\n");
    assert_eq!(content[1]["value"], "　1990（平成2）年5月10日発行\n");
    let s = &content[0]["span"];
    let (a, b) = (s["byte_start"].as_u64().unwrap() as usize, s["byte_end"].as_u64().unwrap() as usize);
    assert_eq!(&src[a..b], "底本：「作品集」文庫社\n");   // terminator inside the span
    assert_eq!(s["line_start"], 3);
}

#[test]
fn stateful_boundary_date_after_colophon_head_is_excluded() {
    // the reviewer's distinguishing case: a date-shaped line AFTER 入力： stays colophon
    let src = "本文。\n\n底本：「X」Y社\n入力：someone\n　2005（平成17）年1月1日作成\n";
    let aat = aat_value_for(src);
    let note = &top_level_blocks(&aat).iter().find(|b| b["kind"] == "source_note").unwrap().clone();
    assert_eq!(note["content"].as_array().unwrap().len(), 1);   // only the 底本 line
}

#[test]
fn tail_free_work_has_no_source_note() {
    let aat = aat_value_for("本文だけ。\n");
    assert!(top_level_blocks(&aat).iter().all(|b| b["kind"] != "source_note"));
}
```

(`top_level_blocks` — helper returning `aat["blocks"].as_array()`; add beside `find_first_node`.)

- [ ] **Step 2: Implement**

1. `aozora_body_range` already computes `body_end`; return the full `Range` from `sanitize_for_aat` and store on `DecodedSource` **both** `sanitized_tail: String` (the `sanitized[body_end..]` slice — the reviewer-identified retention: the full sanitized text was previously dropped, so the range alone was unusable) and `tail_offset: usize` (= `body_end`, sanitized coordinates).
2. Transcribe Task 12's state machine: `enum TailLineClass { TerminalProvenance, Colophon, Blank }` + `fn classify_tail(lines: &[&str]) -> Result<Vec<TailLineClass>>` with the SAME ordered rules and the same head-marker lists (comment cites `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md` as normative); unit tests mirror the Python module's cases one-for-one (drift on either side = red test on that side). Unclassifiable tail line: emit the whole tail as a raw-preserving fallback? NO — fail-closed is wrong here (the corpus scan proved the rule total on this corpus; an unclassifiable line in production input means out-of-corpus input): classify it `Colophon` (excluded from AAT, still measured by the source-region instrument) and record a `warning` (`code: "tail-line-unclassified"`, severity `warning`, message = the head bytes) so nothing is silently interpreted. Test this fallback.
3. In `build_aat`: split `sanitized_tail` into terminator-inclusive lines (`split_inclusive('\n')` — bare-CR tails: split on the same boundary set `line_starts` honors: `\n`, `\r\n`, bare `\r`; reuse/factor the Phase 3 boundary logic rather than re-deriving it); classify; group **contiguous TerminalProvenance lines** (a Blank or Colophon line ends the group) into one `source_note` per group; each line → text inline `{kind:"text", value:<terminator-inclusive line>, span}` with `byte_start = maps.to_source_offset(tail_offset + line_start_in_tail)`, `byte_end = maps.to_source_end(tail_offset + line_end_in_tail)` (NO body_offset — tail offsets are absolute sanitized coordinates; this is the `diagnostics_json_from_bytes` composition at lib.rs:377-378, reuse it), lines via `line_of`. `source_note.span` aggregates first start → last end.
4. Versions → `0.5.0` both crates; tripwire re-paste; regenerate all goldens + the new `terminal-provenance-utf8.txt` golden (content: the Step 1 test's src plus a second 底本-block variant); `python3 reports/aat-fidelity/verify-golden-spans.py` green; hand spot-check the source_note spans against raw bytes per the golden-header procedure.

- [ ] **Step 3: Full workspace** — `cargo test --workspace && cargo clippy --workspace -- -D warnings && bash tests/workspace-no-preserve-order.sh && just preserve-order-hazard-check` → green.

- [ ] **Step 4: Commit + persist C4**

```bash
git add crates/ab-aozora-aat crates/ab-aozora Cargo.lock
git commit -m "feat(parser): C4 — source_note emission for terminal provenance (0.5.0)"
ROOT=$(git rev-parse --show-toplevel); C4=$(git rev-parse HEAD)
python3 -c "import json,sys; p,k,v=sys.argv[1:]; d=json.load(open(p)); d[k]=v; json.dump(d,open(p,'w'),indent=1)" "$ROOT/.superpowers/sdd/phase4-identity.json" c4 "$C4"
```

Ledger: `Phase 4 C4 = $C4`.

---

### Task 15: C4 gate — append confinement, conformance, perf (activation floor), conversion audit (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-12-phase4-c4-confinement.{md,summary.json}`
- Create: `docs/superpowers/reports/2026-07-12-phase4-c4-conformance.{md,summary.json}` (+ `-seed`) and `…-c4-conformance-gate.summary.json`
- Create: `docs/superpowers/reports/2026-07-12-phase4-c4-perf.{md,summary.json,runner.json}`
- Create: `docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.{md,summary.json}` + `…-c4-compat.edn`

**Interfaces:**
- Consumes: C4 from the identity file; the C3 dump (baseline); Task 13's mode (in the C4 checkout); Task 12's split summary.
- Produces: three PASS gate summaries (`stage: "c4"`, gates `confinement` / `conformance` / `perf`, the perf gate carrying `activation_floor_ok`), the conversion-audit pair + compat.edn; dump `/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-<C4:0:7>` (retained — the activation run-set entry points at it).

- [ ] **Step 1: Resolve C4 from the identity file**

```bash
ROOT=$(git rev-parse --show-toplevel)
CANDIDATE=$(python3 -c "import json;print(json.load(open('$ROOT/.superpowers/sdd/phase4-identity.json'))['c4'])")
C3SHA=$(python3 -c "import json;print(json.load(open('$ROOT/.superpowers/sdd/phase4-identity.json'))['c3'])")
grep -F "Phase 4 C4 = $CANDIDATE" "$ROOT/.superpowers/sdd/progress.md"
git push origin HEAD:refs/heads/feat/parser-fork-phase4
```

- [ ] **Step 2: Detached clean build on hinoki**

```bash
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha && git fetch origin && git -C ~/Projects/soranoha/.worktrees/parser-fork-phase4 checkout --detach $CANDIDATE"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase4 && test -z \"\$(git status --porcelain)\" && git rev-parse HEAD"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=$CANDIDATE && cargo build --package ab-aozora --release && ./target/release/ab-aozora --version | grep -F $CANDIDATE && sha256sum ./target/release/ab-aozora"
```

Expected version line: `ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git $CANDIDATE)`. Record `bin_sha256`.

- [ ] **Step 3: Full-corpus dump**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh --adapter ab-aozora --adapter-bin ./target/release/ab-aozora --jobs 32 --report-id phase4-c4 --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-'"${CANDIDATE:0:7}"' > ~/phase4-c4-run.log 2>&1 &'
ssh hinoki.hyakutake-barbel.ts.net 'tail -3 ~/phase4-c4-run.log; ls /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-'"${CANDIDATE:0:7}"'/aat/ab-aozora 2>/dev/null | wc -l'
```

Poll synchronously until 17886.

- [ ] **Step 4: Append-confinement audit vs the C3 dump**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && python3 reports/aat-fidelity/audit-aat-delta.py source-note-append /db/ab-validator/aat-corpus/ab-aozora-phase4-c3-'"${C3SHA:0:7}"'/aat/ab-aozora /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-'"${CANDIDATE:0:7}"'/aat/ab-aozora --summary-json ~/phase4-c4-confinement.json; echo "exit=$?"'
```

Expected: `exit=0`, `compared: 17886`, `identical + source_note_appended == 17886`, and **`source_note_appended == works_with_terminal_provenance` from Task 12's split summary** (bind both numbers in the gate summary details; mismatch = the Rust transcription drifted from the Python rule → STOP, controller escalation).

- [ ] **Step 5: Conformance, both suites** (candidate-bound, C3 summaries as baselines):

```bash
export AB_AOZORA_GIT_REV=$CANDIDATE RUSTC_WRAPPER= SCCACHE_DISABLE=1
git diff --quiet $CANDIDATE HEAD -- crates Cargo.toml Cargo.lock || { echo "HEAD diverged from C4 in code paths"; exit 1; }
cargo build --release -p ab-aozora
./target/release/ab-aozora --version | grep -F "$CANDIDATE"
just aozora-notation-spec-comparison "" docs/superpowers/reports/2026-07-12-phase4-c4-conformance.md docs/superpowers/reports/2026-07-12-phase4-c4-conformance.summary.json
just official-docs-seed-comparison docs/superpowers/reports/2026-07-12-phase4-c4-conformance-seed.md docs/superpowers/reports/2026-07-12-phase4-c4-conformance-seed.summary.json
./target/release/ab-aozora --version | grep -F "$CANDIDATE"
python3 - <<'PYEOF'
import json
d = json.load(open('docs/superpowers/reports/2026-07-12-phase4-c4-conformance.summary.json'))
must = [r for r in d['rows'] if r['adapter'] == 'ab-aozora' and r['level'] == 'must']
fails = [r['vector'] for r in must if r['status'] == 'fail']
skips = [r['vector'] for r in must if r['status'] == 'skip']
print(f"must={len(must)} fail={fails} skip={skips}")
assert len(must) == 25 and not fails and not skips, "25/25 must gate FAILED"
PYEOF
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-12-phase4-c3-conformance.summary.json docs/superpowers/reports/2026-07-12-phase4-c4-conformance.summary.json --adapter ab-aozora
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-12-phase4-c3-conformance-seed.summary.json docs/superpowers/reports/2026-07-12-phase4-c4-conformance-seed.summary.json --adapter ab-aozora
```

Expected: 25/25 must; both comparators exit 0 (conformance vectors have no 底本 tails). Any diff: classify, block, escalate per the standing rule.

- [ ] **Step 6: Perf workset + activation floor**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build --release --manifest-path adapters/aozora/Cargo.toml && AOZ=$(nix build --no-link --print-out-paths .#upstream-parser-aozora)/bin/aozora && nohup python3 reports/aat-fidelity/run-perf-workset.py --workset data/perf-workset.json --corpus /db/ab-validator/perf-workset-corpus-v1 --baseline-cmd "env AB_AOZORA_BIN=$AOZ adapters/aozora/target/release/aozora-adapter --mode aat" --baseline-id-bin adapters/aozora/target/release/aozora-adapter --candidate-cmd "./target/release/ab-aozora --mode aat" --candidate-id-bin ./target/release/ab-aozora --runs 5 --out ~/phase4-c4-perf.runner.json > ~/phase4-c4-perf.log 2>&1 &'
```

Expected: `new_timeouts: 0`, `verdict: PASS`. The gate summary details additionally record `activation_floor_ok: candidate_median <= baseline_median`. **If false: freeze the evidence, then STOP — Tasks 16–19 must not run; escalate for the optimization-task insertion (spec decision 4).**

- [ ] **Step 7: Conversion audit — mapping v2**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build -p ab-aat-to-parser-ir --release && nohup ./target/release/ab-aat-to-parser-ir audit-corpus --aat-dir /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-'"${CANDIDATE:0:7}"'/aat/ab-aozora --mapping data/aat-to-parser-ir-mapping-v2.json --summary-json ~/phase4-c4-audit.summary.json --report-md ~/phase4-c4-audit.md --compat-edn-out ~/phase4-c4-compat.edn --jobs 32 --abc-root data/abc-schemas > ~/phase4-c4-audit.log 2>&1 &'
```

Expected: 17886/17886/0; the summary repeats mapping 0.3.0 + the frozen v2 hash byte-identically (Task 10's value). Locally `cargo test -p ab-aat-to-parser-ir` green at C4.

- [ ] **Step 8: Evidence + freeze** — gate summaries `stage: "c4"` (gates `confinement`/`conformance`/`perf`; confinement details = the audit summary + `split_works_with_terminal_provenance`; conformance details `must_fail: 0, must_skip: 0, differing_full: 0, differing_seed: 0`; perf details `new_timeouts: 0, median_delta_pct, activation_floor_ok`); narrative reports; commit (reports-only):

```bash
git add docs/superpowers/reports/2026-07-12-phase4-c4-* docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-*
git commit -m "docs(reports): phase 4 C4 gate evidence — confinement/conformance/perf/conversion PASS"
```

Ledger: `C4 gates PASS (C4=<sha>, bin=<sha256>, appended=<m>, floor_ok=<bool>)`.

---

### Task 16: C4 producer reports — pre-admission refresh (ceremony step 1)

**Files:**
- Create: `docs/superpowers/reports/2026-07-12-phase4-preadmission-coverage.{md,summary.json}`
- Create: `docs/superpowers/reports/2026-07-12-phase4-preadmission-reconciliation.{md,summary.json}` (or the reconciliation tool's native output names, dated 2026-07-12)
- Create: `docs/superpowers/reports/2026-07-12-phase4-preadmission-next-work.{md,summary.json}`

**Interfaces:**
- Consumes: the C4 conversion-audit summary (Task 15); the current (pre-activation) five-lane matrix inputs and frozen 2026-07-06/07-04 producer summaries.
- Produces: the spec's ceremony-step-1 producer evidence — **these exist BEFORE the C4 registry row and admission capture (Task 17)**. Task 20 re-runs coverage under activated wiring as a second verification, not first evidence.

- [ ] **Step 1: Coverage (direct script invocation — flags are the script's own, no just-positional friction):**

```bash
python reports/parser-ir/publication-coverage.py \
  --parser-ir-schema data/abc-schemas/schemas/parser-ir.schema.json \
  --mapping data/aat-to-parser-ir-mapping-v2.json \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --source-delta-summary docs/superpowers/reports/2026-07-06-text-policy-delta.summary.json \
  --bundle-validation-summary docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json \
  --next-work-summary docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json \
  --summary-json docs/superpowers/reports/2026-07-12-phase4-preadmission-coverage.summary.json \
  --report-md docs/superpowers/reports/2026-07-12-phase4-preadmission-coverage.md
```

(First run `python reports/parser-ir/publication-coverage.py --help` and reconcile this flag list against the actual argparse surface — the justfile recipe `parser-ir-publication-coverage-report` at justfile:529 shows the canonical pairing of inputs; use ITS input paths verbatim wherever they differ from the list above.) Required in the summary: the three counters `unsupported_body_markup_occurrences == 0`, `unknown_region_occurrences == 0`, `unknown_unreviewed_occurrences == 0`; `source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`; `parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"` (still the legacy five — activation has not happened).

- [ ] **Step 2: Reconciliation** — find the recipe: `grep -n "reconciliation" justfile` → run that recipe/script with 2026-07-12-dated outputs. Required: summary verdict `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`.

- [ ] **Step 3: Next-work ledger refresh** — run the `parser-ir-publication-next-work-report` recipe (justfile:435; read its parameter list with `just --show parser-ir-publication-next-work-report`, then invoke positionally, substituting: `CONVERSION_SUMMARY` → `docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json`, `SUMMARY_JSON`/`REPORT_MD` → the 2026-07-12 preadmission names; all other inputs verbatim from the recipe defaults). In the report, cite each open 2026-07-06 next-work item as CLOSED (with the Phase 4 evidence closing it) or CARRIED (bare toggles, keigakomi 44-residual, warigaki/kunten — with why each does not gate admission). **Consumed and closed, never replaced.**

- [ ] **Step 4: Commit (reports-only)**

```bash
git add docs/superpowers/reports/2026-07-12-phase4-preadmission-*
git commit -m "docs(reports): phase 4 pre-admission producer refresh — coverage/reconciliation/next-work"
```

---

### Task 17: C4 registry row + admission capture + policy note (abc — ceremony step 2)

**Files:**
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (append the C4 row)
- Modify: `abc/data/source-region-publication-policy-v0.json` + the vendored copy `data/abc-schemas/data/source-region-publication-policy-v0.json` (byte-identical pair)
- Create: `docs/superpowers/reports/2026-07-12-phase4-admission-report.txt`

**Interfaces:**
- Consumes: Task 15's `…-c4-compat.edn`; Task 16's producer reports (ceremony order: they exist first); Task 12's split report.
- Produces: the C4 row; the captured `admission-report` output proving `:status :admitted`; Task 20's checkpoint re-runs the admission command fail-closed and re-reads the capture.

- [ ] **Step 1: Append the C4 row** — verbatim from the emitted compat.edn (same discipline and `:aat_version 2` check as Task 11).

- [ ] **Step 2: Run and capture admission (pipefail — the tee must not mask the exit code):**

```bash
cd ../abc
set -o pipefail
clojure -M:abc/aat-compat-admission -- --candidates ../ab-validator/docs/superpowers/reports/2026-07-12-phase4-c4-compat.edn | tee ../ab-validator/docs/superpowers/reports/2026-07-12-phase4-admission-report.txt
echo "exit=$?"
```

Expected: `:status :admitted`, `exit=0`. Any `:conflict`: diff row vs emitted candidate byte-wise; the fix is always row-side (copy again verbatim), never candidate-side.

- [ ] **Step 3: Policy note** — both policy files (abc original + vendored snapshot, kept byte-identical): `terminal_provenance` and `colophon_metadata` `measurement_status: "needs_measurement_split"` → `"measured"`; `policy_version` `0.2.0` → `0.3.0`; append to `notes`: `"terminal_provenance/colophon split measured 2026-07-12 (ab-validator docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md)."`

- [ ] **Step 4: Validate + commit**

```bash
clojure -M -m abc.tools.adr-governance
clojure -M:test:kaocha -m kaocha.runner
git add data/aat-parser-ir-compatibility.edn data/source-region-publication-policy-v0.json ../ab-validator/data/abc-schemas/data/source-region-publication-policy-v0.json ../ab-validator/docs/superpowers/reports/2026-07-12-phase4-admission-report.txt
git commit -m "feat(registry): ab-aozora 0.5.0 C4 row, admission :admitted captured; source-region split measured"
```

---

### Task 18: Bundle fixtures (UNSTAGED) + design-bundle validation + plaintext assertion (ceremony step 3)

**Files:**
- Modify (LEFT UNSTAGED — Task 19 stages and commits them inside the atomic activation commit): `abc/examples/ab-validator-output/{parser-ir.json, divergence.json, manifest-inputs.json, source-region-coverage.json}` (+ `run-summary.jsonl`/`comparison-report.json` only if their embedded identity fields are stale)
- Create (committed here): `docs/superpowers/reports/2026-07-12-phase4-bundle-validation.{md,summary.json}`

**Interfaces:**
- Consumes: the C4 dump, mapping v2, `abc.tools.validate-design-bundle`, the Tasks 11/17 registry rows.
- Produces: representative + full-scope validation evidence and the plaintext-omission assertion; the fixture diff sits in the working tree for Task 19.

- [ ] **Step 1: Regenerate the converter-derived fixtures** — read `abc/examples/ab-validator-output/README.md` for the encoded source work. **The example work must contain terminal provenance** so the plaintext assertion has teeth; if the README's work has none, switch the fixture work to one from the C4 dump that does (pick the first work ID listed in Task 12's split report), note the switch in the bundle-validation report, and regenerate ALL six files for that work. Run `ab-aat-to-parser-ir convert` (see `--help` for the exact flags: input AAT doc from the C4 dump, `--mapping data/aat-to-parser-ir-mapping-v2.json`) producing `parser-ir.json` + `divergence.json`; update `manifest-inputs.json` hashes from the C4 audit summary's `mapping` block verbatim; refresh `source-region-coverage.json` from the Task 16 coverage summary.

- [ ] **Step 2: Representative validation** — `cd ../abc && clojure -M:abc/validate-design-bundle` → green (includes `validate-ab-validator-output!` + `compat/validate-registry!` against the working-tree fixtures + new rows).

- [ ] **Step 3: Full-scope validation** — same command with `TEI_SCHEMA_PATH` set to the full P5 RelaxNG. Recover the exact path with `grep -rn "TEI_SCHEMA_PATH" ../ab-validator/docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.md docs/ src/` and reuse that derivation verbatim. → green.

- [ ] **Step 4: Plaintext omission assertion** — locate the plaintext artifact the bundle pipeline renders for the example work: `ls examples/v0/example-work/` and `grep -rn "plaintext" src/abc/tools/validate_design_bundle.clj` identify the artifact path the validator writes/reads (record it in the report). Then:

```bash
python3 - <<'PYEOF'
import json, sys
ir = json.load(open("examples/ab-validator-output/parser-ir.json"))
notes = [n for n in ir.get("nodes", []) if n.get("type") == "source-note"]
assert notes, "example work has zero source-note nodes — assertion has no teeth (Step 1 requires a terminal-provenance work)"
plaintext = open(sys.argv[1] if len(sys.argv) > 1 else "examples/v0/example-work/plaintext.txt", encoding="utf-8").read()
for n in notes:
    probe = n["text"].strip()
    assert probe and probe not in plaintext, f"source-note text leaked into plaintext: {probe[:40]}…"
print(f"PLAINTEXT-OMISSION OK ({len(notes)} source-note nodes checked)")
PYEOF
```

(Pass the located artifact path as argv[1]; the fallback path is a guess to be replaced by the located one.) Record the output in the validation report.

- [ ] **Step 5: Commit evidence ONLY — assert the fixtures are not staged:**

```bash
cd ../ab-validator
test -z "$(git diff --cached --name-only -- ../abc/examples/ab-validator-output)" || { echo "fixtures are staged — unstage before committing evidence"; exit 1; }
git add docs/superpowers/reports/2026-07-12-phase4-bundle-validation.*
test -z "$(git diff --cached --name-only -- ../abc/examples/ab-validator-output)" || { echo "fixtures crept into the index"; exit 1; }
git commit -m "docs(reports): phase 4 bundle validation — representative + full-scope + plaintext omission PASS"
```

Ledger: `Task 18 complete; fixtures modified-unstaged for the activation commit`.

---

### Task 19: The atomic activation commit (ceremony step 4)

**Files (ONE commit — exactly these five surfaces, nothing else):**
- Modify: `reports/aat-fidelity/run-sets/current.json` (aozora entry REPLACED by ab-aozora — exactly five active lanes)
- Create: `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json`
- Modify: `docs/handoffs/ir-publication-coverage-contract.md`
- Modify: `justfile`
- Stage + commit: the Task 18 fixture modifications under `abc/examples/ab-validator-output/`

**Interfaces:**
- Consumes: Tasks 15–18 green (perf `activation_floor_ok: true`, admission `:admitted`, bundle validations PASS).
- Produces: `ab-aozora` is the publication lane; `current.json` holds exactly the five active evidence lanes (generic resolvers keep consuming five, matching the contract — the reviewer's active/archived split). **Branch-only until Task 20 passes; if Task 20 fails, this commit is reverted/dropped on the branch before any merge.**

- [ ] **Step 1: Preconditions** — Task 15 perf summary `activation_floor_ok: true`; Task 17 capture `:admitted`; Task 18 summary all-green; fixtures present as unstaged modifications (`git status --porcelain -- ../abc/examples/ab-validator-output` shows ` M` lines). Any missing → STOP.

- [ ] **Step 2: Run-set swap** — move the `aozora` entry out of `current.json` into the new archive file (a complete run-set document: `schema_version: 1`, `run_set_id: "aozora-legacy-archive-2026-07-12"`, `adapters: [<the aozora entry verbatim>]`), and append the `ab-aozora` entry to `current.json`, copying the removed entry's key shape:

```json
{
  "adapter_id": "ab-aozora",
  "aat_dir": "${AB_DB_ROOT}/aat-corpus/ab-aozora-phase4-c4-<C4:0:7>/aat/ab-aozora",
  "run_descriptor": "<the descriptor file run-aat-full.sh wrote in the C4 dump — ssh hinoki 'ls /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-<C4:0:7>/' and use the actual descriptor path it contains>",
  "expected": {
    "adapter_id": "ab-aozora",
    "adapter_version_contains": "<C4 full sha>",
    "content_hash": "<from the command below>"
  }
}
```

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase4/ab-validator && python3 -c "import sys; sys.path.insert(0, \"reports/lib\"); import aat_hash; print(aat_hash.hash_aat_dir(\"/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-<C4:0:7>/aat/ab-aozora\"))"'
```

- [ ] **Step 3: Coverage contract** — required-evidence lane list becomes `aozora2html, aozora-epub3, aozora-rs, aozora2, ab-aozora` (five lanes, `ab-aozora` designated publication lane); add: legacy `aozora` evidence is archived at `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json` (frozen dump `aozora-full-repin-1a4f864`), retirement pending Task 21.

- [ ] **Step 4: justfile defaults** — `grep -n "adapter aozora\b\|aozora-adapter\|AB_AOZORA_BIN" justfile reports/aat-fidelity/*.sh`, flip publication/measurement defaults (any recipe assuming `--adapter aozora` as THE lane) to `ab-aozora`; leave the conformance recipe's comparison-lane list untouched (Task 21 owns those). List every flipped recipe in the commit body.

- [ ] **Step 5: Validate + the atomic commit**

```bash
python3 reports/aat-fidelity/validate-aat-run-set.py reports/aat-fidelity/run-sets/current.json
python3 reports/aat-fidelity/validate-aat-run-set.py reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json
git add reports/aat-fidelity/run-sets/current.json reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json docs/handoffs/ir-publication-coverage-contract.md justfile ../abc/examples/ab-validator-output
git status --porcelain | grep -v "^A \|^M " && { echo "unexpected paths in the activation commit"; exit 1; } || true
git commit -m "feat(activation): ab-aozora is the publication lane — run-set swap (five active lanes), contract, justfile, bundle fixtures (atomic)"
git push origin HEAD:refs/heads/feat/parser-fork-phase4
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha && git fetch origin && git -C .worktrees/parser-fork-phase4 checkout --detach FETCH_HEAD 2>/dev/null; cd .worktrees/parser-fork-phase4/ab-validator && git checkout origin/feat/parser-fork-phase4 -- reports/aat-fidelity/run-sets/ && python3 reports/aat-fidelity/validate-aat-run-set.py --require-paths reports/aat-fidelity/run-sets/current.json && python3 reports/aat-fidelity/resolve-run-set.py'
```

The hinoki step verifies `--require-paths` + the content-hash gate against the real dump; if it fails, fix and amend BEFORE anything else lands on the branch (the activation commit must remain the single atomic change). Ledger: `ACTIVATION COMMIT <sha> (branch-only until Task 20 passes)`.

---

### Task 20: Wholesale acceptance gate + `verify-phase4-checkpoint.py` (ceremony steps 5–6)

**Files:**
- Create: `reports/aat-fidelity/verify-phase4-checkpoint.py`
- Test: `reports/aat-fidelity/tests/test_verify_phase4_checkpoint.py`
- Create: `docs/superpowers/reports/2026-07-12-phase4-acceptance-wholesale.{md,summary.json}` + `docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.{md,summary.json}`

**Interfaces:**
- Consumes: the six gate summaries, both conversion audits, the split summary, the admission capture + the live admission command, the activation commit sha, Task 16's pre-admission producer reports.
- Produces: `CHECKPOINT OK` — the merge precondition.

- [ ] **Step 1: Post-activation coverage re-verification** — re-run the Task 16 Step 1 coverage invocation with the post-activation matrix/contract state, outputs `…-postactivation-coverage.*`. Required: the same three zero counters, contract verdict, and `FIVE_PARSER_EVIDENCE_COMPLETE` with `ab-aozora` in the lane list. This is the second verification; Task 16 was the evidence of record for admission.

- [ ] **Step 2: Failing verifier tests** — `test_verify_phase4_checkpoint.py`, 16 tests: `test_all_pass`, `test_verdict_fail`, `test_commit_mismatch`, `test_bin_mismatch`, `test_wrong_version_pattern`, `test_duplicate_candidates`, `test_migration_class_sum`, `test_confinement_wrong_mode`, `test_confinement_append_count_mismatch`, `test_audit_failed_files`, `test_audit_wrong_mapping`, `test_admission_rerun_fails`, `test_activation_commit_extra_path`, `test_activation_parent_already_active`, `test_activation_child_missing_lane`, `test_acceptance_counter_nonzero`. Fixtures: temp-dir gate summaries + a scratch git repo (two commits around a synthetic activation diff) built in a pytest fixture — model the harness on `test_verify_phase3_checkpoint.py`'s fixture builders.

- [ ] **Step 3: Implement** — clone the Phase 3 verifier's skeleton with:

```python
CORPUS = 17886
MAPPING_VERSION = "0.3.0"
MAPPING_HASH = "<the frozen v2 hash — copy from the committed C3 audit summary>"
STAGES = {
    "c3": (("migration", "conformance", "perf"),
           r"^ab-aozora 0\.4\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git {c}\)$"),
    "c4": (("confinement", "conformance", "perf"),
           r"^ab-aozora 0\.5\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git {c}\)$"),
}
ACTIVATION_REQUIRED = {
    "ab-validator/reports/aat-fidelity/run-sets/current.json",
    "ab-validator/reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json",
    "ab-validator/docs/handoffs/ir-publication-coverage-contract.md",
    "ab-validator/justfile",
}
ACTIVATION_ALLOWED_PREFIXES = ("abc/examples/ab-validator-output/",)
```

CLI: `--c3-gates P P P --c4-gates P P P --audit-c3 P --audit-c4 P --split P --admission-capture P --admission-cmd "clojure -M:abc/aat-compat-admission -- --candidates <compat.edn>" --activation-commit SHA --run-set P --coverage P --c3 SHA --c4 SHA`. Checks beyond the Phase 3 clones:

- **Activation commit binding** (`git show --name-only --pretty=format: SHA` via subprocess, repo-root cwd): changed paths ⊇ `ACTIVATION_REQUIRED`; every other changed path starts with an `ACTIVATION_ALLOWED_PREFIXES` entry (⇒ no retirement deletions, nothing stray); `git show SHA^:ab-validator/reports/aat-fidelity/run-sets/current.json` parses to a run-set WITH an `aozora` adapter and WITHOUT `ab-aozora`; `git show SHA:…current.json` WITHOUT `aozora`, WITH `ab-aozora` whose `expected.adapter_version_contains == --c4`; exactly five adapters in the child.
- **Admission**: the capture file contains `:status :admitted` AND the `--admission-cmd` subprocess re-runs fail-closed: exit 0 and `:status :admitted` in its stdout (a stale capture cannot pass alone).
- **Confinement**: `identical + source_note_appended == CORPUS` and `source_note_appended == split.works_with_terminal_provenance`.
- **Audits**: both carry mapping 0.3.0 + `MAPPING_HASH`, 17886/17886/0.
- **Conformance gates**: `must_fail == 0 and must_skip == 0 and differing_full == 0 and differing_seed == 0`; **perf gates**: `new_timeouts == 0`; c4 perf additionally `activation_floor_ok == true`.
- **Coverage**: the three counters == 0, contract verdict string, `ab-aozora` in the lane list.

- [ ] **Step 4: Run** — `python -m pytest reports/aat-fidelity/tests/test_verify_phase4_checkpoint.py -v` → 16 passed; then the real checkpoint against the committed artifacts → `CHECKPOINT OK`; then the negative probe (`--c4` set to the C3 sha) → FAIL. Record both invocations verbatim in the wholesale report, alongside the acceptance-criteria table (each 2026-07-06 criterion → its Phase 4 evidence file → PASS).

- [ ] **Step 5: Commit**

```bash
git add reports/aat-fidelity/verify-phase4-checkpoint.py reports/aat-fidelity/tests/test_verify_phase4_checkpoint.py docs/superpowers/reports/2026-07-12-phase4-acceptance-wholesale.* docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage.*
git commit -m "docs(reports): phase 4 wholesale acceptance PASS + checkpoint verifier (CHECKPOINT OK)"
```

Ledger: `CHECKPOINT OK (C3=<sha>, C4=<sha>, activation=<sha>)`.

---

### Task 21: Legacy-lane retirement + phase closure

**Files:**
- Delete: `adapters/aozora/` (entire crate), `reports/aat-fidelity/run-aozora-aat-full.sh`
- Modify: `flake.nix` (remove the `upstream-aozora-src` input and all `AB_AOZORA_BIN` bindings — locate by `grep -n "AB_AOZORA_BIN\|upstream-aozora-src" flake.nix`), `reports/aat-fidelity/run-aat-full.sh` (drop the `aozora` menu entry and `--aozora-bin`), `justfile` (remove the `aozora=inspect:…` and `aozora-adapter=aat:…` conformance lanes and the `adapters/aozora` build recipe), `reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json` (drop the entry's nix `source` pin — its flake input is gone; keep `aat_dir` + `content_hash`: the frozen dump stays verifiable), `docs/handoffs/ir-publication-coverage-contract.md` (retirement executed)
- Modify: `docs/handoffs/2026-07-10-parser-fork-provenance.md` (Phase 4 closure)

**Interfaces:**
- Consumes: Task 20's `CHECKPOINT OK` — hard precondition.
- Produces: the fork is the sole live Aozora parser lane. Frozen dumps, frozen reports, historical registry rows untouched. Rollback = `git revert` of retirement + activation commits.

- [ ] **Step 1: Precondition** — Task 20 ledger line exists; re-run `verify-phase4-checkpoint.py` with the committed arguments right now → exit 0.

- [ ] **Step 2: Excise** — `git rm -r adapters/aozora reports/aat-fidelity/run-aozora-aat-full.sh`; apply the flake/justfile/run-aat-full.sh/archive-run-set edits; then `grep -rn "AB_AOZORA_BIN\|adapters/aozora\|upstream-aozora-src" --include="*.nix" --include="justfile" --include="*.sh" --include="*.py" .` → remaining hits ONLY in frozen `docs/superpowers/reports/**`, handoff history, and this plan/spec; zero hits in live code paths.

- [ ] **Step 3: Verify the world still stands**

```bash
nix flake check --no-build 2>&1 | tail -5
cargo test --workspace
bash tests/workspace-no-preserve-order.sh && just preserve-order-hazard-check
python3 reports/aat-fidelity/validate-aat-run-set.py reports/aat-fidelity/run-sets/current.json
python3 reports/aat-fidelity/validate-aat-run-set.py reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json
just aozora-notation-spec-comparison "" docs/superpowers/reports/2026-07-12-phase4-retirement-conformance.md docs/superpowers/reports/2026-07-12-phase4-retirement-conformance.summary.json
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-12-phase4-c4-conformance.summary.json docs/superpowers/reports/2026-07-12-phase4-retirement-conformance.summary.json --adapter ab-aozora
```

The conformance run now scores four comparison lanes + ab-aozora (the two legacy lanes' rows disappear — expected, recorded); the `ab-aozora` comparator must exit 0 (byte-identical rows vs Task 15).

- [ ] **Step 4: Close the phase** — append the Phase 4 closure section to `docs/handoffs/2026-07-10-parser-fork-provenance.md`: C3/C4 shas + join keys; the evidence-file inventory (six gates, two audits, split, admission, bundle, wholesale, pre/post coverage); activation + retirement commit shas; retained dumps (now + both phase4 dumps); the perf trajectory (C3 and C4 medians vs the legacy lane — closes the −4.09% watch item with numbers); carried items (bare toggles, keigakomi 44-residual, warigaki/kunten vocabulary ADR).

- [ ] **Step 5: Commit + final ledger**

```bash
git add -A
git commit -m "feat(retirement): legacy aozora lane retired — fork is the sole publication parser"
```

Ledger: `Task 21 complete — PHASE 4 COMPLETE (retirement=<sha>). Ready for finishing-a-development-branch.`

---

## Execution notes (controller)

- Tasks 10, 15 are evidence tasks: BLOCKED-by-design outcomes (delta exit 2, floor false, admission conflict) are controller escalations, not implementer retries. Phase 3 precedent governs: instrument-side grammar-fidelity fixes are reports-only commits; parser-side fixes move the candidate and re-run the stage.
- The frozen-hash chain: mapping v2's hash is fixed the moment Task 10's audit runs. Tasks 11, 15, 17, 20 all repeat it. Any mapping edit after Task 10 invalidates C3 evidence — `data/aat-to-parser-ir-mapping-v2.json` is frozen from Task 10 onward.
- Instrument-before-candidate invariant: Task 8 < Task 9 (C3), Task 13 < Task 14 (C4) — never reorder; the hinoki checkouts depend on it.
- The identity file (`.superpowers/sdd/phase4-identity.json`) is git-ignored scratch — if lost, recover C3/C4 from the ledger and `git log` (the close-task commit messages name them).
- After Task 21, hand off to superpowers:finishing-a-development-branch. This session's standing instruction: merge to main and push when done (explicit user authorization given at plan approval).
