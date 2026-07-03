# Restore Trustworthy Measurement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore trustworthy ab-validator measurement by fixing the real-corpus kunten detector, generating the AAT-to-parser-IR mapping from measured folded rule buckets, rerunning current aozora2html full-corpus AAT measurement, and adding the cheap gates/docs that prevent silent drift.

**Architecture:** Treat measurement inputs as production code: the feature index and coverage matrix must recognize real Aozora spellings before corpus conclusions are trusted, and the AAT-to-parser-IR mapping document must be generated from executable mapper policy plus measured folded corpus buckets. Heavy corpus runs stay as explicit operator commands with archived outputs; cheap deterministic drift checks run in Nix. This plan deliberately avoids AAT v2 vocabulary, manifest identity hardening, compatibility-registry hardening, and broad adapter-boundary refactors.

**Tech Stack:** Rust 2024 workspace, TOML data files, `just`, Bash, Nix flakes, Python/DuckDB triage scripts.

## Global Constraints

- Do not add new AAT node kinds or schema fields in this plan.
- Do not start the user-facing owned-mapping CLI, manifest identity hardening, or compatibility-registry hardening in this plan.
- Treat `/db/ab-validator/aat-corpus` as generated measurement output, not source-controlled code.
- Use `tests/fixtures/kunten-source-excerpt.txt` as the regression source for real kunten spellings.
- Keep full-corpus aozora2html runs out of Nix checks; Nix should only gate cheap deterministic checks.
- Preserve existing untracked user files under `docs/handoffs/` and `tests/fixtures/`.
- Do not hand-create `data/aat-to-parser-ir-mapping-v1.json` from the old synthesized 27-rule table.
- Treat `../abc/prototypes/aat-to-parser-ir-probe/mapping.generated.aozora-rs.json` as evidence and candidate shape, not the final ab-validator-owned production artifact.
- Generate the ab-validator mapping artifact from executable mapper policy plus folded measured corpus buckets.
- Keep manifest identity and compatibility-registry hardening paused until the generated mapping artifact exists and validates against the ABC schema.

---

## File Structure

- `data/feature-patterns.toml` owns the feature regexes used by `ab-index`.
- `data/aozora-syntax-coverage.toml` owns coverage matrix row examples, source patterns, and prevalence metadata.
- `crates/ab-index/tests/integration.rs` gets an executable regression proving the feature index detects real kunten fixture spellings.
- `crates/ab-coverage/tests/schema_matrix.rs` gets an executable regression proving the coverage row detector registry detects the same fixture spellings.
- `reports/aat-fidelity/aat_parser_ir_mapping/` owns the executable AAT-to-parser-IR mapping probe policy and measured mapping generator.
- `tests/aat-parser-ir-mapping-smoke.sh` validates the generated mapping against ABC schemas and asserts zero `UNSUPPORTED` over the local aozora-rs AAT corpus.
- `data/aat-to-parser-ir-mapping-v1.json` is generated from local aozora-rs AAT measurements, not hand-written.
- `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md` records the ab-validator-owned mapping generation run and its ABC schema hashes.
- `reports/aat-fidelity/run-aozora2html-aat-full.sh` becomes the repeatable operator script for current-adapter aozora2html AAT corpus measurement.
- `justfile` gets an `aozora2html-aat-full` target that wraps the operator script.
- `tests/aozora2html-aat-full-smoke.sh` gets a one-work smoke path for the script.
- `flake.nix` gets a `taxonomy-drift` check.
- `crates/README.md` gets the crate navigability map drafted by the classification handoff.
- `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md` records the generated-mapping and adapter-boundary decision after the measurement/gating tasks.

---

### Task 1: Fix Kunten Detector And Guard Against False Zero

**Files:**
- Modify: `data/feature-patterns.toml`
- Modify: `data/aozora-syntax-coverage.toml`
- Modify: `crates/ab-index/tests/integration.rs`
- Modify: `crates/ab-coverage/tests/schema_matrix.rs`
- Read: `tests/fixtures/kunten-source-excerpt.txt`
- Read: `tests/fixtures/kunten-source-excerpt.notes.md`

**Interfaces:**
- Consumes: `FeatureDetector::from_toml(path: &Path) -> Result<FeatureDetector>`, `FeatureDetector::detect(text: &str) -> HashMap<String, Vec<usize>>`.
- Consumes: `DetectorRegistry::from_matrix(rows: &[Row]) -> DetectorRegistry`, `DetectorRegistry::detect(row_id: &str, ctx: &DetectorContext<'_>) -> u64`.
- Produces: `kaeriten` and `okurigana` feature keys that match compact real Aozora kunten forms.
- Produces: `kunten.kaeriten` and `kunten.okurigana` coverage rows whose `source_patterns` match compact real Aozora kunten forms.

- [ ] **Step 1: Add the failing `ab-index` regression test**

Append this test before `write_zip_text` in `crates/ab-index/tests/integration.rs`:

```rust
#[test]
fn detects_real_kunten_fixture_spellings() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../tests/fixtures/kunten-source-excerpt.txt")
        .canonicalize()
        .unwrap();
    let text = fs::read_to_string(fixture).unwrap();
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let detected = detector.detect(&text);

    let kaeriten = detected
        .get("kaeriten")
        .expect("real fixture must detect compact kaeriten markers");
    let okurigana = detected
        .get("okurigana")
        .expect("real fixture must detect compact okurigana markers");

    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| kaeriten.contains(&(idx + 1)) && line.contains("［＃レ］")),
        "kaeriten detector must match bare return-point marker ［＃レ］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| kaeriten.contains(&(idx + 1)) && line.contains("［＃一］")),
        "kaeriten detector must match bare return-point marker ［＃一］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| okurigana.contains(&(idx + 1)) && line.contains("［＃（ノ）］")),
        "okurigana detector must match parenthesized marker ［＃（ノ）］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| okurigana.contains(&(idx + 1)) && line.contains("［＃（ス）］")),
        "okurigana detector must match parenthesized marker ［＃（ス）］"
    );
}
```

- [ ] **Step 2: Run the `ab-index` regression and verify it fails**

Run:

```bash
cargo test -p ab-index detects_real_kunten_fixture_spellings -- --nocapture
```

Expected before the fix: FAIL because `kaeriten` and/or `okurigana` is missing from the detected feature map.

- [ ] **Step 3: Add the failing `ab-coverage` detector-registry regression test**

Append this test before `forbidden_combinations_rejected` in `crates/ab-coverage/tests/schema_matrix.rs`:

```rust
#[test]
fn kunten_rows_detect_real_fixture_spellings() {
    use ab_coverage::detectors::{DetectorContext, DetectorRegistry};

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let registry = DetectorRegistry::from_matrix(matrix.rows());
    let fixture = matrix_path()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests/fixtures/kunten-source-excerpt.txt");
    let source = std::fs::read_to_string(&fixture).expect("read kunten fixture");
    let empty_aat = serde_json::json!({
        "version": 1,
        "work_id": "kunten-fixture",
        "blocks": [],
        "meta": {"adapter": "fixture", "adapter_version": "fixture"}
    });
    let ctx = DetectorContext {
        aat: &empty_aat,
        source: &source,
    };

    assert!(
        registry.detect("kunten.kaeriten", &ctx) > 0,
        "kunten.kaeriten must not false-zero on compact real fixture markers"
    );
    assert!(
        registry.detect("kunten.okurigana", &ctx) > 0,
        "kunten.okurigana must not false-zero on compact real fixture markers"
    );
}
```

- [ ] **Step 4: Run the `ab-coverage` regression and verify it fails**

Run:

```bash
cargo test -p ab-coverage kunten_rows_detect_real_fixture_spellings -- --nocapture
```

Expected before the fix: FAIL because the matrix source patterns only describe synthetic forms.

- [ ] **Step 5: Update `data/feature-patterns.toml`**

Replace the two kunten feature patterns with:

```toml
[features.okurigana]
pattern = '［＃「[^」]+」の左に「[^」]+」］|［＃（[ァ-ヴーぁ-ゖ一-龯々〆ヵヶ]{1,12}）］'
description = "Okurigana, reread, or side annotation"

[features.kaeriten]
pattern = '［＃(?:返り点)?[一二三四五六七八九十レ上中下甲乙丙丁天地人]+］|［＃レ点］'
description = "Kanbun return marks"
```

- [ ] **Step 6: Update the two coverage matrix rows**

In `data/aozora-syntax-coverage.toml`, update `kunten.kaeriten` to use real compact examples and patterns:

```toml
source_examples = ["［＃レ］", "［＃一］", "［＃二］", "［＃上］", "［＃返り点一］"]
source_patterns = ['［＃(?:返り点)?[一二三四五六七八九十レ上中下甲乙丙丁天地人]+］', '［＃レ点］']
```

Update `kunten.okurigana` similarly:

```toml
source_examples = ["［＃（ノ）］", "［＃（ス）］", "［＃（レント）］", "［＃訓点送り仮名「読」］"]
source_patterns = ['［＃（[ァ-ヴーぁ-ゖ一-龯々〆ヵヶ]{1,12}）］', '［＃訓点送り仮名「[^」]+」］', '［＃送り仮名「[^」]+」］']
```

For both rows, set stale prevalence metadata to untrusted until Task 3 refreshes it:

```toml
works_with_feature = 0
total_occurrences = 0
coverage_basis = "not_run"
sample_works = []
```

- [ ] **Step 7: Run the focused detector tests**

Run:

```bash
cargo test -p ab-index detects_real_kunten_fixture_spellings -- --nocapture
cargo test -p ab-coverage kunten_rows_detect_real_fixture_spellings -- --nocapture
```

Expected after the fix: both PASS.

- [ ] **Step 8: Run the adjacent matrix/index tests**

Run:

```bash
cargo test -p ab-index
cargo test -p ab-coverage
```

Expected: all tests pass.

- [ ] **Step 9: Commit Task 1**

```bash
git add data/feature-patterns.toml data/aozora-syntax-coverage.toml crates/ab-index/tests/integration.rs crates/ab-coverage/tests/schema_matrix.rs
git commit -m "fix: detect real kunten spellings in corpus measurements"
```

---

### Task 2: Generate Measured AAT-to-Parser-IR Mapping

**Files:**
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/__init__.py`
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`
- Create: `tests/aat-parser-ir-mapping-smoke.sh`
- Create: `data/aat-to-parser-ir-mapping-v1.json`
- Create: `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`
- Read: `../abc/prototypes/aat-to-parser-ir-probe/map.py`
- Read: `../abc/prototypes/aat-to-parser-ir-probe/mapping_doc.py`
- Read: `../abc/schemas/aat-parser-ir-mapping.schema.json`
- Read: `../abc/schemas/parser-ir.schema.json`
- Read: `scratch/morph-full-corpus/aats/aozora-rs-adapter/`

**Interfaces:**
- Consumes: local aozora-rs AAT JSON corpus at `scratch/morph-full-corpus/aats/aozora-rs-adapter`.
- Consumes: ABC mapping schema at `../abc/schemas/aat-parser-ir-mapping.schema.json`.
- Produces: `data/aat-to-parser-ir-mapping-v1.json`, generated from folded measured rule buckets.
- Produces: a summary JSON and markdown report asserting `files_with_unsupported == 0`.

- [ ] **Step 1: Seed the executable mapper policy from ABC**

Run:

```bash
mkdir -p reports/aat-fidelity/aat_parser_ir_mapping
cp ../abc/prototypes/aat-to-parser-ir-probe/map.py reports/aat-fidelity/aat_parser_ir_mapping/mapper.py
cp ../abc/prototypes/aat-to-parser-ir-probe/mapping_doc.py reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py
touch reports/aat-fidelity/aat_parser_ir_mapping/__init__.py
```

Expected: `mapper.py` contains the ADR 0024/I-09/source-encoding policy: `ruby.direction` direct projection, `style` to `emphasis`, and `windows-31j-lossy` to `Shift_JIS` with an `AMBIGUITY` ledger entry.

- [ ] **Step 2: Add the ab-validator generator wrapper**

Create `reports/aat-fidelity/aat_parser_ir_mapping/generate.py` with this content:

```python
#!/usr/bin/env python3
"""Generate an ab-validator-owned AAT->parser-IR mapping from measured AATs."""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

import mapper
import mapping_doc

CATEGORIES = ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL")


def walk_inline_kinds(block: dict) -> list[str]:
    kinds: list[str] = []

    def visit(node: dict) -> None:
        kind = node.get("kind")
        if kind is not None:
            kinds.append(kind)
        for key in ("content", "base_content", "reading_content", "upper", "lower"):
            for child in node.get(key, []) or []:
                if isinstance(child, dict):
                    visit(child)

    for child in block.get("content", []) or []:
        if isinstance(child, dict):
            visit(child)
    return kinds


def map_aat_document(aat: dict) -> tuple[list[dict], list[dict], Counter, Counter, bool]:
    ledger_list: list[dict] = []
    nodes: list[dict] = []
    block_kinds: Counter = Counter()
    inline_kinds: Counter = Counter()
    has_warigaki = False
    offset = 0

    for index, block in enumerate(aat.get("blocks", []) or []):
        block_kinds[block.get("kind")] += 1
        inline_kinds.update(walk_inline_kinds(block))
        has_warigaki = has_warigaki or "warigaki" in inline_kinds
        mapper.map_block(block, nodes, ledger_list, offset, f"blocks[{index}]")

    mapper.map_meta_source(aat, ledger_list)
    ledger_list.append(
        mapper.ledger(
            "INVENTION",
            "(top-level)",
            "schema_id/schema_hash",
            "parser-IR requires schema_id+schema_hash; AAT supplies only version=1",
        )
    )
    mapper.map_warnings(aat, ledger_list)
    ledger_list.append(
        mapper.ledger(
            "INVENTION",
            "(none)",
            "errors[]",
            "parser-IR requires errors[]; AAT has no errors concept -> defaulted empty",
        )
    )
    return ledger_list, nodes, block_kinds, inline_kinds, has_warigaki


def validate_mapping(document: dict, abc_root: Path) -> None:
    try:
        import jsonschema
    except ImportError as exc:
        raise SystemExit("jsonschema is required for mapping validation") from exc

    schema_path = abc_root / "schemas/aat-parser-ir-mapping.schema.json"
    schema = json.loads(schema_path.read_text())
    jsonschema.Draft202012Validator(schema).validate(document)


def write_report(summary: dict, report_path: Path) -> None:
    category_counts = summary["category_counts"]
    lines = [
        "# AAT-to-Parser-IR Mapping Generation",
        "",
        "Date: 2026-07-03",
        "",
        "## Summary",
        "",
        "| Metric | Value |",
        "|---|---:|",
        f"| files scanned | {summary['files_scanned']} |",
        f"| files failed to parse | {summary['files_failed_to_parse']} |",
        f"| files with UNSUPPORTED | {summary['files_with_unsupported']} |",
        f"| files with warigaki | {summary['files_with_warigaki']} |",
        f"| total parser-IR nodes emitted | {summary['total_parser_ir_nodes_emitted']} |",
        f"| total ledger entries | {summary['total_ledger_entries']} |",
        f"| generated mapping rules | {summary['generated_mapping_rules']} |",
        "",
        "## Category Counts",
        "",
        "| Category | Count |",
        "|---|---:|",
    ]
    lines.extend(f"| {category} | {category_counts.get(category, 0)} |" for category in CATEGORIES)
    lines.extend(
        [
            "",
            "## Schema Hashes",
            "",
            f"- mapping schema hash: `{summary['mapping_schema_hash']}`",
            f"- target parser-IR schema hash: `{summary['target_parser_ir_schema_hash']}`",
            "",
            "## Policy Checks",
            "",
            "- `ruby.direction` projects directly into parser-IR.",
            "- `style` maps to parser-IR `emphasis`.",
            "- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` entry.",
            "- Generated mapping is derived from folded measured rule buckets, not the historical 27-rule synthesized table.",
            "",
        ]
    )
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--aat-dir", type=Path, required=True)
    parser.add_argument("--abc-root", type=Path, default=Path("../abc"))
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path)
    parser.add_argument("--assert-zero-unsupported", action="store_true")
    args = parser.parse_args()

    files = sorted(args.aat_dir.glob("*.json"))
    if not files:
        raise SystemExit(f"no AAT JSON files found under {args.aat_dir}")

    category_counts: Counter = Counter()
    mapping_rule_counter: Counter = Counter()
    first_path_by_rule: dict[tuple[str, str, str], str] = {}
    first_note_by_rule: dict[tuple[str, str, str], str] = {}
    block_kind_counts: Counter = Counter()
    inline_kind_counts: Counter = Counter()
    files_failed_to_parse = 0
    files_with_unsupported = 0
    files_with_warigaki = 0
    total_parser_ir_nodes = 0

    for path in files:
        try:
            aat = json.loads(path.read_text())
        except Exception:
            files_failed_to_parse += 1
            continue

        ledger_list, nodes, block_kinds, inline_kinds, has_warigaki = map_aat_document(aat)
        block_kind_counts.update(block_kinds)
        inline_kind_counts.update(inline_kinds)
        total_parser_ir_nodes += len(nodes)
        if has_warigaki:
            files_with_warigaki += 1

        had_unsupported = False
        for entry in ledger_list:
            category_counts[entry["category"]] += 1
            aat_bucket = mapper.aat_pointer_bucket(entry["aat"])
            key = (entry["category"], aat_bucket, entry["parser_ir"])
            mapping_rule_counter[key] += 1
            first_path_by_rule.setdefault(key, entry["aat"])
            first_note_by_rule.setdefault(key, entry["note"])
            if entry["category"] == "UNSUPPORTED":
                had_unsupported = True
        if had_unsupported:
            files_with_unsupported += 1

    mapping_document = mapping_doc.build_mapping_document_from_counts(
        mapping_rule_counter,
        first_path_by_rule,
        first_note_by_rule,
        repo_root=args.abc_root.resolve(),
    )
    validate_mapping(mapping_document, args.abc_root.resolve())

    args.out.parent.mkdir(parents=True, exist_ok=True)
    mapping_doc.write_mapping_document(mapping_document, args.out)

    summary = {
        "files_scanned": len(files),
        "files_failed_to_parse": files_failed_to_parse,
        "files_with_unsupported": files_with_unsupported,
        "files_with_warigaki": files_with_warigaki,
        "total_blocks_scanned": sum(block_kind_counts.values()),
        "total_inline_nodes_scanned": sum(inline_kind_counts.values()),
        "total_parser_ir_nodes_emitted": total_parser_ir_nodes,
        "total_ledger_entries": sum(category_counts.values()),
        "category_counts": {category: category_counts.get(category, 0) for category in CATEGORIES},
        "block_kind_counts": dict(block_kind_counts),
        "inline_kind_counts": dict(inline_kind_counts),
        "generated_mapping_rules": len(mapping_document["transform_rule_descriptions"]),
        "mapping_schema_hash": mapping_document["mapping_schema_hash"],
        "target_parser_ir_schema_hash": mapping_document["target_parser_ir_schema_hash"],
        "mapping_path": str(args.out),
    }

    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(json.dumps(summary, ensure_ascii=False, indent=2) + "\n")
    if args.report_md:
        write_report(summary, args.report_md)

    print(json.dumps(summary, ensure_ascii=False, indent=2))
    if args.assert_zero_unsupported and files_with_unsupported != 0:
        raise SystemExit(f"expected zero UNSUPPORTED files, got {files_with_unsupported}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 3: Make the generator executable**

Run:

```bash
chmod +x reports/aat-fidelity/aat_parser_ir_mapping/generate.py
```

- [ ] **Step 4: Add the mapping smoke test**

Create `tests/aat-parser-ir-mapping-smoke.sh` with this content:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-smoke"
aat_dir="$repo_root/scratch/morph-full-corpus/aats/aozora-rs-adapter"
abc_root="$repo_root/../abc"

rm -rf "$out_dir"
mkdir -p "$out_dir"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json" \
  --assert-zero-unsupported

jq -e '.files_scanned == 17894' "$out_dir/summary.json"
jq -e '.files_with_unsupported == 0' "$out_dir/summary.json"
jq -e '.generated_mapping_rules == 25' "$out_dir/summary.json"
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' "$out_dir/summary.json"
jq -e '([.transform_rule_descriptions[].category] | index("UNSUPPORTED") | not)' "$out_dir/mapping.json"
```

- [ ] **Step 5: Make the smoke test executable**

Run:

```bash
chmod +x tests/aat-parser-ir-mapping-smoke.sh
```

- [ ] **Step 6: Run the smoke test**

Run:

```bash
bash tests/aat-parser-ir-mapping-smoke.sh
```

Expected: PASS, with `17894` scanned files, `0` files with `UNSUPPORTED`, `25` generated mapping rules, and the ABC schema hashes listed in the feedback.

- [ ] **Step 7: Generate the production mapping artifact and report**

Run:

```bash
uv run --isolated --no-project --with 'jsonschema>=4.0' \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  --aat-dir scratch/morph-full-corpus/aats/aozora-rs-adapter \
  --abc-root ../abc \
  --out data/aat-to-parser-ir-mapping-v1.json \
  --summary-json scratch/aat-parser-ir-mapping-summary.json \
  --report-md docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md \
  --assert-zero-unsupported
```

Expected: command exits 0 and writes both `data/aat-to-parser-ir-mapping-v1.json` and the markdown report.

- [ ] **Step 8: Verify the production artifact is generated, not a hand-written table**

Run:

```bash
jq -e '.transform_rule_descriptions | length == 25' data/aat-to-parser-ir-mapping-v1.json
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' data/aat-to-parser-ir-mapping-v1.json
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' data/aat-to-parser-ir-mapping-v1.json
rg -n "files with UNSUPPORTED \\| 0|generated mapping rules \\| 25" docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md
```

Expected: all checks pass. The generated mapping has no `UNSUPPORTED` rule bucket for the measured aozora-rs corpus.

- [ ] **Step 9: Commit Task 2**

```bash
git add \
  reports/aat-fidelity/aat_parser_ir_mapping \
  tests/aat-parser-ir-mapping-smoke.sh \
  data/aat-to-parser-ir-mapping-v1.json \
  docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md
git commit -m "feat: generate measured AAT parser-IR mapping"
```

---

### Task 3: Restore Repeatable Current-Adapter aozora2html AAT Measurement

**Files:**
- Create: `reports/aat-fidelity/run-aozora2html-aat-full.sh`
- Create: `tests/aozora2html-aat-full-smoke.sh`
- Modify: `justfile`
- Read: `crates/ab-check/src/main.rs`
- Read: `crates/ab-check/src/check.rs`
- Read: `reports/aat-fidelity/build-aat-batch-triage.py`
- Read: `adapters/aozora2html/aozora2html-adapter`

**Interfaces:**
- Consumes: `ab-index --corpus --patterns --output`.
- Consumes: `ab-check --index --corpus --adapter --output --aat-output --jobs --per-work-timeout`.
- Consumes: `build-aat-batch-triage.py --reports-dir --aat-dir --db --report-id --out-dir`.
- Produces: an archived run directory containing `index.json`, check reports, persisted AAT JSON, triage DB, triage markdown, and metadata.

- [ ] **Step 1: Create the operator script**

Create `reports/aat-fidelity/run-aozora2html-aat-full.sh` with this content:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

corpus="$repo_root/references/aozorabunko"
out_dir="${AB_AOZORA2HTML_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)}"
jobs="${AB_AOZORA2HTML_AAT_FULL_JOBS:-$(nproc)}"
timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-180s}"
report_id="${AB_AOZORA2HTML_AAT_FULL_REPORT_ID:-aozora2html-full-$(date -u +%F)}"
work_ids=""
features=""
force=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --corpus)
      corpus="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --jobs)
      jobs="$2"
      shift 2
      ;;
    --timeout)
      timeout="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    --work-ids)
      work_ids="$2"
      shift 2
      ;;
    --features)
      features="$2"
      shift 2
      ;;
    --force)
      force=1
      shift
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      exit 2
      ;;
  esac
done

if [[ ! "$jobs" =~ ^[0-9]+$ || "$jobs" == "0" ]]; then
  echo "--jobs must be a positive integer" >&2
  exit 2
fi
if [[ ! -d "$corpus/cards" ]]; then
  printf 'missing Aozora corpus cards directory: %s/cards\n' "$corpus" >&2
  exit 2
fi
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  printf 'output directory already exists: %s\n' "$out_dir" >&2
  printf 'pass --force to replace it\n' >&2
  exit 2
fi

rm -rf "$out_dir"
mkdir -p "$out_dir"

index_path="$out_dir/index.json"
reports_dir="$out_dir/check-reports"
aat_dir="$out_dir/aat"
triage_dir="$out_dir/triage"
db_path="$out_dir/fidelity.duckdb"
adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"

run_just aozora2html-rust-build

run_cargo run -p ab-index -- \
  --corpus "$corpus" \
  --patterns "$repo_root/data/feature-patterns.toml" \
  --output "$index_path"

check_args=(
  run -p ab-check --
  --index "$index_path"
  --corpus "$corpus"
  --adapter "$adapter"
  --output "$reports_dir"
  --aat-output "$aat_dir"
  --jobs "$jobs"
  --per-work-timeout "$timeout"
)
if [[ -n "$work_ids" ]]; then
  check_args+=(--work-ids "$work_ids")
fi
if [[ -n "$features" ]]; then
  check_args+=(--features "$features")
fi

run_cargo "${check_args[@]}"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"

python3 - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" <<'PY'
import json
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, report_id, jobs, timeout, adapter = sys.argv[1:]
repo = pathlib.Path(repo_root)
out = pathlib.Path(out_dir)

def run(args):
    return subprocess.check_output(args, cwd=repo, text=True).strip()

metadata = {
    "generated_at_utc": datetime.now(timezone.utc).isoformat(),
    "repo_head": run(["git", "rev-parse", "HEAD"]),
    "repo_status_short": run(["git", "status", "--short"]),
    "corpus": str(pathlib.Path(corpus).resolve()),
    "report_id": report_id,
    "jobs": int(jobs),
    "timeout": timeout,
    "adapter": adapter,
    "adapter_version": run([adapter, "--version"]),
    "index_path": str(out / "index.json"),
    "reports_dir": str(out / "check-reports"),
    "aat_dir": str(out / "aat"),
    "triage_dir": str(out / "triage"),
    "db_path": str(out / "fidelity.duckdb"),
}
(out / "metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
PY

printf 'aozora2html AAT run complete: %s\n' "$out_dir"
printf 'triage report: %s\n' "$triage_dir/index.md"
```

- [ ] **Step 2: Make the script executable**

Run:

```bash
chmod +x reports/aat-fidelity/run-aozora2html-aat-full.sh
```

- [ ] **Step 3: Add the `just` target**

Add this target after `upstream-xhtml-full` in `justfile`:

```just
aozora2html-aat-full DIR="" JOBS="0" TIMEOUT="180s" REPORT_ID="" WORK_IDS="" FEATURES="":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)"; fi; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	report_id="{{REPORT_ID}}"; if [ -z "$report_id" ]; then report_id="aozora2html-full-$(date -u +%F)"; fi; \
	args=(--out-dir "$run_dir" --jobs "$jobs" --timeout "{{TIMEOUT}}" --report-id "$report_id" --force); \
	if [ -n "{{WORK_IDS}}" ]; then args+=(--work-ids "{{WORK_IDS}}"); fi; \
	if [ -n "{{FEATURES}}" ]; then args+=(--features "{{FEATURES}}"); fi; \
	"{{repo_root}}/reports/aat-fidelity/run-aozora2html-aat-full.sh" "$${args[@]}"
```

- [ ] **Step 4: Add the one-work smoke test**

Create `tests/aozora2html-aat-full-smoke.sh` with this content:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-aat-full-smoke"
corpus="$out_dir/corpus"
work_ids="$out_dir/work-ids.json"

rm -rf "$out_dir"
mkdir -p "$corpus/cards/000250/files"

python3 - "$repo_root" "$corpus" <<'PY'
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile
import sys

repo = Path(sys.argv[1])
corpus = Path(sys.argv[2])
fixture = repo / "tests/fixtures/kunten-source-excerpt.txt"
zip_path = corpus / "cards/000250/files/4644_ruby_15596.zip"
with ZipFile(zip_path, "w", ZIP_DEFLATED) as zf:
    zf.writestr("hoo_kansho07.txt", fixture.read_bytes())
PY

printf '["000250_4644"]\n' > "$work_ids"

"$repo_root/reports/aat-fidelity/run-aozora2html-aat-full.sh" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 180s \
  --report-id aozora2html-aat-full-smoke \
  --force

test -f "$out_dir/run/index.json"
test -f "$out_dir/run/metadata.json"
test -f "$out_dir/run/triage/index.md"
rg -n "aozora2html-aat-full-smoke" "$out_dir/run/triage/index.md"
find "$out_dir/run/check-reports" -name '*.json' | rg -n '.'
find "$out_dir/run/aat" -name '*.json' | rg -n '.'
```

- [ ] **Step 5: Make the smoke test executable**

Run:

```bash
chmod +x tests/aozora2html-aat-full-smoke.sh
```

- [ ] **Step 6: Run the smoke test**

Run:

```bash
bash tests/aozora2html-aat-full-smoke.sh
```

Expected: PASS, with one check report, one persisted AAT, `metadata.json`, and `triage/index.md`.

- [ ] **Step 7: Run the current-adapter full corpus measurement**

Run:

```bash
just aozora2html-aat-full "" 0 180s "aozora2html-full-$(date -u +%F)"
```

Expected: the command completes and prints the archived run directory plus the triage report path.

- [ ] **Step 8: Extract the decision-critical facts**

Run these commands against the printed run directory:

```bash
RUN_DIR=/db/ab-validator/aat-corpus/aozora2html-full-YYYYMMDDTHHMMSSZ

jq '.works_count' "$RUN_DIR/index.json"
find "$RUN_DIR/check-reports" -name '*.json' | wc -l
find "$RUN_DIR/aat" -name '*.json' | wc -l
python3 - "$RUN_DIR" <<'PY'
import json
import pathlib
import sys

run_dir = pathlib.Path(sys.argv[1])
aat_dir = run_dir / "aat"
warigaki_files = set()
warigaki_nodes = 0
kunten_files = set()
kunten_nodes = 0

def walk(value):
    yield value
    if isinstance(value, dict):
        for child in value.values():
            yield from walk(child)
    elif isinstance(value, list):
        for child in value:
            yield from walk(child)

for path in aat_dir.rglob("*.json"):
    payload = json.loads(path.read_text())
    for node in walk(payload):
        if not isinstance(node, dict):
            continue
        if node.get("kind") == "warigaki" or node.get("x-warichu") is True:
            warigaki_files.add(path)
            warigaki_nodes += 1
        syntax_id = str(node.get("x-aozora-syntax-id", ""))
        if (
            node.get("style_type") == "kaeriten"
            or node.get("x-annotation-type") == "okurigana"
            or syntax_id in {"kunten.kaeriten", "kunten.okurigana"}
        ):
            kunten_files.add(path)
            kunten_nodes += 1

print(f"warigaki_files={len(warigaki_files)}")
print(f"warigaki_nodes={warigaki_nodes}")
print(f"kunten_files={len(kunten_files)}")
print(f"kunten_nodes={kunten_nodes}")
PY
```

Expected: counts are recorded in the task notes. The kunten command may print rows or no rows; either result is now meaningful because Task 1 fixed the detector false-zero.

- [ ] **Step 9: Write the run report**

Generate `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md` from the run artifacts:

```bash
RUN_DIR=/db/ab-validator/aat-corpus/aozora2html-full-YYYYMMDDTHHMMSSZ

python3 - "$RUN_DIR" docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md <<'PY'
import csv
import json
import pathlib
import sys

run_dir = pathlib.Path(sys.argv[1])
out_path = pathlib.Path(sys.argv[2])
index = json.loads((run_dir / "index.json").read_text())
metadata = json.loads((run_dir / "metadata.json").read_text())
summary_path = run_dir / "triage/outputs/summary.csv"
with summary_path.open(newline="") as f:
    summary = next(csv.DictReader(f))

aat_dir = run_dir / "aat"
check_reports = sum(1 for _ in (run_dir / "check-reports").rglob("*.json"))
aat_files = sum(1 for _ in aat_dir.rglob("*.json"))
warigaki_files = set()
warigaki_nodes = 0
kunten_files = set()
kunten_nodes = 0

def walk(value):
    yield value
    if isinstance(value, dict):
        for child in value.values():
            yield from walk(child)
    elif isinstance(value, list):
        for child in value:
            yield from walk(child)

for path in aat_dir.rglob("*.json"):
    payload = json.loads(path.read_text())
    for node in walk(payload):
        if not isinstance(node, dict):
            continue
        if node.get("kind") == "warigaki" or node.get("x-warichu") is True:
            warigaki_files.add(path)
            warigaki_nodes += 1
        syntax_id = str(node.get("x-aozora-syntax-id", ""))
        if (
            node.get("style_type") == "kaeriten"
            or node.get("x-annotation-type") == "okurigana"
            or syntax_id in {"kunten.kaeriten", "kunten.okurigana"}
        ):
            kunten_files.add(path)
            kunten_nodes += 1

rows = [
    ("indexed works", index["works_count"]),
    ("check reports", check_reports),
    ("persisted AAT files", aat_files),
    ("reports with failures", summary["reports_with_failures"]),
    ("total failures", summary["total_failures"]),
    ("parse incomplete or missing reports", summary["parse_incomplete_or_missing_reports"]),
    ("source-derived node observations", summary["total_source_derived_nodes"]),
    ("warigaki works", len(warigaki_files)),
    ("warigaki nodes", warigaki_nodes),
    ("kunten works with AAT observations", len(kunten_files)),
    ("kunten AAT observations", kunten_nodes),
]

lines = [
    "# aozora2html Current-Adapter Full-Corpus AAT Run",
    "",
    "Date: 2026-07-03",
    "",
    f"Run directory: `{run_dir}`",
    "",
    "## Summary",
    "",
    "| Metric | Value |",
    "|---|---:|",
]
lines.extend(f"| {name} | {value} |" for name, value in rows)
lines.extend([
    "",
    "## Interpretation",
    "",
    f"- Warigaki policy evidence: aozora2html emitted {warigaki_nodes} warigaki observations across {len(warigaki_files)} works in this current-adapter run.",
    f"- Kunten Gap B evidence: current AAT output contains {kunten_nodes} kunten observations across {len(kunten_files)} works after the real-spelling detector fix.",
    f"- Schema-only adapter-boundary evidence: aozora2html produced {aat_files} persisted AAT files without depending on workspace `ab-*` crates.",
    f"- Style unsupported evidence: the triage recorded {summary['total_source_derived_nodes']} source-derived node observations for follow-up unsupported/style analysis.",
    "",
    "## Reproduction",
    "",
    f"- command: `just aozora2html-aat-full \"\" {metadata['jobs']} {metadata['timeout']} \"{metadata['report_id']}\"`",
    f"- repo head: `{metadata['repo_head']}`",
    f"- adapter version: `{metadata['adapter_version']}`",
    "",
])
out_path.parent.mkdir(parents=True, exist_ok=True)
out_path.write_text("\n".join(lines))
PY
```

- [ ] **Step 10: Commit Task 3**

```bash
git add reports/aat-fidelity/run-aozora2html-aat-full.sh tests/aozora2html-aat-full-smoke.sh justfile docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md
git commit -m "feat: make aozora2html AAT corpus measurement repeatable"
```

---

### Task 4: Add Taxonomy Drift Gate

**Files:**
- Modify: `flake.nix`

**Interfaces:**
- Consumes: `generate_taxonomy --annotation-dir --corpus-dir --reference --write`.
- Produces: `checks.taxonomy-drift`.

- [ ] **Step 1: Add the Nix drift check binding**

In `flake.nix`, after `adapterFidelityNotesSchemaSmokeCheck`, add:

```nix
        taxonomyDriftCheck = pkgs.runCommand "taxonomy-drift-check" {
          nativeBuildInputs = [
            abValidator
            pkgs.diffutils
          ];
        } ''
          tmp="$(mktemp -d)"
          generate_taxonomy \
            --annotation-dir "${source}/references/aozorabunko/annotation" \
            --corpus-dir "${source}/references/aozorabunko" \
            --reference "${source}/references/PARSER_REPORT.md" \
            --write "$tmp/generated-feature-taxonomy.md"
          diff -u "${source}/data/generated-feature-taxonomy.md" "$tmp/generated-feature-taxonomy.md"
          touch "$out"
        '';
```

- [ ] **Step 2: Wire the check into `checks`**

Add this line in the `checks = { ... };` set:

```nix
          taxonomy-drift = taxonomyDriftCheck;
```

- [ ] **Step 3: Run the check**

Run:

```bash
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).taxonomy-drift
```

Expected: PASS when `data/generated-feature-taxonomy.md` matches the generator output. If this fails because the `generate_taxonomy` binary is not installed by `abValidator`, replace `nativeBuildInputs = [ abValidator pkgs.diffutils ];` with a dedicated `rustPlatform.buildRustPackage` check that builds `-p ab-coverage --bin generate_taxonomy` and runs the same command in `checkPhase`.

- [ ] **Step 4: Commit Task 4**

```bash
git add flake.nix
git commit -m "ci: gate generated taxonomy drift"
```

---

### Task 5: Add `crates/README.md`

**Files:**
- Create: `crates/README.md`
- Read: `docs/handoffs/crate-classification.md`
- Read: `Cargo.toml`

**Interfaces:**
- Produces: a navigability map for the 13 Rust workspace crates.

- [ ] **Step 1: Create the README**

Create `crates/README.md` with this content:

```markdown
# ab-validator workspace crates

This directory holds the 13 Rust crates in the ab-validator workspace. Parser adapter checkouts under `adapters/` are separate vendored crates and are excluded from the workspace.

The normative adapter contract is the AAT JSON schema, not a Rust crate:

- Schema: `data/aat-schema.json`.
- Semantics: `docs/aat-contract.md`.
- Adapter fidelity matrix: `docs/adapter-fidelity.md`.

## Adapter-facing helpers

| Crate | Role | Notes |
|---|---|---|
| `ab-source-syntax` | Low-level Aozora source tokenizer and comparison projection helpers. | Shared by adapters and validator code. |
| `ab-ir` | Optional Rust IR/builders for emitting AAT JSON. | Useful convenience layer, but not the normative adapter contract. |

## Evaluation and coverage

| Crate | Binaries | Role |
|---|---|---|
| `ab-check` | `ab-check` | Validates AAT JSON and runs parser invariant checks. |
| `ab-compare` | `ab-compare` | Compares check-report directories and triages structural differences. |
| `ab-oracle` | `ab-oracle` | Evaluates adapter output against curated AAT oracle cases. |
| `ab-coverage` | `ab-coverage`, `ab-coverage-merge`, `generate_taxonomy` | Owns the syntax coverage matrix, prevalence pipeline, matrix merges, and generated feature taxonomy. |
| `ab-index` | `ab-index` | Builds and queries corpus feature indexes from `data/feature-patterns.toml`. |

## Morphology

| Crate | Role | Notes |
|---|---|---|
| `ab-plaintext` | Converts AAT or raw Aozora source into plain-text documents. | Shared by check and morphology flows. |
| `ab-morph-diff` | Morpheme alignment, pairwise diff, n-way regions, and validation. | Core morphology comparison model. |
| `ab-morph-analyzers` | Analyzer trait and Vibrato/Sudachi/Vaporetto adapters. | Real variation seam with multiple implementations. |
| `ab-morph-run` | Morphology runner, warehouse orchestration, reports, and CLI body. | Large leaf orchestration crate. |
| `ab-warehouse` | Parquet warehouse schema, writer, and SQL helpers. | Rust API has one main consumer; the on-disk Parquet schema is the durable contract. |

## Small shared utilities

| Crate | Role |
|---|---|
| `ab-diff-utils` | Shared hashing, first-difference, and frequency helpers used by comparison code. |

## Reading Order

For adapter validation work, start with `ab-check`, `ab-coverage`, `ab-index`, and the AAT schema docs.

For adapter implementation work, start with `data/aat-schema.json`, `docs/aat-contract.md`, then choose either direct JSON emission or the optional `ab-ir` helpers.

For morphology work, start with `ab-plaintext`, `ab-morph-diff`, and `ab-morph-run`.
```

- [ ] **Step 2: Verify crate list coverage**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
readme = Path("crates/README.md").read_text()
crates = [
    "ab-source-syntax", "ab-ir", "ab-index", "ab-check", "ab-compare",
    "ab-coverage", "ab-diff-utils", "ab-morph-diff", "ab-plaintext",
    "ab-morph-analyzers", "ab-morph-run", "ab-warehouse", "ab-oracle",
]
missing = [crate for crate in crates if f"`{crate}`" not in readme]
if missing:
    raise SystemExit(f"missing crates from README: {missing}")
PY
```

Expected: no output and exit code 0.

- [ ] **Step 3: Commit Task 5**

```bash
git add crates/README.md
git commit -m "docs: map workspace crate roles"
```

---

### Task 6: Record Adapter Boundary Version Discipline Decision

**Files:**
- Create: `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`
- Read: `data/aat-to-parser-ir-mapping-v1.json`
- Read: `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`
- Read: `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md`
- Read: `crates/ab-ir/src/lib.rs`
- Read: `crates/ab-ir/Cargo.toml`
- Read: `adapters/aozora-rs/src/aat.rs`
- Read: `adapters/aozora2html/Cargo.toml`
- Read: `adapters/aozora2html/src/lib.rs`

**Interfaces:**
- Consumes: Task 2 generated mapping report.
- Consumes: Task 3 current aozora2html measurement report.
- Produces: a decision record that separates generated mapping ownership from paused manifest/registry hardening.

- [ ] **Step 1: Create the decision record**

Create `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md` with this content:

```markdown
# Mapping and Adapter Boundary Decision

Date: 2026-07-03
Status: proposed

## Context

ABC measurement commits `0f36135` and `ca30a92` changed the AAT-to-parser-IR mapping path from a synthesized manual table to a generated measured artifact. The ab-validator-owned artifact is now `data/aat-to-parser-ir-mapping-v1.json`, generated from executable mapper policy and folded aozora-rs corpus rule buckets.

The current generated mapping evidence is recorded in `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`:

- `ruby.direction` projects directly into parser-IR.
- `style` maps to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` ledger entry.
- The measured aozora-rs corpus has zero `UNSUPPORTED` files.
- The generated mapping validates against ABC `schemas/aat-parser-ir-mapping.schema.json`.

The normative adapter output remains AAT JSON (`data/aat-schema.json`, `docs/aat-contract.md`). Existing adapters use different implementation boundaries:

- `aozora-rs` consumes `ab-ir` Rust types directly.
- `aozora2html` emits AAT JSON without any `ab-*` dependency.
- The current aozora2html full-corpus AAT run completed with the current adapter and produced the measurement recorded in `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md`.

`ab-ir` is useful, but it is not currently version-disciplined as an external contract: it uses the workspace version, has public enums, and has no explicit public API compatibility policy.

## Decision

The adapter boundary is the AAT JSON schema and contract docs. `ab-ir` remains an optional in-workspace convenience library for adapters that want typed builders, not the normative external adapter contract.

The AAT-to-parser-IR mapping document is generated from measured executable policy. It must not be hand-created from the historical 27-rule synthesized table.

Manifest identity and ABC compatibility-registry hardening stay paused until the generated mapping artifact exists, validates, and has the two corpus measurements attached: aozora-rs zero `UNSUPPORTED` and current aozora2html AAT corpus evidence.

## Consequences

- New adapter-facing tooling must consume AAT JSON and the schema contract, not `ab-ir` internals.
- Mapping-consuming tooling must consume `data/aat-to-parser-ir-mapping-v1.json` as a generated artifact, not a copied manual table.
- `ab-ir` changes can remain lockstep with the workspace while adapters are vendored in this repository.
- If `ab-ir` is published or advertised as an external adapter SDK, a separate decision must add semver policy, changelog discipline, and compatibility markers such as `#[non_exhaustive]` where appropriate.
- The owned-mapping CLI must not rely on unpublished `ab-ir` Rust API stability and must not start from manifest identity or compatibility registry work before the generated mapping artifact is in place.
- Durable warigaki policy claims require the current aozora2html full-corpus measurement, not the aozora-rs corpus alone.

## Rejected Alternative

Make `ab-ir` the normative adapter SDK now. This would require versioning work before the measurement loop is restored and would conflict with the proven schema-only aozora2html path.

Hand-create `data/aat-to-parser-ir-mapping-v1.json` from the old 27-rule synthesized table. ABC's full-corpus probe now invalidates that workflow: the live candidate has 25 observed folded buckets, zero `UNSUPPORTED`, and policy changes for ruby direction, style, and lossy Shift_JIS source encoding.
```

- [ ] **Step 2: Link the decision from the measurement report**

Append these lines to both `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md` and `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`:

```markdown
Mapping and adapter-boundary follow-up: `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`.
```

- [ ] **Step 3: Commit Task 6**

```bash
git add \
  docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md \
  docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md \
  docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md
git commit -m "docs: record adapter boundary decision"
```

---

## Final Verification

- [ ] Run focused detector tests:

```bash
cargo test -p ab-index detects_real_kunten_fixture_spellings -- --nocapture
cargo test -p ab-coverage kunten_rows_detect_real_fixture_spellings -- --nocapture
```

- [ ] Run adjacent crate tests:

```bash
cargo test -p ab-index
cargo test -p ab-coverage
```

- [ ] Run measured mapping generation smoke:

```bash
bash tests/aat-parser-ir-mapping-smoke.sh
```

- [ ] Confirm the generated mapping artifact has measured ABC hashes and zero `UNSUPPORTED`:

```bash
jq -e '.transform_rule_descriptions | length == 25' data/aat-to-parser-ir-mapping-v1.json
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' data/aat-to-parser-ir-mapping-v1.json
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' data/aat-to-parser-ir-mapping-v1.json
rg -n "files with UNSUPPORTED \\| 0|generated mapping rules \\| 25" docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md
```

- [ ] Run aozora2html operator smoke:

```bash
bash tests/aozora2html-aat-full-smoke.sh
```

- [ ] Run taxonomy drift:

```bash
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).taxonomy-drift
```

- [ ] Confirm the full run report has non-placeholder measured values:

```bash
rg -n 'indexed works|warigaki works|kunten AAT observations|Run directory' docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md
```

Expected: all commands pass, and the report points to a current full-corpus run directory.

## Self-Review

- Spec coverage: The plan covers the revised sequence: kunten detector first, generated AAT-to-parser-IR mapping second, current-adapter aozora2html full-corpus run third, taxonomy drift fourth, `crates/README.md` fifth, and mapping/adapter-boundary decision sixth.
- Placeholder scan: The plan intentionally contains no deferred implementation slots. The only replaceable value is the concrete run directory emitted by the full-corpus command.
- Type consistency: Test code uses existing public interfaces: `FeatureDetector`, `CoverageMatrix`, `DetectorRegistry`, and `DetectorContext`.
- Scope check: The plan avoids AAT v2 vocabulary, user-facing owned-mapping CLI work, manifest identity hardening, compatibility-registry hardening, parser candidate reports, and coded-warning upstream changes.
