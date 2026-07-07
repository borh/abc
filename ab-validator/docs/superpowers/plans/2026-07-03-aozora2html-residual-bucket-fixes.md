# aozora2html Residual-Bucket Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the verified `normalize_figure_alt` panic fix and produce committed triage artifacts for the aozora2html residual buckets (`adapter_protocol_error`, `adapter_timeout`, `parse_incomplete`, `visible_text_body_order`) so the next session can act on data rather than on hand-waved claims.

**Architecture:** Task 1 is a behavior-preserving Rust fix with a unit test — the only pure code change, independently mergeable. Tasks 2–4 are triage/tooling: each produces a committed artifact (a timeout tail-distribution report + default change, a parse-incomplete classifier script + report, and a VTBO divergence-locator example + characterization report). Their *findings* are recorded observations, not prescribed outputs.

**Tech Stack:** Rust 1.x workspace, `aozora2html-adapter` package, `ab-check` crate, Bash, Python 3, `jq`, the existing `aozora2html-adapter` bash wrapper.

## Global Constraints

- Do not change mapping/parser-IR protocol artifacts (that work is the separate parser-ir protocol-corrections plan).
- Do not port `roxmltree` to `html5ever` in this plan; `invalid_xhtml` remediation is deferred to a follow-up.
- Do not fix upstream `aozora2html` Ruby bugs; only classify and record them.
- Every measurement run reuses the existing release build: `adapters/aozora2html/target/release/aozora2html-adapter` invoked via the `adapters/aozora2html/aozora2html-adapter` bash wrapper.
- All paths assume `cwd = /home/bor/Projects/ab-validator` unless stated.
- The full-corpus artifacts live at `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/`; reference it via `$AB_DB_ROOT/aat-corpus/aozora2html-full-20260703T020301Z` with `AB_DB_ROOT` defaulting to `/db/ab-validator`.

### Scope note (decomposition)

Task 1 is the only pure fix and is independently mergeable. A reviewer may merge Task 1 alone and reject any of Tasks 2–4 without blocking the panic fix. Tasks 2–4 each ship a committed artifact; their observational findings are recorded, not asserted.

---

### Task 1: Fix `normalize_figure_alt` byte-index panic in BOTH helpers (behavior-preserving)

Two private `normalize_figure_alt` helpers in the `aozora2html-adapter` crate have the same byte-slice panic class: `adapters/aozora2html/src/xhtml_mapper.rs` (the one the verified backtrace hit) and `adapters/aozora2html/src/source_derived.rs` (a duplicate with a slightly different fallback). They are NOT semantically identical for inputs like `「外」中` (no trailing `」`), so they must be fixed in place rather than merged — each keeps its own fallback. Tests cover both call paths.

**Files:**
- Modify: `adapters/aozora2html/src/xhtml_mapper.rs:318-326`
- Modify: `adapters/aozora2html/src/source_derived.rs:78-91`
- Test: new `#[cfg(test)] mod tests` block appended to `xhtml_mapper.rs` (which has no test module today)
- Test: new tests appended to the EXISTING `#[cfg(test)] mod tests` in `source_derived.rs:1767`

**Interfaces:**
- Consumes: none (both are private helpers; `xhtml_mapper::normalize_figure_alt` is called from `map_img_gaiji` and `source_note_figure`; `source_derived::normalize_figure_alt` is called from source-note figure parsing).
- Produces: both `fn normalize_figure_alt(raw: &str) -> String` no longer panic on multibyte bracket content; each preserves its pre-fix fallback behavior.

- [ ] **Step 1: Write the failing unit tests for BOTH helpers**

Append to the end of `adapters/aozora2html/src/xhtml_mapper.rs` (after the last existing function, before EOF — this file has no `mod tests` today):

```rust

#[cfg(test)]
mod tests {
    use super::normalize_figure_alt;

    #[test]
    fn normalize_figure_alt_strips_brackets_around_multibyte_content() {
        // '「' and '」' are 3-byte UTF-8 chars. Byte slicing text[1..idx] used to panic
        // because byte 1 falls inside '「'. Char-aware slicing must return the inner text.
        assert_eq!(normalize_figure_alt("「図書館」"), "図書館");
    }

    #[test]
    fn normalize_figure_alt_content_up_to_first_closing_bracket() {
        // Behavior-preserving vs the original text.find('」'): the content up to the
        // FIRST '」' is returned. The xhtml_mapper guard requires ends_with('」').
        assert_eq!(normalize_figure_alt("「外」中」"), "外");
    }

    #[test]
    fn normalize_figure_alt_passes_through_unbracketed_text() {
        assert_eq!(normalize_figure_alt("plain alt"), "plain alt");
    }
}
```

Then in `adapters/aozora2html/src/source_derived.rs`, find the existing `#[cfg(test)] mod tests {` block (it starts at line 1767) and append these two tests inside it (before the closing `}` of the `mod tests` block):

```rust
    #[test]
    fn source_derived_normalize_figure_alt_strips_brackets_around_multibyte_content() {
        // source_derived::normalize_figure_alt has the same byte-slice panic class as
        // the xhtml_mapper helper. Its guard is find('」') first + starts_with('「')
        // + end > 0, so any alt of the form 「...」... panics. Char-aware slicing must
        // return the content up to the first '」'.
        assert_eq!(super::normalize_figure_alt("「図書館」"), "図書館");
    }

    #[test]
    fn source_derived_normalize_figure_alt_no_trailing_bracket_falls_back() {
        // source_derived's branch DOES enter for 「外」中 (no trailing 」): find('」')
        // is Some, starts_with('「') is true, end > 0. Char-aware slicing returns 外.
        // This locks the source_derived call path; it differs from the xhtml_mapper
        // fallback for this input, so the two helpers must NOT be merged.
        assert_eq!(super::normalize_figure_alt("「外」中"), "外");
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run:

```bash
cargo test -p aozora2html-adapter --lib normalize_figure_alt -- --nocapture
```

Expected: `normalize_figure_alt_strips_brackets_around_multibyte_content` panics with:

```
byte index 1 is not a char boundary; it is inside '「'
```

The other two tests may pass or fail; the first is the red signal.

- [ ] **Step 3: Apply the behavior-preserving fix**

Replace the existing `normalize_figure_alt` body (currently `adapters/aozora2html/src/xhtml_mapper.rs:318-326`):

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let mut text = raw.trim();
    if text.starts_with('「') && text.ends_with('」') {
        if let Some(idx) = text.find('」') {
            return text[1..idx].to_string();
        }
    }
    text = text.trim();
    let re = figure_alt_suffix_re();
    let normalized = re.replace_all(text, "").to_string();
    normalized.trim().trim_matches('「').trim_matches('」').to_string()
}
```

with:

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let text = raw.trim();
    if text.starts_with('「') && text.ends_with('」') {
        // '「' is a multibyte char; skip it by char length, then slice up to the
        // first '」'. find('」') returns a byte offset that is a char boundary in a
        // string starting at a char boundary, so the slice is safe.
        let rest = &text['「'.len_utf8()..];
        if let Some(rel) = rest.find('」') {
            return rest[..rel].to_string();
        }
    }
    let re = figure_alt_suffix_re();
    let normalized = re.replace_all(text, "").to_string();
    normalized.trim().trim_matches('「').trim_matches('」').to_string()
}
```

This preserves the xhtml_mapper original `find('」')` (first-occurrence) semantics; it only removes the panic. It intentionally diverges from the handoff's `chars().next()/next_back()` proposal, which would have changed behavior for nested-quote alt text.

- [ ] **Step 3b: Apply the behavior-preserving fix to the duplicate `source_derived` helper**

Replace the existing `normalize_figure_alt` body (currently `adapters/aozora2html/src/source_derived.rs:78-91`):

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let trimmed = raw.trim();
    if let Some(end) = trimmed.find('」') {
        if trimmed.starts_with('「') && end > 0 {
            return trimmed[1..end].trim().to_string();
        }
    }
    let normalized = figure_alt_suffix_re().replace_all(trimmed, "").to_string();
    normalized
        .trim()
        .trim_matches('「')
        .trim_matches('」')
        .to_string()
}
```

with:

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let trimmed = raw.trim();
    if let Some(end) = trimmed.find('」') {
        if trimmed.starts_with('「') && end > 0 {
            // '「' is a multibyte char; slice from after it (byte 3) up to the first
            // '」' byte offset. Both bounds are char boundaries, so the slice is safe.
            return trimmed['「'.len_utf8()..end].trim().to_string();
        }
    }
    let normalized = figure_alt_suffix_re().replace_all(trimmed, "").to_string();
    normalized
        .trim()
        .trim_matches('「')
        .trim_matches('」')
        .to_string()
}
```

This keeps the source_derived guard (`find('」')` first, `end > 0`) and its fallback untouched; only the byte-slice `trimmed[1..end]` is rewritten as a char-safe slice. The two helpers stay separate because their guards/fallbacks differ for `「外」中` (xhtml_mapper falls back; source_derived returns `外`).


- [ ] **Step 4: Run the tests to verify they pass**

Run:

```bash
cargo test -p aozora2html-adapter --lib normalize_figure_alt -- --nocapture
```

Expected: 5 tests pass (3 in `xhtml_mapper`, 2 in `source_derived`).

- [ ] **Step 5: Build the release binary used by the wrapper**

Run:

```bash
cargo build -p aozora2html-adapter --release
```

Expected: exits 0; `adapters/aozora2html/target/release/aozora2html-adapter` is updated.

- [ ] **Step 6: Reproduce the original failing work end-to-end to confirm the panic is gone**

Run:

```bash
mkdir -p /tmp/aozora-panic-fix
python - <<'PY'
import zipfile
z = zipfile.ZipFile('/home/bor/Dependencies/aozorabunko/cards/000125/files/1317_ruby_22263.zip')
z.extract('kokushikan_satsujin_jiken.txt', '/tmp/aozora-panic-fix/')
PY

RUST_BACKTRACE=1 timeout 300 \
  ./adapters/aozora2html/aozora2html-adapter --mode aat \
  < /tmp/aozora-panic-fix/kokushikan_satsujin_jiken.txt \
  > /tmp/aozora-panic-fix/out.json \
  2> /tmp/aozora-panic-fix/err.log
echo "exit=$?"
```

Expected: `exit=0` (previously `exit=101` with the `start byte index 1 is not a char boundary` panic). `out.json` is valid JSON; `err.log` is empty or contains only non-fatal warnings.

- [ ] **Step 7: Commit Task 1**

```bash
git add adapters/aozora2html/src/xhtml_mapper.rs adapters/aozora2html/src/source_derived.rs
git commit -m "fix(aozora2html): use char-aware slicing in both normalize_figure_alt helpers

Byte slicing text[1..idx] / trimmed[1..end] panicked because '「' is a
3-byte UTF-8 char and byte 1 falls inside it. Skip the opening bracket
by len_utf8() and slice up to the first '」' at a char boundary, in both
the xhtml_mapper and source_derived copies. Preserves each helper's
existing guard and fallback; the two differ for 「外」中 so they are not
merged."
```

---

### Task 2: Measure timeout tail distribution and set a new default

**Files:**
- Create: `reports/aat-fidelity/aozora2html-timeout-tail-measure.sh`
- Create: `reports/aat-fidelity/aozora2html-timeout-tail-report.md`
- Modify: `reports/aat-fidelity/run-aozora2html-aat-full.sh:10`

**Interfaces:**
- Consumes: the run's check reports at `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/check-reports/**/*.json` (per-work files keyed `{adapter, adapter_version, results, work_id}`) and the run's `index.json` whose `works[]` rows carry `{id, txt_path}` where `txt_path` is `cards/.../X.zip::entry.txt`. The corpus root for `txt_path` is `/home/bor/Dependencies/aozorabunko`.
- Produces: a tail-distribution JSON + markdown report and, based on the measured maximum, a raised default timeout in `run-aozora2html-aat-full.sh`.

- [ ] **Step 1: Write the tail-measurement script**

Create `reports/aat-fidelity/aozora2html-timeout-tail-measure.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
corpus="${AB_AOZORA_CORPUS:-/home/bor/Dependencies/aozorabunko}"
run_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z"
out_dir="${AB_AOZORA2HTML_TIMEOUT_TAIL_OUT:-$run_dir/triage/outputs/timeout-tail}"
sample="${AB_AOZORA2HTML_TIMEOUT_TAIL_SAMPLE:-10}"
limit_s="${AB_AOZORA2HTML_TIMEOUT_TAIL_LIMIT:-600}"
adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"
mkdir -p "$out_dir"

# Build (work_id, archive, entry, size) rows for the $sample largest timed-out works.
# A timed-out work is identified by results.adapter_timeout.pass == false in its
# check-report; its source path comes from index.json.works[].txt_path (joined on
# work_id), in the form "cards/NNN/files/X.zip::entry.txt".
python - "$run_dir" "$corpus" "$sample" > "$out_dir/sample.tsv" <<'PY'
import json, os, sys, zipfile
run_dir, corpus, sample = sys.argv[1], sys.argv[2], int(sys.argv[3])

# Load the run index: work_id -> txt_path ("cards/.../X.zip::entry.txt").
index = json.load(open(os.path.join(run_dir, "index.json")))
txt_by_id = {w["id"]: w.get("txt_path", "") for w in index.get("works", [])}

timeout_ids = set()
reports_dir = os.path.join(run_dir, "check-reports")
for root, _, files in os.walk(reports_dir):
    for fn in files:
        if not fn.endswith(".json"):
            continue
        try:
            rep = json.load(open(os.path.join(root, fn)))
        except Exception:
            continue
        results = rep.get("results", {})
        at = results.get("adapter_timeout", {})
        if isinstance(at, dict) and at.get("pass") is False:
            timeout_ids.add(rep.get("work_id", ""))

rows = []
for wid in timeout_ids:
    txt_path = txt_by_id.get(wid, "")
    if not txt_path:
        continue
    archive_rel, _, entry = txt_path.partition("::")
    archive = os.path.join(corpus, archive_rel)
    try:
        with zipfile.ZipFile(archive) as z:
            info = z.getinfo(entry) if entry else max(z.infolist(), key=lambda i: i.file_size)
            size = info.file_size
    except Exception:
        size = 0
    rows.append((size, wid, archive, entry))
rows.sort(reverse=True)
for size, wid, archive, entry in rows[:sample]:
    print(f"{wid}\t{archive}\t{entry}\t{size}")
PY

results="$out_dir/results.json"
printf '[' > "$results"
first=1
while IFS=$'\t' read -r wid archive entry size; do
  [[ -z "$wid" ]] && continue
  tmp="$(mktemp --suffix=.txt)"
  python - "$archive" "$entry" "$tmp" <<'PY'
import zipfile, sys
archive, entry, out = sys.argv[1], sys.argv[2], sys.argv[3]
with zipfile.ZipFile(archive) as z:
    data = z.read(entry) if entry else max(z.infolist(), key=lambda i: i.file_size)
    open(out, "wb").write(data if isinstance(data, bytes) else data)
PY
  start=$(date +%s)
  if timeout "$limit_s" "$adapter" --mode aat < "$tmp" > "/tmp/out.$$.json" 2> "/tmp/err.$$.json"; then
    status=ok
  else
    code=$?
    if [[ $code -eq 124 ]]; then status=timeout; else status="error:$code"; fi
  fi
  end=$(date +%s)
  real=$((end - start))
  rm -f "$tmp" "/tmp/out.$$.json" "/tmp/err.$$.json"
  [[ $first -eq 0 ]] && printf ',' >> "$results"
  first=0
  printf '{"work_id":"%s","size":%s,"real_s":%s,"status":"%s"}' "$wid" "$size" "$real" "$status" >> "$results"
done < "$out_dir/sample.tsv"
printf ']' >> "$results"
echo "wrote $results"
python - "$results" "$out_dir" <<'PY'
import json, sys, statistics
results = json.load(open(sys.argv[1]))
rows = [r for r in results if r["status"] == "ok"]
mx = max((r["real_s"] for r in rows), default=0)
med = statistics.median([r["real_s"] for r in rows]) if rows else 0
print(f"ok={len(rows)} max_real_s={mx} median_real_s={med}")
PY
```

Make it executable:

```bash
chmod +x reports/aat-fidelity/aozora2html-timeout-tail-measure.sh
```

- [ ] **Step 2: Run the tail measurement**

Run:

```bash
AB_AOZORA2HTML_TIMEOUT_TAIL_SAMPLE=10 \
AB_AOZORA2HTML_TIMEOUT_TAIL_LIMIT=600 \
  bash reports/aat-fidelity/aozora2html-timeout-tail-measure.sh
```

Expected: writes `…/timeout-tail/results.json` and prints `ok=<N> max_real_s=<M> median_real_s=<m>`. This step takes up to ~10 × 600 s in the worst case; if `ok` is 10, every sampled work completed. **Record the printed line.** Verify the sample is non-empty first by inspecting `$out_dir/sample.tsv` — it must list timed-out work IDs; an empty file means the `results.adapter_timeout.pass` derivation or the `index.json` join failed and must be corrected before the timed run.

- [ ] **Step 3: Write the tail-distribution report**

Create `reports/aat-fidelity/aozora2html-timeout-tail-report.md`. Fill the bracketed values from Step 2's output verbatim:

```markdown
# aozora2html Timeout Tail-Distribution Report

Date: 2026-07-03
Source run: `aozora2html-full-20260703T020301Z` (180 s default harness timeout, 196 timed-out works)

## Method

Sampled the `<sample_size>` largest timed-out works by zipped entry size and
re-ran each with a 600 s wall-clock limit via the `aozora2html-adapter` bash
wrapper (Ruby parser + Rust mapper). Raw results: `…/timeout-tail/results.json`.

## Findings

- sampled: [N] works
- completed (ok): [ok_count]
- median real time: [median] s
- max real time: [max] s
- still timing out at 600 s: [timeout_count]

## Decision

Based on max real time < [max] s:
- [ ] raise the harness default timeout to <NEW_DEFAULT>s in `run-aozora2html-aat-full.sh:10`
- [ ] OR keep 180 s and document the 196 works as a coverage caveat
- [ ] OR implement an adaptive timeout (follow-up)

Selected: [chosen_option]
```

- [ ] **Step 4: Apply the chosen default timeout (if "raise default" was selected)**

In `reports/aat-fidelity/run-aozora2html-aat-full.sh:10`, change:

```bash
timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-180s}"
```

to (substitute the decided value, e.g. `300s`):

```bash
timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-300s}"
```

If the decision was "keep 180s + document," do not edit the script; instead append a "Coverage caveat" note to the tail report.

- [ ] **Step 5: Smoke that the default is applied**

Run:

```bash
rg -n 'AB_AOZORA2HTML_AAT_FULL_TIMEOUT' reports/aat-fidelity/run-aozora2html-aat-full.sh
```

Expected: the line shows the new default (`300s` if Step 4 applied it; `180s` if the caveat path was taken, with the caveat recorded in the report).

- [ ] **Step 6: Commit Task 2**

```bash
git add \
  reports/aat-fidelity/aozora2html-timeout-tail-measure.sh \
  reports/aat-fidelity/aozora2html-timeout-tail-report.md \
  reports/aat-fidelity/run-aozora2html-aat-full.sh
git commit -m "report(aozora2html): measure timeout tail distribution and set default"
```

---

### Task 3: Commit the parse_incomplete classifier and report

**Files:**
- Create: `reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py`
- Create: `reports/aat-fidelity/aozora2html-parse-incomplete-report.md`
- Create: `tests/aozora2html-parse-incomplete-classifier-smoke.sh`

**Interfaces:**
- Consumes: persisted AAT files at `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/*.json`.
- Produces: a deterministic classifier script, a generated classification report, and a smoke test.

- [ ] **Step 1: Write the classifier**

Create `reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py`:

```python
#!/usr/bin/env python
"""Classify aozora2html parse_incomplete AATs by failure mode.

Deterministic regex classification of meta.warnings[0].message for AAT files
whose meta.parse_complete == false. Classes: ruby_structural, invalid_xhtml,
ruby_internal_error, other.
"""
from __future__ import annotations

import argparse
import glob
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

CLASSES = ("ruby_structural", "invalid_xhtml", "ruby_internal_error", "other")
RUBY_STRUCTURAL_RE = re.compile(r"エラー\([^)]*行目\):")


def classify(message: str) -> str:
    if "invalid XHTML:" in message:
        return "invalid_xhtml"
    if "NoMethodError" in message or "private method" in message:
        return "ruby_internal_error"
    if RUBY_STRUCTURAL_RE.search(message):
        return "ruby_structural"
    return "other"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("aat_dir")
    parser.add_argument("--report-md", required=True, type=Path)
    args = parser.parse_args()

    buckets: dict[str, list[str]] = defaultdict(list)
    examples: dict[str, str] = {}
    for path in sorted(glob.glob(f"{args.aat_dir}/*.json")):
        with open(path, encoding="utf-8") as f:
            data = json.load(f)
        if data.get("meta", {}).get("parse_complete", True):
            continue
        warnings = data.get("meta", {}).get("warnings") or []
        msg = warnings[0].get("message", "") if warnings else ""
        cls = classify(msg)
        wid = data.get("work_id", "")
        buckets[cls].append(wid)
        examples.setdefault(cls, msg.strip())

    lines = [
        "# aozora2html parse_incomplete Classification Report",
        "",
        "Date: 2026-07-03",
        f"Source AAT dir: `{'/'.join(args.aat_dir.split('/')[-3:])}`",
        "",
        "## Summary",
        "",
        "| Class | Reports | Example message |",
        "|---|---:|---|",
    ]
    for cls in CLASSES:
        wids = buckets.get(cls, [])
        ex = (examples.get(cls, "") or "").replace("|", "\\|")
        lines.append(f"| {cls} | {len(wids)} | {ex} |")
    lines.append(f"| **Total** | **{sum(len(v) for v in buckets.values())}** | |")
    lines.append("")
    lines.append("## Remediation scope (deferred)")
    lines.append("")
    lines.append(
        "- `ruby_structural`: upstream aozora2html parser rejects edge-case markup; "
        "investigate wrapper pre-normalization (e.g., CRLF) as a follow-up, not here."
    )
    lines.append(
        "- `invalid_xhtml`: mapper strictness; a `roxmltree` -> `html5ever` migration "
        "is a separate plan, not this one."
    )
    lines.append(
        "- `ruby_internal_error`: upstream aozora2html 3.0.1 bugs; file upstream reports only."
    )
    lines.append("- `other`: inspect the example message before routing.")
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.write_text("\n".join(lines) + "\n", encoding="utf-8")

    print(json.dumps({cls: len(buckets.get(cls, [])) for cls in CLASSES}, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

Make it executable:

```bash
chmod +x reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py
```

- [ ] **Step 2: Run the classifier against the full-corpus AATs**

Run:

```bash
aat_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter"
python reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py \
  "$aat_dir" \
  --report-md reports/aat-fidelity/aozora2html-parse-incomplete-report.md
```

Expected: prints a JSON summary whose four class counts sum to 105 (e.g. `{"ruby_structural": 57, "invalid_xhtml": 30, "ruby_internal_error": 17, "other": 1}`). Writes `…-report.md`. **Record the actual counts in the report if they differ from 57/30/17/1.**

- [ ] **Step 3: Write the smoke test**

Create `tests/aozora2html-parse-incomplete-classifier-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/parse-incomplete-classifier-smoke"
aat_dir="$out_dir/aat"
report="$out_dir/report.md"

rm -rf "$out_dir"
mkdir -p "$aat_dir"

cat > "$aat_dir/ruby_struct.json" <<'JSON'
{ "work_id": "ws", "version": 1, "meta": { "parse_complete": false, "warnings": [ {"message": "エラー(123行目): 構文エラー"} ] }, "blocks": [] }
JSON
cat > "$aat_dir/xhtml.json" <<'JSON'
{ "work_id": "wx", "version": 1, "meta": { "parse_complete": false, "warnings": [ {"message": "invalid XHTML: <br> not closed"} ] }, "blocks": [] }
JSON
cat > "$aat_dir/complete.json" <<'JSON'
{ "work_id": "wc", "version": 1, "meta": { "parse_complete": true, "warnings": [] }, "blocks": [] }
JSON

python "$repo_root/reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py" \
  "$aat_dir" --report-md "$report"

grep -F '| ruby_structural | 1 |' "$report"
grep -F '| invalid_xhtml | 1 |' "$report"
grep -F '| **Total** | **2** |' "$report"
```

Make it executable:

```bash
chmod +x tests/aozora2html-parse-incomplete-classifier-smoke.sh
```

- [ ] **Step 4: Run the smoke**

Run:

```bash
bash tests/aozora2html-parse-incomplete-classifier-smoke.sh
```

Expected: exits 0; both `grep` lines match.

- [ ] **Step 5: Commit Task 3**

```bash
git add \
  reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py \
  reports/aat-fidelity/aozora2html-parse-incomplete-report.md \
  tests/aozora2html-parse-incomplete-classifier-smoke.sh
git commit -m "report(aozora2html): commit parse_incomplete classifier and report"
```

---

### Task 4: Build the visible_text_body_order divergence locator and characterize failures

**Files:**
- Create: `crates/ab-check/examples/vtbo_locate.rs`
- Create: `reports/aat-fidelity/aozora2html-vtbo-characterize.sh`
- Create: `reports/aat-fidelity/aozora2html-vtbo-report.md`

**Interfaces:**
- Consumes (all `pub`):
  - `ab_check::encoding::decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource>` where `DecodedSource { text: String, .. }` (`crates/ab-encoding/src/lib.rs:32`, re-exported via `crates/ab-check/src/encoding.rs:8`). Needed because Aozora sources are often Windows-31J, not UTF-8.
  - `ab_check::properties::body_text(text: &str) -> &str` (`crates/ab-check/src/properties.rs:129`) — extracts the Aozora body (drops header/footer) the same way the property does.
  - `ab_check::source_projection::comparison_lossy_body(txt: &str) -> String` (`crates/ab-check/src/source_projection.rs:2`, re-export of `ab_source_syntax::comparison_lossy_body`).
  - `ab_check::aat::comparison_visible_text_projection(aat: &serde_json::Value) -> String` (`crates/ab-check/src/aat.rs:24`).
  - `normalize_visible` is NOT exported (`crates/ab-check/src/properties.rs:250` is private `fn`). The example replicates it EXACTLY: `value.nfkc().collect::<String>().split_whitespace().collect::<Vec<_>>().join(" ")` — note this collapses runs of whitespace to single spaces; it does NOT strip all whitespace (that was the previous plan's bug).

- [ ] **Step 1: Write the locator example**

Create `crates/ab-check/examples/vtbo_locate.rs`:

```rust
//! Locate the first character where the AAT visible-text projection is not a
//! subsequence of the source lossy-body projection, mirroring the exact
//! `visible_text_body_order` property in `crates/ab-check/src/properties.rs`.
//! Used to characterize the 668 VTBO failures.

use std::path::PathBuf;

use ab_check::aat::comparison_visible_text_projection;
use ab_check::encoding::decode_source_bytes;
use ab_check::properties::body_text;
use ab_check::source_projection::comparison_lossy_body;
use serde_json::Value;

#[derive(clap::Parser)]
#[command(name = "vtbo_locate")]
struct Args {
    /// Path to the AAT JSON file (persisted filenames are suffixed, e.g.
    /// 000081_4418-04cb6bb131bc.json; resolve by glob outside this binary).
    #[arg(long)]
    aat: PathBuf,
    /// Path to the RAW Aozora source BYTES (often Windows-31J; decoded here).
    #[arg(long)]
    source: PathBuf,
    /// How many chars of surrounding context to print around the divergence.
    #[arg(long, default_value_t = 40)]
    context: usize,
}

/// Exact copy of `ab_check::properties::normalize_visible` (which is private).
/// NFKC-normalize then collapse runs of whitespace to a single space.
fn normalize_visible(value: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    value
        .nfkc()
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Same subsequence check the property uses (`is_subsequence` in properties.rs):
/// walk projection chars, consuming the haystack via `.any()`; the first char
/// that cannot be matched is the divergence. Returns
/// (divergence_index_in_projection, source_chars_consumed_so_far).
fn first_non_subsequence(projection: &str, source: &str) -> Option<(usize, usize)> {
    let mut si = 0usize;
    let mut source_chars = source.chars();
    for (pi, pc) in projection.chars().enumerate() {
        let mut found = false;
        for sc in source_chars.by_ref() {
            si += 1;
            if sc == pc {
                found = true;
                break;
            }
        }
        if !found {
            return Some((pi, si));
        }
    }
    None
}

fn main() -> anyhow::Result<()> {
    use clap::Parser;
    let args = Args::parse();

    let aat: Value = serde_json::from_str(&std::fs::read_to_string(&args.aat)?)?;

    // Read raw BYTES and decode the same way the harness does (properties.rs is
    // called from check.rs with decode_source_bytes(&txt_bytes).text). Reading
    // with read_to_string would fail on Windows-31J sources.
    let source_bytes = std::fs::read(&args.source)?;
    let decoded = decode_source_bytes(&source_bytes)?;

    // Mirror properties.rs:81 exactly:
    //   normalize_visible(&source_projection::comparison_lossy_body(body_text(txt)))
    let source = normalize_visible(&comparison_lossy_body(body_text(&decoded.text)));
    let projection = normalize_visible(&comparison_visible_text_projection(&aat));

    match first_non_subsequence(&projection, &source) {
        None => {
            println!("OK: projection is a subsequence of source");
            Ok(())
        }
        Some((pi, si)) => {
            let p: Vec<char> = projection.chars().collect();
            let s: Vec<char> = source.chars().collect();
            let pstart = pi.saturating_sub(args.context);
            let pend = (pi + args.context).min(p.len());
            let sstart = si.saturating_sub(args.context);
            let send = (si + args.context).min(s.len());
            println!("divergence_at_projection_index={}", pi);
            println!("source_cursor_consumed={}", si);
            println!("projection[{}..{}]={}", pstart, pend, p[pstart..pend].iter().collect::<String>());
            println!("source[{}..{}]={}", sstart, send, s[sstart..send].iter().collect::<String>());
            Ok(())
        }
    }
}
```

Add the example's dependencies to `crates/ab-check/Cargo.toml` under an `[[example]]` target so dev-only deps are visible:

```toml
[[example]]
name = "vtbo_locate"
path = "examples/vtbo_locate.rs"
```

And ensure `clap`, `anyhow`, `unicode-normalization` are available as dev-dependencies of `ab-check`. Check first:

```bash
rg -n 'clap|anyhow|unicode-normalization' crates/ab-check/Cargo.toml
```

If any is missing, add it to `[dev-dependencies]`, e.g.:

```toml
[dev-dependencies]
unicode-normalization = "0.1"
```

(Use the same versions the workspace already pins elsewhere — run `rg -n 'unicode-normalization' Cargo.lock` to copy the version.)

- [ ] **Step 2: Build the example**

Run:

```bash
cargo build -p ab-check --example vtbo_locate
```

Expected: compiles. The locator replicates `normalize_visible` exactly (it is private in `ab_check::properties`); do NOT swap it for a `use` — there is no public export. If the build fails on `unicode_normalization::UnicodeNormalization` not being in scope, confirm the dev-dependency was added.

- [ ] **Step 3: Sanity-check the locator on the known failing work**

Source resolution: check reports carry only `{adapter, adapter_version, results, work_id}` — there is NO `source_archive`/`entry_name`. The source path comes from joining `work_id` to `index.json.works[].txt_path` (form `cards/NNN/files/X.zip::entry.txt`), resolved against the corpus root. Persisted AAT filenames are suffixed (e.g. `000081_4418-04cb6bb131bc.json`), so resolve by glob.

Run:

```bash
RD="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z"
CORPUS="${AB_AOZORA_CORPUS:-/home/bor/Dependencies/aozorabunko}"

# 1. Join work_id -> txt_path via index.json (NOT the check report).
TXT_PATH=$(jq -r --arg id "000081_4418" '.works[] | select(.id == $id) | .txt_path' "$RD/index.json")
echo "txt_path=$TXT_PATH"
[[ -n "$TXT_PATH" ]] || { echo "work not in index.json"; exit 1; }

# 2. Split "cards/.../X.zip::entry.txt" into archive + entry.
archive_rel=${TXT_PATH%%::*}
entry=${TXT_PATH#*::}
[[ "$entry" == "$TXT_PATH" ]] && entry=""   # no :: separator
archive="$CORPUS/$archive_rel"

python - "$archive" "$entry" "/tmp/vtbo_000081_4418_src.txt" <<'PY'
import zipfile, sys
archive, entry, out = sys.argv[1], sys.argv[2], sys.argv[3]
with zipfile.ZipFile(archive) as z:
    if entry:
        data = z.read(entry)
    else:
        info = max(z.infolist(), key=lambda i: i.file_size)
        data = z.read(info)
open(out, "wb").write(data)
print(archive, entry)
PY

# 3. AAT files are suffixed; glob by work_id prefix.
AAT=$(ls "$RD/aat/aozora2html-adapter/000081_4418"*.json | head -1)
[[ -f "$AAT" ]] || { echo "AAT not found for 000081_4418"; exit 1; }
echo "AAT=$AAT"

cargo run -p ab-check --example vtbo_locate -- --aat "$AAT" --source /tmp/vtbo_000081_4418_src.txt --context 60
```

If the glob finds no AAT, the work produced no AAT (e.g. it timed out / parse-aborted) and is not a real VTBO failure — pick another work_id from Step 4's `failing_works.tsv`.

Expected: prints `divergence_at_projection_index=<N>`, `source_cursor_consumed=<M>`, and two context strings. **Record the index and the two context strings.** If it prints `OK: projection is a subsequence of source`, the locator does NOT mirror the real property — re-check `normalize_visible` and `body_text` against `properties.rs:81` before proceeding.

- [ ] **Step 4: Write the characterization script**

Create `reports/aat-fidelity/aozora2html-vtbo-characterize.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
run_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z"
aat_dir="$run_dir/aat/aozora2html-adapter"
reports_dir="$run_dir/check-reports"
corpus="${AB_AOZORA_CORPUS:-/home/bor/Dependencies/aozorabunko}"
out_dir="${AB_AOZORA2HTML_VTBO_OUT:-$run_dir/triage/outputs/vtbo}"
sample="${AB_AOZORA2HTML_VTBO_SAMPLE:-0}"   # 0 = all 668
mkdir -p "$out_dir"

python - "$run_dir" "$corpus" > "$out_dir/failing_works.tsv" <<'PY'
import json, os, sys
run_dir, corpus = sys.argv[1], sys.argv[2]

# Join work_id -> txt_path via index.json (check reports carry no source path).
index = json.load(open(os.path.join(run_dir, "index.json")))
txt_by_id = {w["id"]: w.get("txt_path", "") for w in index.get("works", [])}

reports_dir = os.path.join(run_dir, "check-reports")
for root, _, files in os.walk(reports_dir):
    for fn in files:
        if not fn.endswith(".json"):
            continue
        try:
            rep = json.load(open(os.path.join(root, fn)))
        except Exception:
            continue
        results = rep.get("results", {})
        vtbo = results.get("visible_text_body_order", {})
        if isinstance(vtbo, dict) and vtbo.get("pass") is False:
            wid = rep.get("work_id", "")
            txt_path = txt_by_id.get(wid, "")
            archive_rel, _, entry = txt_path.partition("::")
            archive = os.path.join(corpus, archive_rel) if archive_rel else ""
            print(f"{wid}\t{archive}\t{entry}")
PY

count=0
while IFS=$'\t' read -r wid archive entry; do
  [[ -z "$wid" ]] && continue
  count=$((count + 1))
  [[ "$sample" -ne 0 && "$count" -gt "$sample" ]] && break
  # AAT files are suffixed (000081_4418-04cb6bb131bc.json); glob by work_id prefix.
  aat=$(ls "$aat_dir/$wid"*.json 2>/dev/null | head -1 || true)
  [[ -f "$aat" ]] || continue
  [[ -n "$archive" && -f "$archive" ]] || continue
  src="/tmp/vtbo_src.$$.txt"
  python - "$archive" "$entry" "$src" <<'PY' || continue
import zipfile, sys
archive, entry, out = sys.argv[1], sys.argv[2], sys.argv[3]
try:
    with zipfile.ZipFile(archive) as z:
        if entry:
            data = z.read(entry)
        else:
            info = max(z.infolist(), key=lambda i: i.file_size)
            data = z.read(info)
        open(out, "wb").write(data)
except Exception:
    sys.exit(1)
PY
  cargo run -p ab-check --example vtbo_locate -- --aat "$aat" --source "$src" --context 60 \
    > "$out_dir/$wid.txt" 2>/dev/null || true
  rm -f "$src"
done < "$out_dir/failing_works.tsv"

echo "characterized $count works; individual outputs in $out_dir/*.txt"
```

Make it executable:

```bash
chmod +x reports/aat-fidelity/aozora2html-vtbo-characterize.sh
```

- [ ] **Step 5: Run the characterization on a bounded sample first**

Run on 20 works to validate the loop before the full 668:

```bash
AB_AOZORA2HTML_VTBO_SAMPLE=20 \
  bash reports/aat-fidelity/aozora2html-vtbo-characterize.sh
```

Expected: writes `…/vtbo/*.txt` for up to 20 works; prints `characterized <N> works`. Inspect 3–5 of the `.txt` outputs and **record the recurring divergence patterns** (e.g., dropped gaiji description, warigaki reorder, caption reattachment) in Step 6.

- [ ] **Step 6: Write the VTBO characterization report**

Create `reports/aat-fidelity/aozora2html-vtbo-report.md`. Fill the bracketed sections from the Step 5 sample outputs:

```markdown
# aozora2html visible_text_body_order Characterization

Date: 2026-07-03
Source run: `aozora2html-full-20260703T020301Z` (668 per-report failures, ~662 unique works)

## Method

For each failing work, ran `cargo run -p ab-check --example vtbo_locate` against
the persisted AAT (resolved by glob, since filenames are suffixed) and the
source bytes (work_id joined to `index.json.works[].txt_path`, extracted from
the zip entry). The locator mirrors the exact `visible_text_body_order`
property: `decode_source_bytes` -> `body_text` -> `comparison_lossy_body` ->
`normalize_visible` (NFKC + whitespace-collapse) on the source side, and
`comparison_visible_text_projection` -> `normalize_visible` on the AAT side,
then finds the first projection char that is not a subsequence of the source.

## Sample findings (first 20)

- divergence indices observed: [list of (work_id, index, one-line context)]
- recurring patterns:
  - [pattern A — e.g., gaiji description retained in projection but stripped by source]
  - [pattern B — e.g., ruby base ordering near warigaki]
  - [pattern C]

## Next-step routing

- adapter-bug-shaped patterns (AAT drops/reorders text the source carries):
  file as adapter issues.
- acceptable-abstraction patterns (AAT preserves text the lossy source strips):
  adjust the property or document as expected.

Full 668-work run is a follow-up; this report establishes the classification
method and the dominant sample patterns.
```

- [ ] **Step 7: Commit Task 4**

```bash
git add \
  crates/ab-check/examples/vtbo_locate.rs \
  crates/ab-check/Cargo.toml \
  reports/aat-fidelity/aozora2html-vtbo-characterize.sh \
  reports/aat-fidelity/aozora2html-vtbo-report.md
git commit -m "tool(ab-check): add visible_text_body_order divergence locator + characterization"
```

---

## Self-Review

- **Spec coverage:** The handoff recommended four next-session items; this plan has one task per item. Fix `normalize_figure_alt` panic → Task 1. Decide timeout policy → Task 2 (measurement + decision + default). parse_incomplete triage → Task 3 (classifier + report; per-class remediation explicitly deferred per scope note). VTBO characterization → Task 4 (locator tool + characterization report). Each residual bucket from the handoff is covered.
- **Placeholder scan:** No "TBD / TODO / implement later" in code steps. The measurement/characterization tasks use `[…]` placeholders only inside `.md` report templates that are *meant to be filled from observed data* — those are fill-in fields, not implementation gaps, and Step instructions explicitly say "record the actual value." No code block contains a placeholder.
- **Type consistency:** `normalize_figure_alt(raw: &str) -> String` (Task 1) matches the call sites at `xhtml_mapper.rs:339` and `:565` and `source_derived.rs` source-note figure parsing. Task 4's locator uses verified `pub` APIs: `ab_check::encoding::decode_source_bytes`, `ab_check::properties::body_text` (`properties.rs:129`), `ab_check::source_projection::comparison_lossy_body` (`source_projection.rs:2`), `ab_check::aat::comparison_visible_text_projection` (`aat.rs:24`). `normalize_visible` (`properties.rs:250`) is private and replicated EXACTLY as `value.nfkc().collect::<String>().split_whitespace().collect::<Vec<_>>().join(" ")` — NOT a strip-all-whitespace (the earlier draft's `filter(!is_whitespace)` was wrong and would have produced false divergences).
- **Verified assumptions (against the persisted run, not guessed):**
  - Check reports carry only top-level `{adapter, adapter_version, results, work_id}` — NO `bucket`/`source_archive`/`entry_name`. Tasks 2 and 4 therefore derive failures from `results.adapter_timeout.pass == false` / `results.visible_text_body_order.pass == false` and join `work_id` to `index.json.works[].txt_path` (form `cards/NNN/files/X.zip::entry.txt`, parsed with `partition("::")`).
  - Persisted AAT filenames are suffixed, e.g. `000081_4418-04cb6bb131bc.json`. Tasks 2/4 resolve by `ls "$aat_dir/$wid"*.json` (and Task 4 Step 3 sanity-checks one explicitly).
  - Aozora sources are often Windows-31J; Task 4 reads bytes and calls `decode_source_bytes` (mirroring `check.rs:125`), not `read_to_string`.
  - Task 3's classifier takes `--report-md` as `type=Path` and imports `pathlib.Path` (the earlier `str` form would have crashed on `.parent`).
