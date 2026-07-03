# aozora2html residual buckets — deep-dive verification memo

Date: 2026-07-03
Status: **verified prep for next session**
Scope: `adapter_timeout`, `adapter_protocol_error`, `parse_incomplete`, and `report_failed_other_property` buckets from `aozora2html-full-20260703T020301Z`.

> Skills used: `research`, `systematic-debugging`, `diagnosing-bugs`.
> This memo does not implement fixes; it provides verified root causes, reproduction commands, and exact classification rules for the next session.

## Executive summary

This memo deepens the bucket characterization in `docs/handoffs/aozora2html-buckets-and-aozora-rs-path.md` and formally verifies each claim before the next session attempts fixes.

Key findings so far:

- **`adapter_protocol_error` (`000125_1317`)**: reproduced with `RUST_BACKTRACE=1`. Root cause is a byte-index slice at `adapters/aozora2html/src/xhtml_mapper.rs:322` in `normalize_figure_alt`. The code does `text[1..idx]` after `text.starts_with('「')`, but '「' is a 3-byte UTF-8 character, so byte index 1 is inside the character. Fix: use `chars().next()` / `chars().next_back()` instead of byte slicing.
- **`adapter_timeout`**: reproduced work `000077_1323` with a 600 s limit; it completes in ~270 s real time. Timeouts are **slow large files**, not hung processes.
- **`parse_incomplete`**: deterministic reclassification of the 105 reports yields **57 Ruby structural errors**, **30 invalid XHTML outputs**, **17 Ruby internal errors**, **1 other**.
- **`report_failed_other_property`**: `visible_text_body_order` dominates (668 reports). Sample work `000081_4418` shows the divergence is subtle; the next session needs the actual `ab_source_syntax::comparison_lossy_body` projection to locate the exact non-subsequence point.

## Method

For each bucket we apply the diagnosing-bugs discipline:

1. Build a tight feedback loop (repro command).
2. Reproduce and minimise.
3. Identify root cause or stable classification.
4. Record exact file/line evidence and proposed fix/classification rule.

All reproduction uses the current release build of the Rust mapper:
`adapters/aozora2html/target/release/aozora2html-adapter`.

---

## 1. `adapter_protocol_error`

### 1.1 Reproduction

```bash
# Extract source for work 000125_1317
python3 - <<'PY'
import zipfile
z = zipfile.ZipFile('/home/bor/Dependencies/aozorabunko/cards/000125/files/1317_ruby_22263.zip')
z.extract('kokushikan_satsujin_jiken.txt', '/tmp/aozora-debug/')
PY

# Run adapter with backtrace
cd /home/bor/Projects/ab-validator
RUST_BACKTRACE=1 timeout 300 \
  ./adapters/aozora2html/aozora2html-adapter --mode aat \
  < /tmp/aozora-debug/kokushikan_satsujin_jiken.txt \
  > /tmp/aozora-debug/out.json \
  2> /tmp/aozora-debug/err.log
```

Result: exit code **101**, stderr captured.

### 1.2 Backtrace (verified)

```
thread 'main' panicked at src/xhtml_mapper.rs:322:24:
start byte index 1 is not a char boundary; it is inside '「' (bytes 0..3 of string)
stack backtrace:
   ...
   4: aozora2html_adapter::xhtml_mapper::normalize_figure_alt
   5: aozora2html_adapter::xhtml_mapper::map_img_gaiji
   6: aozora2html_adapter::xhtml_mapper::map_inline
   7: aozora2html_adapter::xhtml_mapper::map_from_container
   8: aozora2html_adapter::xhtml_mapper::map_blocks_from_xhtml_bytes
   9: aozora2html_adapter::map_with_protocol
  10: aozora2html_adapter::map_with_protocol_bytes
  11: aozora2html_adapter::main
```

### 1.3 Root cause

`src/xhtml_mapper.rs:316-325`:

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let mut text = raw.trim();
    if text.starts_with('「') && text.ends_with('」') {
        if let Some(idx) = text.find('」') {
            return text[1..idx].to_string();  // BUG
        }
    }
    ...
}
```

After `text.starts_with('「')` succeeds, the first character is the 3-byte UTF-8 sequence `E3 80 8C`. `text[1..idx]` slices at byte 1, which is inside that character. Rust panics.

### 1.4 Proposed fix

Replace the byte slice with character-aware trimming:

```rust
fn normalize_figure_alt(raw: &str) -> String {
    let text = raw.trim();
    if text.starts_with('「') && text.ends_with('」') {
        let mut chars = text.chars();
        chars.next();      // remove opening 「
        chars.next_back(); // remove closing 」
        return chars.as_str().to_string();
    }
    ...
}
```

Additionally, the original used `text.find('」')` (first occurrence) while checking `ends_with('」')`. The char-based version removes the last closing quote, which is more consistent with the intent. If nested quotes are possible, `rfind` should be considered; for figure alt text, trimming first/last is sufficient.

### 1.5 Regression test

Add a mapper unit test with a gaiji/figure alt whose description begins with '「' and ends with '」', e.g. `「図書館」` or `「口＋愛」`.

### 1.6 Impact estimate

This is a single-report bucket (1 of 17 886). Fixing it removes the only `adapter_protocol_error` from the current run.

---

## 2. `adapter_timeout`

### 2.1 Feedback loop

Hypothesis: timeout works are large/complex files that exceed the 180 s harness limit. To test whether they are truly hung or just slow, run a representative timeout work with a much longer timeout and wall-clock it.

Representative work chosen: `000077_1323` (source path `cards/000077/files/1323_ruby_30726.zip::05_kaitei_gunkan.txt`, 601 KB zipped text entry).

### 2.2 Reproduction

```bash
python3 - <<'PY'
import zipfile
z = zipfile.ZipFile('/home/bor/Dependencies/aozorabunko/cards/000077/files/1323_ruby_30726.zip')
z.extract('05_kaitei_gunkan.txt', '/tmp/aozora-timeout/')
PY

cd /home/bor/Projects/ab-validator
time timeout 600 ./adapters/aozora2html/aozora2html-adapter --mode aat \
  < /tmp/aozora-timeout/05_kaitei_gunkan.txt \
  > /tmp/aozora-timeout/out.json \
  2> /tmp/aozora-timeout/err.log
```

Result: exit code **0**.

```
real    4m30.389s
user    4m24.916s
sys     0m0.703s
```

The 601 KB work completes successfully when allowed 600 s instead of the harness's 180 s.

### 2.3 Interpretation

Timeouts are **slow large files**, not hung adapters. The 180 s default is too short for the largest sources under the current Ruby parser + Rust mapper pipeline.

### 2.4 Size correlation (verified)

From the corrected handoff: timeout reports have median size 367 KB vs ~11 KB corpus-wide; 50 of 196 timeout reports are >500 KB vs 89 of 17 885 corpus reports.

### 2.5 Open questions for next session

- Is the time dominated by Ruby `aozora2html` parser or by the Rust mapper? (The wrapper does not currently split timing; add per-stage timing if the decision depends on it.)
- What is the tail of the timeout distribution? If most timeouts are <300 s, raising the default to 300–600 s may clear the bucket.
- Should large files get an adaptive timeout (e.g. `max(180 s, size_based_limit)`), or is a single raised default acceptable?

---

## 3. `parse_incomplete`

### 3.1 Reproduction / classification

Classification uses the 105 persisted AAT files with `meta.parse_complete=false` and applies deterministic regexes to `meta.warnings[0].message`:

| Class | Rule | Reports | Sample work IDs |
|---|---|---:|:---|
| `ruby_structural` | contains `エラー(...行目):` and does NOT contain `invalid XHTML` or `NoMethodError` | **57** | `000160_2717`, `000081_454`, `000019_4376` |
| `invalid_xhtml` | contains `invalid XHTML:` | **30** | `000879_24455`, `001096_43672`, `000305_1897` |
| `ruby_internal_error` | contains `NoMethodError` or `private method` | **17** | `000243_1328`, `000250_18353`, `000311_16002` |
| `other` | everything else | **1** | `000301_1872` |
| **Total** | | **105** | |

Classification command (verified):

```python
import json, glob, re
classes = {'ruby_structural': [], 'invalid_xhtml': [], 'ruby_internal_error': [], 'other': []}
for f in glob.glob('aat/aozora2html-adapter/*.json'):
    data = json.load(open(f))
    if data.get('meta', {}).get('parse_complete', True):
        continue
    msg = data['meta']['warnings'][0]['message'] if data.get('meta', {}).get('warnings') else ''
    if 'invalid XHTML:' in msg:
        cls = 'invalid_xhtml'
    elif 'NoMethodError' in msg or 'private method' in msg:
        cls = 'ruby_internal_error'
    elif re.search(r'エラー\([^)]*行目\):', msg):
        cls = 'ruby_structural'
    else:
        cls = 'other'
    classes[cls].append(data['work_id'])
```

### 3.2 Interpretation

- **`ruby_structural` (57)**: upstream `aozora2html` parser rejects edge-case markup (unmatched indentation, CRLF issues, duplicate author lines, etc.). The wrapper already captures these cleanly as `parse_complete=false`.
- **`invalid_xhtml` (30)**: the Ruby parser succeeds but emits XHTML that the Rust mapper's strict XML parser rejects. These are mapper robustness issues.
- **`ruby_internal_error` (17)**: Ruby parser crashes with `NoMethodError` or `private method` errors. These are upstream bugs in `aozora2html` 3.0.1.
- **`other` (1)**: multi-byte punctuation warnings followed by a structural error (`000301_1872`).

### 3.3 Open questions for next session

- For `ruby_structural` cases, can the wrapper pre-normalize inputs to avoid some structural errors (e.g. CRLF), or are they genuine upstream limitations?
- For `invalid_xhtml` cases, can the Rust mapper use a more forgiving HTML parser (e.g. `html5ever`) instead of `roxmltree`?
- For `ruby_internal_error` cases, should they be reported upstream to `aozora2html`?

---

## 4. `report_failed_other_property`

### 4.1 Dominant property: `visible_text_body_order`

668 per-report failures / 662 unique affected works. The property checks whether the AAT visible-text projection is a subsequence of the source body text after NFKC normalization and whitespace removal.

#### 4.1.1 Sample work `000081_4418`

This work fails only `visible_text_body_order`.

- AAT projection length (raw): 9 436 chars.
- Source body length (raw): 10 081 chars.
- AAT contains all expected ruby bases (e.g. `base='蜘蛛'`, `base='狸'`, `base='洞熊'`).
- Reimplementing the AAT projection in Python and comparing it against a naive source body yielded a subsequence match, so the divergence is **not** a simple dropped ruby base.
- The actual `ab-check` property uses `ab_source_syntax::comparison_lossy_body` for the source side, which strips ruby markers, gaiji markers, commands, and bottom-note corrections. A naive body extraction is not sufficient to locate the divergence.

#### 4.1.2 Method for next session

To characterize the 668 failures, run the actual Rust projection side-by-side:

1. Extract source body for each VTBO-failing work.
2. Compute `ab_source_syntax::comparison_lossy_body(source)` and `ab_check::aat::comparison_visible_text_projection(aat)`.
3. Find the first character in the AAT projection that is not a subsequence of the source projection.
4. Bucket divergences by pattern:
   - dropped/duplicated text,
   - warigaki/kunten reordering,
   - caption/figure reattachment,
   - source-derived note preservation,
   - `unmapped-div` text ordering.

#### 4.1.3 Open questions

- Is the dominant pattern source projection being stricter than AAT (AAT preserves text the source projection strips), or AAT reordering/dropping text?
- How many VTBO failures are acceptable abstraction differences vs adapter bugs?

### 4.2 `gaiji_resolution` and `ruby_completeness`

Smaller subsets. Pending:

- Verify whether `gaiji_resolution` failures are due to the known `gaiji.marker.value.kind` compatibility simplification or genuine missing gaiji nodes.
- Verify whether `ruby_completeness` failures are warigaki/kunten-related flattening or standalone ruby bugs.

---

## 5. Recommended next-session order

1. **Fix and land the protocol-error panic** (`normalize_figure_alt`). It is a single-file, single-function bug with a verified repro, a clear fix, and a clear regression test. Estimated impact: removes the only `adapter_protocol_error` report.
2. **Decide on timeout policy**. A 600 s limit clears the sampled timeout work. Options:
   - raise the harness default timeout to 300–600 s, or
   - add an adaptive timeout (`max(180 s, bytes_based_limit)`), or
   - document the 196 reports as a coverage caveat.
3. **Run parse-incomplete triage**. The 105 reports are already classified; the next session should decide per-class remediation (upstream report, wrapper pre-normalization, or more forgiving XML parser).
4. **Characterize `visible_text_body_order` failures** with the actual Rust source projection to find the exact divergence pattern and split adapter bugs from acceptable abstraction differences.

---

## 6. References

- `docs/handoffs/aozora2html-buckets-and-aozora-rs-path.md`
- `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`
- `adapters/aozora2html/src/xhtml_mapper.rs`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/`
