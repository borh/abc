# aozora2html residual-bucket characterization and aozora-rs maintenance path

Date: 2026-07-03
Status: decision recorded
Resolves: `docs/handoffs/aozora-rs-block-content-mut-drift.md`

## Executive summary

- **aozora2html bucket characterization** (full-corpus run `aozora2html-full-20260703T020301Z`):
  - `adapter_timeout` — 196 reports (194 unique works). Strongly correlated with large source files (median entry size 367 KB vs ~11 KB corpus-wide). Timeout is the measurement harness's 180 s wall-clock limit, not an adapter-reported error.
  - `adapter_protocol_error` — 1 report (`000125_1317`), exit code `101`: Rust mapper panic.
  - `parse_incomplete` — 105 reports (103 unique works). Ruby `aozora2html` parser aborts or emits malformed XHTML; the wrapper converts this into `meta.parse_complete=false`, which then trips `parse_completeness`, `gaiji_resolution`, and `ruby_completeness`.
  - `report_failed_other_property` — 710 reports (~704 unique works). Dominated by `visible_text_body_order` (565 single-property, plus combinations), with smaller `gaiji_resolution` and `ruby_completeness` subsets. These are adapter-oracle mismatches where AAT is present and schema-valid but does not pass a heuristic property.
- **aozora-rs maintenance path**: choose **Option 3** — accept the typed adapter as broken-pending-JSON-migration and document it. aozora-rs is excluded from the workspace, has no active measurement/recipe, and the boundary decision has made AAT JSON the normative contract; restoring `ab-ir` API compatibility would be a dead-end detour.

---

## 1. Scope and primary sources

This investigation uses only in-repo artifacts from the current aozora2html full-corpus run and the aozora-rs adapter.

| Source | Path | What it proves |
|---|---|---|
| Run metadata | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/metadata.json` | Timeout = 180 s, jobs = 24, adapter version `aozora2html-adapter 0.1.0 gem-3.0.1` |
| Adapter error CSV | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/triage/outputs/adapter_errors.csv` | 196 `adapter_timeout`, 1 `adapter_protocol_error` |
| Property failure CSV | same `triage/outputs/property_failures.csv` | 668 `visible_text_body_order`, 190 `gaiji_resolution`, 127 `ruby_completeness`, 105 `parse_completeness` |
| Parse-incomplete examples | same `triage/outputs/parse_incomplete_examples.csv` | 105 reports with `parse_complete=false`; most have 3 failing properties |
| Check reports / AAT | `.../check-reports/aozora2html-adapter/` and `.../aat/aozora2html-adapter/` | Concrete failure messages and `meta.warnings` |
| Duplicate work_ids | full index scan | 236 work_ids appear in more than one report (different zip entries / txt_paths); bucket counts are per-report unless noted |
| Harness bucket logic | `crates/ab-check/src/check.rs` | Defines `adapter_timeout`, `adapter_protocol_error`, `fatal_error`, and how exit codes map to buckets |
| Property logic | `crates/ab-check/src/properties.rs` | Defines `parse_completeness`, `gaiji_resolution`, `ruby_completeness`, `visible_text_body_order` |
| Adapter protocol | `adapters/aozora2html/aozora2html-adapter` | Bash wrapper that runs Ruby parser + Rust mapper; non-zero Ruby exit is converted to `--parser-failed`, so the wrapper itself rarely fails |
| aozora-rs drift | `docs/handoffs/aozora-rs-block-content-mut-drift.md` | Three decision options and verified evidence |
| Boundary decision | `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md` | AAT JSON is the normative contract; `ab-ir` is optional in-workspace convenience |
| Workspace membership | `Cargo.toml:20-22` | `adapters/aozora-rs` is excluded from the workspace |
| Build recipes | `justfile` | No aozora-rs recipes; all current aozora recipes target `aozora2html` |

---

## 2. How the buckets are produced

`crates/ab-check/src/check.rs::invoke_and_check` spawns the adapter with a configurable timeout and classifies the result:

| Exit / timing | Report key | AAT saved? | Meaning |
|---|---|---|---|
| 0 or 2 | — | yes | Success; normal property checks run. |
| 1 | `fatal_error` | no | Adapter reported a fatal error (stderr). |
| anything else | `adapter_protocol_error` | no | Unexpected process exit; e.g. Rust panic (exit 101). |
| no exit before timeout | `adapter_timeout` | no | Harness killed the child after `timeout`. |

When the Ruby parser fails, the bash wrapper **does not exit non-zero**. It passes `--parser-failed` to the Rust mapper, which emits an AAT envelope with `meta.parse_complete=false`. That AAT then fails the `parse_completeness` property and usually cascades into `gaiji_resolution` and `ruby_completeness` because the body is empty or truncated.

The policy residual triage (`docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`) groups works into:
- `adapter_timeout_or_protocol_error`
- `schema_invalid_or_no_aat`
- `parse_incomplete` (parse_complete=false)
- `report_failed_other_property` (AAT present but a non-policy property fails)
- `source_feature_without_aat_observation` (clean AAT, source detector sees feature but adapter does not)

---

## 3. Bucket characterization

### 3.1 `adapter_timeout` — 196 reports (1.1 % of 17 886 reports; 194 unique works)

- **Signal**: report contains only `adapter_timeout: {pass:false, message:"adapter timed out"}`; no AAT is produced.
- **Distribution by file size**: timeout reports have a median zipped-text entry size of **367 KB** vs **~11 KB** for the whole corpus. 50 of the 196 timeout reports (25 %) are larger than 500 KB, whereas only 89 of 17 885 corpus reports (0.5 %) exceed 500 KB.
- **Interpretation**: the 180 s wall-clock limit is being exceeded on large or complex sources. The wrapper runs the Ruby parser under `nix develop` and then `exec`s the Rust mapper directly; the harness times the whole wrapper, so the stage that consumed the time is not distinguishable. There is no stderr or partial output to separate Ruby parser slowness from mapper slowness.
- **Policy residual footprint**: 68 of the 264 warigaki/kunten residual reports are in this bucket (32 warigaki + 36 kunten), so timeout is a meaningful contributor to residual policy gaps.
- **Example**: `000077_1323` (`check-reports/aozora2html-adapter/000077_1323-b51132c1dd72.json`).

### 3.2 `adapter_protocol_error` — 1 report

- **Signal**: `adapter_protocol_error: {pass:false, message:"unexpected exit code Some(101)"}`.
- **Report**: `000125_1317`.
- **Interpretation**: exit code 101 is the Rust panic exit code. The Rust mapper panicked before it could write AAT or a `parser-failed` envelope. This is a genuine adapter bug, distinct from the parser-level errors that are caught and converted to `parse_complete=false`.
- **Next step if revived**: reproduce with `RUST_BACKTRACE=1` and fix the mapper panic. Likely candidates are unchecked slice/index operations or `unwrap` calls in `adapters/aozora2html/src/source_derived.rs` or `xhtml_mapper.rs`.

### 3.3 `parse_incomplete` — 105 reports (103 unique works)

- **Signal**: `parse_completeness` fails because `meta.parse_complete=false`; AAT is present and schema-valid but has `blocks: []` or a truncated body.
- **Co-failing properties** (per-report, 105 reports):
  | Pattern | Reports |
  |---|---:|
  | fail all three (`gaiji_resolution` + `parse_completeness` + `ruby_completeness`) | 59 |
  | fail exactly two of the three | 39 |
  | fail only `parse_completeness` | 7 |
  This is expected because an aborted parse produces no AAT body, so source-side gaiji/ruby markers often have no AAT counterpart.
- **Root-cause shape** from `meta.warnings`:
  - About half are **Ruby parser structural errors**: e.g. `字下げを閉じようとしましたが、字下げ中ではありません`, `字下げ中に本文が終了しました`, `改行コードを、「CR+LF」にあらためてください`, `parser encounted author twice`, `字詰め中に本文が終了しました`.
  - About half are **malformed XHTML or Ruby internal errors**: `invalid XHTML: expected 'div' tag, not 'body'`, `expected '"' not '<'`, `malformed entity reference`, `a non-XML character '\u{c}' found`, plus Ruby `NoMethodError` (`undefined method 'close_tag' for nil`, `private method 'print' called for nil`).
  - A handful are multi-byte-punctuation warnings followed by structural errors.
- **Interpretation**: the failures split roughly evenly between upstream `aozora2html` Ruby parser limitations on edge-case markup and cases where the Ruby parser's XHTML output is not well-formed XML and the Rust mapper rejects it. Neither class is fixable by tuning the AAT contract — they are adapter-side parser/mapping robustness issues. Exact 3-way counts depend on classification rules; the qualitative split is the stable finding.
- **Examples**: `000125_45231` (indentation close error), `000019_4376` (invalid XHTML), `000083_1051` (body ended during indentation).

### 3.4 `report_failed_other_property` — 710 reports (~704 unique works)

- **Signal**: AAT is present, schema-valid, parse_complete=true, but one or more heuristic properties fail.
- **Property combination breakdown** (per-report counts; the same work may appear in more than one row if measured from different zip entries):
  | Failing property set | Reports |
  |---|---:|
  | `(visible_text_body_order,)` | 565 |
  | `(gaiji_resolution, visible_text_body_order)` | 88 |
  | `(gaiji_resolution,)` | 24 |
  | `(gaiji_resolution, ruby_completeness)` | 9 |
  | `(ruby_completeness,)` | 9 |
  | `(ruby_completeness, visible_text_body_order)` | 9 |
  | `(gaiji_resolution, ruby_completeness, visible_text_body_order)` | 6 |
- **`visible_text_body_order`**: 668 per-report failures / 662 unique affected works. The AAT's visible-text projection is not a subsequence of the source body text. This is typically caused by text reordering or abstraction mismatches — e.g. warigaki/kunten layouts, captions moved onto figures, source-derived notes, or `unmapped-div` wrappers that preserve text the source projection does not account for. Example: `000082_49526` contains a table-of-contents rendered as `jisage_block` + `unmapped-div` structures; the source projection and AAT projection diverge in ordering.
- **`gaiji_resolution`**: 190 per-report failures / 187 unique affected works. Either fewer AAT `gaiji` nodes than source markers, or an AAT gaiji node lacks both `resolved` and `unresolved_reason`. This is partly a known compatibility simplification: the adapter emits only `Image`/`UnicodeCodepoint`/`Unknown` for `gaiji.marker.value.kind`, while other adapters emit full debug strings.
- **`ruby_completeness`**: 127 per-report failures. A source ruby marker has no corresponding AAT ruby node. Often occurs alongside warigaki/kunten where the adapter flattens ruby into plain text or wraps it in an unmapped structure.
- **Policy residual footprint**: 167 of the 264 warigaki/kunten residual reports are in this bucket (16 warigaki + 151 kunten). These are the reports that need adapter-oracle characterization to decide whether the failure is an adapter bug or an acceptable abstraction difference.

### 3.5 `source_feature_without_aat_observation`

Not requested, but relevant context: 32 reports have clean AAT and no property failures, yet the source detector still sees warigaki/kunten markers. These are source-detector false positives or adapter-observation gaps, not adapter failures. They were already separated into `source-feature-gap-*` worksets.

---

## 4. aozora-rs maintenance-path decision

### 4.1 Drift recap

`adapters/aozora-rs/src/aat.rs` calls `ab_ir::block_content_mut(...)` at lines 464, 642, and 753. `crates/ab-ir/src/lib.rs` no longer defines that function. `cargo build` in `adapters/aozora-rs/` fails with three `E0425` errors (`docs/handoffs/aozora-rs-block-content-mut-drift.md`).

The drift is deeper than one function: the pre-removal `block_content_mut` matched `Block::Break { content, .. }`, but the current `Block::Break` has only `kind: BreakKind`. Restoring the old accessor would require either an empty mutable Vec (e.g. `thread_local!` static) or a panic arm for `Break` — both are worse than migrating the call sites.

### 4.2 Boundary context

`docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md` has already decided:

> The adapter boundary is the AAT JSON schema and contract docs. `ab-ir` remains an optional in-workspace convenience library for adapters that want typed builders, not the normative external adapter contract.

Consequence: `ab-ir` may change lockstep with the workspace. Adapters that depend on `ab-ir` internals are responsible for keeping up; the workspace does not guarantee their build.

### 4.3 Evaluated options

| Option | Description | Verdict |
|---|---|---|
| 1 | Migrate aozora-rs off `block_content_mut` to direct field mutation or replacement-block construction. | Technically correct, but unjustified overhead because aozora-rs is not a live measurement target. |
| 2 | Restore `block_content_mut` with a contorted `Break` arm (static empty Vec or panic). | Reintroduces an API the workspace does not use and violates the boundary decision. Rejected. |
| 3 | Accept aozora-rs as broken-pending-JSON-migration and document it. | Aligns with the boundary decision and current measurement reality. **Chosen.** |

### 4.4 Evidence that aozora-rs is not a live target

- `Cargo.toml:20-22` excludes `adapters/aozora-rs` from the workspace; `cargo test --workspace` does not build it.
- `justfile` has no recipes for aozora-rs; all aozora recipes target `aozora2html`.
- Current measurement runs use `aozora2html-adapter` and emit AAT JSON without any `ab-*` dependency.
- The generated AAT-to-parser-IR mapping (`data/aat-to-parser-ir-mapping-v1.json`) was produced from the aozora2html corpus and the aozora-rs corpus evidence, but the active adapter path going forward is JSON-based.
- **Caveat**: aozora-rs was actively developed through 2026-05-04 (`ed37b70`, `2ef2dc5`, `eacd5b3`). The boundary decision postdates that work and supersedes the typed-adapter direction, but the broken state should not be silently forgotten if that work resumes.

### 4.5 Decision

**Choose Option 3.** Do not restore `block_content_mut` and do not migrate the three call sites now. Instead:

1. Document in `adapters/aozora-rs/README.md` (or create one) that the typed adapter is currently not building against the workspace `ab-ir` and is pending migration to the AAT-JSON boundary. Include the last active commit date (2026-05-04, `ed37b70`) and a re-evaluation trigger: revive only if aozora-rs is explicitly chosen as a live measurement target again.
2. Update `docs/handoffs/aozora-rs-block-content-mut-drift.md` status to **decided** and reference this file.
3. Treat any future revival of aozora-rs as a JSON-emitting adapter (Option 1-style migration at that time), not as a reason to restore `ab-ir` API surface.

### 4.6 Trade-off: why Option 3 over Option 1 now

Option 1 (migrate the three call sites to use `block_content` + replacement blocks) is technically small and would make `adapters/aozora-rs/` build again. The trade-off is:

- **Pros of Option 1**: preserves a buildable typed reference adapter; keeps the option of reviving aozora-rs without a larger rewrite.
- **Cons of Option 1**: it adds code changes to an adapter no current gate exercises, and it preserves the implicit `ab-ir`-as-adapter-SDK assumption that the boundary decision explicitly rejected. Any future `ab-ir` refactor could break aozora-rs again, creating ongoing maintenance burden for an API the workspace can change at will.

Given that the active measurement path is JSON-only, the lower-maintenance choice is to leave aozora-rs broken-with-notice and migrate it to AAT JSON if and when it is revived as a live target. This is a judgment call, not a verified fact; it is recorded here so it can be revisited if aozora-rs becomes active again.

---

## 5. Recommended next actions

1. **For aozora2html buckets**:
   - Timeout: consider either a per-file adaptive timeout or a separate "large file" fast path. The current 180 s limit is a measurement gate, not a parser bug, but it skews coverage toward smaller works.
   - Protocol error: reproduce `000125_1317` with backtrace and fix the Rust mapper panic.
   - Parse incomplete: bucket into "Ruby structural error" vs "malformed XHTML" vs "Ruby internal error" and decide which are upstream `aozora2html` limitations vs adapter-wrapper bugs.
   - Report-failed-other-property: start adapter-oracle characterization with `visible_text_body_order`, since it is the dominant residual.
2. **For aozora-rs**: document the broken-pending-JSON-migration state and close the handoff.
   **Done:** `adapters/aozora-rs/README.md` created; `docs/handoffs/aozora-rs-block-content-mut-drift.md` marked decided.

---

## 6. References

- `docs/handoffs/aozora-rs-block-content-mut-drift.md`
- `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`
- `docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`
- `docs/superpowers/reports/2026-07-03-aozora2html-source-feature-gap-classification.md`
- `crates/ab-check/src/check.rs`
- `crates/ab-check/src/properties.rs`
- `adapters/aozora2html/aozora2html-adapter`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/metadata.json`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/triage/outputs/adapter_errors.csv`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/triage/outputs/property_failures.csv`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/triage/outputs/parse_incomplete_examples.csv`
