# Full-Corpus Probe: AAT→Parser-IR Divergence Measurement

> Disposable probe run 2026-07-02 over the **full real aozora-rs AAT corpus**
> (17,894 documents). This upgrades the inferences in
> `docs/handoffs/real-corpus-validation.md` from sample-based to
> corpus-scale measurement, and overturns two of its findings.

## 1. Method

`prototypes/aat-to-parser-ir-probe/batch_aggregate.py` imports the probe
mapper's functions in-process and runs them over every file in
`ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter/*.json`.
No subprocess; no file clobber. Aggregates ledger entries across all 17,894
documents. Raw output: `docs/handoffs/_probe-full-corpus-raw.txt`.

## 2. Corpus-scale facts (measured, not sampled)

| Metric | Value |
|---|---:|
| files scanned | 17,894 |
| files failed to parse | 0 |
| files with ≥1 ledger entry | 17,894 (100%) |
| files with UNSUPPORTED | 5,474 (30.6%) |
| files with warigaki | 0 (0%) |
| total blocks scanned | 645,618 |
| total inline nodes scanned | 7,837,124 |
| total parser-IR nodes emitted | 7,800,123 |
| total ledger entries across corpus | 8,323,736 |

### Real node-kind distribution (full corpus)

| Block kinds | Count | Inline kinds | Count |
|---|---:|---|---:|
| `paragraph` | 637,088 | `text` | 4,204,219 |
| `heading` | 8,530 | `ruby` | 3,548,226 |
| | | `gaiji` | 56,187 |
| | | `style` | 28,492 |

Confirms `real-corpus-validation.md` Finding A at full scale: real aozora-rs
output uses only 2 block kinds and 4 inline kinds. The synthesized 27-entry
ledger's `accent`/`figure`/`warigaki`/`caption`/`tcy`/etc. are all genuinely
absent.

## 3. Divergence category distribution (measured)

| Category | Count | % |
|---|---:|---:|
| INVENTION | 3,864,599 | 46.4% |
| LOSS | 3,702,413 | 44.5% |
| STRUCTURAL | 645,618 | 7.8% |
| AMBIGUITY | 82,611 | 1.0% |
| UNSUPPORTED | 28,495 | 0.3% |

**Most loss is concentrated in a handful of rules firing on nearly every file**, not spread across the 27-rule vocabulary:

- `ruby.direction` LOSS — fires ~1,764,113× (every ruby; 3,548,226/2 since paired)
- `ruby.scope` INVENTION — fires on every ruby (3,548,226×)
- `meta.warnings[].severity` / `.code` INVENTION — 94,307× each
- Per-file constants (×17,894 each): `meta.adapter`, `meta.adapter_version`,
  `meta.parse_complete`, `meta.metrics`, `meta.semantic_summary` (LOSS);
  `schema_id/schema_hash`, `errors[]`, `source.normalization`,
  `source.source_path` (INVENTION); `meta.source_hash` (AMBIGUITY);
  plus one STRUCTURAL per paragraph block.

## 4. Findings that CHANGE the prior validation report

### Finding G (overturns Finding B/D of real-corpus-validation.md) — `style` is UNSUPPORTED at scale, not mapped

The real-corpus-validation report claimed (Finding B) that real `style` nodes
map cleanly to parser-IR `emphasis` and that a new STYLE→EMPHASIS rule would
make INVENTION-3 theoretical-only. **The probe mapper does not implement that
mapping** — it flags `style` as `UNSUPPORTED`:

> `inline_container kind 'style' has no first-class parser-IR node; only
> emphasis.text/style exist`

**Measured impact:** 28,492 `style` nodes across **5,474 files (30.6% of the
corpus)** are flagged UNSUPPORTED. This is not a theoretical guard — it is the
**single most common UNSUPPORTED** in real data. If the owned-mapping v1 policy
makes `UNSUPPORTED` a hard-fail (as `owned-mapping-design.md` §2 specifies for
warigaki's U-01), then **30.6% of real aozora-rs works would fail mapping by
default.**

**Refinement to `owned-mapping-design.md`:** The `style` node kind must be
mapped (STYLE→EMPHASIS with `style_type` preserved verbatim) rather than
treated as UNSUPPORTED. The probe mapper's "no first-class node" framing was a
*mapper limitation*, not a schema limitation — the probe even noted
parser-IR's `emphasis` exists. So:

- Add rule **I-09 STYLE→EMPHASIS** (project `style_type` into `emphasis.style`,
  preserve child content) — this was correctly identified in Finding B but
  must be a *mapping rule*, not an assertion that INVENTION-3 becomes
  theoretical.
- **Demote the existing U-01 (warigaki = UNSUPPORTED) critique's severity
  context:** warigaki fires 0 times in real data; `style`-as-UNSUPPORTED is the
  real load-bearing case. The `style` issue makes "UNSUPPORTED = refuse" a
  dangerous default policy — if applied uniformly it breaks 30.6% of works.

### Finding H — the "refuse on UNSUPPORTED" default is unsafe for real data

`owned-mapping-design.md` §2 sets `UNSUPPORTED → refuse to map` (hard exit).
At corpus scale this would refuse **5,474 of 17,894** works — because every
`style` node currently classifies as UNSUPPORTED (until I-09 is added).

Even after adding I-09, the policy question stands: should `UNSUPPORTED` be a
hard fail at all? The categories it guards (warigaki) fire 0×; the category it
*currently* catches (`style`) should be mapped. **Recommendation: demote
UNSUPPORTED from `refuse` to `drop-sidecar` + a recorded divergence entry of
critical severity, and add a separate explicit `--strict` mode that refuses**
for pipeline stages that need hard guarantees. The default should not be able
to halt 30% of corpus ingestion on a single node kind.

### Finding I — per-file ledger size is large; sidecar storage is a real concern

Per-file entry counts: median ~200 entries, long tail to **151,681 entries in
one file**. If every divergence is emitted as a JSONL sidecar (per
`owned-mapping-design.md` §1.1), the worst-case file produces a 150k-line
JSONL. Across the corpus the probe generated ~8.3M ledger entries.

Most entries are highly repetitive (~1.76M `ruby.direction` LOSS + ~3.5M
`ruby.scope` INVENTION fire on structurally identical nodes). **Refinement:**
the divergence sidecar should **aggregate by rule_id + category** (a per-rule
count) rather than emit one record per occurrence. A per-occurrence sidecar at
this scale is storage noise; a per-work summary
`{rule_id, category, count, first_path}` captures the same information in
~constant size per work. This changes §1.1 of the owned-mapping-design spec.

## 5. What the corpus probe did NOT settle

| Question | Why | Next probe needed |
|---|---|---|
| Does aozora2html emit warigaki across a full corpus? | No aozora2html full-corpus AAT run exists locally (every aat dir is aozora-rs). | Run aozora2html over corpus, re-run the aggregate. |
| Is `style` the *only* node aozora-rs classifies UNSUPPORTED? | The 28,495 UNUPPORTED entries are all `style` (sample inspection), but I did not exhaustively classify all 28,495 by aat pointer. | A 1-line counter in batch_aggregate.py: bucket UNSUPPORTED by `aat` prefix. |
| Real loss magnitude for the *designed* mapping (with I-09 added)? | The probe mapper lacks I-09, so its UNSUPPORTED count overstates real loss. | Re-run after patching map.py to map `style`→`emphasis`. |

## 6. Net refinements to owned-mapping-design.md

| § | Refinement | Evidence |
|---|---|---|
| 1.1 | Divergence sidecar aggregates per-rule (count + first-path), not per-occurrence. | Finding I: 150k-line worst case; 8.3M corpus-wide. |
| 2 (U-01) | Warigaki refusal fires 0× in real data — keep but annotate as theoretical-for-aozora-rs. | 0 warigaki in 17,894 docs. |
| 2 (UNSUPPORTED default) | **Demote `refuse` → `drop-sidecar` + critical-severity record** by default; add explicit `--strict` for refuse. | Finding H: refuse would halt 30.6% of works. |
| 2 (new rule I-09) | Add STYLE→EMPHASIS mapping (`style_type` → `emphasis.style`, preserve children). | Finding G: 28,492 nodes in 5,474 files. |
| 2 (LOSS-11/12) | Add `meta.metrics` / `meta.semantic_summary` LOSS entries (fire every file). | Confirms real-corpus-validation Finding C at scale. |
| 4 (Task 7) | Regression test must map a real aozora-rs AAT and assert **zero `style` UNSUPPORTED after I-09**, not just the synthesized sample. | The synthesized sample passed; real data exposed the `style` gap. |

---

*Probe: `prototypes/aat-to-parser-ir-probe/batch_aggregate.py`. Raw output:
`docs/handoffs/_probe-full-corpus-raw.txt`. Run 2026-07-02.*

## 7. Follow-up run: ADR 0024 + I-09 probe (2026-07-03)

Raw output: `docs/handoffs/_probe-full-corpus-adr0024-i09-raw.txt`.

Changes from the 2026-07-02 run:

- `ruby.direction` now projects directly into parser-IR `ruby.direction` per
  ADR 0024, so the previous `ruby.direction` LOSS class disappears.
- `style` inline containers now map to parser-IR `emphasis` with
  `style_type` preserved as `emphasis.style` (I-09), so the 28,492 `style`
  entries move from UNSUPPORTED to AMBIGUITY.
- `batch_aggregate.py` now folds occurrence indices in top-rule reporting and
  buckets UNSUPPORTED entries by AAT pointer, so the report measures rule
  classes instead of producing a path-by-path manual inventory.

Measured results over the same 17,894 aozora-rs AAT files:

| Metric | Value |
|---|---:|
| files scanned | 17,894 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 3 |
| files with warigaki | 0 |
| total parser-IR nodes emitted | 7,828,615 |
| total ledger entries across corpus | 4,781,187 |

| Category | Count | % |
|---|---:|---:|
| INVENTION | 3,864,599 | 80.8% |
| STRUCTURAL | 645,618 | 13.5% |
| LOSS | 159,855 | 3.3% |
| AMBIGUITY | 111,112 | 2.3% |
| UNSUPPORTED | 3 | 0.0% |

The remaining UNSUPPORTED entries are not `style` or warigaki. All three are
`meta.source_encoding=windows-31j-lossy`, folded to AAT pointer bucket
`meta.source_encoding`.

Next inference: I-09 solves the load-bearing node-mapping issue for aozora-rs.
The residual UNSUPPORTED policy is now a source-encoding/lossiness question,
not evidence for a broad hand-written AAT node mapping table.

## 8. Follow-up run: source-encoding policy + generated mapping candidate (2026-07-03)

Raw output: `docs/handoffs/_probe-full-corpus-adr0024-i09-encoding-raw.txt`.
Generated candidate:
`prototypes/aat-to-parser-ir-probe/mapping.generated.aozora-rs.json`.

Policy change from §7: `windows-31j-lossy` now projects to parser-IR
`source.encoding = "Shift_JIS"` and records an AMBIGUITY ledger entry. The
source family is known; only the lossy-decoding qualifier has no parser-IR
field.

Measured results over the same 17,894 aozora-rs AAT files:

| Metric | Value |
|---|---:|
| files scanned | 17,894 |
| files failed to parse | 0 |
| files with UNSUPPORTED | 0 |
| files with warigaki | 0 |
| total parser-IR nodes emitted | 7,828,615 |
| total ledger entries across corpus | 4,781,187 |

| Category | Count | % |
|---|---:|---:|
| INVENTION | 3,864,599 | 80.8% |
| STRUCTURAL | 645,618 | 13.5% |
| LOSS | 159,855 | 3.3% |
| AMBIGUITY | 111,115 | 2.3% |
| UNSUPPORTED | 0 | 0.0% |

The generated mapping candidate has 25 observed rule buckets, validates
against `schemas/aat-parser-ir-mapping.schema.json`, and embeds live schema
hashes:

- mapping schema hash:
  `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target parser-IR schema hash:
  `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

Next inference: for the measured aozora-rs corpus, the mapping table should be
generated from folded rule buckets, not hand-transcribed from the synthesized
sample ledger. Unobserved adapter shapes such as `warigaki` remain policy
questions for aozora2html measurement, not reasons to expand the aozora-rs
mapping candidate manually.
