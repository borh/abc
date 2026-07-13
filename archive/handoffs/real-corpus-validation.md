# Real-Corpus Validation: Owned-Mapping & Validate-Bundle Specs

> Status: evidence from real emitted AAT + real Aozora source. Probes run
> 2026-07-02. This document refines (does not overturn) the design specs at
> `docs/handoffs/owned-mapping-design.md` and
> `docs/handoffs/validate-bundle-simplification-spec.md`.

## 1. Probe: real-emitted-AAT node-kind frequency

**Data source:** `ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter/` —
17,894 real AAT documents emitted by the aozora-rs adapter over a full Aozora
corpus run. Sampled 2,000 (two xargs batches: 1,636 + 364).

### Block kinds (real data)

| Kind | Count (2,000-file sample) | In synthesized probe ledger? |
|---|---:|---|
| `paragraph` | 59,276 | yes |
| `heading` | 348 | yes |
| **all others** (`jisage_block`, `quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block`) | **0** | tested as STRUCTURAL |

### Inline kinds (real data)

| Kind | Count (2,000-file sample) | In synthesized probe ledger? |
|---|---:|---|
| `text` | 341,013 | yes |
| `ruby` | 278,929 | yes |
| `gaiji` | 5,747 | yes |
| `style` | 2,443 | **no** (probe synthesized `accent` instead) |
| **all others** (`accent`, `figure`, `caption`, `tcy`, `raw`, `warigaki`, `font_size`, `keigakomi`, `yokogumi`) | **0** | tested as LOSS/UNSUPPORTED/INVENTION |

**meta keys present in every file:** `adapter`, `adapter_version`, `metrics`,
`parse_complete`, `semantic_summary`, `source_encoding`, `source_hash`, `warnings`.

### Finding A — the synthesized 27-entry ledger over-covers kinds that don't occur

The probe at `docs/handoffs/aat-parser-ir-mapping-probe.md` synthesized an AAT
containing `accent`, `figure`, and `warigaki` to exercise every schema node kind
(per the task instruction). Real aozora-rs output uses **only 4 inline kinds**.
So 6 of the 27 ledger entries (LOSS-4 accent.name, LOSS-5/6/7 figure fields,
INVENTION-3 accent.code→emphasis.style, UNSUPPORTED-1 warigaki) describe
constructs that **do not appear** in 17,894 real documents. The designed
loss-handling policy is therefore over-engineered for the real loss profile;
it is correct as a *schema-completeness* guard but not as a description of
actual loss.

**Refinement for `owned-mapping-design.md`:** Add an "Expected real-loss
profile" subsection noting that on aozora-rs real output the lossy mapping
fires overwhelmingly on:

- `ruby.direction` (LOSS-2) — ~279k×/2,000 files
- `gaiji.unicode` (LOSS-3) and `gaiji.raw_marker` (INVENTION-2) — ~5.7k×
- `heading.style` (LOSS-1) and `heading.level` domain mismatch (AMBIGUITY-1) — ~348×
- `meta.adapter` / `adapter_version` / `parse_complete` (LOSS-8/9/10) — **every file**
- `meta.metrics` and `meta.semantic_summary` — **every file** (currently unlisted
  in the 27-entry ledger; see Finding C)

The `accent` / `figure` / `warigaki` entries are theoretical-completeness guards,
not real loss.

### Finding B — real `style` node maps differently than the synthesized `accent`

The probe's INVENTION-3 entry ("map accent.code → emphasis.style") was
synthesized because no real fixture emitted an `accent`. Real aozora-rs data
emits **`style`** nodes directly (2,443 occurrences in the sample), e.g.:

```json
{"content":[{"kind":"text","value":"とも","x-provenance":"parser_normalized"}],
 "kind":"style","style_type":"boten"}
```

`style_type: "boten"` (傍点 = emphasis dots) maps to parser-IR `emphasis` far
more cleanly than the synthesized `accent→emphasis` path. The mapping spec
needs a **new STYLE→EMPHASIS rule** (not the synthesized ACCENT rule):

| rule_id | AAT field | parser-IR target | action | note |
|---|---|---|---|---|
| (new) I-09 | `blocks[*].content[*].style.style_type` | `nodes[*].type='emphasis'.style` | project | `boten`/`sessji` → parser-IR emphasis style; preserve verbatim where parser-IR permits. |

INVENTION-3 should be reclassified as **theoretical-only** (no real data exercises it).

### Finding C — `meta.metrics` and `meta.semantic_summary` are unlisted LOSS

The 27-entry probe ledger lists `meta.adapter`, `adapter_version`,
`parse_complete` as LOSS-8/9/10 but omits two meta keys present in **every**
real AAT: `metrics` and `semantic_summary`. Both are adapter-faithfulness /
analytics metadata with no parser-IR home.

**Refinement:** add two LOSS entries (L-11 `meta.metrics`, L-12
`meta.semantic_summary`) to the mapping document and the loss-handling table;
both take default action `drop-sidecar` (publication identity does not need
adapter analytics, but the drop must be recorded).

## 2. Probe: warigaki/warichu in real data

**Two independent checks, both zero:**

1. **Real emitted AAT:** `grep -rl '"warigaki"' scratch/morph-full-corpus/aats/`
   → **0** across all 17,894 aozora-rs AAT docs.
2. **Real Aozora source** (`/home/bor/Dependencies/aozorabunko/cards/`): grepped
   `割書`, `割注`, `＃割書`, `＃割注` → **0** in txt, **0** in the 35,794 html
   files.

**But:** the aozora2html adapter **supports** warigaki (fixture
`adapters/aozora2html/tests/fixtures/warichu_basic.aat.json` emits a `warigaki`
node with upper/lower content from `割注` markup). So warigaki is a real parser
capability that does not occur in this corpus snapshot, or does not occur via
aozora-rs (the only full-corpus run available locally).

### Finding D — "refuse warigaki" (U-01) is a safe-but-theoretical guard

The `owned-mapping-design.md` spec makes warigaki the sole `refuse-to-map`
hard-fail (exit non-zero). Real-corpus evidence: this refusal would affect
**0 of 17,894** aozora-rs-emitted works. The policy is therefore low-risk for
the v0 aozora-rs path.

**Open probe (cannot resolve here):** no aozora2html full-corpus AAT run exists
in `scratch/` (every aat directory is `aozora-rs-adapter`). Since aozora2html
is the adapter that *does* emit warigaki, a full aozora2html corpus run is
needed to know whether warigaki refusal would break real aozora2html output.
Until then, keep U-01 as `refuse` but annotate it:

> **U-01 annotation:** Refusal affects 0% of the observed aozora-rs full-corpus
> output (17,894 docs). Aozora2html supports warigaki but no full-corpus AAT run
> exists to measure its real frequency. Re-evaluate U-01 after an aozora2html
> full-corpus run; if warigaki occurs above a threshold (propose 0.1%), demote
> to `drop-sidecar` + record, not `refuse`.

## 3. Probe: validate-bundle derivation mechanism feasibility

The `validate-bundle-simplification-spec.md` proposes deriving expected rule-id
sets from authoritative sources instead of hard-coding them. The verification
report confirmed the premises (rule-ids exist in both inline and source) but did
NOT test the derivation mechanism itself.

### Finding E — derivation is mechanically feasible (corrected)

My first regex probe returned 0 (looked for `ident=` and `sch:rule id=` — wrong
attributes). The correct extraction:

```bash
grep -oE 'abc-[a-z-]+' schemas/tei-profile.sch | sort -u      # 13 rule-ids
grep -oE 'ident="(abc-[a-z-]+)"' schemas/tei-profile.odd | sort -u   # 13 constraintSpecs
```

Both sources expose exactly the 13 `abc-*` rule-ids via simple grep, because:

- `.sch` (generated by `extract-isosch.xsl` from `.odd`) puts rule-ids in
  `<pattern id="abc-...">` attributes.
- `.odd` puts them in `<constraintSpec ident="abc-...">`.

**So the spec's central mechanism is feasible** — no XML parser needed; a
grep + set-comparison (inline ∪ source, inline ∩ fixtures, source \ fixtures)
suffices. This upgrades the spec's confidence from "premises verified" to
"mechanism verified."

**Caveat the spec should add:** the `.sch` is a *generated* artifact (comment:
`This file generated by 'extract-isosch.xsl'`). Deriving from `.odd`
( the source) is more robust than deriving from `.sch` (the derived), because
the ODD→SCH generation is itself a build step that could drift. Recommend the
implementation derive from `.odd` `constraintSpec ident` as the authoritative
source, and treat `.sch` as a cross-check, not the primary.

### Finding F — the inline expected-set count was undercounted by the spec

`grep -oE 'abc-[a-z-]+' src/abc/tools/validate_design_bundle.clj | sort -u`
returns **17**, not 13 — because the file also contains `abc-changelog-check`,
`abc-linked-art-bundle`, `abc-materialized-import`, and a regex artifact
`abc-v` (from `abc:vocabulary` or similar, not a rule). The genuine Schematron
subset is 13 (matching the spec), but anyone implementing the derivation must
filter to the Schematron-rule subset specifically (e.g., derive the universe
from `.odd`, then intersect with the file), not grep the whole validation file.

## 4. Net refinements to the specs

| Spec | Refinement | Effort |
|---|---|---|
| `owned-mapping-design.md` | Add "Expected real-loss profile" subsection (Finding A): real losses concentrate on ruby/gaiji/meta, not accent/figure/warigaki. | editorial |
| `owned-mapping-design.md` | Add STYLE→EMPHASIS rule (Finding B); reclassify INVENTION-3 as theoretical-only. | editorial |
| `owned-mapping-design.md` | Add LOSS-11/12 for `meta.metrics`/`meta.semantic_summary` (Finding C). | editorial |
| `owned-mapping-design.md` | Annotate U-01 warigaki refusal as theoretical-for-aozora-rs; require aozora2html full-corpus run before finalizing (Finding D). | editorial + deferred probe |
| `validate-bundle-simplification-spec.md` | Note derivation source = `.odd` constraintSpec idents (primary) over `.sch` (derived cross-check) (Finding E). | editorial |
| `validate-bundle-simplification-spec.md` | Warn implementer that grep returns 17 not 13; must filter to Schematron subset (Finding F). | editorial |

## 5. Remaining probes (not run here, with reasons)

| Probe | Why not run | What it would settle |
|---|---|---|
| aozora2html full-corpus AAT run → warigaki frequency | Requires running the aozora2html adapter over the full corpus (heavy; not a 5-minute probe). | Whether U-01 `refuse` would break real aozora2html output. |
| Run probe mapper (`prototypes/aat-to-parser-ir-probe/map.py`) over the full 17,894 real AATs | The probe mapper exists; feasible in minutes. Would produce a *real* divergence frequency distribution rather than the synthesized 27. | Confirms/refutes the "real loss concentrates on ruby/gaiji/meta" claim with exact counts. |
| Nix eval time/memory on 1k-work slice (bounded-workset) | Requires Nix + corpus env (heavy). | ADR 0003/0005 direction. |
| ab-ir git-churn probe (how often ab-ir APIs aozora-rs uses have changed) | Feasible (git log). Low effort. | Whether the adapter↔ab-ir coupling is load-bearing or theoretical. |

**Recommendation:** the highest-leverage *cheap* probe still unrun is **running
the existing probe mapper over the full 17,894 real AATs** — it converts the
"A/real-loss-profile" claim from inference to measurement. I did not run it
here to avoid over-running, but it is a minutes-scale bash invocation and should
be the next validation step before the owned-mapping design is committed to an
ADR.

---

*Probes run 2026-07-02. Real corpus: `ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter/` (17,894 docs). Real source: `/home/bor/Dependencies/aozorabunko/`.*
