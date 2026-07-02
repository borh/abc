# AAT → Parser-IR Mapping Probe — Disposable

> **DISPOSABLE PROBE** (per hammock-driven-design + prototype skills). This is a
> throwaway experiment that settles one crux: are the AAT/parser-IR divergences
> *survivable in a concrete mapping*, or do they reveal *genuinely distinct
> concerns*? Nothing here is production. No schema, fixture, or production
> source was modified.
>
> Probe artifacts: `prototypes/aat-to-parser-ir-probe/`
> (`aat-sample.json`, `map.py`, `parser-ir.probe.json`, `ledger.md`).

## Inputs

- **AAT document**: `prototypes/aat-to-parser-ir-probe/aat-sample.json` — a
  synthesized minimal AAT v1 grounded in **real emitted fixtures** from
  `ab-validator/adapters/aozora2html/tests/fixtures/` (`ruby_basic`,
  `gaiji_marker`, `figure_image_caption`, `warichu_basic`). The `heading` block
  and `accent` inline are synthesized from `aat-schema.json` +
  `aat-contract.md` because **no real adapter fixture emits those two shapes
  yet** (grep for `"kind": "accent"` and `"kind": "heading"` returns no adapter
  fixture). The doc covers all node kinds requested: paragraph, text, ruby,
  gaiji, heading, figure, accent, warigaki — with spans and `x-provenance`.
- **Mapper**: `prototypes/aat-to-parser-ir-probe/map.py` (Python; chosen for
  speed). Honest best-effort: preserves what survives and emits a divergence
  ledger entry for every node/field the target cannot represent without loss,
  ambiguity, or invention.

## Structural transform rule used (flat `nodes[]` vs nested `blocks`)

Flatten `blocks[].content[]` (and `blocks[].children[]` for block containers)
into a single parser-IR `nodes[]` in document order. **Block containers**
(`paragraph`, `heading`, `block_container`) are **not** emitted as parser-IR
nodes — parser-IR has no paragraph/block concept — so the block boundary and
its span are dropped; only their inline children survive. `heading` is special:
parser-IR's heading node takes a single `text` string, so the inline
`content[]` is text-projected and any non-text inline (ruby/gaiji inside a
heading) is flattened to its text projection. Spans: AAT uses decoded-UTF8
`byte_start/byte_end` + 1-based `line_start`; parser-IR `start/end` semantics
are schema-unspecified, so `start=byte_start`, `end=byte_end`, `line=line_start`,
`column=null` (unknown).

## Run + verification

```sh
cd prototypes/aat-to-parser-ir-probe
python3 map.py aat-sample.json          # prints parser-IR + ledger; writes parser-ir.probe.json + ledger.md
```

Sample output (first lines):
```
=== GENERATED parser-ir.json (probe) ===
{
  "schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "schema_hash": "sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f",
  ...
  "nodes": [
    { "type": "heading", "span": {...}, "text": "第一章", "level": 1 },
    { "type": "text",    "span": {...}, "text": "吾輩は" },
    { "type": "ruby",    "span": {...}, "ruby": { "base": "猫", "reading": "ねこ", "scope": "explicit" } },
    ...
```

The generated `parser-ir.probe.json` was validated against the real
`schemas/parser-ir.schema.json` with `jsonschema` and **conforms** — i.e. the
mapping produces a schema-valid parser-IR document. That validity is exactly
the point: *mappability is not the question; information loss is*.

## Divergence ledger

Total entries: **27**. Per category:

| Category | Count |
| --- | --- |
| LOSS | 10 |
| AMBIGUITY | 4 |
| INVENTION | 8 |
| UNSUPPORTED | 1 |
| STRUCTURAL | 4 |

The full table is regenerated at `prototypes/aat-to-parser-ir-probe/ledger.md`.
Reproduced here:

| AAT field/node | parser-IR target | category | note |
| --- | --- | --- | --- |
| blocks[0][block=heading] | (none) | STRUCTURAL | block container of kind 'heading' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[0].heading.level | heading.level | AMBIGUITY | AAT heading.level range 1-3 vs parser-IR 1-6; values fit but domain differs |
| blocks[0].heading.style | (none) | LOSS | heading.style='main' dropped |
| blocks[1][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[1].content[1].ruby.scope | ruby.scope | INVENTION | AAT has no scope field; defaulted to 'explicit' |
| blocks[1].content[1].ruby.direction | (none) | LOSS | direction=right dropped; parser-IR ruby has no direction |
| blocks[1].content[3].gaiji.raw_marker | gaiji.raw_marker | INVENTION | AAT has no raw source marker; used description as raw_marker |
| blocks[1].content[3].gaiji.resolved | gaiji.resolved | AMBIGUITY | AAT resolved is string (the chosen char); parser-IR resolved is boolean (was it resolved?) |
| blocks[1].content[3].gaiji.unicode | gaiji.unicode | LOSS | AAT does not separate unicode codepoint from resolved string |
| blocks[1].content[4].accent | emphasis | AMBIGUITY | accent mapped to emphasis; accent code/name semantics not preserved |
| blocks[1].content[4].accent.code | emphasis.style | INVENTION | used accent.code='CU' as free-form style string |
| blocks[1].content[4].accent.name | (none) | LOSS | accent.name='circumflex' has no parser-IR field |
| blocks[2][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[2].content[0].figure.filename | image.src | INVENTION | filename is not a resolved source path; used as src |
| blocks[2].content[0].figure.css_class | (none) | LOSS | css_class=source-note dropped; parser-IR image has no such field |
| blocks[2].content[0].figure.width | (none) | LOSS | width=400 dropped; parser-IR image has no such field |
| blocks[2].content[0].figure.height | (none) | LOSS | height=300 dropped; parser-IR image has no such field |
| blocks[3][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[3].content[1].warigaki | (none) | UNSUPPORTED | parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost |
| meta.source_hash | source.work_content_hash | AMBIGUITY | AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ |
| (none) | source.normalization | INVENTION | parser-IR requires normalization enum; AAT has none -> defaulted 'source' |
| (none) | source.source_path | INVENTION | parser-IR source_path optional; AAT has none -> null |
| meta.adapter | (none) | LOSS | adapter='aozora2html' dropped; parser-IR carries no producer identity / parse status |
| meta.adapter_version | (none) | LOSS | adapter_version dropped; parser-IR carries no producer identity / parse status |
| meta.parse_complete | (none) | LOSS | parse_complete=True dropped; parser-IR carries no producer identity / parse status |
| (top-level) | schema_id/schema_hash | INVENTION | parser-IR requires schema_id+schema_hash; AAT supplies only version=1; producer must hardcode ABC's identifier |
| (none) | errors[] | INVENTION | parser-IR requires errors[]; AAT has no errors concept -> defaulted empty |

## Verdict

**(c) — the divergences reveal GENUINELY DISTINCT concerns (adapter faithfulness
vs publication identity).**

The mapping is *mechanically possible* (I produced a schema-valid parser-IR
from a real-shaped AAT in 27 disciplines), but it is **not collapsible**:
collapsing into one schema forces either ab-validator to abandon its
adapter-fidelity vocabulary or ABC to absorb provenance/metrics it has no use
for, and the coordinate systems (decoded-UTF8 byte offsets vs.
schema-unspecified start/end) are genuinely incompatible. The three most
decisive entries:

1. **`warigaki` → (none), UNSUPPORTED** — AAT has a split-line note with
   `upper[]`/`lower[]` inline arrays; parser-IR has no warigaki node at all.
   The information can only be flattened to text, permanently losing the
   structure. This is a real AAT-domain construct with no publication-IR home.
2. **`meta.adapter` / `adapter_version` / `parse_complete` → (none), LOSS (×3)**
   — the entire AAT *producer identity and parse-status* story has no
   parser-IR field. parser-IR is input/content-centric (`work_content_hash`,
   `encoding`, `normalization`); AAT is adapter/run-centric. Dropping these is
   not an accident of an unfinished mapper — it is the defining difference
   between "did the adapter faithfully preserve its upstream parser?"
   (AAT/result-axis) and "what is the canonical text of this work?"
   (parser-IR/identity-axis). See the contract's independent Result Axes
   (`aat-contract.md`).
3. **`(top-level)` → `schema_id/schema_hash`, INVENTION** — parser-IR *requires*
   ABC's schema identity, but AAT carries only `version=1`. The producer must
   hardcode ABC's `schema_hash`. This is the seam where the two concerns
   literally cannot be unified: the boundary is owned by ABC's content-addressed
   schema identity, which AAT's integer-versioned adapter contract cannot
   supply. Combined with the structural **LOSS of paragraph block boundaries
   (STRUCTURAL ×4)**, parsing reassembles publication text while discarding
   adapter structure — exactly the division of labor the boundary doc
   hand-waves.

So: not (a) trivially mappable, and only weakly (b) "Option C with an owned
mapping." Option C (distinct but reconcilable) is viable **only if the mapping
is made a first-class, schema-addressable artifact with a named owner** — the
mapping itself is lossy enough that "just emit parser-IR" silently hides real
information unless the loss is audited per-version.

## Ownership of the mapping

**Owned by ab-validator (the producer), with the *target schema* owned by ABC.**

One-line reason: only the producer knows which AAT fields are *faithful*
(`x-provenance = "source-derived"` vs parser-emitted) versus synthesized, so
only it can make the lossy-drop decisions responsibly; ABC owns the *shape* of
what it accepts, but ab-validator owns the *translation* and must record which
AAT version it mapped from and what it dropped.

**Schema-addressability:** the mapping deserves its **own** identity — a
`mapping_schema_hash` / `mapping_version` (e.g. `aat-to-parser-ir-mapping v1`)
declared in the emitted parser-IR (alongside `derived_from_aat_version` and
`derived_from_aat_adapter`), independent of both AAT's `version=1` and the
parser-IR `schema_hash`. Today the probe *hardcodes* ABC's
`parser_ir_schema_hash` — that is exactly the missing schema-addressable seam:
the lossy transform rules (this whole ledger) change on their own cadence
(ruby `direction` policy, gaiji `raw_marker` convention, warigaki flattening
rule), so they warrant their own versioned contract, registered in the
compatibility registry the prior handoff already called for. Without it, a
silent change to "what we drop" is undetectable by ABC's hash check.

---

*Probe status: COMPLETE. Disposables live under
`prototypes/aat-to-parser-ir-probe/` and may be deleted once the verdict is
captured into an ADR; the durable outputs are this report's verdict + the
ownership/mapping-version recommendation.*
