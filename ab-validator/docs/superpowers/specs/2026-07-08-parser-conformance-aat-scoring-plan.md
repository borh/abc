# Plan: score AAT-mode parsers on the conformance suite → cross-parser comparison

**Status:** handoff for a fresh session. Self-contained. Read this top to bottom
before touching code.

## Goal

Turn the Aozora notation conformance run from **0 pass / 759 warning / 3 fail**
into a real **per-vector × per-parser conformance matrix**, by making the
AAT-mode adapters actually *scored* against the spec's canonical vectors instead
of skipped. The output is a systematic, apples-to-apples comparison of every
Aozora parser against a common spec.

### Why this matters (the research framing — keep it in view)

- This comparison **is** a research deliverable in its own right: a spec-anchored,
  per-construct comparison of every Aozora parser (aozora, aozora2, aozora2html,
  aozora-rs, aozora-epub3).
- It **motivates a new/forked parser empirically**: if no existing parser passes
  all `must`-level vectors (the reference is already 3 fails; the AAT adapters are
  entirely unscored today), that measured gap is the justification to build one —
  not an assertion.
- It gives the eventual parser its **acceptance criterion for free**: "green
  across the 127 vectors."
- It **sidesteps the wall the corpus-fidelity path hit** (see
  `docs/superpowers/reports/2026-07-08-corpus-adapter-fidelity.md`): the vectors
  are *canonical* controlled fixtures with known-correct expected output, so
  scoring is clean per-vector pass/fail — no counting/granularity problems.

Note on authority: the conformance suite is the third-party **P4suta**
`upstream-aozora-notation-spec`. Per a standing project decision it is
**corroborating evidence, not authoritative** (the official 青空文庫 docs + the
works themselves are the authority — see memory `aozora-syntax-map-initiative`).
That is fine here: as a *fixed, neutral reference* it is a valid comparison
instrument. Do not present "passes P4suta" as "correct"; present it as
"conforms to the P4suta reference."

## Current state & root cause

- Harness: `ab-validator/reports/parser-conformance/run-aozora-notation-spec.py`.
  Recipe: `just aozora-notation-spec-comparison` (`justfile:89-102`).
- Last committed run:
  `docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json`
  — 127 vectors × 6 adapters = 762 rows; **0 pass / 759 warning / 3 fail**.
- **Root cause of the 759 warnings** — `run-aozora-notation-spec.py:85-89`:
  ```python
  if adapter.mode == "aat":
      warnings.append(f"{projection} comparison skipped: "
                      f"AAT adapter does not expose aozora inspect {projection}")
      return
  ```
  The harness only knows how to read the reference `aozora` adapter's `inspect`
  mode (node/pairs/diagnostics projections). AAT-mode adapters emit AAT JSON, so
  every projection comparison is skipped → warning. Only the reference `aozora`
  (inspect mode) is really scored; its 3 fails are the entire signal today.

## Vector anatomy (verified)

Pinned spec: `nix build .#upstream-aozora-notation-spec` →
`<store>/conformance/vectors/<name>/vector.json`. 127 vectors.

Each `vector.json` top-level keys: `name`, `meta`, `source`, `expected`.
- `meta.feature` (24 families: accent, angle_quote, annotation, bouten, break,
  composite, container, gaiji, heading, horizontal, keigakomi, kunten, layout,
  plain, recovery, ruby, sashie, structural-marker, tables_columns, tate_chu_yoko,
  tcy, warichu, kaeriten, …), `meta.level` ∈ {must, should, may},
  `meta.spec_section`, `meta.note` (provenance).
- `source` — the Aozora source text (e.g. `"青空の下を歩く［＃「青空」に傍点］\n"`).
  (The harness reads `vector["source"]`; ignore the sometimes-present null `input`.)
- `expected` projections: `nodes`, `pairs`, `diagnostics`, `serialize`, `html`.
  - **`nodes` is the key comparable** and present on **122/127** vectors. Format:
    `[{"kind":"bouten","span":{"start":0,"end":6}}, …]` — an ordered sequence of
    `{kind, span}`. `span` uses the spec's §3 **byte** model. NB: AAT adapter output
    carries **no spans** (Spike finding 1), so only the ordered `kind` sequence is
    comparable — drop `span` from both sides. (AAT schema spans, when present, use
    `byte_start/byte_end` + `line_start/line_end`, not `line/char`.)
  - The 5 without `nodes` (e.g. `accent_decomposition_applied`) carry only
    diagnostics/html/pairs/serialize — handle as a documented skip, not a fail.

## Spike findings (2026-07-08, verified empirically) — read before coding

Ran the built native AAT adapters (`aozora`, `aozora2`, `aozora-rs`) on
representative vectors from the pinned spec store copy. Results **overturn two of
the plan's original assumptions**; the corrected Approach A below reflects them.

1. **AAT output carries NO spans.** Real adapter output is e.g.
   `{"kind":"style","style_type":"boten","content":[…]}` — there is no `span`
   field at all. The AAT *schema* defines spans, but production adapters omit them
   (confirmed: `docs/aat-contract.md:210-212`, `docs/aat-span-audit.md`). ⇒ **Span
   reconciliation is impossible.** The comparison is a **kind-sequence** diff only
   (strip spans from `expected.nodes` too). What was the plan's "fallback tier" is
   the *only* tier. (Silver lining: it is far less brittle than byte-span matching.)
2. **The AAT→spec map is PER-ADAPTER, because adapters represent the same
   construct differently.** For the same container source, `aozora2` emits flat
   `{"kind":"raw","source":"BlockStart(Chitsuki)"}` / `BlockEnd(...)` marker nodes,
   while `aozora-rs` emits typed nodes (or drops the construct). For a directive,
   `aozora2` emits `{"kind":"raw","source":"<directive text>"}`; `aozora-rs` drops
   it. So the projection must walk the tree and handle **both** typed AAT nodes and
   `raw` marker nodes, with per-adapter branches. There is no single universal map.
3. **Spec `emphasis` is a GENERIC decoration wrapper.** bold, italic, box-enclosure,
   accent-dot, and font-size referents **all** project to spec `emphasis` (41 in the
   suite); only 傍点 → `bouten` (13). AAT distinguishes these
   (`style_type=bold`/`italic`, `font_size`, …) and the projection must collapse
   them to `emphasis`. `style_type=boten` → `bouten` is the one clean 1:1. Note the
   spec `nodes` vocabulary has **no `bousen` kind** — decide where AAT `bousen` maps
   (likely `emphasis`) and document it.
4. **Adapters visibly DROP constructs** (aozora-rs emits bare text for box/accent;
   both drop some containers). Post-projection these become genuine sequence
   divergences — real conformance signal, exactly what we want.
5. **Distinct spec `kind`s across the 122 nodes vectors** (count): `emphasis` 41,
   `containerOpen` 25, `containerClose` 24, `directive` 17, `bouten` 13, `ruby` 9,
   `gaiji` 9, `headingHint` 4, `pageBreak` 3, `illustration` 3, `kaeriten` 2,
   `heading` 2, `lineFontSize` 2, `combineUpright` 2, `marginNote` 2, and singletons
   (`angleQuote`, `bodyEnd`, `alignEnd`, `center`, `forcedBreak`, `indent`,
   `lineBold`, `sectionBreak`). Map the high-frequency structural kinds first.
6. **`ab-aozora` (adapters/aozora, `--mode aat`) shells out to the upstream
   `aozora` binary** and needs `AB_AOZORA_BIN` set (the recipe provides it; a bare
   invocation errors `spawn aozora … No such file or directory`). Not a bug.
7. **Level distribution:** must 25 / should 99 / may 3. The harness already
   downgrades non-`must` failures to warnings (`evaluate()`, lines 110-115) — keep
   that severity logic untouched: must sequence-mismatch → `fail`, should/may → `warning`.

## The engineering — make AAT adapters scorable

**Approach A (corrected) — AAT → spec-`kind` *sequence* projection.** For an
AAT-mode adapter, run it (`--mode aat`) on `vector.source`, walk its `blocks` tree
in document order, map each node to its spec-`kind` (or nothing), and produce an
ordered `[kind, …]` list. Compare that to the `kind` sequence of `expected.nodes`
(spans dropped from both sides). Reuses the existing 122 `expected.nodes` with no
new fixture authoring. Two sub-problems:

1. **Vocabulary map: AAT node → spec `kind`, per adapter as needed.** The existing
   `reports/aat-fidelity/corpus-adapter-fidelity-classifier.py` maps AAT nodes to
   internal *construct IDs* (`decoration.boten`), **not** spec `kind`s — useful as a
   signature reference but it is NOT the map; the AAT→spec-`kind` map is new work.
   Build it by enumerating the distinct spec `kind`s (finding 5) and pairing each to
   its AAT signature(s), handling typed nodes AND `raw` markers (finding 2), and the
   `emphasis` collapse (finding 3). Where an AAT node has no spec counterpart (or
   vice-versa), record it as a finding, don't crash.
2. **Ordering & tree flatten.** AAT `blocks` are nested (`paragraph` → `content` →
   nested `style`/`ruby`/…). Flatten depth-first in source order to a flat kind
   sequence. `text` nodes → nothing. Decide container handling: spec emits paired
   `containerOpen`/`containerClose`; emit those from `raw BlockStart/BlockEnd` (or
   typed block open/close) at the right positions.

Implement A inside (a copy/extension of) `run-aozora-notation-spec.py`: replace the
`adapter.mode == "aat"` skip with an `aat_nodes_projection(adapter, source)` that
produces the comparable kind sequence, then diff against the spans-stripped
`expected.nodes`. Keep the reference `aozora` inspect path unchanged so both are
scored side by side. **Bonus oracle:** `ab-aozora` (the reference parser in AAT
mode) should reproduce `aozora`-inspect's `nodes` sequence once the projection is
right — use it as a self-check that the projection is faithful.

**Alternatives (record why not chosen if you deviate):**
- **B — author `expected.aat` per vector.** Cleanest long-term but requires
  hand-authoring canonical AAT for ~127 vectors. The 46 `data/aat-oracle-cases.toml`
  cases already cover a subset and can seed it. Large.
- **C — extend `ab-oracle` to cover the vectors.** `ab-oracle` **already** scores
  AAT adapters cleanly against canonical AAT (this session produced
  `docs/superpowers/reports/2026-07-08-adapter-preservation-live-oracle-report.json`
  via it — see its Method block). Reuse-heavy for the AAT-scoring engine, but still
  needs per-vector expected AAT (overlaps B). Good if A's span reconciliation turns
  out nasty.

Pick A first; fall back to C+B if the `nodes` projection can't be made faithful.

## Deliverable

- Updated harness that scores AAT adapters, and a regenerated
  `docs/superpowers/reports/<date>-aozora-notation-spec-comparison.summary.json`
  + Markdown matrix with **real pass/should-fail/must-fail per (vector, adapter)**.
- A short findings report (`docs/superpowers/reports/<date>-parser-conformance-comparison.md`)
  that reads the matrix: per feature family, which parsers conform / diverge;
  the `must`-level failures per parser; and the headline — whether *any* parser
  passes all `must` vectors (the parser-motivation result).

## Acceptance criteria

1. `just aozora-notation-spec-comparison` produces a summary where AAT adapters
   have real `pass`/`fail` rows on the ≥122 `nodes`-bearing vectors (not blanket
   `warning`). The 5 no-`nodes` vectors and any genuinely-unsupported projection
   remain explicit, *logged* skips.
2. The vocabulary map and span-tolerance decisions are written down (in the
   harness or the findings report), not implicit.
3. The findings report states, with numbers, the per-parser `must`-conformance
   and whether a new parser is warranted.

## Task breakdown (ordered)

1. ~~**Spike**~~ **DONE 2026-07-08** — see "Spike findings" above. Decision: **A
   (kind-sequence, spans dropped)**, not C. Distinct kinds dumped; adapters run;
   per-adapter `raw`-vs-typed representation and the `emphasis` collapse identified.
2. Build `aat_nodes_projection()` + the vocabulary map (typed nodes + `raw`
   markers + `emphasis` collapse); wire it into the harness replacing the
   `mode==aat` skip. Compare kind sequences (strip spans from `expected.nodes`).
3. Run full 127 × 6; iterate the map until unmapped-kind noise is only genuine
   divergences. Cross-check `ab-aozora`(aat) against `aozora`(inspect).
4. Regenerate summary + write the findings report.
5. Commit under `docs/superpowers/reports/`; keep the harness change minimal and
   reviewed.

## Gotchas / environment (learned this session)

- **Build the adapters first.** Native (fast, self-contained): `cargo build
  --release --manifest-path adapters/aozora2/Cargo.toml` and `.../aozora-rs/…`.
  Reference/wrappers need runtimes: `aozora` needs
  `AB_AOZORA_BIN=$(nix build --no-link --print-out-paths .#upstream-parser-aozora)/bin/aozora`;
  `aozora-epub3` needs Java + `AB_AOZORAEPUB3_JAR` **and is stdin-incompatible**
  (requires `--source <file>`, so it won't drop into a stdin harness — the
  existing spec harness already handles adapter invocation, follow its pattern);
  `aozora2html` needs the Ruby gem (`--aozora2html-bin` + gem home). The
  `aozora-notation-spec-comparison` recipe already wires all of these — start from it.
- **`python3` on this box has no `duckdb` module** (bare interpreter). Use the
  `duckdb` CLI or a nix/uv python if you touch DuckDB. The conformance harness
  itself is plain python + json, so this likely won't bite.
- The reference `aozora` adapter is the only one with an `inspect` mode; keep it
  as the baseline column.
- P4suta corroborating-not-authoritative (see Authority note above).

## Prior context to read

- Memory: `aozora-syntax-map-initiative` (the whole initiative arc, decisions,
  and the corpus-pass obstacle) and `aat-adapter-evaluation`.
- Committed reports (all `docs/superpowers/reports/`):
  `2026-07-08-adapter-preservation-live-verification.md` (matrix is stale, live
  oracle), `2026-07-08-corpus-adapter-fidelity.md` (why corpus per-construct
  regeneration is blocked — the granularity wall this plan avoids).
- `ab-oracle` machinery: `crates/ab-oracle/`, `reports/aat-fidelity/run-cross-adapter-report.sh`
  (the working AAT-scoring engine, for Approach C).
- AAT schema/contract: `data/aat-schema.json`, `docs/aat-contract.md`.
- Coverage matrix (the map): `data/aozora-syntax-coverage.toml` (its per-adapter
  `aat_fidelity` cells are known-stale; do not trust them — this conformance run
  is a cleaner signal).
