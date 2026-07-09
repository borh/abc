# Hydrated Examples for Interestingness Reports

**Date:** 2026-07-10
**Status:** Proposed
**Scope:** A `hydrate-interesting` CLI subcommand that turns the span references in a `summarize-warehouse-interesting` artifact into a self-contained, human-readable example bundle: projected-text snippets, per-analyzer analysis tables, verbatim Aozora markup slices, AAT node context, and work metadata (title, author name, year).

## Goal

The interestingness ranking (see `2026-07-05-interestingness-ranking-design.md`) emits `region_examples` as bare identifiers — `(source_id, text_id, region_index, char_start, char_end)` — because the warehouse deliberately stores no source text. A reader of `interesting-dictcmp-m2-full.txt` sees that pattern `pos1 surface:色 名詞 vs 接尾辞` occurred, but not the sentence it occurred in, which work it came from, or who wrote it. Resolving one example by hand requires joining four data sources on two machines.

The goal: one command, run where the warehouse lives, that hydrates every ranked finding into exact, quotable examples with full work attribution — as a static bundle that stays useful after the run directory is deleted or the bundle is copied to another machine.

## Non-goals

- **No TEI emission.** No TEI corpus exists yet (the `aozora-tei-eaj-sanitized-*` corpus directory is AAT for the 50-work alignment workset, not TEI XML). When the ADR 0028 annotation-view family lands, the `aat_nodes` layer of this bundle is the natural join point; nothing here blocks on it.
- **No re-ranking.** The command is a pure post-processor of an existing ranking artifact; scores, order, and pattern identity pass through untouched.
- **No marimo explorer integration.** The explorer can grow an equivalent view later; this design ships the static bundle only.
- **No warehouse schema change.** Strictly a reader of existing tables.

## Inputs and CLI

```
ab-morph-run hydrate-interesting \
  --interesting <interesting-*.json>        # ranking artifact (JSON format, not TSV)
  --run-dir <warehouse run dir>             # sources/morphemes/morpheme_features/
                                            # nway_region_analyzers/projection_spans/aozora_works
  --output-dir <dir>                        # writes examples.md + examples.json
  [--abc-catalog <dir>]                     # ABC export with works/ + persons/ (author names etc.)
  [--corpus-index <index.json>]             # default: derived from sources.aat_path layout
                                            # (<corpus>/aat/<adapter>/x.json → <corpus>/index.json)
  [--aozora-root <dir>]                     # aozorabunko checkout for original .txt slices
  [--context-chars <n>]                     # default 40 (each side)
  [--limit <n>]                             # default: all rows present in the input JSON
  [--force]                                 # overwrite output dir contents
```

Every optional input degrades a layer rather than failing the build (see Error handling).

## Architecture

```
interesting-*.json ──┐            per source (examples grouped, AAT loaded once):
run dir (parquet)  ──┤   1. re-project AAT → plaintext (+ spans)
AAT corpus         ──┼─► 2. snippet window around [char_start, char_end)
ABC catalog        ──┤   3. per-analyzer table (segmentation + features)
aozora originals   ──┘   4. original-markup slice (byte spans → verbatim source)
                         5. metadata join (aozora_works ⋈ ABC works/persons)
        ▼
examples.md + examples.json   (self-contained, provenance header)
```

Hydration cost is bounded by the ranking artifact, not the corpus: default limits give ≤ 50 patterns × ≤ 5 examples plus the anomaly channel — a few hundred examples over at most a couple hundred sources. Parquet reads are filtered to that source-id set.

### Layer 1 — projected-text snippet

Re-derive the projected plaintext with `visible_text_projection_with_spans` (`ab-plaintext`, `aat.rs`) on the AAT JSON at `sources.aat_path`. This is the same function the analysis pipeline used (`from_aat_value_with_spans` in `pipeline.rs`), so char offsets agree by construction.

**Offset safety invariant:** after re-projection, `projected chars == sources.source_chars` for that source. On mismatch (AAT file changed since the run), all snippets for that source are replaced by a `projection-mismatch` error record — never a silently wrong quote.

Snippet = `[char_start − context, char_end + context)` clipped to document bounds, region marked with `【】` (CommonMark emphasis is unreliable inside CJK runs). The JSON form keeps `before` / `region` / `after` separate so consumers can re-mark.

### Layer 2 — per-analyzer analysis table

For `(source_id, region_index)`: segmentation per analyzer from `nway_region_analyzers.parquet`; token features (pos1–pos4 and whatever the profile recorded) from `morphemes.parquet` + `morpheme_features.parquet` by char-overlap with the region. Analyzers with identical analyses are grouped into one row, mirroring how pattern strings group them. Analyzer ids are shortened for display (`vibrato:unidic-csj-202512` → `csj`) with a legend once at the top of the bundle; JSON always carries full ids.

### Layer 3 — original Aozora markup slice

The AAT (aozora-adapter raw form) tiles the original source file with byte spans, but parser-derived ruby/gaiji nodes carry empty `source` fields — the verbatim markup must come from the original `.txt`:

1. Map the region's char range to contributing AAT nodes via the spans from re-projection (equivalently `projection_spans.parquet`; re-projection spans are used since they are already in hand).
2. Take the covering `[min byte_start, max byte_end)` over those nodes' AAT spans.
3. Locate the original file: `text_id` → corpus `index.json` works entry → `txt_path` under `--aozora-root`; decode windows-31j.
4. **Hash gate:** verify the file's sha256 against AAT `meta.source_hash` before slicing; on mismatch, omit the layer with a `source-hash-mismatch` error rather than quote wrong bytes.

### Layer 4 — AAT node context

The contributing nodes' RFC 6901 pointers (`/blocks/41/content/3`) plus `inline_kind` / `is_ruby_base` / `is_gaiji` flags. Cheap (already computed for Layer 3), useful for debugging projection artifacts.

### Layer 5 — work metadata

`aozora_works.parquet` (join on `source_id`) gives `work_id`, `title`, `author_person_id`, plus year/orthography where present. With `--abc-catalog`: `works/<work_id>.json` adds subtitle, `first_published`, `ndc`, `card_url`, contributor roles; `persons/<person_id>.json` adds names (family/given, readings, romaji). Without the catalog, the author renders as `person:000879`. The warehouse's "never a free-text name" rule (`import_aozora.rs`) is respected — names enter only at report time from ABC person records.

## Output

`examples.md` — findings in rank order; per finding: rank, kind, rrf score, pattern id, the pattern's analyzer split; per example: a `### 『title』 author（year・orthography）— text_id, region, chars` heading with card-URL link, snippet blockquote, analyzer table, fenced markup slice, AAT node line. Anomaly-channel rows get the same per-example treatment in a trailing `## Anomalies` section. Provenance header: run_id, sha256 of the input interesting JSON, catalog path, flags, `built_at_utc`, plus the analyzer-id legend.

`examples.json` — the input artifact's rows (and its anomaly entries) passed through, each extended with:

```jsonc
"hydrated_examples": [{
  "source_id": "…", "text_id": "…", "region_index": 526,
  "char_start": 769, "char_end": 784,
  "snippet": { "before": "…", "region": "…", "after": "…" },
  "analyzer_analyses": [ { "analyzer_ids": ["…"], "tokens": [ { "surface": "…", "features": {…} } ] } ],
  "aozora_markup": { "text": "…《…》…", "byte_start": 123, "byte_end": 456 },
  "aat_nodes": [ { "pointer": "/blocks/41/content/3", "inline_kind": "ruby", "is_ruby_base": true, "is_gaiji": false } ],
  "work": { "work_id": "…", "title": "…", "author": { "person_id": "…", "family_name": "…", … },
            "first_published": "…", "orthographic_style": "…", "ndc": "…", "card_url": "…" },
  "errors": []
}]
```

Layers that could not be hydrated are `null` with a machine-readable reason appended to `errors`.

## Error handling

Per-example degradation, never build failure. Error vocabulary:

| code | meaning | effect |
|---|---|---|
| `aat-missing` | `sources.aat_path` unreadable | example has metadata layer only |
| `projection-mismatch` | re-projected char count ≠ `sources.source_chars` | snippet/markup/AAT layers omitted for that source |
| `source-hash-mismatch` | original `.txt` sha256 ≠ AAT `meta.source_hash` | markup layer omitted |
| `original-missing` | no `--aozora-root`, or `txt_path` unresolvable | markup layer omitted |
| `work-record-missing` | no ABC `works/<work_id>.json` | metadata from `aozora_works` only |
| `person-record-missing` | no ABC `persons/<person_id>.json` | author shown as person id |
| `works-sidecar-missing` | run has no `aozora_works.parquet` | metadata block reduced to text_id |

Exit non-zero only on structural failure (unreadable interesting JSON, missing run dir / required parquet, unwritable output dir). Final summary line: examples fully / partially / not hydrated, by error code.

## Determinism

Output is a pure function of declared inputs. All iteration orders explicit (rank, then `region_index`); no wall-clock values except the single `built_at_utc` provenance field. Re-running on identical inputs yields byte-identical outputs modulo that field.

## Testing

In `ab-morph-run` beside the summarizer tests:

- **Unit:** snippet windowing at document start/end and clipped windows; region marking; analyzer grouping and id shortening; error-vocabulary mapping.
- **Fixture integration:** a small synthetic run (AAT fixtures with ruby + gaiji, parquet written through the existing warehouse writer, matching original `.txt` in windows-31j) → golden `examples.md` snapshot plus JSON assertions, covering ruby inside the snippet window and a correct markup byte slice.
- **Degradation:** catalog absent; original `.txt` absent; deliberately corrupted source hash; run without `aozora_works.parquet` — bundle builds with the expected `errors[]`.

## Integration

- `just` recipe in `ab-validator/justfile` wrapping the subcommand for warehouse runs under `AB_MORPH_WAREHOUSE_DIR`.
- `reports/morph-warehouse/README.md` section documenting the invocation, including that hydration runs on the machine holding the run dir (hinoki for full-corpus runs; use the Tailscale FQDN — the bare `hinoki` ssh alias resolves elsewhere) and the bundle is then copied next to the existing txt/json reports.
- Provenance lives inside the bundle itself (no `workflow-run.json` shell wrapper needed).

## Source code references

Symbol-anchored (look up with `rg`):

| Component | Crate / file | Symbols |
|---|---|---|
| Ranking artifact shape | `ab-morph-run` `summary/interesting.rs` | `summarize_warehouse_interesting`, `RegionExampleOut`, `InterestingOutputFormat` |
| Example cap | `ab-morph-run` `summary/interesting.rs`, `main.rs` | `max_region_examples` |
| Projection | `ab-plaintext` `aat.rs` | `visible_text_projection_with_spans`, `ProjectionSpan`, `json_pointer` |
| Pipeline offset provenance | `ab-morph-run` `pipeline.rs` | `from_aat_value_with_spans` |
| Works sidecar | `ab-morph-run` `import_aozora.rs` | `author_person_id` ("never a free-text name") |
| Warehouse tables | `ab-morph-run` `warehouse/schema.rs` | `projection_spans`, `nway_region_analyzers` columns |
