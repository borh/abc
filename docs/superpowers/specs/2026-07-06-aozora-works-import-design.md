# `import-aozora-metadata`: `aozora_works.parquet` Producer

Design for item 1 of the 2026-07-06 handoff (`docs/superpowers/plans/2026-07-06-next-session-handoff.md`). Child of the governing spec `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` (§Cross-Repo Dependency on `abc`, §v2 sidecar `aozora_works.parquet`, §Error Behavior). Both readers (in-memory `interesting.rs` and DuckDB `interesting_sql.rs`) already consume the sidecar; this design specifies the missing producer plus two small reader hardenings.

## Goal

`ab-morph-run import-aozora-metadata --run-dir <run> --from <abc export dir>` materializes `<run_dir>/aozora_works.parquet` as an imported projection of ABC's `metadata-record.schema.json`, flipping the ranker's rarity basis from distinct sources to distinct ABC works (`rarity_basis = "work"`).

## Measured facts (2026-07-06, canonical warehouse `full-2026-07-05_164518-jobs0`)

- ABC export: `/home/bor/Projects/abc/out/corpus/` — `works/<work_id>.json` (17,810 metadata records, `source_csv_provenance` stripped) and `persons/` (1,334, not consumed here). Owner confirmed this is the canonical export layout.
- All export records declare the same `metadata_record_schema_hash`: `sha256:692dfa23215ea6c58e21f5d293b364dff06371c75fb32ebc9694f52e8e5c9b1f` (SHA-256 over RFC-8785 JCS bytes of the schema file; verified against ABC `src/abc/tools/manifest.clj`).
- Warehouse `source_id` format: `<person_id>_<card_id>-<hash12>` (e.g. `000136_731-9559c30ae312`). ABC `work_id` = card id zero-padded to 6 digits (`000731`); verified via `card_url`. The person prefix is *not* part of work identity — the same card can appear under two person pages (author + translator; 7 such collisions in the corpus).
- Coverage: 17,885 sources → 17,598 distinct works; 17,596 have export records. Exactly 2 sources are unmappable: `000025_kantou-20379f2add12` (non-numeric slug) and `000989_352-69e66488e1fe` (card 000352 absent from the export).
- **Dedup semantics (corrects the governing spec's claim):** card-level `work_id` merges *multi-file cards* — 287 extra sources, 1.6% of the corpus (e.g. three files under one card). Paired 旧字/新字 editions are **separate Aozora cards = separate ABC work_ids** and are *not* merged: 254 strict pairs (same title, same author, differing `orthographic_style`, same subtitle) exist in the warehouse, ~1.4% of works. Large same-title groups are serials (e.g. 銭形平次捕物控: 438 cards distinguished by subtitle) and are correctly separate works. The "~2× inflation" is real *per affected pattern* (a pattern in both files of one card counts twice), not corpus-wide.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| 1 | Import from `../abc/out/corpus` layout (`works/<work_id>.json`); `--from` required, no default path compiled in | Owner-confirmed canonical export; cross-repo location stays a CLI fact |
| 2 | No `SCHEMA_VERSION` bump; defer to Phase 3 (`projection_spans`) | Readers probe the sidecar by presence and degrade honestly — the shipped "v2-forward probe" contract. Amend governing-spec Decision 3: the bump lands with the first *analysis-pass-produced* sidecar; post-hoc imported, presence-probed sidecars are version-neutral. No mutation of the canonical run's `runs.parquet`; no `sql.rs` change now |
| 3 | Schema-drift gate = pinned constant | The importer compiles in the schema hash it was written against (`sha256:692dfa…`). Every record's declared `metadata_record_schema_hash` must equal it; mismatch → hard error naming expected and observed hashes. An ABC schema change updates the constant and the field mapping together in one reviewed change. No JCS implementation needed in Rust |
| 4 | Run-scoped import (per-run sidecar) | Readers probe `<run_dir>/aozora_works.parquet`; rows exactly cover the run's sources; re-import per run is seconds |
| 5 | `work_id` = ABC card id verbatim; `rarity_basis = "work"` keeps ABC's meaning of "work" | Governing-spec Decision 10 (imported projection, no invented identity). Paired-edition merging is an ABC-owned identity problem; if ABC ever models edition clusters, that arrives as a **new** basis value (e.g. `"work_cluster"`), never a silent semantic change under `"work"` |
| 6 | Unmappable sources are skipped with a warning; zero-mapped import is a hard error | Readers fall back to `source_id` as the rarity key for unmapped sources, so skipping is honest. A present-but-empty sidecar would yield `rarity_basis = "work"` with `total_work_count = 0` and degenerate IDF — refuse to create one |
| 7 | Run-dir immutability gains one sanctioned exception: presence-probed sidecar files, written atomically | Import is post-hoc by design. Atomic tmp+rename in the run dir prevents torn reads; refuse-overwrite without `--force`; every summary output already records `rarity_basis`, so downstream artifacts name which world they saw |
| 8 | Provenance = pinned schema hash + `metadata_record_retrieved_at` (import-invocation time) | Content changes under an unchanged schema are distinguishable only by timestamp. Accepted: the projection is cheap to regenerate, never hand-edited, and the reproducibility claim is "re-derivable from ABC" |

## CLI contract

```
ab-morph-run import-aozora-metadata --run-dir <warehouse run dir> --from <abc export dir>
```

- `--from` points at the export root (the directory containing `works/`).
- Refuses to overwrite an existing `<run_dir>/aozora_works.parquet` unless `--force` (existing CLI convention).
- Prints a summary to stderr: works imported, sources mapped, sources skipped with their ids.

## Data flow

1. Read `source_id` values from `<run_dir>/sources.parquet`.
2. Parse each as `<person>_<card>-<hash12>`; `work_id = zero-pad(card, 6)`. Non-conforming ids → skipped (warned, counted).
3. For each distinct `work_id`, load `<from>/works/<work_id>.json`. Missing file → that work's sources are skipped (warned, counted). Malformed JSON or validation failure → hard error naming the file (absence is a coverage gap; corruption is a contract violation).
4. Validate each record (below), project to columns; one output row per (`work_id`, `source_id`) pair.
5. If zero sources mapped → hard error, nothing written. Otherwise write to a temp file in the run dir, then atomic rename to `aozora_works.parquet`.

Composition: pure core (`parse_source_id`, `validate_record`, `project_row`, year extraction) with an IO shell (sources read, JSON loads, parquet write). Each pure unit is testable without a warehouse.

## Column projection (all 9 spec columns; readers require only the first two)

| Column | Type | Derivation |
|---|---|---|
| `work_id` | Utf8 | From `source_id` parse; must match `^[0-9]{6}$` **and** equal the record's `work.work_id` |
| `source_id` | Utf8 | Verbatim from `sources.parquet` |
| `title` | Utf8 | `work.title` verbatim |
| `author_person_id` | Utf8, nullable | First contributor with `relation_to_work = 著者` in record order (ABC pre-sorts contributors by `person_id`); null if none. Never a free-text name |
| `publication_year` | Int32, nullable | Prefer `work.first_published`, else the first non-null `source_editions[].first_edition_year` in order; extract the first substring matching `[0-9]{4}` (handles `1988（昭和63）年10月25日`); null when neither parses (e.g. 元号-only dates) |
| `orthographic_style` | Utf8 | Verbatim; must be one of `新字新仮名`, `新字旧仮名`, `旧字新仮名`, `旧字旧仮名`, `その他` |
| `genre` | Utf8, nullable | Always null (not in `metadata-record`; populated opportunistically by future ABC exports) |
| `metadata_record_schema_hash` | Utf8 | Record's declared hash, stored verbatim (equals the pinned constant by validation) |
| `metadata_record_retrieved_at` | Utf8 | ISO 8601 timestamp of the import invocation, identical for all rows of one import |

## Error behavior

| Situation | Behavior |
|---|---|
| `--run-dir` missing `sources.parquet` | Hard error, exit 1 |
| `--from` missing a `works/` directory | Hard error, exit 1 |
| Record's declared schema hash ≠ pinned constant | Hard error naming expected and observed hashes (governing spec §Error Behavior row) |
| Record fails validation (`work_id` regex/equality, `orthographic_style` enum, missing consumed field) | Hard error naming the file |
| Work's JSON file malformed | Hard error naming the file |
| Work's JSON file absent from export | Skip that work's sources, warn with ids, continue |
| `source_id` doesn't parse as `<person>_<card>-<hash12>` | Skip, warn with id, continue |
| Zero sources mapped | Hard error; no file written |
| Output exists without `--force` | Refuse, exit 1 |

## Reader hardenings (same change, `interesting.rs`)

1. `read_optional_work_map`: treat a present-but-empty `aozora_works.parquet` as absent (defense-in-depth against the degenerate `total_work_count = 0` state the importer refuses to create).
2. `rarity_config` denominator: `total = distinct mapped works + count of unmapped sources` (today unmapped sources contribute per-source rarity keys but are dropped from the total — an off-by-2 on the canonical run). The DuckDB path (`rarity_sql`) gets the equivalent `coalesce` counting.

Expected canonical-run outcome: 17,883 rows (17,885 − 2 skipped), 17,596 distinct works, `total_work_count = 17,598` (17,596 works + 2 fallback source keys), `rarity_basis = "work"`.

## Governing-spec amendments (part of implementation)

1. §Aozora-Specific Corpus Handling / Deduplication and the §Per-Signal rarity note: replace the paired-edition justification with the measured semantics above (multi-file cards merged; paired editions are separate cards; serials are correctly separate; future ABC edition clustering arrives as a new basis value).
2. §Cross-Repo Dependency: resolve the `(TBD: …)` import-step note with the actual CLI and the `out/corpus/works/<work_id>.json` layout.
3. Decision 3: amend to "bump at first analysis-pass-produced sidecar (Phase 3); post-hoc imported presence-probed sidecars are version-neutral".
4. §Error Behavior: add the zero-mapped-import hard-error row.

## Testing

All tests run as `cargo test -p ab-morph-run --features test-analyzer` (bare invocation false-fails 2 bin tests).

- **Unit (pure core):** `parse_source_id` (valid, `kantou`, translator-collision person prefixes, malformed); year extraction (`1988（昭和63）年10月25日`, plain `1907`, 元号-only → null, null inputs); author selection (single 著者, multiple contributors, 翻訳者-only → null); `validate_record` (hash mismatch names both hashes, enum violation, `work_id` mismatch).
- **Integration (temp dirs):** fixture export dir + fixture run dir → import → assert row count, all 9 columns, skip warnings; error cases per the table; `--force` overwrite; empty-map reader hardening; denominator arithmetic with unmapped sources.
- **Compatibility:** existing summarizer tests (`write_aozora_works` fixture, `work_map_switches_rarity_basis_and_dedups`) must pass unchanged — the reader contract (name-based `work_id`/`source_id` lookup) is untouched.
- **Real-data validation:** import into the canonical run, then `summarize-warehouse-interesting --limit 50 --format json` (~10 min, DuckDB engine): confirm `rarity_basis = "work"` and the expected counts above; diff the top-50 against `scratch/full-novel-interesting.json` to see the rarity reshuffle (expected small; ~1.6% of sources merged).

## Non-goals

- Paired-edition (旧字/新字) merging — requires an ABC-side edition-cluster concept; tracked as the `work_cluster` note in Decision 5.
- Importing `persons/` (person registry stays ABC-owned; display strings computed at query time if ever needed).
- `genre` population, `SCHEMA_VERSION` bump, `sql.rs` reader-max relaxation (Phase 3).
- Any change to ranking behavior beyond the rarity basis flip.
