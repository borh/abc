# Tier 1 Done: Mechanical Taxonomy Generator (Rust, ab-coverage)

> Replaces the hand-written-and-drifted §0 of `references/PARSER_REPORT.md`
> with a GENERATED artifact. NO Python. Lives in the ab-validator Rust
> workspace per the repo's Rust+Clojure contract.

## What was built

A new bin on the **existing** `ab-coverage` crate (no new crate — respects the
prior audit finding that the workspace already has too many):

- `crates/ab-coverage/src/bin/generate_taxonomy.rs` (745 LOC) — reads the two
  canonical Aozora sources and emits a generated feature taxonomy, AND
  subsumes the disposable Python verifier (generation + verification in one
  Rust tool).
- `crates/ab-coverage/Cargo.toml` — `[[bin]] generate_taxonomy` entry.
- `data/generated-feature-taxonomy.md` (455 lines) — the generated artifact,
  byte-reproducible from sources.

### Sources read at runtime (NOT `include_str!` — `references/` is gitignored + workspace-excluded)

1. `references/parsers/AozoraEpub3-JDK21/chuki_tag.txt` — 643 tab-delimited
   rows with `############ category ############` headers, parsed into
   `(family, marker_name, lineno)` triples.
2. `/home/bor/Dependencies/aozorabunko/annotation/*.html` — 13 UTF-8 prose
   spec pages; HTML tags stripped, `［＃...］` markers extracted and normalized
   (`[0-9０-９]+`→`N`, concrete quoted content→`○○`).

### Output shape

```
### Layout / レイアウト
| ID | Feature | Example | Sources | Verdict |
| L1 | 改丁 | `［＃改丁］` | chuki_tag.txt:10; layout_1; layout_3 | VERIFIED |
| L3 | 改頁 | `［＃改頁］` | chuki_tag.txt:12 | CHUKI-ONLY |
...
```

Grouped by family (Layout / Headings / External Characters / Kunten / Emphasis
/ Graphics / Other), with `Sources` provenance per row and a `Verdict` column.

## Verification (independently confirmed, not trusting the subagent report)

| Check | Command | Result |
|---|---|---|
| Builds | `cargo build -p ab-coverage --bin generate_taxonomy` | ✓ finished |
| Bin unit tests | `cargo test -p ab-coverage --bin generate_taxonomy` | ✓ 2 passed, 1 ignored (real-source test) |
| Lib tests unaffected | `cargo test -p ab-coverage` | ✓ 5 passed |
| Runs | `cargo run --bin generate_taxonomy -- --write <path>` | ✓ exit 0, "Wrote 423 features" |
| Deterministic (the gate-critical property) | diff of two consecutive stdout runs | ✓ identical |
| Byte-reproducible committed file | diff of `--write` regen vs `data/generated-feature-taxonomy.md` | ✓ byte-identical |
| L13 段組み error corrected | generator now derives L13 from chuki_tag (CHUKI-ONLY), not UNVERIFIED | ✓ the hand-report's error is gone |

## What the generator fixes vs the hand-written §0

| Hand-written §0 (drifted) | Generated taxonomy |
|---|---|
| 85 feature rows, 72 with NO marker example (NO-MARKER) | 423 features, all with provenance (NO-MARKER = 0 by construction) |
| 1 UNVERIFIED claim (L13 段組み — in neither source) | L13 correctly listed as CHUKI-ONLY (it IS in chuki_tag) |
| Non-diffable against sources | `--write` regenerates byte-identically → drift-gate-able |

## Net verdict distribution (generated, full corpus)

```
  VERIFIED      : 133   (in BOTH chuki_tag + manual)
  CHUKI-ONLY    : 160   (in chuki_tag only)
  MANUAL-ONLY   : 130   (in manual only)
  UNVERIFIED    :   0   (generated → none can be unverified by construction)
  NO-MARKER     :   0   (generated → all have a source)
```

The 290 CHUKI-ONLY + MANUAL-ONLY rows are real findings: 160 features the
canonical tag-table covers that the prose manual doesn't spell out as
`［＃...］` examples, and 130 the manual documents that `chuki_tag.txt`
omits. These are the *real* coverage picture the hand-written §0 was missing.

## Caveat (one real one)

- The `println!` (stdout) path adds a trailing newline the `fs::write`
  (`--write`) path doesn't. **Not** non-determinism — just a path-specific
  formatting inconsistency. The drift gate must compare `--write` regen to the
  committed file (both appples-to-apples), not stdout-to-committed. Confirmed
  byte-identical under `--write`.

## Next steps (Tier 2 + drift gate, not yet done)

1. **Nix drift gate** (abc repo, mirrors the existing `tei-profile-drift`
   pattern): a `checks.taxonomy-drift` derivation that runs the bin with
   `--write` against the committed `data/generated-feature-taxonomy.md` and
   fails on any diff. This is the move that turns "did the report drift?" into
   a CI question. ~1 hour. **Highest-leverage remaining step.**
2. **Tier 2: empirical aozora-rs coverage probe** (Clojure, in abc — the
   PARSER_REPORT §4 scorecard is hand-guessed ✅/❌; the full-corpus AAT run
   at `scratch/morph-full-corpus/aats/aozora-rs-adapter/` (17,894 docs)
   makes real coverage empirically checkable). New `abc.tools.parser-coverage-probe`
   + deps.edn alias + Nix app, same shape as `aozora-history-audit`.
3. **Wire the abc `references/PARSER_REPORT.md` §0 to consume the generated
   file** — replace the hand-written §0 with a line pointing to the generated
   artifact (or a generated-and-committed copy under abc control). Defer until
   the drift gate is green so the hand-written section isn't deleted before
   the generator is trusted in CI.

The Tier 1 generator is the linchpin: once the drift gate (next step #1) is
in place, the question "is the report correct?" stops being a verifier-run
question and becomes a Nix-check question that fails the build on drift —
exactly the pattern already proven for `tei-profile-drift` and ADR 0001.

---

*Built 2026-07-02. Committed as `bc24b5f Add generate_taxonomy bin to ab-coverage`.*

---

## Errata 2026-07-02 (data-driven correction)

The generator design described above was REVISED after the user flagged
( correctly) that `chuki_tag.txt` is not authoritative (it's AozoraEpub3's
third-party converter table) and license-entangling. See
`taxonomy-authority-correction.md` for the measurement and
`authoritative-registry-design.md` + `authoritative-registry-evidence.md`
for the corrected design.

**Current state (commit 657eead):**
- `chuki_tag.txt` is no longer read as a source (deliberately; the only
  mention in source is a comment explaining it's not read).
- Generator derives EXCLUSIVELY from `annotation/*.html` (262→271
  DOCUMENTED features) + optional real `cards/*.txt` corpus (4,085 OBSERVED
  + 164 DOCUMENTED-AND-OBSERVED + 16 DEPRECATED ground-truth entries).
- `Verdict::ChukiOnly` and the `aozoraepub3_handles` advisory column are
  removed. Status is now {DOCUMENTED, OBSERVED,
  DOCUMENTED-AND-OBSERVED, DEPRECATED}.
- Output has 0 `chuki` references (verified). Deterministic under both
  annotation-only and +corpus modes (verified).
- The 5 known spec gaps (Unicode-codepoint gaiji ×N, figure-with-dimensions,
  kunten 返り点 (ツ)/(フ)) surface as OBSERVED.

**Two-modes model (the right abstraction):**
- annotation-only → the authoritative "what Aozora says exists" surface
  (~271 DOCUMENTED features) — cheap, CI-friendly
- +corpus → the ground-truth "what authors actually write" surface
  (~4,347 entries) — heavy walk, periodically refreshed, not per-build

**PARSER_REPORT.md** moved from `references/` (gitignored) to
`abc/docs/parser-report.md` (first-class doc, gitignore contract restored —
it was the only tracked file in an ignored tree, a hygiene defect introduced
by a force-add).
