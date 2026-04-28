# Morph Analyzer Adapters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build reusable plaintext source extraction plus Vibrato and Sudachi analyzer adapters that emit `ab_morph_diff::Analysis` values.

**Architecture:** `ab-plaintext` converts source formats into `PlainTextDocument`; Aozora Bunko honbun is the first input format. `ab-morph-analyzers` consumes plaintext documents, loads analyzer dictionaries, constructs morpheme spans with a shared builder, and returns typed analyses for `ab-morph-diff`.

**Tech Stack:** Rust edition 2024, `ab-source-syntax`, `ab-morph-diff`, `encoding_rs`, `serde`, `vibrato-rkyv` from `https://github.com/o24s/vibrato-rkyv.git`, Sudachi pinned to the benchmark revision, `zstd` for Sudachi dictionary cache decompression, symlinked dictionaries.

---

## Corrected Decisions From Review

- Sudachi has no default dictionary path because the symlinked `dictionary/system.dic.zst` is not Sudachi-compatible. The adapter accepts explicit raw `.dic` or `.dic.zst` paths and smoke tests use `AB_SUDACHI_DICT`.
- The flake exposes `.#sudachi-dictionary-full`, downloaded from `http://sudachi.s3-website-ap-northeast-1.amazonaws.com/sudachidict/sudachi-dictionary-20260116-full.zip`, and the default dev shell sets `AB_SUDACHI_DICT` to its `share/sudachi/system.dic`.
- Vibrato default input is the symlinked `dictionary/optimized/unidic-cwj-202512.dic.zst` available in the local dictionary folder.
- Vibrato loading follows the documented `vibrato-rkyv` path: uncompressed `.dic` via `Dictionary::from_path(path, LoadMode::TrustCache)`, and `.dic.zst` via `Dictionary::from_zstd(path, CacheStrategy::Local)`.
- `comparison_lossy_body` returns `Cow<'_, str>` and must be converted with `.into_owned()`.
- Empty plaintext output is valid and not an error.
- Gaps are represented by uncovered source spans in `Analysis`; adapters do not emit synthetic gap morphemes.
- Sequential token matching skips only Unicode whitespace. Sudachi uses analyzer-reported byte offsets to avoid normalization desync.
- `MorphAnalyzer::analyze` takes `&self` because this phase does not cache mutable workers.
- Span and feature helpers are crate-private.

## File Structure

```text
dictionary                                  // symlink to ../vibrato-pipe/dictionary
Cargo.toml                                  // workspace members and deps
Cargo.lock                                  // updated by Cargo

crates/ab-plaintext/
├── Cargo.toml
└── src/
    ├── lib.rs                              // PlainTextDocument, SourceFormat, public API
    └── aozora.rs                           // Aozora honbun decoding/body extraction/projection

crates/ab-morph-analyzers/
├── Cargo.toml
└── src/
    ├── lib.rs                              // stable public exports
    ├── error.rs                            // AnalyzerError
    ├── span_builder.rs                     // common token-to-span construction
    ├── features.rs                         // feature normalization helpers
    ├── vibrato.rs                          // VibratoAnalyzer
    └── sudachi.rs                          // SudachiAnalyzer and SudachiMode
```

## Implementation Tasks

- [x] Add workspace members and dependency pins.
- [x] Create `dictionary -> ../vibrato-pipe/dictionary` symlink.
- [x] Implement `ab-plaintext` with Aozora honbun conversion.
- [x] Implement `ab-morph-analyzers` core trait, errors, private feature helpers, and private span builder.
- [x] Implement Vibrato adapter using the requested fork.
- [x] Implement Sudachi adapter with `.dic.zst` decompression cache and analyzer offsets.
- [x] Add normal unit tests that do not require dictionaries.
- [x] Add ignored smoke tests for dictionary-backed adapters.
- [x] Run package checks and tests.

## Verification Commands

```bash
cargo test -p ab-plaintext
cargo test -p ab-morph-analyzers --lib
cargo check -p ab-morph-analyzers
```

Optional dictionary-backed smoke tests:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" cargo test -p ab-morph-analyzers -- --ignored
```

## Self-Review

The plan now resolves all blocking review issues before implementation: Sudachi raw dictionary loading, Cow-to-String conversion, gap semantics, whitespace handling, Sudachi normalization, public API width, empty plaintext behavior, and dependency pin comments.
