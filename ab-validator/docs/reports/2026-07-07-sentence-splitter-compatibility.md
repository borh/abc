# Sentence Splitter Compatibility Report

**Date:** 2026-07-07
**Decision:** Parser-IR sentence rows use the Rust splitter behavior covered by
the shared fixture matrix below.

| Input | Expected Sentences | Result |
|---|---|---|
| `吾輩は猫である。名前はまだ無い。` | `吾輩は猫である。` / `名前はまだ無い。` | pass |
| `え！？本当。` | `え！？` / `本当。` | pass |
| `これは3.14です。終わり。` | `これは3.14です。` / `終わり。` | pass |
| `彼は言った。）次。` | `彼は言った。）次。` | pass |
| `一行目\n二行目。` | `一行目\n二行目。` | pass |

## Commands

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-plaintext -- sentence_split_tests
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

## Corpus Sample

Minimum checked sample inventory:

| Fixture | Paragraph Objects |
|---|---:|
| `ab-validator/tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json` | 4 |
| `ab-validator/tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json` | 1 |

The fixture matrix above captures the known Rust/Clojure divergence from this
design slice: punctuation immediately followed by a closing bracket or quote.
That divergence is now covered by Rust and Clojure tests before parser-IR
sentence emission is enabled.
