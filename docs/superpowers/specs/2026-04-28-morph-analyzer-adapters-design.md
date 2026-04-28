# Morph Analyzer Adapters and Plaintext Source Design

Status: approved for inline implementation
Date: 2026-04-28

Scope: add a reusable plaintext source layer and analyzer adapters that turn
plaintext documents into `ab_morph_diff::Analysis` values. The first analyzer
targets are Vibrato using the `o24s/vibrato-rkyv` fork and Sudachi using the API
shape from the local `vibrato-pipe` benchmark. The first source format is Aozora
Bunko honbun, but the reusable library is plaintext-oriented rather than named
after Aozora.

## 1. Motivation

`ab-morph-diff` accepts in-memory analyses and has no dependency on real
analyzers. The next layer loads source text, runs analyzers, and produces
validated analyses without mixing in reporting, aggregation, or CLI
presentation.

The source layer is not called `aozora-honbun`. Aozora honbun is only the first
input format. The durable artifact is a plaintext representation that can also
feed future Markdown and TEI export.

## 2. Crate Boundaries

### 2.1 `ab-plaintext`

Location: `crates/ab-plaintext/`

Responsibility: convert supported source formats into a stable plaintext
document.

Initial model:

```rust
pub struct PlainTextDocument {
    pub text_id: String,
    pub source_format: SourceFormat,
    pub text: String,
}

pub enum SourceFormat {
    AozoraHonbun,
}
```

Initial API:

```rust
pub fn from_aozora_honbun_bytes(
    text_id: impl Into<String>,
    bytes: &[u8],
) -> PlainTextDocument;
```

The function is intentionally infallible in this phase. UTF-8, UTF-8 BOM, and
Shift_JIS are decoded lossily where necessary, and an empty post-projection body
is still a valid plaintext document. Corpus policy can decide later whether
empty texts are warnings or rejects.

Aozora honbun conversion follows existing project behavior:

- decode UTF-8 with BOM, UTF-8, or Shift_JIS;
- select text after the second separator line of at least 20 hyphens;
- trim the first `底本：` or `底本:` colophon marker;
- use `ab-source-syntax` to project Aozora source markup into comparison-visible
  plaintext.

Known limitation: rare colophon forms introduced only by a separator line are
not detected in this phase.

### 2.2 `ab-morph-analyzers`

Location: `crates/ab-morph-analyzers/`

Responsibility: load analyzer dictionaries, tokenize `PlainTextDocument.text`,
and produce `ab_morph_diff::Analysis`.

Initial public model:

```rust
pub trait MorphAnalyzer {
    fn analyzer_id(&self) -> &str;

    fn analyze(
        &self,
        document: &PlainTextDocument,
    ) -> Result<ab_morph_diff::Analysis, AnalyzerError>;
}
```

The trait uses `&self` because this phase constructs per-call workers/tokenizers
rather than caching mutable analyzer state. If corpus profiling shows per-call
construction dominates runtime, worker pooling can be added with evidence.

The public API is intentionally narrow: `MorphAnalyzer`, `AnalyzerError`,
`VibratoAnalyzer`, `SudachiAnalyzer`, and `SudachiMode`. Span builders and
feature parsers are crate-private implementation details.

## 3. Dictionary Policy

Dictionaries are large and must not be copied into this repository.

Create a repository-root symlink:

```text
dictionary -> ../vibrato-pipe/dictionary
```

Vibrato default path:

```text
dictionary/optimized/unidic-cwj-202512.dic.zst
```

The current symlinked dictionary tree does not contain a Sudachi-compatible
dictionary. `dictionary/system.dic.zst` is not usable with Sudachi and is not a
default.

Sudachi constructors require an explicit dictionary path. The reproducible
project source for this path is the Nix flake package
`.#sudachi-dictionary-full`, which downloads:

```text
http://sudachi.s3-website-ap-northeast-1.amazonaws.com/sudachidict/sudachi-dictionary-20260116-full.zip
```

and exposes:

```text
$out/share/sudachi/system.dic
```

The default dev shell sets:

```text
AB_SUDACHI_DICT=$out/share/sudachi/system.dic
```

Sudachi cannot mmap `.dic.zst` directly, so explicit zstd paths are decompressed
into a local cache before calling `JapaneseDictionary::from_cfg`, using:

```text
<dictionary parent>/.cache/ab-validator/sudachi/<file stem>
```

Explicit raw `.dic` paths are used directly.

Convenience defaults resolve the repository-root `dictionary` symlink from the
crate manifest directory instead of assuming the process current working
directory is the workspace root. Explicit caller-provided paths are left
unchanged.

Normal unit tests must not require the symlink. Dictionary-backed smoke tests
are ignored.

## 4. Vibrato Adapter

Use the fork:

```toml
vibrato-rkyv = { git = "https://github.com/o24s/vibrato-rkyv.git", rev = "6467251cdb945f8f0ca0c6bfd8c96036ab9d4e12" }
```

Use the API shape shown in the local fork examples:

```rust
use vibrato_rkyv::{CacheStrategy, Dictionary, LoadMode, Tokenizer};

let raw = Dictionary::from_path("path/to/system.dic", LoadMode::TrustCache)?;
let zstd = Dictionary::from_zstd("path/to/system.dic.zst", CacheStrategy::Local)?;
let tokenizer = Tokenizer::new(dict);
let mut worker = tokenizer.new_worker();
worker.reset_sentence(text);
worker.tokenize();
for token in worker.token_iter() {
    let surface = token.surface();
    let feature = token.feature();
}
```

Feature extraction parses the comma-separated UniDic-style feature string into
deterministic keys. `*` and empty fields become `None`; extra fields become
`field_N`.

`VibratoAnalyzer::from_dictionary_path` dispatches by extension:

- `.zst`: `Dictionary::from_zstd(path, CacheStrategy::Local)`;
- any other path: `Dictionary::from_path(path, LoadMode::TrustCache)`.

The default analyzer id is:

```text
vibrato:unidic-cwj-202512
```

## 5. Sudachi Adapter

Use the Sudachi API pattern from:

```text
../vibrato-pipe/third-party/vibrato-rkyv/vibrato/benches/tokenization_sudachi.rs
```

Support all three Sudachi split modes:

```text
sudachi-a
sudachi-b
sudachi-c
```

Sudachi feature maps use stable keys where available:

```text
pos1,pos2,pos3,pos4,c_type,c_form,dictionary_form,normalized_form,reading_form
```

Sudachi may rewrite or normalize input internally. Therefore the adapter must use
analyzer-reported byte offsets when available and set the public morpheme
surface from the original plaintext slice at that span. The normalized/dictionary
forms remain feature values. This keeps `ab-morph-diff` validation based on the
original source text.

## 6. Span Construction, Gaps, Whitespace, and Normalization

Adapters must not silently search ahead through arbitrary text.

Shared builder policy:

1. Tokens may provide analyzer-reported byte spans.
2. Tokens with byte spans use those spans and slice the original plaintext for
   the public morpheme surface.
3. Tokens without byte spans are matched sequentially from a cursor.
4. Sequential matching may skip Unicode whitespace between tokens, preserving it
   as uncovered text. This covers Aozora line breaks and ASCII spaces commonly
   ignored by analyzers.
5. Sequential matching must not skip non-whitespace text. A non-whitespace cursor
   mismatch returns `AnalyzerError::SurfaceMismatch`.
6. End-of-text gaps are allowed. `ab-morph-diff` derives coverage mismatches
   from morpheme spans later; adapters do not create synthetic morphemes for
   gaps.

This resolves the earlier ambiguity: gaps are represented by absence of
morpheme coverage inside an `Analysis`, not by adapter-emitted coverage objects.

## 7. Non-Goals

This phase does not add:

- a CLI;
- corpus aggregation;
- Markdown or TEI export;
- multiway analyzer comparison;
- dictionary download logic;
- analyzer output presentation.

## 8. Test Requirements

Required plaintext tests:

- UTF-8 Aozora honbun with two separators returns only body plaintext;
- Shift_JIS Aozora honbun decodes and returns body plaintext;
- colophon beginning at `底本：` is removed;
- Aozora ruby and annotations are projected through `ab-source-syntax`;
- whitespace-only or empty projected bodies still produce documents.

Required analyzer-builder tests:

- multibyte token surfaces produce correct byte and char spans;
- whitespace gaps between tokens are preserved as uncovered text;
- analyzer-provided spans use the original source slice as public surface;
- `*` and empty feature values become `None`;
- a non-whitespace surface mismatch returns a typed error.

Required ignored smoke tests:

- Vibrato loads `dictionary/optimized/unidic-cwj-202512.dic.zst` when the symlink
  exists and tokenizes a short plaintext sentence;
- Sudachi loads the dictionary path supplied by `AB_SUDACHI_DICT`, decompressing
  it first if it ends in `.zst`, and tokenizes the same sentence in modes A, B,
  and C.

## 9. Self-Review

Spec coverage:

- Analyzer-independent core remains in `ab-morph-diff`.
- Plaintext source conversion is reusable and not named `aozora-honbun`.
- Vibrato uses the requested fork and pinned main revision.
- Sudachi is based on the requested local benchmark and handles `.dic.zst`
  explicitly.
- Dictionaries are symlinked rather than copied.
- Aozora honbun is the only initial input source format.

Deliberate decisions:

- A separate `ab-plaintext` crate keeps future Markdown and TEI export away from
  analyzer dependencies.
- Analyzer helper functions remain crate-private to avoid locking internals into
  the public API.
- Empty plaintext documents are valid data, not errors.
