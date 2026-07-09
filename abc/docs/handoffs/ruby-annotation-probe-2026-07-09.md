# Ruby Annotation Probe: Token Alignment and Reading Divergence

Date: 2026-07-09

Status: evidence note. This is a bounded throwaway probe, not a production
implementation and not an identity-bearing pipeline artifact. It informs the
ADR 0027 amendment and the ADR 0028 proposal.

## Question

`parser-ir-plaintext-body-v1` keeps ruby **base text** and resolved gaiji but
drops ruby **readings**, span boundaries, and direction
(`src/abc/tools/parser_ir_plaintext.clj`). Before tokenization ADRs commit to
that input view, two claims needed measurement:

1. Does dropping ruby spans lose *segmentation* information — should ruby
   spans constrain tokenization?
2. Does dropping ruby readings lose *annotation* information that tokenizer
   output could otherwise reconstruct?

## Method

- Probe script: `prototypes/ruby-annotation-probe/ruby_probe.py`
  (disposable; approximates the plaintext view by stripping Aozora markup
  directly, not by running the parser-IR pipeline).
- Corpus: flake input `aozorabunko-src`
  (`github:aozorabunko/aozorabunko/0e9ea3e5`), 14,049 `*_ruby_*.zip` works;
  deterministic stride sample of 80 works; 20,834 ruby spans with token
  overlap; 0 skipped works, 0 alignment failures.
- Ruby spans recorded in plaintext unicode-scalar offsets — the same
  coordinate family as ADR 0027's
  `token-index-v1 + unicode-scalar-value-input-spans`.
- Tokenizers: MeCab with NINJAL UniDic 2512 MeCab-format data — the same
  dictionary source data the ab-validator flake compiles into Vibrato
  dictionaries. Primary: `unidic-novel`; comparison: `unidic-cwj`.
- Reading comparison: author ruby (katakana-normalized) vs UniDic `kana`
  (surface reading), with `pron` fallback (rendaku voicing), a
  dakuon-variant loose pass, and a crude historical-kana normalization pass
  (kunojiten, small-kana, medial h-row, -au/-eu/-yau shifts).

## Results (unidic-novel; unidic-cwj in parentheses)

Boundary alignment of ruby spans against token spans:

| measure | count | rate |
|---|---|---|
| ruby spans measured | 20,834 | |
| aligned: span = whole token(s) | 14,128 | 67.8% (70.2%) |
| — single token | 11,998 | |
| — multiple whole tokens | 2,130 | |
| straddle: a token crosses a span edge | 6,706 | 32.2% (29.8%) |
| — end-only straddle (stem ruby: base kanji + okurigana in token) | 6,213 | 92.6% of straddles |
| — ruby reading is a prefix of the straddling token's reading | 5,399 / 6,012 checkable | 89.8% |

Reading agreement on aligned, comparable spans (14,067):

| comparison | count | rate |
|---|---|---|
| exact match | 9,462 | 67.3% (63.2%) |
| + dakuon-variant / pron match | 45 | |
| + historical-kana normalization match | 161 | |
| total explainable agreement | 9,668 | 68.7% (64.4%) |
| substantive disagreement | 4,399 | 31.3% (35.6%) |

Substantive disagreements are dominated by author-intentional readings:
gikun and foreign-word glosses (釘靴《ネイルド・ブーツ》, 魅力《チヤアム》,
顯微鏡《ミクロスコオプ》), lexical reading choices (二言《ふたこと》 vs ニゲン,
乳母《ばあや》 vs ウバ), name readings, rendaku, and some genuine tokenizer
errors. See `prototypes/ruby-annotation-probe/per-work-results.json`.

Dictionary effect: `unidic-novel` vs `unidic-cwj` differ moderately in reading
agreement (68.7% vs 64.4%) and strongly in unknown-token rate inside ruby
spans (61 vs 271 no-reading tokens).

## Interpretation

1. **Ruby spans are not word boundaries.** 32.2% of spans do not align with
   token edges, overwhelmingly legitimate stem ruby (reading covers only the
   kanji stem; the token continues into okurigana) that is *prefix-consistent*
   with the tokenizer's reading. Feeding ruby spans to a tokenizer as hard
   segmentation constraints would corrupt about a third of affected tokens.
   The "plaintext tokenization loses segmentation accuracy" concern is not
   supported by this probe.
2. **Ruby readings are gold annotation data that tokenizer output cannot
   reconstruct.** Even with the best-fitting period dictionary, 31.3% of
   author readings differ substantively from UniDic's surface reading. This is
   selection bias working *for* the corpus: authors write ruby precisely where
   readings are non-obvious. Dropping readings from the analysis chain is a
   permanent loss; no dictionary upgrade recovers them.
3. **Span-intersection joining works and is tokenizer-agnostic.** With ruby
   spans and token spans expressed in the same plaintext unicode-scalar
   coordinate system, every sampled span joined deterministically
   (aligned / stem-prefix / conflict classification; zero misaligned tokens
   across 160 work-runs). No tokenizer changes, TEI input view, or direct
   parser-IR tokenization path is needed to preserve ruby in the token chain.
4. **Dictionary choice is a real but second-order effect**, supporting
   `unidic-novel` (already compiled as a Vibrato dictionary in the
   ab-validator flake) as the first candidate tokenizer profile for Aozora
   prose.

## Caveats

- The probe strips Aozora markup with its own heuristics (implicit ruby base
  detection, header/footer trimming); it does not run adapter → AAT →
  parser-IR → plaintext. Rates are estimates, not renderer-exact.
- Reading comparison conventions (kana field vs pron field, historical-kana
  normalization) are crude; the substantive-disagreement rate is an upper
  bound containing some residual orthographic noise.
- MeCab was used as the runner for convenience; dictionaries are the same
  NINJAL 2512 data as the flake's Vibrato dictionaries, but Vibrato/Sudachi
  runner behavior was not itself measured. Exposing the Vibrato/Sudachi CLIs
  through the flake would let a future profile-evidence probe run the real
  runners.

## Probe Host

- Repository HEAD: `745e03716ac806cbfd1fe1a2f45c68c0db975b00`
- Corpus store path: `aozorabunko-corpus` from flake input rev `0e9ea3e5`
- MeCab 0.996; UniDic 2512 (`unidic-novel`, `unidic-cwj`)
