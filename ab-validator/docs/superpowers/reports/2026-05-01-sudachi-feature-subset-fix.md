# Sudachi feature subset fix, 2026-05-01

## Symptom

The 12-source warehouse subset showed `sudachi-c` producing massive `pos1=空白` feature patterns in lexical regions, for example:

- `助詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 57,787 examples.
- `名詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 35,488 examples.

This was not a plausible analyzer difference.

## Root cause

`SudachiAnalyzer` configured the tokenizer with `InfoSubset::empty()`. That is appropriate for tokenization-only benchmarks, but this adapter reads POS and form fields from each `Morpheme`:

- `part_of_speech()` for `pos1..pos4`, `c_type`, `c_form`.
- `dictionary_form()`.
- `normalized_form()`.
- `reading_form()`.

With an empty subset, Sudachi does not guarantee those fields are populated correctly. Mode C exposed this as stale/default `空白` POS values on lexical surfaces.

A second safety issue was present: one `MorphemeList` was reused across chunks. The fix now creates a fresh list per chunk so chunk results cannot retain prior entries.

## Fix

`SudachiAnalyzer` now requests the exact Sudachi word-info fields it consumes:

- `SURFACE`
- `POS_ID`
- `NORMALIZED_FORM`
- `DIC_FORM_WORD_ID`
- `READING_FORM`

It also allocates a fresh `MorphemeList` for each chunk.

## Regression evidence

Added `sudachi_chunking_does_not_reuse_morpheme_features_across_chunks`, which builds a multi-chunk Sudachi-C input and asserts non-whitespace morphemes do not inherit `pos1=空白`.

The test failed before the fix with examples like:

```text
lexical morphemes inherited blank POS features: ["吾輩は猫である", "。", ...]
```

It passes after the fix.

## Real subset verification

Reran the same 12-source warehouse subset as `subset-12-sudachi-features-2026-05-01`.

Runtime/memory:

- Wall time: 55.82s.
- Max RSS: 6,295,940 KB, about 6.0 GiB.
- Storage: 38M.
- Errors: 0.

The top lexical `pos1` feature patterns are now plausible category disagreements rather than `sudachi-c=>空白` artifacts:

1. `助動詞=>vibrato ; 助詞=>sudachi-a+sudachi-c` — 460 examples.
2. `動詞=>vibrato ; 名詞=>sudachi-a+sudachi-c` — 451 examples.
3. `接尾辞=>sudachi-a+sudachi-c ; 補助記号=>vibrato` for surface `ッ` — 238 examples.
4. `助動詞=>sudachi-a+sudachi-c ; 助詞=>vibrato` — 229 examples.
5. `名詞=>sudachi-a+sudachi-c ; 記号=>vibrato` — 184 examples.

A direct table query shows `sudachi-c pos1=空白` remains only for whitespace surfaces such as newline, fullwidth space, and ASCII space.

## Impact

The warehouse feature reports are now usable for linguistic triage. The remaining high feature-region counts mostly reflect real schema/category differences between Sudachi and UniDic/Vibrato, not missing Sudachi-C feature data.
