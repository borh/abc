# LF plaintext canonicalization impact on morph corpus comparisons

Date: 2026-04-29

## Goal

Regenerate compact morph comparison artifacts after canonicalizing plaintext line endings from CRLF/bare-CR to LF, then compare the new corpus-wide summaries against the previous compact baseline.

## Inputs

Previous compact artifact:

```bash
scratch/morph-full-corpus-compact-canonical/comparisons.jsonl.zst
```

New compact artifact:

```bash
scratch/morph-full-corpus-compact-lf-canonical/comparisons.jsonl.zst
```

AAT input directory:

```bash
scratch/morph-full-corpus/aats
```

Both runs cover 17,894 AAT inputs and compare `vibrato` against `sudachi-c`.

## Regeneration command

```bash
cargo build --release -p ab-morph-run

rm -rf scratch/morph-full-corpus-compact-lf-canonical
mkdir -p scratch/morph-full-corpus-compact-lf-canonical

AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-full-corpus-compact-lf-canonical/analyses.jsonl.zst \
    --comparisons-output scratch/morph-full-corpus-compact-lf-canonical/comparisons.jsonl.zst \
    --examples-output scratch/morph-full-corpus-compact-lf-canonical/examples.jsonl.zst \
    --errors-output scratch/morph-full-corpus-compact-lf-canonical/errors.jsonl.zst \
    --manifest-output scratch/morph-full-corpus-compact-lf-canonical/manifest.json \
    --jobs 8 \
    --progress-interval-seconds 30
```

The run completed in about 996 seconds. It produced zero error rows.

Peak observed progress-line memory was approximately 15.9 GB RSS/PSS. Memory oscillated and dropped after large documents completed, which is consistent with concurrent large-source processing rather than monotonic retention.

## Artifact sizes

| artifact | previous | LF-canonical |
| --- | ---: | ---: |
| analyses.jsonl.zst | 696 KiB | 696 KiB |
| comparisons.jsonl.zst | 1.5 MiB | 1.5 MiB |
| examples.jsonl.zst | 5.2 MiB | 5.4 MiB |
| errors.jsonl.zst | 4 KiB | 4 KiB |
| manifest.json | 4 KiB | 4 KiB |

## Corpus-wide totals

| metric | previous | LF-canonical | delta |
| --- | ---: | ---: | ---: |
| comparison rows | 17,894 | 17,894 | 0 |
| segmentation regions | 7,358,031 | 5,858,351 | -1,499,680 |
| split regions | 934,697 | 934,279 | -418 |
| merge regions | 6,053,204 | 4,554,005 | -1,499,199 |
| resegment regions | 370,130 | 370,067 | -63 |
| feature-difference regions | 156,465,071 | 157,965,418 | +1,500,347 |
| coverage-mismatch regions | 0 | 0 | 0 |
| null boundary-F1 rows | 2 | 2 | 0 |
| average non-null boundary F1 | 0.967181027 | 0.976767119 | +0.009586092 |

Interpretation: LF canonicalization removed about 1.5M segmentation regions, almost entirely merge regions. The corresponding increase in feature-difference regions is expected: many spans that used to be segmentation differences are now aligned one-to-one and can expose feature differences instead.

## Previously inspected source IDs

| source_id | previous F1 | LF F1 | previous segmentation | LF segmentation | delta segmentation | previous feature diffs | LF feature diffs |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `000293_48490-bc2210d727db` | null | null | 0 | 0 | 0 | 1 | 1 |
| `000293_60442-9301b1e3a1e3` | null | null | 0 | 0 | 0 | 1 | 1 |
| `000081_43733-4063e7c46297` | 0.6884927066450566 | 0.7139495798319326 | 114 | 79 | -35 | 948 | 983 |
| `001529_50685-dd3b2fe4e5bf` | 0.9560166438604819 | 0.9592061346585344 | 31,082 | 30,098 | -984 | 396,836 | 397,817 |
| `000311_2012-1bae69ee8181` | 0.9741528542158167 | 0.9839042682781187 | 20,709 | 15,620 | -5,089 | 494,071 | 499,167 |
| `JISTABLE-a3b6bf10cde1` | 0.9489953408684274 | 0.9647802832790403 | 21,247 | 12,388 | -8,859 | 241,304 | 250,163 |

The null-boundary rows are unchanged because they are still whitespace/control-only texts. LF canonicalization fixes CRLF splitting noise, not empty/control-only source records.

## Largest segmentation reductions

| source_id | previous segmentation | LF segmentation | delta | previous F1 | LF F1 |
| --- | ---: | ---: | ---: | ---: | ---: |
| `001099_46996-10fe9133d385` | 28,913 | 12,048 | -16,865 | 0.930568145806157 | 0.9705510138632039 |
| `001025_50909-dabf470f1f18` | 18,247 | 5,538 | -12,709 | 0.9142952333604899 | 0.958334809662552 |
| `JISTABLE-a3b6bf10cde1` | 21,247 | 12,388 | -8,859 | 0.9489953408684274 | 0.9647802832790403 |
| `000216_45567-ca9949266c2b` | 20,694 | 12,110 | -8,584 | 0.9607516404477459 | 0.9817477670871603 |
| `001562_56146-66c41ed10b8f` | 22,680 | 14,639 | -8,041 | 0.9665090872418888 | 0.9823634088814873 |
| `001562_33224-e48e57f82f86` | 17,467 | 9,454 | -8,013 | 0.969251671051036 | 0.9865743411225665 |
| `001562_57875-10bbfac54ee2` | 21,246 | 14,190 | -7,056 | 0.9610896095615166 | 0.9791521365283271 |
| `000885_2557-2fafffa94217` | 8,898 | 1,859 | -7,039 | 0.8965048993133247 | 0.978461486645785 |
| `000961_4820-1fa47562950a` | 12,094 | 6,524 | -5,570 | 0.9539221749733223 | 0.9725722612283837 |
| `001562_56145-c9fe64a731a3` | 18,455 | 12,994 | -5,461 | 0.9603355529062836 | 0.9788587331837568 |

## Top summary changes

Lowest non-null boundary F1 improved from `0.6884927066450566` to `0.7139495798319326`. Several previous low-F1 rows left the top 10 entirely after CRLF normalization.

The highest segmentation count still belongs to `001529_50685-dd3b2fe4e5bf`, but it dropped from 31,082 to 30,098. The rest of the top segmentation list changed more substantially; for example, `001099_46996-10fe9133d385` dropped from 28,913 to 12,048 segmentation regions.

The top feature-difference rows are broadly stable by identity, but counts increased where CRLF-related segmentation differences became one-to-one regions with feature diffs.

## Bounded example whitespace signal

The compact examples still contain substantial whitespace-only evidence:

| metric | previous | LF-canonical |
| --- | ---: | ---: |
| example rows | 178,908 | 178,908 |
| whitespace-only source excerpts | 66,295 | 52,384 |
| segmentation examples | 173,059 | 170,663 |
| feature-diff examples | 5,849 | 8,245 |

LF canonicalization removed about 13,911 whitespace-only examples from the bounded examples, but 52,384 remain. They are now mostly LF/newline/full-width-space spans rather than CRLF spans.

Example remaining whitespace-only evidence:

```text
"\n\n\u3000"   Vibrato: ["\n\n", "\u3000"]   Sudachi: ["\n\n\u3000"]
"\n\u3000"     Vibrato: ["\n", "\u3000"]       Sudachi: ["\n\u3000"]
```

## Conclusion

LF canonicalization was worth doing. It removed a large CRLF-specific segmentation artifact and improved corpus-wide boundary metrics without introducing coverage mismatches or run errors.

It does not fully solve whitespace noise. Remaining whitespace-only spans are real analyzer differences over newlines and indentation/full-width spaces. The next improvement should be comparison/reporting policy, not more plaintext mutation:

1. Add whitespace-only classification to compact examples and summaries.
2. Add `summarize-compact` filters or sort modes that exclude whitespace-only structural evidence.
3. Keep raw metrics available, but add lexical/whitespace-adjusted metrics for corpus triage.
