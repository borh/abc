# N-way pattern counts split artifact

Date: 2026-05-01

## Change

Exact N-way pattern counts now write to a dedicated narrow JSONL artifact via `--nway-pattern-counts-output`.

`nway.jsonl(.zst)` no longer embeds `pattern_counts` in newly generated rows. It remains a source-level compact N-way summary plus bounded examples. This prevents the N-way summary artifact from ballooning when exact pattern reporting is enabled.

## Smoke command

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -q -p ab-morph-run -- analyze-aat \
    --aat-dir scratch/morph-pattern-counts-smoke/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --output-dir scratch/morph-pattern-counts-smoke \
    --nway \
    --nway-pattern-counts \
    --jobs 1
```

Input: smallest available checked AAT file, `scratch/morph-full-corpus/aats/aozora-rs-adapter/000183_52736-4806bcb5510c.json`.

## Smoke results

| artifact | rows | compressed size |
|---|---:|---:|
| `analyses.jsonl.zst` | 3 | 4.0K |
| `comparisons.jsonl.zst` | 3 | 4.0K |
| `examples.jsonl.zst` | 30 | 4.0K |
| `nway.jsonl.zst` | 1 | 4.0K |
| `nway-pattern-counts.jsonl.zst` | 915 | 12K |
| `errors.jsonl.zst` | 0 | 4.0K |

Schema check: the new `nway.jsonl.zst` row did not contain a `pattern_counts` field.

## Pattern summary command

```bash
cargo run -q -p ab-morph-run -- summarize-nway-patterns \
  --pattern-counts scratch/morph-pattern-counts-smoke/nway-pattern-counts.jsonl.zst \
  --kind segmentation \
  --limit 5
```

Top smoke rows:

| examples | pattern |
|---:|---|
| 1 | `sudachi-a+vibrato:unidic-cwj-202512:[牧野|信一] ; sudachi-c:[牧野信一]` |
| 1 | `sudachi-a+vibrato:unidic-cwj-202512:[短篇|小説] ; sudachi-c:[短篇小説]` |
| 1 | `sudachi-a+vibrato:unidic-cwj-202512:[長|さ] ; sudachi-c:[長さ]` |

## Operational note

The previous exact full-corpus run produced a `5.6G` `nway.jsonl.zst` because exact pattern counts were embedded in each source row. New full runs should use:

- `--output-dir scratch/...`
- `--nway`
- `--nway-pattern-counts`

Then use `summarize-nway` on `nway.jsonl.zst` and exact `summarize-nway-patterns` on `nway-pattern-counts.jsonl.zst`.
