# Release Qualification Measurement — 2026-08-14

Evidence for
`docs/superpowers/specs/2026-08-14-release-qualification-by-output-identity-design.md`
(Q1, Q4, Q6, Q7, Q8). Harness:
`2026-08-14-release-qualification-measurement.py`; machine-readable result:
`2026-08-14-release-qualification-measurement.summary.json`.

**These digests are not approvable.** The harness canonicalizes with Python
`json.dumps(sort_keys=True)`, not `rfc8785-safe-integer-json-string-v1`. They are
valid evidence for cost and determinism only.

## Reproduce

```sh
./2026-08-14-release-qualification-measurement.py --out /tmp/qual \
  --corpus    "$(nix build ./ab-validator#aozorabunko-corpus --no-link --print-out-paths)" \
  --parser    "$(nix build ./ab-validator#ab-aozora --no-link --print-out-paths)/bin/ab-aozora" \
  --converter "$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths)/bin/ab-aat-to-parser-ir" \
  --mapping   ab-validator/data/aat-to-parser-ir-mapping-v2.json \
  --jobs 32
```

Peak disk ≈ 23 GB (two converter passes). The v2 mapping is required: the v1
mapping rejects `ab-aozora`'s AAT v2 by design.

## Identities

| Input | Hash |
|---|---|
| catalog zip | `sha256:5ea13273dd457f89…` |
| `ab-aozora` | `sha256:35d8a9df839246fa…` |
| `ab-aat-to-parser-ir` | `sha256:de820929edbcf5b2…` |
| aat→parser-IR mapping v2 | `sha256:73e1df7af2818487…` |
| population manifest (`population.jsonl`, 7.4 MB, not committed) | `sha256:ec24775f015abaed…` |
| harness | recorded in the summary |

## Population (Q7)

| Quantity | Count |
|---|---:|
| **qualified works** | **17,602** |
| zips rejected `not-catalog-text-zip` | 285 |
| zips with more than one `.txt` member | 0 |
| archives recovered by EOCD trim | 1 |
| members admitted despite a CRC-32 mismatch | 1 |

Two archives need the authoritative reader's tolerance, and **both are admitted
by `abc.tools.source-bundle/inspect-zip`**:

- `cards/001505/files/58100_txt_60357.zip` — a decoy EOCD after the intact
  archive; 984 trailing bytes trimmed, archive identity unchanged
  (`source_bundle.clj` names this archive explicitly, with a regression test).
- `cards/001393/files/50710_ruby_36965.zip` — member
  `fushigino_kunino_alice_musical.txt` has a stored CRC-32 that disagrees with
  its content. Java's reader admits it; Python's `zipfile` rejects it.

A bare `zipfile` reader silently drops both, which is how an earlier revision of
this measurement reported 17,601. **The instrument must follow ABC's admission
boundary, not its own.**

## Cost (Q1, Q4, Q8), 32 jobs

| Stage | Seconds |
|---|---:|
| build population (single-threaded) | 4.2 |
| extract | 2.7 |
| parse | 12.7 |
| convert, pass 1 | 233.0 |
| convert, pass 2 | 239.5 |
| projected digest — AAT | 2.5 |
| projected digest — parser-IR | 9.0 |
| projected digest — divergence | 0.2 |
| **qualification runtime (one pass, all three digests)** | **≈264 s (4.4 min)** |

Failures: **zero** at parse, convert pass 1, and convert pass 2.

Projection is cheap once parallelized — 11.7 s for all three against 233 s of
conversion. The converter dominates by roughly 20×.

## Determinism (Q6)

Two full converter passes over all 17,602 works, compared **per work**:

| Comparison | Differing works |
|---|---:|
| parser-IR projected digest | **0** |
| divergence projected digest | **0** |

## Aggregate digests (evidence only, not approvable)

```
aat         sha256:9106a675f6e4bd4478bf7d1018876f685891b493239819b4c592a0c6b1563aa1
parser-IR   sha256:ff3bd5ffcc04964db2d5901c3322d616fa5edd3f9308f680d76a2e6177c711de
divergence  sha256:6a8714d3963fa2c4077eb35373a4a0d1dd2428ef6c63eb54b53f94a0361e7170
```

## Incidental finding — slug person id is CSV-order dependent

The recovered archive's slug is `058100_001505_001505_58100_txt_60357`. Reading
the catalog **first-wins** instead of last-wins yields `058100_000010_…` — a
different identity for the same work. `catalog-index` is a last-wins reduce, and
the harness matches it. This is the 1,508-row basename collapse in E13 made
concrete: work identity is stable only while the catalog bytes are pinned, which
is why the design binds `catalog_csv_hash`.

## Scalar-domain scan (spec E20)

Instrument: `2026-08-14-release-qualification-domain-scan.py`.

```sh
for d in aat ir1 div1; do ./2026-08-14-release-qualification-domain-scan.py "/tmp/qual/$d/*.json"; done
```

| Output family | Files | Violations |
|---|---:|---:|
| AAT | 17,602 | **0** |
| parser-IR | 17,602 | **0** |
| divergence | 17,602 | **0** |

A violation is any float, any integer outside ±(2^53−1), any non-string object
key, or any other JSON type. Zero across the board, so
`rfc8785-safe-integer-json-string-v1` is usable without a fallback.
