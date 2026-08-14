# Release Qualification Measurement — 2026-08-14

Evidence for
`docs/superpowers/specs/2026-08-14-release-qualification-by-output-identity-design.md`
(Q1, Q4, Q6, Q7, Q8, Q10). Harness:
`2026-08-14-release-qualification-measurement.py`; machine-readable result:
`2026-08-14-release-qualification-measurement.summary.json`.

**The Python harness's digests are not approvable.** It canonicalizes with
`json.dumps(sort_keys=True)`, not `rfc8785-safe-integer-json-string-v1`, so its
values are evidence for cost and determinism only. The approvable digests are
computed separately by
`2026-08-14-release-qualification-projected-digest.clj` and reported below.

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
| projected digest — AAT (Python instrument) | 2.5 |
| projected digest — parser-IR (Python instrument) | 9.0 |
| projected digest — divergence (Python instrument) | 0.2 |
| qualification runtime with the Python instrument | ≈264 s (4.4 min) |
| **qualification runtime with the named canonicalizer** | **≈666 s (11.1 min)** |

Failures: **zero** at parse, convert pass 1, and convert pass 2.

The 4.4-minute figure is superseded. Under the Python stand-in, projection cost
11.7 s and the converter dominated by ~20×; under
`rfc8785-safe-integer-json-string-v1` projection costs 413.9 s and dominates the
converter (see Q10 below). Only the last row is a qualification-runtime estimate
for the design as specified.

## Determinism (Q6)

Two full converter passes over all 17,602 works, compared **per work**:

| Comparison | Differing works |
|---|---:|
| parser-IR projected digest | **0** |
| divergence projected digest | **0** |

A third data point, from regenerating the parser-IR tree hours later in a fresh
process to compute the Q10 digests: the converter's divergence output matched
the retained first-pass bytes for **all 17,602 works, byte-for-byte**, with zero
conversion failures. Determinism holds across process restarts, not just across
two passes of one run.

## Aggregate digests — Python instrument (evidence only, not approvable)

```
aat         sha256:9106a675f6e4bd4478bf7d1018876f685891b493239819b4c592a0c6b1563aa1
parser-IR   sha256:ff3bd5ffcc04964db2d5901c3322d616fa5edd3f9308f680d76a2e6177c711de
divergence  sha256:6a8714d3963fa2c4077eb35373a4a0d1dd2428ef6c63eb54b53f94a0361e7170
```

## Aggregate digests — `rfc8785-safe-integer-json-string-v1` (Q10)

Instrument: `2026-08-14-release-qualification-projected-digest.clj`, which calls
`abc.tools.hash/sha256-json-rfc8785-safe-integer-v1` — the canonicalizer the
design names — rather than reimplementing it.

```sh
cd abc && QUAL_DIR=/tmp/qual QUAL_JOBS=32 \
  clojure -J-Xmx12g -M -i docs/superpowers/reports/2026-08-14-release-qualification-projected-digest.clj \
    -e '(release-qualification-projected-digest/report!)'
```

Both levels use the same canonicalizer:

```
per-work  = sha256(rfc8785-safe-integer-json-string-v1(projected document))
aggregate = sha256(rfc8785-safe-integer-json-string-v1({slug -> per-work}))
```

| Family | Projection | Works | Documents projected | Aggregate |
|---|---|---:|---:|---|
| AAT | `aat-behavior-v1` | 17,602 | 17,602 | `sha256:328bbb4fcd0bac2a6e6677134aca172446d9bc1c3a58567dc5679385d8cd76b0` |
| parser-IR | `parser-ir-behavior-v1` | 17,602 | 17,602 | `sha256:e5e6505156aa2f7674dd1ba71a7038e80b5b4b52c77d93ec5b4842afabe0545b` |
| divergence | `divergence-behavior-v1` | 17,602 | 17,602 | `sha256:3478a12a740da00651b03a4a0527a60cc60497bbd4a0a76676023495ec5eeb8a` |

`documents_projected` is a fail-closed guard, not decoration: a projection that
matched no document would digest the excluded field everywhere and silently
defeat the design, so the instrument aborts at zero. All three fired on every
document.

The aggregate folds a **map**, not a byte concatenation, so row order cannot
enter the value. Verified: reshuffling the population and re-running produces
byte-identical aggregates (design D10).

**These are the approvable values.** They replace the Python digests above for
every purpose except cost and determinism evidence.

### The named canonicalizer costs ~35× the Python instrument

| Family | Python instrument | `rfc8785-safe-integer-json-string-v1` | Ratio |
|---|---:|---:|---:|
| AAT | 2.5 s | 93.9 s | 38× |
| parser-IR | 9.0 s | 317.1 s | 35× |
| divergence | 0.2 s | 2.9 s | 15× |
| **all three** | **11.7 s** | **413.9 s** | **35×** |

Same 32-way parallelism, same machine, same trees. Qualification runtime is
therefore **≈666 s (11.1 min)**, not the ≈264 s (4.4 min) the Python figure
implied — the digest step goes from 4% of the run to 62% of it.

Caveat on what this measures: the instrument parses each document with charred
and builds the canonical form as a string, on a 12 GB heap. A streaming
production implementation would be faster by an unmeasured margin. What is
measured is that the *named* canonicalizer, called the obvious way, does not
have the cost profile the Python stand-in suggested.

### Per-work digests: 5 duplicate groups, identical in all three families

Per-work digests (`*.per-work.jsonl`, 2.3 MB each, not committed):

| Family | sha256 |
|---|---|
| AAT | `7ee944f86bfbb4f2…` |
| parser-IR | `07e6719c59d13883…` |
| divergence | `ef505b39b91896e6…` |

17,602 rows each, of which **17,597 are distinct**: five pairs of works produce
byte-identical projected output, and **the five groups are exactly the same in
all three families**.

```
045183_000107_000019_45183_ruby_23453  045183_000107_000107_45183_ruby_23453
050558_000975_000150_50558_ruby_61314  050558_000975_000975_50558_ruby_61314
062694_002402_001085_62694_ruby_78206  062694_002402_002385_62694_ruby_78206
045218_000183_000183_45218_ruby_30423  052796_000183_000183_52796_txt_44496
052731_000183_000183_52731_txt_42925   052743_000183_000183_52743_txt_43388
```

The first three are one 作品ID filed under two card directories — the cross-card
duplication E13 found, which is precisely why the card directory is in the slug.
The last two are *different* 作品IDs whose texts are identical.

Two things this confirms. Work identity stays injective where content is not, so
duplicates remain distinct manifest rows rather than colliding — which is the
failure the naive basename keying produced (E12). And a duplicate that is a
duplicate in AAT is a duplicate in parser-IR and divergence too, which is what a
deterministic pipeline over identical input should give.

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
