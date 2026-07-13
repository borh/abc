# Annotation join statistics — first real-pipeline run (2026-07-10)

Status: completed run + root-cause analysis; evidence for ADR 0028 D7/D9.
Tooling: `soranoha annotation-join-stats` (merged 4f90e720), design spec
`docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md`
(slice B). Predecessor: `docs/handoffs/ruby-annotation-probe-2026-07-09.md`
(throwaway markup-stripping + MeCab; this run replaces both caveats with the
real adapter → AAT → parser-IR → renderer pipeline and the pinned Vibrato
runner).

## Headline

**The renderer's span coordinates are sound; the adapter/mapping chain's
ruby *base extents* are not.** Across 407 works the token-span walk had zero
failures and the join was fully deterministic — but ruby spans disagree with
token boundaries at the start far more often than raw-markup ruby does
(conflict 17.6% vs the probe's 2.4% start-straddle), and inspection
attributes the elevation to ruby-base attachment defects upstream of the
renderer, plus a tail of degraded single-line parses. D7's
`span_preservation` question is answered in an unexpected direction: a
renderer-level preservation assertion would pass while the artifact is
already wrong — the fidelity gap lives in adapter → AAT → parser-IR.

## Run inputs (exploratory provenance floor, D9)

- abc commit: `4f90e720` (merged main; `soranoha annotation-join-stats`).
- AAT dump: `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/`
  (aozora adapter; NOTE: its `metadata.json` records a dev-build adapter
  path `/home/bor/Projects/ab-validator/.../aozora-adapter`, i.e. the dump
  predates store-pinned generator identity — a staleness-era caveat).
- Converter: `ab-aat-to-parser-ir` from
  `/nix/store/d74yv48ixp583hzrhi53bynm1kvx5nrd-ab-aat-to-parser-ir-0.1.0`.
- Mapping: `ab-validator/data/aat-to-parser-ir-mapping-v1.json`
  (sha256 `89f7f31929707ca8…`, 707,478 bytes).
- Tokenizer: `vibrato-tokenize` flake app
  (`/nix/store/i005f1yq1g90pgmkkz7jh7mmd5945hx8-vibrato-cli-0.7.7-6467251`,
  dictionaries
  `/nix/store/31dq6n6l3iq3vkb49nf119x10q7xddik-vibrato-dictionaries`),
  `AB_VIBRATO_DICT=unidic-novel` (UniDic 2512, the probe's primary).
- Sample: stride-44 over the dump's 17,886 AAT files (sorted) → 407 files;
  all converted (0 failures), all tokenized, 0 span-walk failures,
  0 skipped works.
- Outputs: `/db/soranoha/annotation-join-stats/aozora-full-20260705-stride44-unidic-novel-2026-07-10/`
  (`per-work.jsonl`, `aggregate.json`, `report.md`).

Operator-side steps (scripts committed under
`prototypes/annotation-join-stats-run-2026-07-10/`):

```sh
# 1. sample + convert (from anywhere)
ls $DUMP/aat/aozora-adapter | sort | awk 'NR % 44 == 1' > sample.txt
ab-aat-to-parser-ir convert --aat $DUMP/aat/aozora-adapter/$f \
  --mapping ab-validator/data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out stats-in/$id/parser-ir.json --divergence-out .../divergence.json
# 2. render plaintexts (from abc/)
clojure -M prototypes/annotation-join-stats-run-2026-07-10/render_plaintexts.clj stats-in plaintext
# 3. tokenize (one vibrato process; EOS-per-line splitting)
AB_VIBRATO_DICT=unidic-novel python3 .../tokenize_works.py plaintext tokens \
  $(nix eval --raw ./ab-validator#apps.x86_64-linux.vibrato-tokenize.program)
# 4. stats (from abc/)
clojure -M:abc/soranoha annotation-join-stats stats-in tokens $OUT
```

## Numbers

407 works, 3,047 ruby annotations, 2 gaiji (both aligned-single).

| ruby classification | count | rate | probe (80 ruby works, MeCab) |
|---|---|---|---|
| aligned-single | 789 | 25.9% | 57.6% |
| aligned-multi | 946 | 31.0% | 10.2% |
| stem-prefix | 776 | 25.5% | ~29.8% |
| conflict (start-misaligned/degenerate) | 536 | 17.6% | ~2.4% |

Sampling caveats (why these are not yet "the" corpus rates):

- File-level sampling: the dump indexes every corpus text file; median
  rendered body is 30 scalars — most entries are fragments/cards, not
  works. Only 84/407 entries carry ruby at all, and the top two contribute
  71% of all ruby spans.
- The two dominant works (`000118_1745`, `000077_1323`) are degraded
  parses: whole file on line 1, `unmatched_close` warnings, stray `》`
  leaking into text and ruby bases (e.g. base `り》に出`).

## Root cause of the conflict elevation (verified against raw sources)

Adapter/mapping ruby-base attachment is systematically wrong even in
clean works. Verified example (work `000050_50770`, card 000050, file
`50770_ruby_44352.zip`):

- Source markup: `蘆《あし》を吹《ふ》く風` — per Aozora rules the second
  base is the kanji run `吹`.
- Parser-IR node: `{"ruby": {"base": "を吹", "reading": "ふ",
  "scope": "explicit"}}` — base over-extends into the preceding particle
  `を`, and `scope` claims an explicit `｜` that the source does not have.
- Rendered consequence: the annotation span starts one scalar early, the
  tokenizer boundary sits between `を` and `吹`, and the join correctly
  reports `conflict`.

The truncation direction also occurs (`田圃《たんぼ》` → base `圃`,
`東隣《ひがしどなり》` → base `隣`, observed in the conflict inspection of
`000118_1745`, though that work's parse is degraded). Conflict rates stay
in the 10–32% band even for works with a single benign warning, so the
defect is not confined to degraded parses.

The probe never saw this because it re-parsed ruby from raw markup with its
own regex — it measured the corpus, not the pipeline. This run measures the
pipeline, which is exactly what ADR 0028's join classification was built to
surface: **`conflict` is functioning as an adapter-fidelity detector.**

## Implications for ADR 0028 open questions

- **D7 (`span_preservation`).** Two separable properties emerged:
  (1) *coordinate preservation* — renderer spans align to the rendered
  plaintext deterministically; zero walk failures over 154,222 tokens; a
  renderer-level `span_preservation` flag would be true and is cheap to
  assert. (2) *extent fidelity* — whether the span covers the ruby base the
  source author wrote — is violated upstream, and no renderer-level flag
  can attest to it. Recommendation: define `span_preservation` (if
  ratified) as claim (1) only, and treat claim (2) as an ab-validator
  adapter-fidelity property with its own audit, not a manifest field.
- **D9 (exploratory provenance floor).** The "Run inputs" block above is
  the demonstrated floor: {abc commit, AAT dump id + its generator caveat,
  converter store path, mapping hash, tokenizer store paths + dictionary,
  sample rule}. Everything needed to re-derive or dispute the numbers fits
  in one section; recommend ratifying this field set as the D9 floor for
  join views.

## Follow-ups (recorded, not started)

1. **ab-validator ruby-base fidelity audit**: compare adapters (aozora vs
   aozora-rs vs aozora2) on 《》 base attachment against the Aozora
   kanji-run rule; the `を吹`/`scope explicit` case is the seed test.
2. Re-run these statistics after the base-attachment fix; only then are
   corpus-level alignment rates meaningful as linguistics rather than
   fidelity telemetry.
3. Work-level sampling utility (filter fragments; sample ruby-bearing
   works) so N means "works", not "files".
4. Dump refresh: regenerate the aozora AAT dump under store-pinned
   generator identity (the 20260705 dump predates it).
