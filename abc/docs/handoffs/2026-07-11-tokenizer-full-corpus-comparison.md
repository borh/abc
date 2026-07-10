# Tokenizer-profile comparison, full corpus (2026-07-11)

Corpus-wide (stride-1) extension of the stride-44 tokenizer-profile
comparison (`docs/handoffs/2026-07-10-tokenizer-profile-comparison.md`),
run entirely on hinoki from the clean `aozora-full-repin-1a4f864` AAT
dump: all 17,886 dump files, 3,556,636 ruby and 41,551 gaiji
annotations — 19× the sampled measurement. Two independent tracks:

1. **Join-stats runs** — three `soranoha annotation-join-stats-run`
   executions, identical except for the analyzer
   (`vibrato:unidic-{novel,qkana,kindai-bungo}-202512`), each with a
   schema-valid workflow-run record (`soranoha validate-workflow`:
   `workflow_valid: true`).
   Plans: `data/annotation-join-stats-plans/aozora-repin-1a4f864-full-*.json`
   Outputs: `hinoki:/db/soranoha/annotation-join-stats/aozora-repin-1a4f864-full-{unidic-novel,qkana,kindai-bungo}-2026-07-10/`
2. **Morph-warehouse n-way run** — the token-level comparator over the
   same dump with the same three profiles as analyzers:
   run `dict-cmp-profiles-repin-full-2026-07-10` (16,994 sources
   analyzed, 1,160 per-source error rows, input-set-indexed), report at
   `hinoki:/db/ab-validator/morph-warehouse/reports/dict-cmp-profiles-repin-full-2026-07-10/`.

## Ruby classification rates, same-denominator

Over the 17,560 file-entries all three profiles tokenized
(N = 3,303,854 ruby annotations, identical by construction):

| class | unidic-novel | qkana | kindai-bungo |
|---|---|---|---|
| aligned-single | 57.40% | 57.33% | 57.14% |
| aligned-multi | 10.53% | 10.53% | 10.58% |
| stem-prefix | 30.07% | 30.11% | 30.21% |
| conflict | 2.00% | 2.03% | 2.07% |

Full-run rates (17,886 entries for novel/kindai-bungo, 17,560 for
qkana) differ from the table above by ≤0.05pp per class.

## Findings

1. **The stride-44 conclusion holds at corpus scale: join classification
   is not a profile discriminator.** Maximum spread across the three
   profiles is 0.26pp (aligned-single); conflict spans 2.00–2.07%. The
   sample-vs-corpus shifts (aligned-multi 8.8%→10.5%, stem-prefix
   31.2%→30.1%) are corpus-composition effects, not profile effects —
   they move all three profiles together.
2. **Corpus-wide, the profile differences are widespread but individually
   small — with locatable exceptions.** 1,604/17,560 entries change
   their ruby-conflict count under qkana vs unidic-novel, 2,335 under
   kindai-bungo. Almost all shift by ±1–3; the tails are the interesting
   works: `000118_1745` (+252 conflicts under qkana, 55,614 rubies) and
   `001034_4823` (+169 under kindai-bungo, 25,808 rubies) are the
   corpus-scale analogues of the stride-44 黒襟飾組 case and the seed
   set for any per-work profile-selection heuristic.
3. **Token-level, kindai-bungo is the outlier — not qkana.** Pairwise
   n-way disagreement regions (morph-warehouse, 16,994 sources):
   novel↔qkana 0.47M, kindai-bungo↔novel 1.52M, kindai-bungo↔qkana
   1.34M. The dominant patterns are lexicalized classical forms the
   modern dictionaries split: 彼の (45,488 regions), いまし (24,890),
   だって/います/いない/かな (17–18k each). unidic-novel and qkana
   segment modern-orthography text nearly identically; the qkana
   difference concentrates in kana-spelling handling, the kindai-bungo
   difference in lexicon granularity.
4. **qkana's only per-work failure mode is emitted-surface
   normalization.** All 326 qkana tokenize errors (and zero for the
   other profiles, zero span-walk failures anywhere) are the same
   class: the dictionary emits a surface (e.g. 　　～～… runs) that no
   longer byte-matches the source text, so span reconstruction
   fails. These works are recorded in `tokens/tokenize-errors.jsonl`
   and excluded fail-closed; they are the residual to reclaim if qkana
   coverage matters.
5. **The repin dump is not one-file-per-work.** 236 work ids span 281
   extra AAT files (multi-file works/fragments with distinct content
   hashes). Stride-44 sampling never collided, so this surfaced only at
   stride 1. The pipeline now keys every stage by the full file stem
   `<work-id>-<hash12>`; per-work aggregation across fragments remains a
   follow-up (work_count in these outputs counts file-entries).
6. **字下げ leakage observation (from the n-way report):** 22,849
   disagreement regions contain 字下げ tokens, suggesting Aozora
   layout-note text survives into the tokenized plaintext stream in
   some works — worth a targeted look at the renderer/projection before
   treating those regions as linguistic disagreement.

## Engine notes

- **All tokenization/parsing now lives in Rust.** `ab-morph-run
  tokenize-plaintext` (new subcommand) tokenizes the rendered
  plaintexts directly: per-work `tokens.jsonl` ({surface, char_start,
  char_end}), deterministic `tokenize-errors.jsonl` sidecar (per-work
  analyzer errors tolerated, warehouse-style; infrastructure errors
  fatal), one summary JSON line on stdout. The abc tokenize step is a
  single subprocess call and never parses tokenizer output text. Env
  contract: `AB_MORPH_RUN_BIN` (replaces `AB_VIBRATO_TOKENIZE_BIN`),
  plus `AB_VIBRATO_DICT_DIR`/`AB_VIBRATO_CACHE_DIR` consumed by the
  analyzer machinery. Plan `tokenizer_dict` now carries the full
  analyzer spec verbatim (`vibrato:unidic-qkana-202512`).
- The previous CLI-wrapper seam failed at corpus scale on the JVM 2 GiB
  array cap (whole-corpus MeCab stdout in one string) and would have
  silently double-counted duplicate work-ids; the Rust seam surfaced
  both.
- Operational gotchas for hinoki runs (fish/tmux launch, nix-baked
  `/build` cache fallback ⇒ always set `AB_VIBRATO_CACHE_DIR`, `nix run
  .#soranoha` cd's into the store ⇒ launch via `nix develop -c clojure
  -M:abc/soranoha` from `abc/`) are recorded in project memory.
- Cost: convert ≈45 min per profile (dominant, three in parallel),
  tokenize ≈5–10 min at `--jobs 8`, join-stats ≈15 min; morph-warehouse
  full three-profile run ≈17 min wall at auto-jobs 23. Disk: ≈17 GB per
  join-stats run, 24 GB for the warehouse run.

## Follow-ups

- Work-level aggregation over file-stem entries (fragments currently
  count separately) — unchanged from the re-run handoff, now with the
  concrete 236-work duplicate list as input.
- Reclaim the 326 qkana surface-mismatch works (normalization-aware
  span reconstruction, or record spans directly from the tokenizer
  instead of re-matching).
- Investigate 字下げ layout-note leakage into rendered plaintext.
- The per-work conflict-delta tails (finding 2) are the evidence set
  for ADR 0028 request-set-level profile selection.
