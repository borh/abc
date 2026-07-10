# Annotation join-stats re-run from the repin dump (2026-07-10)

Re-run of the corpus join statistics
(`docs/handoffs/2026-07-10-annotation-join-stats-run.md`) from the clean
`aozora-full-repin-1a4f864` AAT dump, after the ruby-base fidelity audit
(`ab-validator/docs/handoffs/2026-07-10-ruby-base-fidelity-audit.md`)
established that the first run's numbers measured dump staleness, not
alignment. This run is fully workflow-backed: every stage executed as an
`abc.tools.workflow` step and the outputs sit beside schema-valid
`workflow-plan.json` / `workflow-run.json` provenance records
(`soranoha validate-workflow` passes on the run record).

## How it ran

New command (this branch):

    soranoha annotation-join-stats-run <plan-json> <out-root>

with the committed plan
`data/annotation-join-stats-plans/aozora-repin-1a4f864-stride44-unidic-novel.json`
(aat_dir = the repin dump, stride 44, mapping
`aat-to-parser-ir-mapping-v1`, tokenizer_dict `unidic-novel`). Steps:
`sample-aat` → `convert-parser-ir` (pinned `ab-aat-to-parser-ir`) →
`render-plaintexts` (`render-with-annotations`) → `tokenize` (pinned
`vibrato-tokenize`, one process, per-line EOS accounting) → `join-stats`
(`abc.tools.annotation-join-stats`). External binaries arrive fail-closed
from `AB_AAT_TO_PARSER_IR_BIN` / `AB_VIBRATO_TOKENIZE_BIN` and are recorded
as step inputs (nix store paths), the plan and mapping with content hashes.

Run inputs (also machine-recorded in the workflow run):

- out-root: `/db/soranoha/annotation-join-stats/aozora-repin-1a4f864-stride44-unidic-novel-2026-07-10/`
- AAT dump: `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864/aat/aozora-adapter` (2026-07-08, repo head `6d29b565`, upstream parser `1a4f864`)
- converter: `/nix/store/qk4hi24hajajk3w02qxnb19j1mgavnmp-ab-aat-to-parser-ir-0.1.0` (built from the flake at this branch's HEAD)
- tokenizer: `/nix/store/y6bpr3nq504hk9ixx022h6zizpa2aq5b-vibrato-tokenize`, `AB_VIBRATO_DICT=unidic-novel`, cache at `/db/soranoha/cache/vibrato`
- sample: 407 of 17,886 AAT files (stride 44), 0 skipped works, 0 span-walk failures

## Numbers

407 works, 188,292 ruby and 1,086 gaiji annotations (first run, same
stride, saw 3,047 ruby / 2 gaiji — the stale dump had dropped ~98% of ruby
in-sample, matching the corpus-wide 99.1% loss the audit measured).

| ruby classification | count | rate | stale-dump run | 80-work probe |
|---|---|---|---|---|
| aligned-single | 109,215 | 58.0% | 25.9% | — |
| stem-prefix | 58,822 | 31.2% | 25.5% | — |
| aligned-multi | 16,601 | 8.8% | 31.0% | — |
| conflict | 3,654 | 1.9% | 17.6% | 2.4% |

- **Conflict collapsed 17.6% → 1.9%**, right at the original MeCab probe's
  2.4%. The audit's conclusion is confirmed end-to-end: the conflict class
  detects adapter/dump infidelity, and with a clean dump the residual is
  ordinary tokenizer-vs-ruby-extent disagreement.
- The rate profile (58% aligned-single, 31% stem-prefix) now reads as
  linguistics: stem-prefix is dominated by okurigana — UniDic segments
  吹く as one token while the ruby base is 吹, so the base is a strict
  token prefix. These are the numbers ADR 0028's D7/D9 discussion should
  cite.
- gaiji (n=1,086, up from 2): 39% aligned-single, 30% conflict. Gaiji-base
  conflicts are expected where unresolved gaiji render as descriptions
  inside the plaintext while tokenizers split them arbitrarily — small N,
  flagged for a follow-up look, not blocking.

## Engine notes

- First launch died on a full `/home`: the vibrato dictionary cache
  (~1 GB) decompresses under `XDG_CACHE_HOME` by default. The workflow-run
  record captured `sample-aat`/`convert`/`render` as passed and `tokenize`
  as failed — but the recorded error was an opaque stdin `Broken pipe`.
  Fixed in this branch: `run-process!` tolerates the broken pipe so the
  child's exit code and stderr reach the error record; regression test
  pins it. Operational note: set `AB_VIBRATO_CACHE_DIR` somewhere roomy
  (`/db/soranoha/cache/vibrato` here).

## Follow-ups

1. Work-level (not file-level) sampling utility — unchanged from the first
   run's list; 407 file-level entries still include fragment files.
2. Gaiji-classification pass over the 329 gaiji conflicts (small, bounded).
3. First tokenizer-profile comparison (unidic-novel vs qkana/kindai-bungo)
   is now meaningful — same plan, different `tokenizer_dict`.
4. Regenerate dumps under store-pinned generator identity (unchanged; the
   repin dump metadata still records a dev-build adapter path).
