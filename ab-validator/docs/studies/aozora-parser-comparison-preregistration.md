# Aozora Parser Neutral Comparison Preregistration

Status: Frozen before refreshed measurements

Study ID: `aozora-parser-neutral-comparison-2026-07`

Machine contract: `aozora-parser-comparison-preregistration.json`

Schema: `../../schemas/parser-comparison-study.schema.json`

## Purpose and boundary

This study characterizes existing Aozora parsers and supplies falsifiable
baselines for Soranoha's project-owned parser. It cannot decide parser
ownership, admit a parser/adapter/mapping tuple, or grant publication authority.
The custom parser receives no pass from ownership. Native behavior and behavior
introduced by Soranoha adapters are separate result lanes.

The machine contract is normative for research questions, candidate rules and
revisions, corpus pins, modes, denominators, outcome/missingness rules, axes,
uncertainty, and analysis. The signed-off hashes cover both representations and
the schema.

## Candidate inventory fixed before results

The bounded ecosystem survey identified `aozora`, `aozora2`, `aozora-rs`,
`aozora2html`, `aozora-epub3`, and `aozora-parser.js`. The first five have
reproducible pins and are included even if they later fail to build or run.
`aozora-parser.js` is retained as an explicit exclusion because its pinned
source lacks a dependency lock and cannot currently produce a reproducible
executable build. Downloaders, text cleaners, tokenizers of already-stripped
text, and parser-identical forks are excluded by role rather than observed
quality. `ab-aozora` is the separately identified custom baseline and will be
measured in the shared-instrument appendix.

## Frozen procedure

All official notation vectors at the pinned revision and all valid primary
text members from the pinned Aozora Bunko snapshot form the corpora. No work or
vector may be removed after parser output is inspected. Each included parser is
run first in its native output mode and then through the pinned adapter. A
native failure cannot be hidden by adapter fallback, and adapter-derived
features cannot be credited as native capability.

Counts retain failures, 300-second timeouts, explicit unsupported outcomes,
and silent drops in their declared denominators. Missing values are never
imputed. Owned-contract fields without a competitor analogue are
non-comparable, rather than competitor zeros. Exact counts accompany rates;
Wilson intervals and seeded bootstrap performance intervals are reported as
specified in the machine contract.

Results are published per axis: construct coverage, fidelity, robustness,
diagnostics, spans, performance, maintenance, packaging, and license. An
unqualified aggregate score or overall winner is prohibited. Any weighting
must name its downstream use case and publish the registered sensitivity
analysis.

Malformed-input robustness uses every case in
`fixtures/parser-comparison-robustness-v1.json`; diagnostic scoring uses every
prelabelled case, severity, and UTF-8 byte span in
`fixtures/parser-comparison-diagnostics-v1.json`. Their hashes and exact
per-parser/per-mode inclusion rules are frozen in the machine contract.

Performance uses `fixtures/performance-largest-six-v1.json`, selected by
descending uncompressed size with ascending work-ID tie breaking. Each
executable existing parser runs in the registered candidate and work order with
one warm-up and five measured repetitions, single-process concurrency, the
registered x86_64-linux root-flake environment, networking disabled, and a
300-second limit. Timeouts are right-censored at 300 seconds and also counted
as timeout failures; they are never discarded from denominators or latency
summaries.

## Integrated baseline observation

Commit `ac2be926738f919faf44300e2999b3548d724297` is an ancestor of the
then-current integrated main `36bf29005719b58fe744b7c96d3a45e451f6c943` and
is frozen as the custom-parser comparison baseline. On 2026-07-14 the
repository's Nix cargo check, clippy, and formatting checks passed, as did the
focused Phase 4 checkpoint verifier test suite. These observations establish a
build-clean measurement revision only; they do not establish comparison,
admission, or release results.

Verification exposed pre-existing whitespace damage after the original Phase
4 integration; mechanical repair commit `6f95f62140d978ded5f1f7ae78f2dab5da0d5ed8`
restored the formatter-clean baseline without changing the frozen custom-parser
revision or its behavior. Documentation-only repair
`3683aaf413a6ffaa2ae08f65dc021ed51b0f348a` restored the clean clippy gate.

Refreshed measurements may start only from a commit later than the commit that
introduces this preregistration and its checked hash manifest.
