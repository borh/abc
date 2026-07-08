# Handoff: Aozora parser comparison → fork/build decision

**Status:** handoff for a fresh session. Self-contained; read top to bottom.
**Branch:** `feat/parser-comparison-faithful` (worktree `.worktrees/parser-comparison`,
**unmerged**, 8 commits off `main`@`82f85ee`). Working tree clean.
**Prereqs:** work in a worktree (memory `work-in-worktrees`); cargo builds churn
`adapters/aozora-rs/Cargo.lock` → `git checkout --` it before committing.

## What this session did

Answered the research question **"which existing Aozora parser should a new/forked
Rust parser be based on, and how much of real markup can each represent?"** — as a
publishable-draft study measuring 5 parsers three complementary ways plus
performance, on a verified-fair basis.

**Main deliverable:** `docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`
(read this first — it consolidates everything with methodology + threats-to-validity).

## Headline findings (all in the study)

1. **No parser passes all 25 `must` conformance vectors.** A new/forked parser is
   empirically warranted; coverage across parsers is complementary, not nested.
2. **Fair frequency-weighted corpus coverage** (how much of *real* markup, over
   ~17,800 works, normalized + fairness-audited): **aozora 0.972 > aozora-epub3 0.940
   ≈ aozora-rs 0.937 > aozora2 0.840 > aozora2html 0.745.** ruby is ~90% of mass.
3. **Recommendation: base the new parser on `aozora-pipeline`** (the reference
   parser). It leads on *every* axis — corpus coverage (0.972), conformance breadth
   (22/25 must), and speed (1.25 s, no timeouts) — is actively maintained, MIT/Apache.
4. **`aozora-core` (aozora2)** is trivially forkable + ~95% capable BUT
   **catastrophically slow** (16.9 s median, 2/6 timeouts) — undercut on two axes.
5. **`aozora-rs`** is fastest (~90×) and strong on corpus coverage, but weak
   conformance (a headerless-vector artifact) and real heading/tcy gaps; candidate
   only if throughput dominates.
6. **`aozora2html`'s low coverage is *robustness*, not fidelity** — it fails on ~193
   ruby-heavy works; its per-work ruby fidelity is ~1.0.

## Key correctness lessons (do not regress)

- **Measurement must be normalized across adapter representation variants** or it is
  nonsense: ab-aozora emits ruby ~99% as `raw`+`x-source-marker-kind` (naive
  `kind=="ruby"` → reference parser scores ~0 on ruby); aozora2html emits boten as
  `sesame_dot`/dot-variants, bousen as `underline_solid`, headings as `unmapped-h4/h5`.
  Signatures are derived from a **full-corpus vocabulary audit**
  (`2026-07-08-adapter-aat-vocabulary-audit.txt`), not guesses.
- **This normalization belongs in the AAT contract** (canonical `style_type` enum;
  typed-node ≡ marker-kind). It also shrinks downstream IR `AMBIGUITY` sidecars —
  see the `bouten` AAT→IR→TEI trace (study §2).
- **Corpus coverage conflates fidelity and robustness** — separate them (see below).
- **P4suta is corroborating, not authoritative** (official 青空文庫 docs + works are).

## Artifacts (all `docs/superpowers/reports/`, 2026-07-08-*)

- `aozora-parser-comparison-study.md` — the study (start here)
- `parser-conformance-comparison.md`, `aozora-notation-spec-comparison.{md,summary.json}`
- `parser-fork-candidacy-faithful-comparison.md`, `parser-divergence-attribution.json`,
  `aozora-rs-core-capability.json`
- `official-docs-seed-comparison.md`, `parser-performance-sample.{md,json}`
- `normalized-corpus-coverage.json`, `adapter-aat-vocabulary-audit.txt`

**Tooling** (`reports/parser-conformance/` + `reports/aat-fidelity/`):
`run-aozora-notation-spec.py` (AAT scoring, extended this session),
`attribute-divergences.py`, `measure-aozora-rs-core.py`, `author-official-seed.py`,
`normalized-corpus-coverage.py`, `ruby-coverage-per-work-diagnostic.py`.
**Adapter change:** `adapters/aozora-rs/src/dump.rs` + `--mode retokenized`
(measurement-only; default `--mode aat` unchanged).

## Open work (prioritized) — from study §8

1. **Split coverage into fidelity vs robustness.** Recompute corpus coverage over
   the **intersection** of works *all* adapters completed (pure fidelity), and report
   per-adapter work-completion rate separately (robustness). This is the biggest
   remaining fairness refinement — it lifts aozora2html's fidelity to its true ~1.0.
2. **Expand the perf sample** beyond 6 works and pinpoint aozora-core's timeout
   pathology (which inputs; is it a cheap fix — if so aozora-core re-enters contention).
3. **Normalize the AAT `style_type`/marker contract** (canonical enum; typed≡marker)
   — improves measurement *and* the live pipeline.
4. **Expand the official-docs seed** (§4.5) from 11 clean cases to edge cases + precise
   spans (a full independent instrument, not a seed).
5. **Deferred:** repair the aozora-rs adapter's typed AAT projection so the production
   adapter reflects the parser (memory `aozora-rs-adapter-repair-deferred`).

## Immediate next decisions

- **Bank the branch** (merge to main or PR) — it is a coherent, self-contained
  research deliverable. Use `finishing-a-development-branch`.
- Then either **close the §8 fairness items** (esp. #1, the fidelity/robustness split),
  **or begin the parser design** (`brainstorming` → `writing-plans`) on the
  `aozora-pipeline` base — noting the acceptance gate is bigger than conformance
  (the 2026-07-06 admission criteria: parser-IR validity, publication accounting,
  zero unknown-markup, perf).

## Environment / data locations

- Conformance vectors: `nix build .#upstream-aozora-notation-spec`/conformance/vectors (127).
- Full-corpus AAT (per adapter, ~17,800 works each):
  `/db/ab-validator/aat-corpus/{aozora,aozora2,aozora2html,aozora-epub3}-full-*/aat/<adapter>/*.json`
  and `/db/ab-validator/fidelity-corpus/{aozora2,aozora-rs}/aat/<adapter>/*.json`.
- Coverage denominators (source occurrences): `2026-07-08-corpus-adapter-fidelity.summary.json`.
- Perf: corpus = nix `aozorabunko-corpus`; index = `/db/ab-validator/aat-corpus/index.json`;
  recipe `just parser-performance-all-parsers` (needs INDEX, CORPUS; SAMPLE/LIMIT_S envs).
- Reference parser needs `AB_AOZORA_BIN`; aozora2html needs gem home; epub3 needs the
  jar — the `aozora-notation-spec-comparison` recipe wires all of these.
- `python3` on this box has no `duckdb`; the analysis scripts are plain python+json.
