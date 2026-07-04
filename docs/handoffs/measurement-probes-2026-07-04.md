# Measurement Probes: Bounded Worksets, Marker Registry, aozora2html Residuals

Date: 2026-07-04

Status: evidence note. These are bounded probes, not production
implementations.

## Summary

Three pre-decision tracks now have first measurements:

- Bounded-workset Nix evaluation is not killed by 100/1,000/5,000 selected
  works on a synthetic evaluator-only probe. Per-work derivations grow with the
  number of works; 100-work batches and a single requested-set/CAS-style
  derivation are effectively flat at this scale.
- The Aozora annotation manual extraction is mechanically bounded: the 13 live
  `annotation/*.html` pages produce 614 marker occurrences, 355 distinct raw
  markers, and 262 normalized marker templates. The registry problem is now
  curation and governance, not source discovery.
- Existing ab-validator residual reports are sufficient for the paper caveat:
  parser-IR conversion is corpus-clean, while aozora2html warigaki/kunten
  policy evidence remains lower-bound because adapter runtime/oracle residuals
  are still classified work.

## Probe Host

- Repository HEAD: `fdef008d9fc9afe0440483c04185a984aafbe97d`
- Nix: `nix (Nix) 2.34.7`
- Kernel: `Linux 7.1.2 x86_64 GNU/Linux`
- CPU: `AMD Ryzen 9 7950X3D 16-Core Processor`
- RAM: `98454132 kB`

This is a local workstation probe, not the CI runner required by ADR 0003's
acceptance criteria.

## 1. Bounded-Workset Nix Cost Probe

### Method

Temporary expression:
`/tmp/abc-bounded-workset-probe.nix`.

Command shape:

```sh
nix-instantiate --eval --strict --json /tmp/abc-bounded-workset-probe.nix \
  --arg n "$n" --argstr mode "$mode"
```

Each mode was warmed once, then measured three times with GNU `time` from
`/run/current-system/sw/bin/time`.

Modes:

- `per-work`: one tiny derivation per selected work.
- `batch`: one tiny derivation per 100 selected works.
- `cas`: one tiny requested-set/CAS-style derivation carrying the selected
  works as JSON.

Raw timing rows are in `/tmp/abc-bounded-workset-probe-reps.tsv`.

### Results

| Mode | Works | Derivations | Mean wall seconds | Wall range | Mean max RSS KB |
|---|---:|---:|---:|---:|---:|
| per-work | 100 | 100 | 0.213 | 0.21-0.22 | 96,531 |
| per-work | 1,000 | 1,000 | 0.237 | 0.23-0.24 | 104,025 |
| per-work | 5,000 | 5,000 | 0.383 | 0.37-0.39 | 139,889 |
| batch | 100 | 1 | 0.203 | 0.20-0.21 | 94,943 |
| batch | 1,000 | 10 | 0.207 | 0.20-0.21 | 96,169 |
| batch | 5,000 | 50 | 0.217 | 0.21-0.22 | 99,647 |
| cas | 100 | 1 | 0.203 | 0.19-0.21 | 95,925 |
| cas | 1,000 | 1 | 0.207 | 0.20-0.21 | 96,404 |
| cas | 5,000 | 1 | 0.217 | 0.21-0.22 | 101,501 |

### Interpretation

The per-work shape remains well under ADR 0003's 30 second / 2 GB envelope for
this synthetic evaluator-only probe, but it is the only shape whose evaluator
memory visibly grows with selected works. The batch and requested-set shapes
keep both derivation count and evaluator memory nearly flat through 5,000
works.

This does not decide ADR 0003. It does not measure:

- real manifest parsing,
- source snapshot input cost,
- cold build time,
- incremental rebuild after one changed work,
- per-work failure sidecar attribution,
- CI runner behavior.

The result is enough to keep Alternatives A, B, and C alive. The next
bounded-workset probe should use representative manifests and measure cold and
incremental builds. If Alternative C rebuilds the whole requested set on any
index change, it loses fine-grained invalidation despite the flat evaluator
cost.

## 2. Aozora Marker Registry Extraction Probe

### Method

Source: live Aozora annotation index at
`https://www.aozora.gr.jp/annotation/`.

The index resolved to 12 linked annotation-family pages plus the index page
itself, matching the expected 13 annotation pages:

- `annotation/index.html`
- `annotation/layout_1.html`
- `annotation/layout_2.html`
- `annotation/layout_3.html`
- `annotation/heading.html`
- `annotation/external_character.html`
- `annotation/kunten.html`
- `annotation/emphasis.html`
- `annotation/graphics.html`
- `annotation/etc.html`
- `annotation/duplication.html`
- `annotation/extra.html`
- `annotation/henkoten.html`

Temporary artifacts:

- fetched HTML: `/tmp/abc-aozora-annotation-probe/`
- summary: `/tmp/abc-aozora-annotation-probe/summary.txt`
- normalized summary:
  `/tmp/abc-aozora-annotation-probe/normalized-summary.txt`
- generated draft registry:
  `/tmp/abc-aozora-annotation-probe/draft-marker-registry.edn`

Extraction stripped HTML tags, read headings, found `［＃...］` marker strings,
and normalized quoted text, numbers, JIS coordinate references, Unicode code
points, and placeholder circles.

### Results

| Metric | Count |
|---|---:|
| pages | 13 |
| marker occurrences | 614 |
| unique page-local markers | 399 |
| unique global raw markers | 355 |
| normalized global marker templates | 262 |
| singleton normalized templates | 231 |
| multi-example normalized templates | 31 |

The 262-template normalized count matches the earlier
`aozora-manual-integration-audit.md` figure, which is a useful consistency
check.

Heuristic first-pass buckets over raw markers:

| Bucket | Raw markers |
|---|---:|
| emphasis | 93 |
| range/control | 71 |
| needs-curation | 54 |
| heading | 52 |
| targeted-inline | 32 |
| layout/indent | 27 |
| page/column | 26 |
| external-character | 20 |
| inline/layout | 19 |
| graphics | 5 |

### Interpretation

The generate-then-curate path is viable, but curation cost is not zero:

- source page citations and normalized marker templates are mechanical;
- the first draft can have 262 entries with `:status :needs-review`;
- if descriptions are mandatory before promotion, all 262 entries need
  description authoring or review;
- if descriptions can remain `NEEDS_REVIEW`, the first manual pass should
  focus on the 54 raw-marker heuristic bucket and on observed-only corpus
  constructs from the earlier audit.

The next design decision is file format/location and drift gate, not whether
the 13 pages can be harvested.

## 3. aozora2html Residual Bucket Characterization

No new full-corpus adapter run was needed. The existing ab-validator reports
already classify the residuals relevant to ABC's paper caveats:

- `../ab-validator/docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md`
- `../ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- `../ab-validator/docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- `../ab-validator/docs/superpowers/reports/2026-07-03-aozora2html-policy-residual-triage.md`
- `../ab-validator/reports/aat-fidelity/aozora2html-timeout-tail-report.md`
- `../ab-validator/reports/aat-fidelity/aozora2html-parse-incomplete-report.md`

Parser-IR conversion evidence:

| Corpus | Files | Result |
|---|---:|---|
| aozora-rs-adapter | 17,894 | 17,894 succeeded, 0 failed |
| aozora2html-adapter | 17,689 | 17,689 succeeded, 0 failed |
| combined | 35,583 | 35,583 succeeded, 0 failed |

Policy residual evidence:

| Family | adapter timeout/protocol | parse incomplete | other failed property | source feature without AAT observation | schema invalid/no AAT |
|---|---:|---:|---:|---:|---:|
| warigaki | 32 | 5 | 16 | 29 | 0 |
| kunten | 36 | 10 | 151 | 3 | 0 |

Other relevant measurements:

- residual policy union: 264 works;
- timeout-tail sample: 5 largest timed-out works, 2 completed within 300 s,
  3 still exceeded the 600 s measurement ceiling;
- parse-incomplete classification: 105 reports total, with 57
  `ruby_structural`, 30 `invalid_xhtml`, 17 `ruby_internal_error`, and 1
  `other`.

### Interpretation

For ABC's paper:

- It is safe to claim the measured AAT-to-parser-IR conversion is corpus-clean
  for the two admitted adapter/version tuples and mapping hash.
- It is not safe to claim aozora2html warigaki/kunten policy evidence is a
  complete corpus oracle. That evidence remains a lower bound because
  residual runtime, parse-completeness, visible-text order, and
  source-feature-vs-AAT-observation buckets remain.
- These residuals are adapter-fidelity work, not a parser-IR protocol blocker
  and not a reason to change ABC's admitted mapping registry entries.

## ADR Implications

- ADR 0002 remains Draft. The conversion reports are decision-quality evidence
  for the parser-IR mapping boundary, but ABC still needs to decide how parser
  candidate reports from ab-validator are cited as parser-selection evidence.
- ADR 0003 remains Draft. This probe removes the fear that bounded selected
  worksets are obviously too expensive for Nix evaluation, but it does not yet
  satisfy the build/incremental/CI acceptance criteria.
- ADR 0004 remains Draft and untouched. Release security is not affected by
  these probes.
- ADR 0005 remains Draft. The bounded-workset evidence reinforces splitting
  reproducibility materialization, operational orchestration, storage/packing,
  query runtime, API/service, and retention decisions before accepting a broad
  operational-runtime ADR.
