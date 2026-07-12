# Aozora Replay Harness — Design

Status: draft for review
Date: 2026-07-12
Prior spec: `2026-07-11-aozora-evolution-simulation-testing-design.md` (this is
its "replay harness over the real pinned upstream" Future Work item)

## Problem

The simulation harness proves abc's audit machinery against *synthetic*
histories with known ground truth. It cannot say what the machinery reports on
the *real* aozorabunko history — and today nothing pins that behavior. A
classifier or ingest change (or an upstream pin bump) can silently change what
the audit reports on real data, with no alarm and no reviewable diff.

The replay harness runs the real upstream history through the existing
`scan-history!` machinery and pins the findings as a committed baseline, so
real-data behavior changes surface as reviewable diffs — the same
alert-and-adjudicate protocol the known-divergences table established.

## Measured context (2026-07-12)

- The catalog ZIP (`index_pages/list_person_all_extended_utf8.zip`) is touched
  by **~4,900 upstream commits**, 2013-01 → present, near-daily (GitHub API).
- Year-sampled, that is **~14 representatives / ~13 pairs**; each pair costs
  roughly one corpus ingest + validation (estimate 2–10 min; measured
  precisely on the first real run). A full replay is an on-demand ~1–2 h job,
  not a CI job.
- The flake input `aozorabunko-src` is a **snapshot tarball — no git
  history**; replay requires a real clone. A full clone with content blobs is
  tens of GB; a blobless partial clone (`--filter=blob:none`) keeps commits
  and trees only, fetching blobs on demand.
- **Probe-verified (disposable experiment, 2026-07-12):** the project's JGit
  stack opens a partial clone (`repositoryformatversion=1`, promisor remote),
  `commits-touching-path` tree-walks it correctly (oldest→newest), and
  `write-blob-at!` reads CLI-prefetched blobs. An unfetched blob fails as a
  clean `MissingObjectException` — loud, not wrong.

## Design decisions

1. **Oracle: contract + baseline.** (a) Failure contract: the replay must
   complete without forbidden exception classes; every surviving pair
   produces a report. (b) A committed baseline of per-pair semantic digests;
   `--check` diffs against it and fails on change; changes are adjudicated by
   a human and re-pinned.
2. **Committed baseline tier: year-sampled, full history.** Finer sampling
   (month, windows) stays available as ad-hoc CLI flags whose output is never
   committed.
3. **Clone management: harness-owned partial clone** in a persistent cache
   dir, with CLI-git used for exactly two operations (clone/fetch, blob
   prefetch via promisor `cat-file`); everything else stays JGit through the
   existing audit code. `--aozora-repo` bypasses clone management entirely.
4. **Enforcement: pin-coupled cheap test.** A milliseconds `:unit` test
   asserts baseline integrity and `baseline pin == flake.lock pin`. A pin
   bump fails unit CI until the replay is re-run and the baseline
   re-committed. Code-change re-runs are procedural: a baseline update lands
   in the same commit as the change that caused it (snapshot-test
   convention).
5. **Robustness to real history: pre-validated representatives with pinned
   exclusions.** `scan-history!` is a sequential chain; one unreadable 2013
   artifact must not hold the whole harness hostage. Sampled representatives
   whose ZIP blob fails pre-validation are dropped from pairing and recorded
   in the baseline's `excluded` list with a reason — visible and adjudicated,
   never silent, never fatal.

## Architecture

New tool namespace `abc/src/abc/tools/aozora_replay.clj` (CLI shaped like the
other `abc.tools.*` CLIs, via `abc-cli/run-cli!`), composed of four units,
each testable alone:

1. **`ensure-clone!`** — effect; owns the cache-dir place.
   `git clone --filter=blob:none --no-checkout <remote-url> <cache-dir>` when
   absent; when present, first verifies provenance —
   `git remote get-url origin` must equal `remote-url`, else `ex-info`
   `{:cache-dir :expected-url :actual-url}` (the baseline must never record
   one source while replaying another) — then `git fetch origin <to-ref>`.
   Shells via argv vectors (no string interpolation). Failures throw
   `ex-info` with `{:cache-dir :remote-url}` and the cause chained.
2. **`plan`** — pure given repo state. Calls the audit namespace's scan
   planning to enumerate sampled representative refs for
   `{:zip-path :sample-period :from-ref :to-ref}`. **Production touch:** the
   currently-private planning in `aozora_history_audit.clj`
   (`history-scan-refs` + `sample-commits-by-period`) is promoted to one
   public fn `scan-plan` `[repo opts] → [ref-string …]`; `scan-history!` is
   refactored to call it. No behavior change (pinned by existing unit + sim
   tests P10–P12).
3. **`prefetch-and-prevalidate!`** — runs identically for the managed cache
   and `--aozora-repo` (a supplied repo may itself be partial, shallow, or
   incomplete — completeness is never assumed; the flag only skips
   `ensure-clone!`). For each planned ref, outcomes are tiered by CAUSE,
   never lumped:
   - **Path absent in the tree at that ref** (JGit TreeWalk finds no entry):
     a source fact → exclusion, reason `"missing-at-ref"`.
   - **Object promised but locally unavailable** (JGit
     `MissingObjectException`): attempt one CLI promisor fetch
     (`git cat-file blob <ref>:<zip-path>`); if the object is then readable,
     continue. If the fetch fails or the repo has no promisor remote →
     **environment failure, loud** `ex-info`
     `{:ref :zip-path :repo :cause-tier :missing-local-object}` — never an
     exclusion, because recording it as history would silently change the
     sampled chain.
   - **Blob readable but malformed as source data** (bytes don't open as a
     ZIP / no `.csv` entry / zero data rows): a source fact → exclusion,
     reason `"unreadable-zip"` / `"no-csv-entry"` / `"no-data-rows"`.
   Exclusions are recorded as `{"ref" r "period" p "reason" …}`; only these
   source-fact exclusions may affect pairing.
4. **`replay!`** — runs `scan-history!` over the surviving refs (pairs are
   consecutive sampled representatives), digests each pair, assembles the
   baseline document, and either writes it (`--update`) or diffs it against
   the committed one (`--check`).

**Second production touch (required by exclusions):** `scan-history!` plans
its refs internally, so excluded representatives would re-enter the scan.
It gains an optional `:refs` key — an explicit, already-planned ref vector
that bypasses internal planning (pairing, reporting, and work-dir semantics
unchanged). When `:refs` is absent, behavior is byte-identical to today
(pinned by the existing unit + sim tests); the replay always passes `:refs`
= the surviving plan.

### Baseline document

Committed at `abc/test/resources/aozora-replay-baseline.json`, written with
the deterministic JSON writer. No timestamps, no filesystem paths.

```json
{
  "baseline_format": 1,
  "remote_url": "https://github.com/aozorabunko/aozorabunko.git",
  "pin_rev": "<40-hex from flake.lock>",
  "zip_path": "index_pages/list_person_all_extended_utf8.zip",
  "sample_period": "year",
  "excluded": [ { "ref": "…", "period": "2013", "reason": "no-csv-entry" } ],
  "pairs": [
    {
      "previous_ref": "…", "current_ref": "…", "period": "2014",
      "status": "ok | validation_failed",
      "drift_summary": { …verbatim the pair's drift report "summary" map… },
      "ingest": {
        "works_written": 0, "works_skipped": 0,
        "skipped_work_ids": ["…"],
        "persons_written": 0,
        "person_conflicts": ["<person_id>", "…"]
      }
    }
  ]
}
```

- `drift_summary` is the classifier's own deterministic count map
  (split/merge candidates, ambiguous replacements, metadata corrections,
  edge/person/work counts) — pinned verbatim, not re-derived.
- `validation_failed` pairs are *recorded*, never hidden: a period whose
  current corpus fails validation is itself a pinned fact to adjudicate.
- `person_conflicts` carries the conflicted person ids (from the D4
  `:person-conflicts` policy), `skipped_work_ids` the per-pair guard skips —
  both expected empty on recent history (measured: 0 on 2017-11 and on the
  pinned snapshot), both informative on early history.

### `--check` diff classification

`--check` exits 1 on any difference and prints a per-pair classification
(`unchanged | replaced | added | removed`) plus exactly one verdict. The
verdicts are defined over input refs vs output digests, so an output change
on unchanged inputs can never masquerade as a pin bump:

- **configuration-change** — `baseline_format`, `zip_path`,
  `sample_period`, or `remote_url` differ. Never expected implicitly;
  requires a deliberate, explained baseline rewrite.
- **pin-bump-shaped** — requires ALL of: (a) `pin_rev` changed; (b) every
  common pair byte-identical, except that the final pair of the old baseline
  may be `replaced` ONLY in the strict form: same `previous_ref`, different
  `current_ref` (the final period gained a genuinely newer representative) —
  a digest change on an unchanged `(previous_ref, current_ref)` input is
  NEVER pin-bump-shaped; (c) `added` pairs appear only after that point;
  (d) `excluded` gains entries only for periods newer than the old
  baseline's last period. Expected after a pin bump with no code change.
- **behavioral-change** — everything else. In particular: any pair whose
  input refs are unchanged but whose digest differs (abc code now reports
  differently on identical real inputs), any change at all while `pin_rev`
  is unchanged (code change or nondeterminism — both demand investigation),
  or exclusions appearing for historical periods. Requires adjudication in
  the changing PR.

### Enforcement test

`abc/test/abc/tools/aozora_replay_test.clj` (`:unit` suite, milliseconds):

- baseline parses; `baseline_format` known; pairs sorted by period;
  refs are 40-hex; digest fields present and shaped.
- `pin_rev` equals `nodes.aozorabunko-src.locked.rev` parsed from the
  **authoritative lock: `abc/flake.lock`** (JSON — not regex over
  `flake.nix`). This is the lock of the flake that declares
  `aozorabunko-src` and whose recipes the replay uses; the CLI's default
  `--to-ref` reads the same file, so test and tool cannot diverge.
- the root `flake.lock` (`../flake.lock` from the test's cwd) carries the
  SAME `aozorabunko-src` rev — a partial pin bump (one lock, not the other)
  fails here with a message naming both files, instead of the root-built
  environment quietly using a different catalog than the baseline pins.

### CLI and recipes

Flags: `--check` / `--update` (exactly one required), `--sample-period`
(default `year`), `--to-ref` (default: the flake.lock pin), `--from-ref`,
`--cache-dir` (default `$XDG_CACHE_HOME/abc/aozorabunko.git`, falling back to
`~/.cache/abc/aozorabunko.git`), `--remote-url` (default the GitHub URL),
`--aozora-repo` (use an existing clone; disables only `ensure-clone!` —
prefetch/pre-validation always run, unit 3), `--baseline` (default the
committed path), `--work-dir`
(tool-owned, audit semantics). Non-default sampling/window flags refuse
`--update` of the default baseline path (ad-hoc runs write wherever
`--baseline` points, never the committed file).

Root justfile: `replay-aozora` (= `--check`) and `replay-aozora-update`.

## State, time, identity

- The cache clone is the design's one mutable shared place. Single-runner by
  convention; concurrent replays contend on git locks and are unsupported
  (documented, not locked — YAGNI).
- The baseline's identity spans three time axes — upstream pin, sampling
  plan, abc code. The document pins the first two explicitly (`pin_rev`,
  `sample_period`, `excluded`, the ref pairs); the third is attributed
  procedurally: baseline updates land in the same commit as the code change
  that caused them, and the diff classifier separates pin-bump-shaped from
  behavioral changes.
- Upstream history rewrites (force-push) would orphan baseline refs: replay
  fails loudly on missing refs; the remedy is adjudicating a fresh baseline.

## Failure handling

Two-tier, same taxonomy as everything else:

- **Environment tier (loud):** clone/fetch/network failures, cache
  provenance mismatch, promised-but-unavailable local objects (after one
  promisor fetch attempt — see unit 3), baseline file unreadable, and
  forbidden exception classes escaping `scan-history!` — normatively the
  prior spec's §Failure Taxonomy list: `NullPointerException`,
  `AssertionError`, `StackOverflowError`, raw `java.util.zip.ZipException`
  (on real data such an escape is a new bug find — welcome). All `ex-info`
  with diagnostic keys (`:cache-dir`, `:remote-url`, `:ref`, `:baseline`)
  except the forbidden classes, which propagate as themselves. If the scan
  itself hits a missing path at a ref, that is a harness invariant violation
  (pre-validation should have excluded or aborted first) — also loud.
- **Source-fact tier (absorbed + pinned):** pre-validation source facts
  (path absent at ref, malformed/empty catalog bytes) become `excluded`
  entries; per-work ingest faults inside a pair are already absorbed and
  surface in the digest (`works_skipped`, `skipped_work_ids`); validation
  failures surface as `status: validation_failed`.

## Testing

- **Pure units** (fixture-driven, `:unit`): digest shaping from a fixture
  pair report; baseline compare + diff classification covering each verdict
  boundary — unchanged; pin-bump-shaped (strict final-pair form: same
  `previous_ref`, new `current_ref`, plus appends); behavioral-change for a
  digest change on unchanged refs, for any change with unchanged `pin_rev`,
  and for a historical-period exclusion; configuration-change for each
  header field; flake.lock pin parsing (both locks, agreement and
  divergence fixtures); pre-validation tiering on crafted inputs (valid
  zip, non-zip bytes, zip without `.csv`, header-only CSV, path absent at
  ref vs missing local object).
- **Plumbing integration** (`:unit`, seconds, no network): a tiny local repo
  built with the sim render substrate (`sim-render/commit-zip-at!` etc. — the
  same substrate the audit unit tests use), replay run end-to-end with
  `--aozora-repo` and a temp `--baseline`: `--update` writes a well-formed
  baseline; immediate `--check` passes; a re-run after one more upstream-like
  commit classifies as pin-bump-shaped; a doctored baseline classifies as
  behavioral-change; `ensure-clone!` against a cache whose `origin` URL
  differs from `--remote-url` throws the provenance `ex-info`.
- **`scan-plan` extraction + `:refs` option safety:** existing audit unit
  tests + sim P10–P12 pin the no-`:refs` path across the refactor; the
  plumbing integration test exercises the `:refs` path end-to-end, and one
  audit unit test asserts `:refs` produces the same pairs as internal
  planning on the same repo.
- **The real replay run is the acceptance test** (below); it is never part of
  any test suite.

## Acceptance criteria

1. First real run completes: `just replay-aozora-update` from a cold cache —
   recording (in the PR description) clone size, plan/walk time, per-pair
   time, and any `excluded` entries with reasons (settles assumptions A5/A6
   from the design review).
2. The produced baseline is human-reviewed pair-by-pair (this review is the
   adjudication of what the classifier historically reports on real data)
   and committed.
3. `just replay-aozora` immediately after: green (same-machine determinism).
4. `:unit` suite green, including the pin-coupling test; the pin-coupling
   assertion demonstrably fails on a fixture with a mismatched rev (tested
   via fixture, not by editing flake.lock).
5. `scan-history!` behavior unchanged: full `:unit` + `:simulation` suites
   green after the `scan-plan` extraction.

## Out of scope

- Month-sampled committed baseline, nightly/scheduled runs (hinoki), and any
  automation that re-runs replay on classifier changes (procedural for now).
- Drift-sidecar interplay and content-side evolution (separate Future Work
  items with their own specs).
- Any change to what the classifier reports; the replay pins, it does not
  judge.

## Prior art

Snapshot/characterization testing against pinned external data: Jest/insta
snapshot conventions (updates are reviewed diffs; snapshots minimal and
time-free) and Rust crater-style runs (regressions classified by humans).
Both map onto choices here: digests instead of full reports, no wall-clock in
the artifact, alert → adjudicate → re-pin.
