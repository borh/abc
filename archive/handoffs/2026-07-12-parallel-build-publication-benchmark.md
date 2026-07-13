# Parallel build-publication subset benchmark — 2026-07-12

- Spec: docs/superpowers/specs/2026-07-12-parallel-build-publication-design.md
- Plan: docs/superpowers/plans/2026-07-12-parallel-build-publication.md
- Harness: scripts/benchmark-build-publication.sh (repo root), one discarded
  warm-up per arm, median-of-5, methodology matching
  docs/handoffs/2026-07-11-annotation-join-overlap-benchmark.md.
- Host: farspark, 32 cores (`nproc`). Corpus = nix-pinned aozorabunko-src
  (commit 0e9ea3e, `/nix/store/5k95mnllvkjj8qpga9ssb3x3v84ivbwf-source`).
- Subset: `CARD_COUNT=30` card directories → **894 works selected**
  (from source-selection-report.json). Reduced from the plan's default 200
  because each sequential run is ~7.4 min; 30 cards keeps the full
  warm-up + 5×2 timed matrix near ~40 min total while still exercising 894
  independent works through both loops. Same subset used for both arms.
- Parallel arm resolved concurrency: 32 (`--concurrency 0` → all cores).

## Results

| arm | resolved concurrency | wall times (s) | median (s) |
|-----|----------------------|----------------|------------|
| sequential (`--concurrency 1`) | 1 | 442.67, 445.37, 447.32, 448.22, 443.55 | 445.37 |
| parallel (`--concurrency 0`) | 32 | 40.97, 41.33, 42.77, 42.92, 45.36 | 42.77 |

**Speedup: 10.41×.**

Byte-identity diff of one sequential vs one parallel output root (excluding
`build-plan.json`, `workflow-run.json`, `workflow-plan.json`): clean —
`outputs byte-identical`.

## Notes

- 10.4× on 32 cores is sublinear, as expected for this workload: each work
  spawns two external subprocesses (the aozora2html adapter and
  `ab-aat-to-parser-ir convert`), so per-work wall time is dominated by
  process startup and I/O rather than in-JVM CPU, and the fixed thread pool
  saturates on subprocess/OS scheduling well before 32× CPU scaling. The
  absolute win (7.4 min → 0.7 min on this subset) is the headline; the schema
  and static-hash caches (Tasks 4, 6) further reduce the per-work constant
  independent of concurrency.
- Byte-identity between the two arms confirms the parallelization and the
  atomic person-file write (Task 3) preserve deterministic output — the
  shared `persons/<id>.json` writers race harmlessly.
- Two script adaptations from the plan's draft, both host/flake correctness
  fixes, not methodology changes: build `.#apps.<system>.soranoha.program`
  (soranoha is a flake app, not a package, so `nix build .#soranoha` does not
  resolve it) and use `command -v time` rather than a hardcoded
  `/usr/bin/time` path.
- Full-corpus (17,879-work) validation run on hinoki is follow-up, not a gate
  for this change (spec §Benchmark and acceptance).
