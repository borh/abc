# Content-Side Evolution Simulation — Design

Date: 2026-07-12
Status: approved design, pre-plan
Parent: `2026-07-11-aozora-evolution-simulation-testing-design.md` (Future
Work: "content-side evolution simulation"), scoped per user decisions to the
cross-snapshot reuse core.

## Problem

The content pipeline (`cards/NNNNNN/files/*.zip` →
`abc.tools.soranoha-build-publication` → per-work materialized sources and
publications) has only single-state, single-work fixture tests
(`abc/test/abc/tools/soranoha_test.clj`). Nothing verifies the pipeline's
*evolution* semantics across snapshots:

- byte-identical content is reused, not rebuilt (no spurious rebuild);
- changed content is rebuilt, never carried forward stale (no stale reuse —
  the silent-wrongness failure class);
- hash pins (`official-source.json source_hash`,
  `source_work_content_hash.txt`) stay consistent with the actual ZIP bytes;
- integrity faults fail loudly at their documented boundaries.

## Scope decisions (user-selected)

1. **SUT slice: cross-snapshot reuse core.** Simulate evolving content trees
   and drive `build-publication` with a stubbed parser; assert
   skip/reuse/rebuild against a hash oracle and pin-chain faults. The
   `snapshot-index` and `workflow.cache` validators stay future work.
2. **Model: extend the existing sim model.** Content state joins the
   generated catalog coherently (テキストファイルURL basename join). Unlike
   the drift-sidecar work (observer layer), content is the *subject* here, so
   touching `abc.sim.model`/`abc.sim.gen` is justified — additively.
3. **Faults: sampled** (4 representative integrity faults), same contract
   style as the drift-sidecar fault work.
4. Sim coverage only: no production namespace changes. Divergences found are
   recorded in `abc.sim.divergences/table` (D7+) and adjudicated as separate
   work.

## Verified key facts (design rests on these)

1. **Invocation.** `build-publication!`
   (`soranoha_build_publication.clj:694-724`) is callable as a function;
   `parse-args` requires `--aozora-root --config --output-root`, and
   `--snapshot-date` is checked inside `build-publication!`. If the
   output-root already exists it must be re-run with `--replace`, and the
   existing directory becomes `prior-output-root` (self-referential prior
   cache). The build runs in a temp sibling dir and is atomically promoted
   (`prepare-output-root!`/`promote-output-root!`, lines 524-540).
   `--aozora-root` is a **plain directory**; git provenance is best-effort
   (falls back to slurping `.git/HEAD`; `aozora_git_commit` may be nil).
2. **Reuse gate.** `publication-up-to-date?` (lines 545-553) requires
   `tei.manifest.json` present AND `source_work_content_hash.txt` content
   `=` the work's `source_hash` = **sha256 of the raw ZIP file**. Statuses
   from `materialize-one-publication!` (564-601): `"skipped"` (current
   pub-dir already up-to-date — unreachable in normal runs because pub-dirs
   are built in a fresh temp root), `"reused"` (prior pub-dir up-to-date →
   flat files copied), `"passed"` (fresh build; marker written after),
   `"failed"` (only with `continue_on_failure`; otherwise rethrow).
3. **Selection.** Catalog read from
   `index_pages/list_person_all_extended_utf8.zip` (first `.csv` entry).
   Join rule: work-zip **basename** must equal the basename of some row's
   テキストファイルURL; candidate relpath must match
   `^cards/[0-9]{6}/files/[^/]+\.zip$`. Rejection reasons:
   `not-under-cards-files`, `not-catalog-text-zip`, `not-selected`. Empty
   selection throws `"no catalog-backed work ZIPs were successfully
   derived"`. `catalog-index` maps basename→row with **last row winning**
   on duplicate basenames; the winning row supplies `作品ID`/`人物ID` for
   the slug `<work-id>_<person-id>_<zip-stem>`.
4. **Stub point.** `*derive-parser-ir!*` (lines 266-269) is called with
   `{:parser-profile :source-bytes :aat-file :parser-ir-file
   :divergence-file}` where `:source-bytes` is the **first `.txt` member**
   of the ZIP (central-directory order, `windows-31j` entry-name charset).
   A ZIP with no `.txt` member throws clean
   `ex-info "work ZIP contains no .txt member" {:path}`. Structurally
   corrupt ZIPs fall back to a `7zz` subprocess — environment-dependent, so
   the sim never injects structural corruption.
5. **Predicted divergence (D7 candidate).** The real adapter chain sets
   parser-IR `source.work_content_hash` to sha256 of the **member bytes**:
   `ab-aozora-aat` hashes the bytes it decodes
   (`ab-validator/crates/ab-aozora-aat/src/lib.rs:239`) and
   `ab-aat-to-parser-ir` copies `meta.source_hash` into
   `source.work_content_hash`
   (`ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs:2149`). But
   `official-source.json source_hash` is sha256 of the **raw ZIP file**, and
   `snapshot-input` (`materialize_source_snapshot.clj:44-84`) throws
   `"official-source source_hash differs from parser-IR work_content_hash"`
   unless they are equal. Container hash ≠ member hash, so composing
   `build-publication` → `source-snapshot-workset` →
   `materialize-source-snapshot!` should **always throw** on real outputs.
   Existing tests never compose the two (each uses its own internally
   consistent fixture; the build-publication test stub writes a fixed fake
   hash and never runs `snapshot-input`).
6. **Config.** `--config` must validate against
   `schemas/soranoha-publication-build-config.schema.json`
   (`additionalProperties: false`, all seven keys required). Only
   `parser_profile` and `continue_on_failure` are consumed by build code;
   with the stub bound, `parser_profile` is never resolved to a real
   adapter.

## Architecture

Generated model histories gain a content axis. A state is rendered to a
plain temp directory shaped like an aozora-root (catalog ZIP + card trees +
decoys + fake `.git/HEAD`). `build-publication!` runs against it with a
**realistic stub** for `*derive-parser-ir!*`. Evolution = render a later
state of the same history into a fresh aozora-root and re-run
`build-publication!` with `--replace` into the same output-root, so the
previous output becomes the prior cache. Oracles predict selection, per-slug
statuses, and all hash pins from the model alone.

**Realistic-stub stance.** The stub mimics the real adapter: it writes
`work_content_hash = sha256(:source-bytes)` (member-bytes hash), plus the
schema-required fields copied from the existing test stub
(`soranoha_test.clj:77-104`). A "conforming" stub that wrote the raw-ZIP
hash would model an adapter that does not exist and would mask the
member-vs-container divergence (Key Fact 5). Consequence: the pin-chain
property's desired behavior fails under current behavior and is gated as
divergence **D7**.

## Components

### 1. `abc/test/abc/sim/model.clj` (modify — additive)

- New state key `:contents`: `sorted-map` of `wid → {:text <string>}`.
  `bootstrap` initializes it empty. The rendered zip name is derived, not
  stored: `<wid>_t.zip`, member `<wid>.txt`, so basenames are unique per
  work by construction.
- Three new total events (no-op when preconditions fail, like all events):
  - `:add-content {:wid :text}` — applies iff work exists and has no
    content;
  - `:edit-content {:wid :text}` — applies iff content exists and `:text`
    differs;
  - `:remove-content {:wid}` — applies iff content exists.
- `:remove-work` also removes the work's content entry.
- New invariant in `check-invariants!`: every `:contents` key is a key of
  `:works`; every `:text` is a non-blank string containing the work's wid
  (uniqueness of member bytes across works, needed by the stub and oracle).

### 2. `abc/test/abc/sim/gen.clj` (modify — additive)

- Generated `:text` values embed the wid (e.g. `"作品<wid> 本文…<suffix>"`)
  so member bytes are unique per work and edits are guaranteed to change
  bytes.
- `:add-content` and `:remove-content` join the benign mix at moderate
  weight. `gen-add-content` only targets states where the number of
  content-bearing works is below a cap (default 4) — a generator-side
  bound, not a model rule — to keep the real `materialize-publication!`
  cost per run small.
- **`:edit-content` is seeded-only — it never appears in the benign mix.**
  This makes `find-applied :edit-content` locate the seeded edit uniquely
  in the applied log (same mechanism as the forced drift intents), which
  P16.2 uses to place its `s-before` checkpoint.
- `history-gen` gains no new required knobs; the content cap is a
  `:content-cap` option with default 4.
- A `content-history-gen` (thin wrapper over `history-gen`) seeds the
  shape the evolution property needs: two `:add-content` events on
  distinct bootstrap works early in the event list, and one
  `:edit-content` on the **first** of them later in the list — so that at
  the moment the edit applies, the second work's unchanged content is
  positioned to be `"reused"` while the edited work is `"passed"`. Benign
  events (rare `:remove-work`/`:remove-content`) may still invalidate the
  shape — which is why applicability is measured by ratio counters
  (≥ 9/10), not assumed.

### 3. `abc/test/abc/sim/render.clj` (modify — additive)

- New header テキストファイルURL appended to `headers` (44 columns), blank
  for works without content. For a content-bearing work the value is
  `https://www.aozora.gr.jp/cards/<card-pid>/files/<wid>_t.zip` where
  `card-pid` = the **smallest pid over all of the work's edges** in the
  projection (deterministic; shared with the oracle).
  Existing catalog-side sim tests are unaffected: no CSV bytes are pinned
  in-sim (the replay baseline pins real history, a separate artifact), and
  ingest ignores unknown columns.
- `text->zip-bytes [text wid]` — deterministic ZIP bytes (single member
  `<wid>.txt`, UTF-8 text bytes, fixed mtime 0), same construction
  discipline as `csv->zip-bytes`.
- `write-aozora-root! [dir state]` — writes into a plain directory:
  - `index_pages/list_person_all_extended_utf8.zip` (existing CSV render);
  - `cards/<card-pid>/files/<wid>_t.zip` for every content-bearing work in
    the projection;
  - two deterministic decoys: `cards/999999/files/decoy.zip` (valid path,
    basename not in catalog → `not-catalog-text-zip`) and
    `support/tools.zip` (→ `not-under-cards-files`);
  - `.git/HEAD` containing `"sim-fixture-head\n"`.

### 4. `abc/test/abc/sim/oracle.clj` (modify — additive)

- `content-hash [state wid]` — `sha256` of `(text->zip-bytes text wid)`
  formatted `sha256:<hex>`; the prediction is independent because the SUT
  hashes the file it finds on disk.
- `card-pid [projection wid]` — smallest pid over the work's edges (same
  rule as render).
- `expected-selection [state]` →
  `{:selected [{:work_id :person_id :slug :text_zip_relpath :source_hash}]
    :rejected [{:path :reason}]}`.
  Selected = content-bearing works in the projection. `:person_id` is
  derived from the **shared pure row projection**: the oracle calls
  `render/model->rows` (the same function that produces the CSV the SUT
  reads) and applies `catalog-index`'s documented last-row-wins reduction
  — reduce rows in order into a basename→row map, take the winning row's
  人物ID. The oracle never reconstructs the row ordering itself, so a
  future change to `model->rows` serialization cannot make render and
  oracle disagree for accidental reasons. Rejected always contains the two
  decoys and the catalog ZIP itself (`not-under-cards-files`).
- `expected-statuses [prev-state cur-state]` → map of `slug → "reused" |
  "passed"` over current-state slugs: `"reused"` iff the same slug existed
  in `expected-selection prev-state` **and** the zip bytes are identical
  (same text, same wid); otherwise `"passed"`. Slugs absent from the
  current selection are absent from the map. `"skipped"` is never
  predicted; the property asserts its count is 0.

### 5. `abc/test/abc/sim/content_sim_test.clj` (new)

Namespace `abc.sim.content-sim-test` (matches the `-sim-test` suite
pattern). Contains:

- `realistic-stub` — writes `aat.json`/`divergence.json` as in the existing
  test stub, and `parser-ir.json` with
  `["source"]["work_content_hash"] = (hash/format-sha256 (hash/sha256-bytes
  source-bytes))` plus the schema-required fields.
- `write-config! [dir continue-on-failure?]` — schema-valid config JSON.
- `run-build! [aozora-root out-root snapshot-date replace?]` — binds the
  stub, calls `build-publication!` with the flag vector, returns the parsed
  reports (`source-selection-report.json`,
  `publications/publications-report.json`).
- Properties P16.1–P16.4 below, using `harness/check!`, ratio counters, and
  the forbidden-throw taxonomy.

### 6. `abc/test/abc/sim/divergences.clj` (modify)

- New entry **D7** (`:open`): "build-publication parser-IR
  `work_content_hash` (member-bytes hash, per the real adapter chain) can
  never equal `official-source.json source_hash` (raw-ZIP hash), so
  `materialize-source-snapshot!` over a build-publication materialized root
  always throws `official-source source_hash differs from parser-IR
  work_content_hash`. Desired: the two tools compose; which hash is
  canonical is an ADR-level adjudication." Gated with
  `div/expected-failure*`.

## Properties

- **P16.1 build** — generate a history, take its final state, render, build
  (fresh output-root, `continue_on_failure false`).
  - If `expected-selection` is empty: assert the build throws the clean
    `ex-info "no catalog-backed work ZIPs were successfully derived"` and
    count the run as inapplicable (ratio counter; applied ratio ≥ 9/10
    enforced via generator seeding).
  - Otherwise: exit normally; selection report `selected_sources`
    (`text_zip_relpath` list) and rejected paths/reasons match the oracle
    exactly; for every selected work, `official-source.json source_hash`
    and `source.manifest.json manifest_identity_object.work_content_hash`
    equal `oracle/content-hash`; publications report has every slug
    `"passed"`, `failed 0`, `skipped 0`.
- **P16.2 evolution** (the core) — checkpoints are placed around the
  seeded edit, not at the first non-empty selection (which would precede
  the second work's content and could never produce a `"reused"` slug):
  locate the seeded `:edit-content` via `find-applied` (unique, since the
  benign mix never emits `:edit-content`), let `i` = the index of that
  event in the history's events vector; `s-before` = `states[i]` (the
  fold state immediately **before** the edit applies — after both seeded
  `:add-content` events), `s-after` = the final state. Runs where
  `find-applied` returns nil (seeded edit no-opped, e.g. its work's
  content was removed first) or where `expected-selection s-before` is
  empty skip all assertions and count toward the ratio denominator only.
  All other runs execute the full assertion set; a run ticks as *applied*
  only when the expected status map contains at least one `"passed"` and
  one `"reused"` — genuine evolution exercised — and
  `assert-applied-ratio!` runs against that counter (the seeded shape
  makes this the overwhelmingly common case: the edited work rebuilds,
  the untouched second work reuses). Build `s-before` → out-root; render
  `s-after` to a fresh aozora-root; rebuild with `--replace` into the
  same out-root.
  - Publications-report statuses equal `expected-statuses s-before
    s-after`; `skipped 0`, `failed 0`.
  - **No-stale-reuse invariant:** for every current slug,
    `publications/<slug>/source_work_content_hash.txt` equals
    `oracle/content-hash` of the *current* state.
  - Reused slugs' publication files are byte-identical to the prior run's.
  - Zero-change leg: rebuild `s-after` again with `--replace` → every slug
    `"reused"`.
- **P16.3 pin-chain (D7)** — after a P16.1-style build, run
  `source-snapshot-workset/workset-from-root` + `write-workset!` over the
  materialized root, then `materialize-source-snapshot!`.
  - Desired behavior (gated by `div/expected-failure*` under D7): the
    composition succeeds.
  - Hard assertions on current behavior: the throw is
    `clean-ex-info?` with keys
    `[:work :parser-ir-path :official-source-path :work-content-hash
    :official-source-hash]`, is not a forbidden class, and
    `:work-content-hash` equals the member-bytes hash while
    `:official-source-hash` equals the raw-ZIP hash (pinning *why* it
    fails, not just that it fails).
  - **Evidentiary boundary:** P16.3 composes ABC with a behavioral stub,
    not the actual Rust adapter binaries. It pins the ABC-side composition
    *under the current adapter hash contract* (member-bytes hash), which
    was verified by code inspection at design time. If the Rust chain's
    contract changes while the stub does not, D7 keeps reporting the old
    divergence — the contract itself is guarded by the ab-validator
    follow-up check named in Future work, at the ownership boundary.
- **P16.4 faults** — deterministic single-run tests (not generative), one
  synthetic state with 2 content works, mirroring the drift-sidecar fault
  style:
  - **F1 tamper-after-pin:** rewrite one work's zip in the aozora-root with
    different bytes after the first build; rebuild with `--replace` →
    status `"passed"` (never `"reused"`), marker and `official-source.json`
    carry the new hash.
  - **F2 pin mismatch:** covered by P16.3's hard assertions (the
    representative loud-throw boundary for the pin chain).
  - **F3 corrupt prior marker:** corrupt, then delete,
    `source_work_content_hash.txt` in the prior output's pub-dir → rebuild
    yields `"passed"` both times (fail-safe rebuild; corruption of the
    cache marker can cause a rebuild, never a reuse).
  - **F4 no-`.txt` ZIP:** replace one work's zip with a zip containing only
    a `.png` member (structurally valid — no `7zz` path); with
    `continue_on_failure false` the build throws clean
    `ex-info "work ZIP contains no .txt member"` with `:path` in ex-data.

## Error handling

- Any un-gated throw from the SUT is asserted against
  `harness/forbidden-throw?` (NullPointerException, AssertionError,
  StackOverflowError, raw ZipException) before being rethrown or matched.
- Expected throws are matched with `harness/clean-ex-info?` and the
  documented ex-data keys.
- Temp dirs (aozora-roots, output-roots, config dirs) are created and
  removed per run with the existing `render` temp-dir helpers.

## Runtime bounds

`materialize-publication!` is real (plaintext + TEI + validation per passed
work). Bounds: content cap 4; `check!` counts start at 10 for P16.1 and 5
for P16.2 (each P16.2 run performs three builds; CI runs three seeds, so
the starting counts imply ~30 P16.1 builds and ~45 P16.2 builds). **Task 1
of the plan benchmarks one complete representative P16.2 case** — four
content works, at least one rebuilt and one reused, all three legs — and
reports both cold and warm focused-namespace wall time, since a single-work
multiplication would miss catalog rendering, temp-tree promotion, prior-dir
copying, multi-work validation, and JVM warm-up. Counts are calibrated from
that measurement toward a ~90 s focused-namespace target, with a floor:
**at least 10 genuine-evolution (applied) P16.2 cases across the CI seeds**
are preserved even if the wall-clock target must give. The measurement and
calibration are recorded in the plan's task report.

## Non-goals

- No production changes (D7 adjudication — including which hash is
  canonical — is separate ADR-level work).
- No `snapshot-index` or `workflow.cache` properties (future work, per
  scope decision).
- No git-history rendering for the aozora-root (plain dirs; the SUT does
  not require a repo).
- No real parser/adapter execution; no structural-ZIP-corruption faults
  (`7zz` environment dependency).
- No CLI-level (`soranoha/run!`) exit-code testing; the existing fixture
  test covers that wrapper.

## Acceptance criteria

- `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test`
  green with CI seeds; focused wall time within the calibrated bound.
- `--focus :simulation` and `--focus :unit` green; no existing test
  modified except additively (`model.clj`, `gen.clj`, `render.clj`,
  `oracle.clj`, `divergences.clj`).
- D7 recorded in `abc.sim.divergences/table` as `:open` with a dated case
  description; P16.3 gates only the composition success, with the
  current-behavior throw pinned hard.
- P16.1/P16.2 non-vacuity enforced via `ratio-counter` +
  `assert-applied-ratio!` (≥ 9/10 applicable runs).
- No production namespace under `abc/src/` is modified.

## Future work

- D7 adjudication: decide the canonical `work_content_hash` (raw ZIP vs
  member bytes) at ADR level; then either `snapshot-input`, the
  official-source writer, or the adapter contract changes, and the D7 gate
  flips to a regression guard.
- ab-validator contract check: a focused Rust test asserting that the
  adapter's AAT `meta.source_hash` is the sha256 of the member bytes it
  receives (`decode_source_bytes`), so a change to the adapter hash
  contract surfaces at the ownership boundary rather than silently
  invalidating P16.3's stub premise.
- `snapshot-index` identity-invalidation and `workflow.cache` staleness
  properties (the deferred halves of the parent spec's content-side item).
- Content-side faults through the `7zz` fallback in an environment that
  pins the binary.
