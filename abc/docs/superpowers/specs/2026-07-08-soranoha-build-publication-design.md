# Soranoha Build Publication Command Design

Status: Provisional design, revised after critical review
Date: 2026-07-08
Owner: Soranoha architecture track

## Problem

Soranoha can already run the publication half of a snapshot build from a
materialized work root:

```bash
soranoha publication-rehearsal \
  <materialized-root> \
  <output-root> \
  <request-set-label> \
  <snapshot-scope> \
  <snapshot-date>
```

That command already performs source-snapshot generation, request-set
resolution, snapshot-root reproduction, reports, staged publication layout, and
validation. The remaining user-facing gap is earlier in the pipeline: given an
official `aozorabunko` checkout, there is no single supported command that
selects real work ZIPs, materializes `official-source.json`,
`metadata-record.json`, `aat.json`, `parser-ir.json`, and
`source.manifest.json`, then hands that materialized root to the existing
publication rehearsal pipeline.

The new command should close that gap without becoming a second identity model
or a duplicate implementation of `publication-rehearsal!`.

## Related Decisions

This command composes existing contracts:

- ADR 0001 defines manifest identity and content-addressed artifacts.
- ADR 0002 defines parser evidence as the parser-selection authority gate.
- ADR 0003 defines the bounded materialization and Nix evaluation envelope.
- ADR 0005 defines failure manifests and operational runtime semantics.
- ADR 0026 defines request-set identity and resolver behavior.
- `2026-07-07-soranoha-snapshot-publication-design.md` defines snapshot
  publication layout, parser evidence hashes, and publication guardrails.

`build-publication` does not reopen those decisions. It must satisfy them by
recording enough provenance to audit source selection and by delegating the
already-existing rehearsal stages.

## Design Goal

Add one stable command:

```bash
nix run .#soranoha -- build-publication \
  --aozora-root /path/to/aozorabunko \
  --config abc/config/publication-basic-ja.json \
  --snapshot-date 2026-07-08 \
  --output-root target/soranoha/builds/aozora-publication-2026-07-08
```

The command produces a local run root containing:

1. normalized build config and build plan;
2. source-selection report over the official Aozora checkout;
3. materialized work root;
4. a nested `rehearsal/` root produced by `publication-rehearsal!`.

The existing inspectable TEI path, relative to the nested rehearsal root, is:

```text
<output-root>/rehearsal/snapshot-root/artifacts/works/<slug>/tei/tei.xml
```

## Non-Goals

- Do not define new artifact identity rules.
- Do not reimplement source-snapshot, request-set resolution, reproduction,
  reports, staging, or validation already owned by `publication-rehearsal!`.
- Do not publish tokenizer outputs or tokenizer-backed metrics.
- Do not make the Nix evaluator enumerate the corpus or output matrix.
- Do not require the official Aozora repository to be a Nix store input for
  local development runs.
- Do not hide failures in logs only; every known failed intended work becomes
  a value in the output tree.

## Command Contract

The command is an operational pipeline boundary. It consumes places and values:

- `--aozora-root`: a local checkout of `github:aozorabunko/aozorabunko`.
- `--config`: a JSON build config value.
- `--snapshot-date YYYY-MM-DD`: required for non-smoke scopes. Smoke fixtures
  may default to the UTC date at command start, never the local timezone date.
- `--output-root`: a local run directory.

Optional flags:

- `--allow-dirty-aozora`: records dirty upstream state instead of rejecting it.
- `--stage`: requests staged publication output. The first implementation may
  keep staging always enabled because `publication-rehearsal!` already stages.
- `--fail-fast`: stops at the first materialization failure. Default is to
  continue and emit failure values when the intended coordinate is known.
- `--replace`: permits replacing an existing completed output root.

Output discipline:

- The command must not delete a user-supplied output root in place.
- It writes to a sibling or child work directory such as
  `<output-root>.tmp-<run-id>` or `<output-root>/.in-progress/<run-id>`.
- After source materialization and rehearsal validation complete, it atomically
  promotes the completed run to `<output-root>` or swings a stable symlink.
- A failed run leaves the in-progress directory available for inspection.
- Resumable output reuse is a separate design.

## Build Config

The raw config is recorded as `build-config.input.json`. The command writes a
normalized config as `build-config.normalized.json`; this normalized value has
defaults applied, labels resolved, and is the config value whose hash is stored
in `build-plan.json`. The config hash is operational provenance, not a manifest
identity coordinate.

Minimum schema:

```json
{
  "schema_version": "soranoha-publication-build-config-v1",
  "request_set_label": "full-corpus-publication-basic-ja",
  "snapshot_scope": "aozora-full-corpus-publication",
  "parser_profile": "parser-ir-publication-basic-ja-v1",
  "publication_profile": "parser-ir-publication-policy-v0",
  "failure_policy": {
    "allow_nonzero_failures": true
  }
}
```

Rules:

- `request_set_label` selects the existing request-set definition to resolve
  inside `publication-rehearsal!`.
- `snapshot_scope` and `snapshot_date` feed source-snapshot identity.
- `parser_profile` resolves to a parser materialization profile object. That
  object names the adapter executable, adapter version requirement, parser-IR
  schema hash, AAT-to-parser-IR mapping hash, and ADR 0002 parser evidence
  labels or hashes. The citable `parser_evidence_hashes` are already
  identity-bearing through snapshot-index identity; the profile label itself is
  operational selection metadata.
- `publication_profile` resolves to an existing publication policy value. For
  the first implementation this should resolve to
  `data/parser-ir-publication-policy-v0.json` and its canonical policy hash,
  not to an unstructured string. That policy hash is the output-format policy
  already used by parser-IR publication materialization.
- `failure_policy` is copied into the generated build plan and must agree with
  the failure semantics accepted by the source-selection and materialization
  stages.
- Unknown config keys are rejected in the first schema. The command can add an
  explicit `extensions` object in a future schema revision if openness is
  needed.

## Output Layout

The command writes a run root:

```text
<output-root>/
  build-config.input.json
  build-config.normalized.json
  build-plan.json
  source-selection-report.json
  materialized-root/
    works/<slug>/
      official-source.json
      metadata-record.json
      aat.json
      parser-ir.json
      source.manifest.json
      warnings.jsonl
      failure.manifest.json
  rehearsal/
    source-snapshot/
      source-snapshot.workset.edn
      source-snapshot.json
    request-sets/<request-set-label>.json
    snapshot-root/
      artifacts/works/<slug>/
        parser-ir/parser-ir.json
        parser-ir/parser-ir.manifest.json
        plaintext/plain.txt
        plaintext/plaintext.manifest.json
        tei/tei.xml
        tei/tei.manifest.json
        tei/tei-validation-result.json
        tei/preservation.json
      snapshot-index.json
      run-summary.json
    reports/
      publication-report.json
      layout-report.json
    publication/
      index.json
      run-summary.json
      <layout-policy-defined artifact paths>
    rehearsal-report.json
```

`rehearsal/` is produced by the existing `publication-rehearsal!` command. The
`publication/` layout is the layout defined by
`abc.tools.soranoha-stage-publication`, not a second layout specified here.

The parser-IR file under `materialized-root/` is the producer value consumed by
source-snapshot and snapshot reproduction. The parser-IR file under
`rehearsal/snapshot-root/artifacts/works/<slug>/parser-ir/` is the publication
artifact copy generated from that producer value. Release validation must
verify the artifact content hash against the producer-derived manifest; the two
locations must not be independently regenerated.

The current compatibility path
`target/soranoha/full-corpus-publication-basic-ja` may remain as a
compatibility symlink or copy target in a separate operational slice, but this
command's canonical output is the explicit `--output-root`.

## Pipeline Shape

`build-publication` has two owners:

1. **New source materialization owner:** validate Aozora checkout, select work
   ZIPs, create `materialized-root/`.
2. **Existing rehearsal owner:** call `publication-rehearsal!` with
   `materialized-root/`, `rehearsal/`, `request_set_label`, `snapshot_scope`,
   and `snapshot_date`.

Stages 4 through 7 of the earlier sketch are not new code. They are the
existing rehearsal command:

```clojure
(publication-rehearsal!
  materialized-root
  rehearsal-root
  request-set-label
  snapshot-scope
  snapshot-date)
```

The build command may wrap the call to add atomic output promotion and
top-level build provenance, but it must not duplicate the internal rehearsal
steps.

## Stage 1: Validate Aozora Root

The command verifies:

- `git -C <aozora-root> rev-parse HEAD` succeeds;
- `index_pages/list_person_all_extended_utf8.zip` exists;
- `cards/` exists;
- the configured catalog ZIP path is inside `index_pages/`;
- selected official work source paths are under
  `cards/<person-id>/files/*.zip`.

Dirty policy:

- Dirty means `git status --porcelain -- index_pages cards` is non-empty.
- A clean working tree at any commit is allowed.
- Modified or untracked files outside `index_pages/` and `cards/` do not make
  source selection dirty, but their presence may be recorded for diagnostics.
- By default, dirty source paths under `index_pages/` or `cards/` are rejected.
- `--allow-dirty-aozora` allows the run and records the dirty status output.

The build plan records:

- `aozora_git_commit`;
- `aozora_relevant_dirty`;
- `aozora_dirty_paths` when allowed;
- `aozora_catalog_zip_hash`;
- the hash of the normalized build config.

## Stage 2: Select Work Sources

The source enumerator selects works from the official catalog ZIP and joins
them to actual `cards/*/files/*.zip` entries.

A selected source must have:

- a catalog work id;
- a catalog person/card identity;
- an official text ZIP path matching `cards/[0-9]{6}/files/*.zip`;
- a materializable text member selected by the parser profile;
- a `source_hash` computed from the selected source ZIP bytes.

Support files are not assumed away by a regex alone. The implementation must
write `source-selection-report.json` with:

- total catalog rows;
- selected work ZIP count;
- rejected or missing source count;
- non-selected ZIP count under `cards/*/files/`;
- classification counts for non-selected ZIPs, when detectable;
- per-selected-work source ZIP path and source hash.

For full-corpus admission, support-file exclusion is a measured coverage gate:
the report must show that every selected source came from a catalog-backed work
row and that non-work/support ZIPs were excluded before materialization.

Rows without a materializable official source are recorded as source-selection
failures in `build-plan.json` and, when an intended work coordinate is known,
as failure manifests under `materialized-root/works/<slug>/`.

## Stage 3: Materialize Work Root

For each selected work, the command writes the work-level files required by
`abc.tools.source-snapshot-workset`:

- `official-source.json`
- `metadata-record.json`
- `aat.json`
- `parser-ir.json`
- `source.manifest.json`

This is the genuinely new hard boundary. It is a Clojure orchestrator over a
Nix-provided Rust adapter executable, not a pure Clojure parser rewrite.

### Adapter Invocation Protocol

The parser profile resolves to a command template whose argv is explicit in
the normalized config. The build command invokes that command per selected
work, or per bounded batch after the parallel/batch design lands.

Required inputs per work:

- absolute path to the selected official source ZIP;
- catalog row or normalized catalog metadata JSON;
- output work directory;
- parser profile JSON path and profile hash;
- expected parser-IR schema hash;
- expected AAT-to-parser-IR mapping hash;
- citable ADR 0002 parser evidence hashes;
- run-local temp directory.

Required successful outputs:

- `official-source.json`, including source ZIP relative path and source hash;
- `metadata-record.json`, using the same catalog parsing semantics as
  `abc.tools.aozora-ingest`;
- `aat.json`;
- `parser-ir.json`;
- `source.manifest.json`;
- `warnings.jsonl`, possibly empty.

The adapter must not depend on the process current directory. All paths passed
across the process boundary are absolute or rooted in the work output
directory. The Clojure wrapper validates every required output before treating
the work as materialized.

Exit handling:

- Exit 0 with valid outputs materializes the work.
- Exit 0 with missing or invalid outputs becomes a materialization failure.
- Non-zero exit becomes a materialization failure.
- Known work failures emit `failure.manifest.json` with source path, source
  hash, parser profile hash, adapter executable identity, exit code, and
  bounded stdout/stderr references.
- Unknown global failures stop the command.

Concurrency:

- The first implementation may be sequential for smoke and demo fixtures.
- Full-corpus publication is not admitted until the materializer has a measured
  concurrency or batch strategy that fits ADR 0003's cost envelope.
- The concurrency setting is operational; it is recorded in `build-plan.json`
  but is not identity-bearing.

## Stage 4: Delegate to Publication Rehearsal

After `materialized-root/` is complete enough under the configured failure
policy, `build-publication` calls:

```bash
soranoha publication-rehearsal \
  <output-root>/materialized-root \
  <output-root>/rehearsal \
  <request-set-label> \
  <snapshot-scope> \
  <snapshot-date>
```

`publication-rehearsal!` already writes:

- `rehearsal/source-snapshot/source-snapshot.workset.edn`
- `rehearsal/source-snapshot/source-snapshot.json`
- `rehearsal/request-sets/<request-set-label>.json`
- `rehearsal/snapshot-root/`
- `rehearsal/reports/publication-report.json`
- `rehearsal/reports/layout-report.json`
- `rehearsal/publication/`
- `rehearsal/rehearsal-report.json`

The build command records the request-set id, source snapshot hash, snapshot
identity hash, artifact counts, and rehearsal report path in its top-level
`build-plan.json` or `build-summary.json`.

## Identity and Time

The command creates a run at a place. The identity-bearing values remain:

- source snapshot hash;
- request set id;
- artifact manifests and content hashes;
- snapshot identity hash;
- schema hashes;
- parser evidence hashes;
- publication policy hashes.

The command-level `build-plan.json` is operational provenance. It records the
inputs, normalized config hash, upstream git commit, source-selection report
hash, selected work source hashes, output locations, adapter profile hashes,
and stage status. It is not an identity-bearing artifact.

Reproducibility claim:

- Rerunning the same clean upstream commit with the same normalized config,
  same toolchain, same parser evidence hashes, same publication policy hash,
  and same per-work source ZIP hashes should produce the same source snapshot
  hash, request set id, artifact hashes, and snapshot identity hash.
- A matching git commit is not sufficient evidence by itself. Reviewers verify
  reproduction at the per-work source hash and manifest content-hash layer.
- If a rerun differs, validation should expose the first changed source hash,
  content hash, or manifest identity conflict.

## Error Handling

Errors divide into three classes:

- **Command errors:** invalid flags, missing Aozora root, malformed config,
  missing required snapshot date, dirty relevant upstream paths without
  override. These stop the command before output materialization.
- **Selection errors:** catalog rows or source ZIPs that cannot be mapped to
  official work sources. These are counted in `build-plan.json`; known work
  coordinates get failure values.
- **Materialization errors:** adapter, parser, validation, or manifest
  generation failures. Known intended coordinates get failure manifests.
  Unknown global failures stop the command.
- **Rehearsal errors:** failures from `publication-rehearsal!`. These stop the
  command because source-snapshot, request-set, snapshot-root, report, staging,
  and validation semantics are owned by that existing command.

The default mode is batch-friendly: continue per-work failures and emit values.
`--fail-fast` is for local debugging.

## Nix Boundary

Nix exposes the command and the adapter executables. It must not make the full
Aozora repository contents a flake-evaluated matrix.

Expected surface:

```text
apps.x86_64-linux.soranoha
```

The command may run inside `nix run` and consume a local `--aozora-root`.
Pinned official source inputs remain useful for CI fixtures and release
rehearsals, but local full-corpus generation must not require rebuilding a
flake output for every work.

ADR 0003 scope:

- Smoke and demo `build-publication` runs are in scope for the first
  implementation.
- Full-corpus runs are a goal, but are not accepted for release until measured
  source selection, materialization concurrency, rehearsal runtime, closure
  size, and validation cost are recorded against ADR 0003's envelope.

## Alternatives Considered

### A. Keep Multi-Command Workflow

This preserves narrow tools but fails the user boundary. It makes ordinary TEI
inspection depend on undocumented command ordering and fixture-vs-real request
set knowledge.

Rejected as the primary interface.

### B. Make One Monolithic Generator

A single generator could read Aozora and write TEI directly. That would be
easy to run but would bypass source snapshots, request sets, manifests, and
snapshot identity.

Rejected because it braids source selection, parser execution, publication
rendering, and identity into one unreviewable runtime.

### C. Add an Orchestrating Command Over Existing Values

The command owns official-source discovery and materialized-root creation, then
delegates the publication half to `publication-rehearsal!`. Existing values
remain canonical.

Accepted. This is the smallest interface that fixes usability without
inventing a second publication protocol.

## Implementation Boundaries

Likely files:

- `abc/src/abc/tools/soranoha.clj`: add command dispatch only.
- `abc/src/abc/tools/soranoha_build_publication.clj`: new focused namespace for
  config parsing, build planning, atomic output handling, source
  materialization orchestration, and delegation to `publication-rehearsal!`.
- `abc/src/abc/tools/aozora_ingest.clj`: reuse or extend existing catalog ZIP
  parsing rather than introducing a duplicate catalog reader.
- `abc/src/abc/tools/aozora_publication_source.clj`: only if source ZIP
  selection and support-file coverage are too large for `aozora_ingest.clj`;
  it must reuse ingest's catalog parsing.
- `abc/schemas/soranoha-publication-build-config.schema.json`: config schema.
- `abc/config/publication-basic-ja.json`: default publication config.
- `abc/test/abc/tools/soranoha_build_publication_test.clj`: command,
  orchestration, atomic-output, and rehearsal-delegation tests.
- `abc/test/abc/tools/aozora_publication_source_test.clj`: source-selection
  tests, especially support-file exclusion and live-catalog coverage fixtures.

The implementation should avoid growing `abc.tools.soranoha` into a pipeline
module. That namespace remains the CLI dispatcher and shared snapshot commands.

## Acceptance Criteria

- `soranoha build-publication --aozora-root <fixture> --config <config>
  --snapshot-date <date> --output-root <dir>` produces TEI/plaintext snapshot
  output from a fixture official Aozora checkout shape.
- The command delegates source-snapshot through staging/report/validation to
  `publication-rehearsal!`; tests should detect if those steps are copied into
  a parallel implementation.
- Source selection includes only catalog-backed work ZIPs under
  `cards/<person-id>/files/*.zip`.
- A support ZIP under a non-work path is ignored or rejected before
  materialization.
- A support or non-text ZIP under `cards/*/files/` is counted in
  `source-selection-report.json` and is not materialized unless it is tied to a
  materializable catalog work source.
- The generated request set subject count equals the sum of requested input
  views over selected works, not blindly the work count.
- The generated rehearsal snapshot root validates with `soranoha validate`.
- The generated TEI files are inspectable under
  `<output-root>/rehearsal/snapshot-root/artifacts/works/*/tei/tei.xml`.
- The command writes `build-plan.json`,
  `source-selection-report.json`,
  `rehearsal/request-sets/<request-set-label>.json`,
  `rehearsal/snapshot-root/snapshot-index.json`, and
  `rehearsal/snapshot-root/run-summary.json`.
- Running a non-smoke scope without an explicit `--snapshot-date` fails.
- Running with dirty relevant Aozora paths fails unless `--allow-dirty-aozora`
  is supplied.
- A late rehearsal failure leaves the in-progress output available for
  inspection and does not destroy the previous completed output root.
- Full `nix flake check` covers the smoke fixture path.

## Follow-Ups

- Add `--resume` once build-plan checkpoints and partial output validity are
  specified.
- Add measured parallel or batch materialization before admitting full-corpus
  publication runs under ADR 0003.
- Add staged publication hosting upload after the local staged layout is
  stable.
- Add tokenizer/profile-backed outputs only after tokenizer artifact identity
  is implemented.
