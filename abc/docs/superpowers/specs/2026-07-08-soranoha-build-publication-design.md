# Soranoha Build Publication Command Design

Status: Provisional design
Date: 2026-07-08
Owner: Soranoha architecture track

## Problem

Soranoha can already reproduce snapshot roots from resolved request sets, but
the path from an official `aozorabunko` checkout to inspectable TEI and
plaintext output is still a multi-command expert workflow:

1. discover real Aozora work sources;
2. materialize per-work parser inputs and metadata;
3. build a source snapshot;
4. resolve a request set against that generated source snapshot;
5. reproduce the snapshot root;
6. validate and optionally stage publication layout.

That is the wrong user boundary. A reviewer or maintainer should be able to
provide the official upstream checkout plus a Soranoha build config and get a
new TEI/plaintext snapshot root in one command. The command must not become a
new identity system or hide the canonical intermediate values.

## Design Goal

Add one stable command:

```bash
nix run .#soranoha -- build-publication \
  --aozora-root /path/to/aozorabunko \
  --config abc/config/publication-basic-ja.json \
  --output-root target/soranoha/builds/aozora-publication-2026-07-08
```

The command produces a complete local build directory containing the
materialized work root, generated source snapshot, resolved request set,
snapshot root with TEI/plaintext artifacts, validation reports, and optional
staged publication layout.

## Non-Goals

- Do not define new artifact identity rules.
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
- `--output-root`: a local output directory. The command may delete and
  recreate it. Resumable output reuse is a separate design.

Optional flags:

- `--snapshot-date YYYY-MM-DD`: overrides the config date or defaults to the
  current local date at command start.
- `--allow-dirty-aozora`: records dirty upstream state instead of rejecting it.
- `--stage`: writes publication layout after snapshot validation.
- `--fail-fast`: stops at the first materialization failure. Default is to
  continue and emit failure values when the intended coordinate is known.

The first implementation should keep the argument surface small. Extra tuning
belongs in the config file, not as an expanding CLI flag set.

## Build Config

The config is a JSON value. It is hashed and recorded in run summaries, but it
is not itself a manifest identity coordinate.

Minimum schema:

```json
{
  "schema_version": "soranoha-publication-build-config-v1",
  "request_set_label": "full-corpus-publication-basic-ja",
  "snapshot_scope": "aozora-full-corpus-publication",
  "parser_profile": "parser-ir-publication-basic-ja-v1",
  "publication_profile": "tei-publication-basic-ja-v1",
  "stage_publication": false,
  "failure_policy": {
    "allow_nonzero_failures": true
  }
}
```

Rules:

- `request_set_label` selects the existing request-set definition to resolve
  against the generated source snapshot.
- `snapshot_scope` and `snapshot_date` feed source-snapshot identity.
- `parser_profile` names the parser/materializer policy for Aozora source to
  parser-IR. Its hash is recorded in operational provenance until parser
  profile identity is promoted to a manifest coordinate.
- `publication_profile` names the parser-IR to TEI/plaintext policy.
- `failure_policy` is copied into the generated snapshot plan and contributes
  to snapshot identity through the existing snapshot-index policy hash.
- Unknown config keys are rejected in the first schema. The command can add an
  explicit `extensions` object in a future schema revision if openness is
  needed.

## Output Layout

The command writes:

```text
<output-root>/
  build-config.json
  build-plan.json
  materialized-root/
    works/<slug>/
      official-source.json
      metadata-record.json
      aat.json
      parser-ir.json
      source.manifest.json
      warnings.jsonl
      failure.manifest.json
  source-snapshot/
    source-snapshot.workset.edn
    source-snapshot.json
  request-set.json
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
    ...
```

`publication/` is present only when `--stage` or config
`stage_publication: true` is selected.

The inspectable TEI path is:

```text
<output-root>/snapshot-root/artifacts/works/<slug>/tei/tei.xml
```

The current compatibility path
`target/soranoha/full-corpus-publication-basic-ja` may remain as a
compatibility symlink or copy target in a separate operational slice, but the
build command's canonical output is the explicit `--output-root`.

## Stage Model

### 1. Validate Aozora Root

The command verifies:

- `.git` exists or `git -C <aozora-root> rev-parse HEAD` succeeds;
- `index_pages/list_person_all_extended_utf8.zip` exists;
- `cards/` exists;
- every selected official work source path is under
  `cards/<person-id>/files/*.zip`.

Default dirty policy:

- Reject a dirty upstream checkout.
- Record `aozora_git_commit`, `aozora_git_dirty`, and
  `aozora_catalog_zip_hash` in `build-plan.json` and run reports.
- `--allow-dirty-aozora` allows the run but sets `aozora_git_dirty: true`.

### 2. Select Work Sources

The source enumerator selects works from the official catalog ZIP and joins
them to actual `cards/*/files/*.zip` entries. Support files are excluded by
construction because a selected source must have:

- a catalog work id;
- a catalog person/card identity;
- an official text ZIP path matching `cards/[0-9]{6}/files/*.zip`;
- a source hash computed from that ZIP.

Rows without a materializable official source are recorded as source-selection
failures in `build-plan.json` and, when an intended work coordinate is known,
as failure manifests under `materialized-root/works/<slug>/`.

### 3. Materialize Work Root

For each selected work, the command writes the work-level files required by
`abc.tools.source-snapshot-workset`:

- `official-source.json`
- `metadata-record.json`
- `aat.json`
- `parser-ir.json`
- `source.manifest.json`

This stage owns adapter/parser execution. It may call existing ab-validator
or ABC tools, but its output contract is the materialized work root shape.

Materialization failures are values. If the source work coordinate is known,
the command writes a `failure.manifest.json` with the diagnostic and continues
unless `--fail-fast` is set.

### 4. Build Source Snapshot

The command delegates to the existing `source-snapshot` logic over
`materialized-root/` and writes:

- `source-snapshot/source-snapshot.workset.edn`
- `source-snapshot/source-snapshot.json`

This preserves the existing source snapshot identity contract and avoids
embedding source-selection logic into request-set identity.

### 5. Resolve Request Set

The command resolves `request_set_label` against the generated source snapshot:

```bash
soranoha resolve-request-set \
  <label> \
  <output-root>/request-set.json \
  <output-root>/source-snapshot/source-snapshot.json
```

The resolved request set must not be fixture-sized unless the generated source
snapshot is fixture-sized. The command records subject count and request set id
in `build-plan.json`.

### 6. Reproduce Snapshot Root

The command delegates to existing reproduction logic against
`request-set.json`, but with an explicit output root:

```text
<output-root>/snapshot-root
```

The current `reproduce` command defaults to `target/soranoha/<label>`. The
implementation should factor its internal `materialize-snapshot-root!` path so
`build-publication` can pass the explicit root without copying the logic.

Publication-only request sets, such as
`full-corpus-publication-basic-ja`, contain no analysis recipes and therefore
write parser-IR, plaintext, TEI, manifests, snapshot index, and run summary,
but not analysis artifacts.

### 7. Validate and Report

The command validates:

- source snapshot schema and hash;
- request set schema and request-set id;
- snapshot index schema and snapshot identity;
- referenced manifest file existence and hashes;
- run summary coherence;
- TEI validation results for produced TEI artifacts.

It writes:

- `reports/publication-report.json`
- `reports/layout-report.json`

If staging is enabled, it delegates to `stage-publication` and validates the
staged root as well.

## Identity and Time

The command creates a run at a place. The identity-bearing values remain:

- source snapshot hash;
- request set id;
- artifact manifests and content hashes;
- snapshot identity hash;
- schema hashes;
- parser evidence hashes;
- policy hashes.

The command-level `build-plan.json` is operational provenance. It records the
inputs, selected config hash, upstream git commit, output locations, and stage
status. It is not an identity-bearing artifact.

Rerunning the same clean upstream commit with the same config and toolchain
should produce the same source snapshot hash, request set id, artifact hashes,
and snapshot identity hash. If it does not, validation should expose the first
changed content hash or manifest identity conflict.

## Error Handling

Errors divide into three classes:

- **Command errors:** invalid flags, missing Aozora root, malformed config,
  dirty upstream without override. These stop the command before output
  materialization.
- **Selection errors:** catalog rows or source ZIPs that cannot be mapped to
  official work sources. These are counted in `build-plan.json`; known work
  coordinates get failure values.
- **Materialization errors:** parser, TEI, validation, or manifest generation
  failures. Known intended coordinates get failure manifests. Unknown global
  failures stop the command.

The default mode is batch-friendly: continue per-work failures and emit values.
`--fail-fast` is for local debugging.

## Nix Boundary

Nix should expose the command and tools. It should not make the full Aozora
repository contents a flake-evaluated matrix.

Expected surfaces:

```text
apps.x86_64-linux.soranoha
apps.x86_64-linux.abc-soranoha
```

The command may run inside `nix run` and consume a local `--aozora-root`.
Pinned official source inputs remain useful for CI fixtures and release
rehearsals, but local full-corpus generation must not require rebuilding a
flake output for every work.

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

The command owns workflow and operational provenance, while existing values
remain canonical.

Accepted. This is the smallest interface that fixes usability without
inventing a second publication protocol.

## Implementation Boundaries

Likely files:

- `abc/src/abc/tools/soranoha.clj`: add command dispatch and orchestration.
- `abc/src/abc/tools/soranoha_build_publication.clj`: new focused namespace for
  config parsing, build planning, and stage orchestration.
- `abc/src/abc/tools/aozora_publication_source.clj`: new focused namespace for
  official Aozora root validation and real-work source selection.
- `abc/schemas/soranoha-publication-build-config.schema.json`: config schema.
- `abc/config/publication-basic-ja.json`: default publication config.
- `abc/test/abc/tools/soranoha_build_publication_test.clj`: command and
  orchestration tests.
- `abc/test/abc/tools/aozora_publication_source_test.clj`: source-selection
  tests, especially support-file exclusion.

The implementation should avoid growing `abc.tools.soranoha` into a pipeline
module. That namespace remains the CLI dispatcher and shared snapshot commands.

## Acceptance Criteria

- `soranoha build-publication --aozora-root <fixture> --config <config>
  --output-root <dir>` produces TEI/plaintext snapshot output from a fixture
  official Aozora checkout shape.
- Source selection includes only catalog-backed work ZIPs under
  `cards/<person-id>/files/*.zip`.
- A support ZIP under a non-work path is ignored or rejected before
  materialization.
- The generated request set subject count equals the generated source snapshot
  work count for publication-only builds.
- The generated snapshot root validates with `soranoha validate`.
- The generated TEI files are inspectable under
  `<output-root>/snapshot-root/artifacts/works/*/tei/tei.xml`.
- The command writes `build-plan.json`, `request-set.json`,
  `snapshot-root/snapshot-index.json`, and `snapshot-root/run-summary.json`.
- Running with a dirty Aozora checkout fails unless `--allow-dirty-aozora` is
  supplied.
- Full `nix flake check` covers the smoke fixture path.

## Follow-Ups

- Add `--resume` once build-plan checkpoints and partial output validity are
  specified.
- Add parallel materialization controls after the sequential fixture path is
  correct.
- Add staged publication hosting upload after the local staged layout is
  stable.
- Add tokenizer/profile-backed outputs only after tokenizer artifact identity
  is implemented.
