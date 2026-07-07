# Soranoha Snapshot Publication and Reproduction Design

Status: Provisional design
Date: 2026-07-07
Owner: Soranoha architecture track

This design defines the first public full-corpus Soranoha snapshot shape. It
records the project direction for reproducible corpus publication without
making the Nix evaluator enumerate every parser, tokenizer, recipe, and output
combination.

## Problem

Soranoha needs a first public corpus product that is useful to researchers and
reviewers:

- TEI and plaintext corpus artifacts are published online.
- Tokenizer outputs and simple stylometric analysis artifacts are targeted for
  the same corpus snapshot, after the tokenizer artifact identity contract is
  accepted.
- A user can run the flake entrypoint to reproduce the data locally.
- Large derived query products, such as Parquet, are reproducible but are not
  assumed to be cached as flake packages or Nix binary-cache artifacts.

The core risk is treating Nix as the analysis planner. Nix should pin tools,
schemas, source inputs, registries, request sets, and materialization commands.
It should not eagerly materialize a `works x parsers x tokenizers x recipes x
formats` matrix during flake evaluation.

## Relationship to Existing ADRs

This design adopts existing ADR contracts rather than replacing them:

- **ADR 0001** remains authoritative for per-artifact manifest identity and
  reproducibility conflicts.
- **ADR 0002** remains the parser-selection gate. This design only states that
  a selected parser/adapter tuple is authoritative for a named snapshot, not
  for all future Soranoha work.
- **ADR 0003** remains the materialization cost gate. The first public snapshot
  must satisfy ADR 0003's cost-envelope criteria for the full-corpus request
  set before publication, or ADR 0003 must be revised before the snapshot
  ships.
- **ADR 0004** remains deferred for formal signing and provenance attestations.
  Unsigned snapshots rely on HTTPS delivery of the snapshot index from the
  project's canonical domain; consumers who need stronger guarantees should
  verify artifact content hashes against the published index.
- **ADR 0005** remains authoritative for the file/CLI operational surface,
  failure manifests, and database-free first publication path.
- **ADR 0026** is adopted for analysis recipe identity, recipe-registry
  bindings, request-set hashing, request-set resolution metadata, and copied
  producer-field validation. This design adds the snapshot publication layer
  above ADR 0026. It does not supersede ADR 0026.

ADR 0026 currently accepts only the token-independent
`parser-ir-plaintext-body-v1` analysis slice. Tokenized artifacts and
tokenizer-backed analysis require a focused ADR 0026 extension or a new ADR
before they can be published as canonical snapshot artifacts.

## Design Decisions

| Decision | Status | Reason |
|---|---|---|
| Release signing and formal release verification are deferred. | Accepted for this snapshot track | ADR 0004 remains important, but it is not a blocker for the first public data snapshot. |
| The first public corpus object is named a snapshot. | Accepted | A snapshot is an immutable captured state of source, toolchain, request sets, manifests, and derived artifacts. It avoids overloading software release or API version terms. |
| Snapshot labels use `soranoha-snapshot-YYYY-MM-DD-NN`. | Accepted | The label is compact, sortable, URL-safe, and supports multiple same-day publications. |
| Snapshot labels are public locators, not identity. | Accepted | Identity remains in request-set hashes, source snapshot hashes, manifest/content hashes, recipe hashes, tokenizer profile hashes, and schema hashes. |
| The first snapshot should target the full corpus. | Accepted direction | Existing full-corpus evidence reports 17,894 aozora-rs AAT documents parsed with zero file parse failures. A current snapshot-scale report must re-confirm the exact selected corpus. |
| Parser authority is scoped to the snapshot. | Pending ADR 0002 closure | When a parser/adapter tuple is selected, it is authoritative for this snapshot's publication products, not a forever parser decision. |
| Static files plus manifests are authoritative. | Accepted | ADR 0005's file/CLI-oriented operational surface remains the right first publication shape. |
| Databases and Parquet are derived views. | Accepted | They may be generated for query convenience, but they are not canonical identity stores. |
| Nix is the reproduction entrypoint. | Accepted | `nix run <url>#soranoha` should reproduce selected data locally, not merely fetch a prebuilt cache. |
| Flake outputs expose dimensions but do not enumerate the matrix. | Accepted | Request sets select bounded combinations; flake outputs expose tools, registries, schemas, and named request sets. |

## Snapshot Label

Format:

```text
soranoha-snapshot-YYYY-MM-DD-NN
```

Example:

```text
soranoha-snapshot-2026-07-07-01
```

Rules:

- `NN` is a zero-padded same-day sequence number.
- A snapshot label, once published, is immutable.
- A mutable `latest` pointer may exist but must not be used as a citation
  target.
- The snapshot index records the identity-bearing hashes.
- Rebuilding the same inputs should reproduce the same artifact hashes, but it
  does not create a new public snapshot unless intentionally published under a
  new label.

Public URL shape:

```text
/snapshots/soranoha-snapshot-2026-07-07-01/index.json
/snapshots/soranoha-snapshot-2026-07-07-01/manifests/...
/snapshots/soranoha-snapshot-2026-07-07-01/artifacts/...
/latest/index.json
```

`latest/index.json` is a small mutable pointer document, not a symlink and not
a citable snapshot index. It records the current snapshot label, immutable
snapshot index URL, snapshot identity hash, and update time. Validators should
warn when a citation or reproducibility claim uses a `latest` URL instead of a
concrete snapshot label.

## Snapshot Index Identity

The snapshot index is a JSON artifact. It has two different hashes:

- `snapshot_identity_hash`, which identifies the selected snapshot content and
  policies; and
- the ordinary byte/content hash of the `index.json` file, which may differ
  when non-identity locator fields such as `snapshot_label` differ.

The snapshot identity rule is:

```text
snapshot_identity_hash =
  sha256:<hex of SHA-256(RFC8785-JCS(snapshot_index_identity_object))>
```

`snapshot_index_identity_object` contains only identity-bearing fields:

- `snapshot_index_schema_hash`
- `request_set_id`
- `source_snapshot_hash`
- `manifest_index_hash`
- `artifact_set_hash`
- `failure_policy_hash`
- `layout_policy_hash`
- `schema_hashes`
- `parser_evidence_hashes`
- `tokenizer_profile_hashes`
- `analysis_recipe_hashes`

`snapshot_label`, generated time, operator, base URL, mirror URL, local paths,
run id, and `latest` pointer data are excluded from
`snapshot_index_identity_object`.

Empty arrays represent "none applicable" for array fields inside
`snapshot_index_identity_object`. Null is not used for array fields.

Hash arrays inside `snapshot_index_identity_object` sort lexicographically by
the full `sha256:<hex>` string before JCS canonicalization. Arrays of structured
objects use the explicit sort rule defined by that object's schema.

These identity fields use provisional canonical shapes until dedicated schemas
are added:

- `failure_policy_hash` is the JCS SHA-256 hash of an object with
  `schema_id`, `allow_nonzero_failures`, `max_failure_rate`,
  `per_artifact_kind`, and `per_diagnostic_tolerances`. `max_failure_rate` is a
  number or null. `per_artifact_kind` and `per_diagnostic_tolerances` are
  objects keyed by stable artifact-kind and diagnostic-code strings; tolerance
  entries contain `max_count` and `max_rate`, each number or null.
- `layout_policy_hash` is the JCS SHA-256 hash of an object with `schema_id`,
  `layout_kind`, `loose_artifact_kinds`, `batched_artifact_kinds`,
  `batch_target_work_count`, `archive_format`, and `member_path_template`.
  Artifact-kind arrays sort lexicographically by string.
- `parser_evidence_hashes` is an array of SHA-256 report hashes for
  parser-selection and conversion-compatibility evidence accepted for the
  snapshot parser/adapter tuple. The array sorts lexicographically by full
  `sha256:<hex>` string. The detailed evidence entries remain governed by the
  parser-evidence citation contract.

`artifact_set_hash` is the JCS SHA-256 hash of the sorted artifact-reference
identity array. Each artifact-reference identity contains:

- `artifact_id`
- `artifact_kind`
- `validation_status`
- `manifest_content_hash`
- `content_hash`, or JSON null for failed/skipped targets
- `sidecar_role`, when applicable

Artifact-reference identities sort by `artifact_id`, then `artifact_kind`, then
`sidecar_role` with null treated as the empty string for comparison only. Two
snapshot indexes containing the same artifact-reference identities in different
order have the same `artifact_set_hash` and therefore the same
`snapshot_identity_hash`.

The public `index.json` may include relative URLs, display labels, sizes, MIME
types, and generated timestamps for usability. Those fields are not part of
snapshot identity unless a later snapshot-index schema explicitly promotes
them. Once a snapshot label is published, the label must keep resolving to the
same index bytes; corrections use a new snapshot label.

## Flake Surface

The flake should expose stable ingredients and commands:

```text
apps:
  soranoha
  soranoha-validate

packages:
  schemas
  recipe-registry
  tokenizer-profiles
  request-sets
  small-fixtures
```

The flake should avoid exposing full-corpus generated artifacts as ordinary
default packages. Full-corpus outputs can be produced by `nix run` commands
that write to an explicit local output directory.

The `soranoha` app is a thin command dispatcher over component tools; it is not
a new monolithic runtime. Subcommands may delegate to publication, analysis,
tokenizer, index, and validation components, but the app boundary is the stable
user entrypoint.

Suggested command shape:

```sh
nix run <url>#soranoha -- reproduce \
  --request-set full-corpus-basic-ja \
  --output ./out/soranoha-full

nix run <url>#soranoha -- validate \
  --root ./out/soranoha-full

nix run <url>#soranoha -- snapshot-index \
  --root ./out/soranoha-full \
  --snapshot-label soranoha-snapshot-2026-07-07-01
```

Additional discovery commands:

```sh
nix run <url>#soranoha -- list-request-sets
nix run <url>#soranoha -- list-recipes
nix run <url>#soranoha -- list-tokenizer-profiles
nix run <url>#soranoha -- explain-request-set full-corpus-basic-ja
```

`list-tokenizer-profiles` may return no accepted profiles until the tokenizer
artifact extension lands.

The important boundary is:

```text
flake outputs = tools, schemas, registries, named request sets, small checks
request sets = selected corpus, parser/input views, tokenizer profiles, recipes
local output = full reproduced corpus artifacts and optional derived views
```

## Request Sets

A request set is the artifact-selection unit. It turns possible coordinates
into a bounded realized set, using ADR 0026's canonical
`request_set_identity_object` and `request_set_id` hashing rule.

The first full-corpus request set should include:

- the selected source corpus snapshot,
- the snapshot publication parser/adapter tuple,
- parser-IR plaintext-body input view,
- TEI and plaintext publication outputs,
- the first token-independent actionable stylometric analysis recipe set,
- missing/failure policy,
- optional derived-view policy.

After the tokenizer artifact contract is accepted, the full-corpus request set
can add the first tokenizer profile and tokenizer-backed recipe set.

Request-set labels are lookup handles, not identity. Label bindings resolve to
request-set content hashes and registry-entry hashes per ADR 0026. The label
pattern is:

```text
<scope>[-<product>]-<profile>
```

Where `scope` is `smoke`, `demo`, or `full-corpus`; `product` is optional and
narrows the target set; and `profile` names a semantic request profile such as
`basic-ja`.

When the product component is omitted, the request set includes all accepted
artifact classes for that scope and profile.

Named request sets should include at least:

```text
smoke-basic-ja
demo-basic-ja
full-corpus-publication-basic-ja
full-corpus-analysis-basic-ja
full-corpus-basic-ja
```

`full-corpus-basic-ja` is the intended snapshot request profile. It is not
publication-complete until its accepted contract includes every artifact class
the snapshot promises.

The full request set is identity-bearing. Batch size, concurrency, local output
directory, and run id are operational metadata and do not participate in
request-set identity.

## Published Static Layout

A snapshot index should be the discovery root. It should reference artifact
manifests and content by relative URL plus hash.

The default layout is mixed:

- TEI and plaintext are loose per-work files because humans and lightweight
  tools benefit from direct URL-per-artifact access.
- Tokenized outputs and analysis results are batched archives because they are
  numerous, machine-oriented, and expected to be scanned through the snapshot
  index.
- The manifest index maps every artifact id to either a loose relative URL or
  an archive URL plus member path.

This layout is provisional and must be measured, but it is the implementation
target unless the cost probe disproves it.

Provisional layout:

```text
snapshots/soranoha-snapshot-YYYY-MM-DD-NN/
  index.json
  request-set.json
  run-summary.jsonl
  manifests/
    manifest-index.json
    by-work/...
    failures/...
  artifacts/
    tei/by-work/...
    plaintext/by-work/...
    tokenized/batches/tokenized-batch-0001.tar.gz
    analysis/batches/analysis-batch-0001.tar.gz
  derived/
    parquet/...
    duckdb/...
```

`derived/` is optional for the first snapshot. If present, those files are
rebuildable views over canonical manifests and sidecars.

## Failure Policy

Every work in the request set must produce either successful artifacts for the
requested roles or indexed failure/skipped manifests explaining why the role
was not materialized.

Rules:

- Parser or parser-IR validation failure blocks downstream publication,
  tokenization, and analysis for that work; the upstream failure is indexed.
- TEI validation failure must not be published as a successful TEI artifact.
  Invalid bytes may be retained only as diagnostic sidecars if the failure
  policy permits them.
- Plaintext, TEI, tokenized, and analysis stages each report their own
  diagnostic code when they fail independently.
- The snapshot index records success, warning, failure, and skipped counts by
  artifact kind and diagnostic code.
- A snapshot may publish with a non-zero failure count only when the
  `failure_policy_hash` identifies a policy that permits it. The first public
  full-corpus snapshot should record, not hide, any non-zero failure rate.

Future release-smoke gates may impose maximum failure rates. Until then, the
failure rate is part of the snapshot's published evidence, not an implicit
success claim.

## Components

| Component | Purpose | Inputs | Outputs | State / Time / Identity |
|---|---|---|---|---|
| Flake entrypoint | Provide reproducible commands and pinned toolchain | `flake.lock`, app args | CLI execution environment | Lockfile pins tool versions; command args are operational. |
| Request-set registry | Name accepted request sets | Canonical request-set JSON | request-set hashes and labels | Uses ADR 0026 request-set identity and resolution metadata. |
| Recipe registry | Name analysis recipes | Canonical recipe JSON | recipe hashes and semantic bindings | Uses ADR 0026 append-only binding log; recipe content hash is identity. |
| Tokenizer profile registry | Name tokenizer build/dictionary/config profiles | tokenizer package, dictionary, profile JSON | tokenizer profile hash | Deferred until tokenizer artifact contract is accepted. |
| Publication materializer | Produce TEI and plaintext from parser-IR | parser-IR, metadata, persons, policy files | TEI, plaintext, manifests, validation result | Artifact ids and content hashes identify outputs. |
| Tokenization materializer | Produce tokenized slices | plaintext-body input view, tokenizer profile | token stream sidecars and manifests | Extension track; identity contract must be accepted first. |
| Analysis materializer | Produce stylometric result slices | input view, recipe, optional tokenized slice | analysis-result sidecars and manifests | ADR 0026 governs token-independent slices; tokenizer-backed slices are extension work. |
| Snapshot indexer | Assemble static discovery index | manifests, request set, run summary | `index.json`, manifest indexes | Snapshot label is locator; `snapshot_identity_hash` is identity. |
| Validator | Check schemas and cross-artifact invariants | snapshot root | validation report/failure | Rechecks ADR 0001 conflicts, ADR 0026 copied fields, and snapshot-index identity. |

## First Analysis Product

The first public snapshot target needs both text-derived and tokenizer-derived
useful metrics. Implementation is sequenced so the token-independent analysis
contract lands first.

The token-independent slice follows ADR 0026:

- input view: `parser-ir-plaintext-body-v1`,
- tokenizer fields null,
- per-work analysis manifests and `analysis-result` sidecars;
- recipe identity and request-set identity exactly as defined by ADR 0026.

The first implementation slice is publication plus token-independent analysis.
It may be used for validation and cost probes, but it is not the complete
public snapshot product if the public snapshot promise includes tokenizer
outputs.

The tokenizer-backed slice needs an additional narrow contract before canonical
publication:

- tokenized artifact schema,
- tokenizer profile identity,
- token stream coordinate system,
- tokenized sidecar role,
- copied producer-field validation,
- analysis recipe rules for consuming token streams.

Provisional metric set:

- text metrics: character count, line count, paragraph count where available,
  sentence count, sentence length distribution, script/class ratios;
- tokenizer extension metrics: token count, type count, type-token ratio, STTR,
  token length distribution;
- stylometric basics: lexical richness metrics only when formula, denominator,
  token filter, and edge-case behavior are fixed in recipe JSON.

## Tokenizer Artifact Extension

Before tokenized outputs are published as canonical snapshot artifacts, a
focused tokenizer identity contract must define:

- `artifact_kind = "tokenized"` manifest requirements;
- a `token-stream` sidecar role or equivalent accepted sidecar role;
- tokenizer build, dictionary, profile/config, normalization, granularity, and
  token-output schema identity fields;
- the input view consumed by tokenization, initially
  `parser-ir-plaintext-body-v1`;
- token coordinate system and span semantics;
- copied producer-field validation from the plaintext/parser-IR producer
  manifest;
- failure semantics for tokenizer crashes, dictionary absence, and span drift;
- request-set resolver behavior when labels select tokenizer-backed recipes.

This extension should reuse ADR 0026's registry and request-set model rather
than creating a parallel tokenizer registry model.

## Open Questions

| Question | Type | How to resolve |
|---|---|---|
| Which tokenizer/profile is first? | Hard decision | Compare availability under Nix, dictionary licensing, Japanese support, output stability, and span behavior. |
| What is the exact tokenized artifact identity contract? | Hard design | Add a focused ADR or extend ADR 0026 before publishing tokenized slices. |
| What exact metric formulas are accepted? | Hard product/design | Define recipe JSON fixtures and tests; avoid metric ids with informal definitions. |
| What is the authoritative full-corpus source snapshot? | Empirical/policy | Produce a source snapshot manifest with hash and archive/source ids. |
| Does the selected parser/adapter still parse the whole selected corpus? | Empirical | Re-run full-corpus parser report for the snapshot request set. |
| Does the mixed static layout fit full-corpus costs? | Empirical check on chosen direction | Compare the default mixed layout against all-loose and all-batched alternatives for file count, upload size, and local validation. |
| Can full reproduction run without unacceptable local disk/time cost? | ADR 0003 publication gate | Run ADR 0003-style full-corpus and subset measurements with recorded hardware before publication. |

## Acceptance Criteria

- A snapshot label matching `soranoha-snapshot-YYYY-MM-DD-NN` is generated and
  recorded in the snapshot index.
- The snapshot index records request-set hash, source snapshot hash, manifest
  index hash, schema hashes, parser identity, tokenizer profile hashes, recipe
  hashes, and artifact content hashes.
- The snapshot index includes `snapshot_identity_hash` computed from
  `snapshot_index_identity_object`.
- `nix run <url>#soranoha -- reproduce --request-set full-corpus-basic-ja`
  can reproduce the selected full-corpus outputs to a local directory.
- `nix run <url>#soranoha -- validate --root <dir>` validates the reproduced
  snapshot root.
- The first implementation slice locally materializes TEI, plaintext,
  token-independent analysis results, manifests, failure manifests, request
  set, and snapshot index.
- The first snapshot label may publish TEI, plaintext, token-independent
  analysis, and all required manifests without tokenized outputs.
- The complete public snapshot target must not claim tokenizer outputs until
  the tokenizer artifact extension is accepted and implemented.
- Large derived query products are either omitted from the first snapshot or
  marked as derived/rebuildable views over canonical manifests.
- The flake does not expose a Cartesian matrix of full-corpus generated
  artifacts as package attributes.
- The full-corpus request set satisfies ADR 0003's recorded cost-envelope gate
  before public publication.

## Dev Handoff

First implementation slice: publication plus token-independent analysis.

1. Add snapshot label/index schema and fixture.
2. Add `soranoha` flake app wrapper with `list-request-sets`,
   `explain-request-set`, and fixture `snapshot-index` commands.
3. Add named request-set fixtures: `smoke-basic-ja`, `demo-basic-ja`,
   `full-corpus-publication-basic-ja`, and
   `full-corpus-analysis-basic-ja`, and `full-corpus-basic-ja`.
4. Add validation that snapshot indexes reference immutable labels and include
   identity-bearing hashes.

Tokenizer extension slice:

1. Add a focused tokenized artifact ADR or ADR 0026 extension.
2. Add tokenizer profile registry bindings using the adopted registry model.
3. Add tokenized artifact schema, sidecar role, materializer, and validation.
4. Add tokenizer-backed analysis recipes and request-set fixtures.
5. Promote tokenizer outputs into the complete public snapshot target only
   after the extension passes validation.

Non-production probes still needed:

- full-corpus parser/adapter report for the selected snapshot source;
- full-corpus materialization cost report;
- static layout file-count and transfer-size probe;
- tokenizer profile selection probe;
- metric recipe fixture review.

## Correction and Rollback

Snapshot labels are immutable. If a published snapshot has a structural problem
such as a broken index, missing failure manifests, incorrect layout metadata,
or wrong artifact hashes, publish a corrected snapshot under a new label. Do
not mutate the old index in place.

The project may publish a non-identity supersession catalog entry that marks an
older snapshot as superseded and points to the corrected snapshot. The old
snapshot index remains available for audit.

If full-corpus Nix reproduction cannot satisfy ADR 0003's cost envelope, keep
the snapshot schemas, labels, request sets, and manifest contracts, but do not
publish the full public snapshot until either the materialization strategy is
reworked or ADR 0003 is revised.
