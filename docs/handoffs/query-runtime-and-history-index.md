# Query Runtime and History Index Handoff

Status: provisional / pre-decision
Date: 2026-07-04

## Verdict

XTDB v1 has been removed from ABC's runtime dependency surface. That cleanup
does not decide ABC's query runtime.

ABC should keep canonical truth in ordinary versioned files:

- upstream Aozora git refs,
- source corpus snapshots,
- artifact manifests,
- parser-IR, TEI, plaintext, tokenization, and report artifacts,
- RDF/PROV-O publication views.

Query runtimes should be generated views over those files. The current
provisional direction is:

- SQLite or a file index for small coordinate/history lookup.
- DuckDB/Parquet for analytical scans and corpus-scale measurement tables.
- RDF/SPARQL only when graph traversal becomes a central user workflow.
- XTDB v2 or Dolt-like stores only if ABC later needs a durable bitemporal
  application database, not merely snapshot comparison.

## Problem

ABC has two related but distinct query needs:

1. Query upstream Aozora git evolution: which refs changed metadata or sources,
   which generated person/work records changed, and which drift-sensitive
   identities require review.
2. Query ABC artifact evolution: which source snapshot, work content hash,
   parser adapter/version, mapping hash, schema hash, TEI profile, tokenizer,
   and output artifact belong to a visible publication or paper demo.

Those needs should not be solved by treating a database as the source of
identity. The source of identity remains the manifest/source/report files and
their hashes.

## Non-Goals

- Do not replace canonical manifests with database rows.
- Do not introduce an always-on service or network API for v0.
- Do not migrate the retired XTDB v1 path to XTDB v2 as part of dependency
  cleanup.
- Do not force analytical token/fidelity facts, provenance graph traversal,
  and coordinate lookup through one storage engine.
- Do not make the paper demo depend on a mutable local database.

## Current Evidence

- `abc.tools.aozora-history-audit` already extracts upstream Aozora ZIP files
  at git refs, ingests previous/current snapshots, validates the current
  corpus, and emits JSON reports. Its `--scan-history` mode walks adjacent
  commits that changed the configured CSV ZIP path.
- The paper demo already has a source corpus snapshot and per-artifact
  manifests under `paper/demo-*-real/`.
- TEI generation already lives in the parser-IR publication-rendering layer:
  `src/abc/tools/parser_ir_tei.clj` renders parser-IR values to a TEI body,
  and `src/abc/tools/materialize_publication.clj` adds the TEI header,
  validation result, and manifests.
- ab-validator already uses DuckDB/Parquet successfully for parser-fidelity and
  morph/token analytical workflows. That is strong evidence for analytical
  scans, not necessarily for ABC's coordinate/history index.
- `docs/design-survey.md` and ADR 0005 both leave query runtime undecided.

## Terms

| Term | Meaning |
|---|---|
| Canonical file | A hash-addressed source, manifest, report, or publication artifact that remains valid without a database. |
| Query pack | A generated, disposable index over canonical files. It can be deleted and rebuilt. |
| Coordinate lookup | Queries by work ID, person ID, content hash, artifact ID, source snapshot hash, adapter/version, schema hash, or output kind. |
| Upstream evolution | Changes across Aozora git refs, including generated record changes and drift candidates. |
| Artifact evolution | Derivation chains among source snapshots, parser-IR, TEI, plaintext, tokenization, reports, and manifests. |
| TEI generation | A publication renderer/materializer concern: parser-IR in, TEI artifact plus validation sidecar and manifest out. |

## Access Patterns

| Actor | Objective | Current obstacle | Capability when solved |
|---|---|---|---|
| Paper author | Show the source to parser-IR to TEI/plaintext chain for a demo work | Manifest data is split across JSON files | Query one work and list its source, plaintext, and TEI artifacts with hashes |
| Maintainer | Review Aozora upstream changes | `aozora-history-audit` emits JSON, but ad hoc filtering is manual | Query scan pairs, candidate counts, and changed participants |
| Release reviewer | Check what artifacts derive from a source snapshot | Requires file traversal | Query artifacts by `corpus_snapshot_hash` and output kind |
| Parser evaluator | Join adapter evidence with ABC admission and output artifacts | Evidence spans ab-validator reports plus ABC manifests | Query by adapter/version and mapping hash |
| Analyst | Scan token/fidelity facts across many works | Row-oriented manifest indexes are not enough | Query Parquet/columnar facts with DuckDB or similar |

## TEI Generation Boundary

TEI generation is upstream of the query pack and downstream of parser-IR
admission:

```text
ab-validator parser/AAT evidence
  -> ABC compatibility registry admission
  -> parser-IR publication rendering
  -> plaintext / TEI artifacts, validation sidecars, manifests
  -> generated query pack rows over those artifacts
```

The TEI renderer is not a query runtime and is not an XTDB replacement. It is
the publication artifact producer accepted by ADR 0025 and validated by the TEI
profile gates from ADR 0012. Query runtimes may index TEI manifests, validation
status, profile hashes, and artifact hashes, but they do not own TEI creation.

## Candidate Runtime Matrix

| Criterion | Status Quo: JSON files only | SQLite generated index | DuckDB/Parquet generated pack | RDF/SPARQL view | XTDB v2 / Dolt-like store |
|---|---|---|---|---|---|
| Coordinate lookup | Works by scripts, awkward interactively | Strong; small, portable, easy indexes | Works, but heavier startup/storage for tiny row sets | Possible if expressed as graph | Strong if modeled, but too much operational weight now |
| Upstream history scan summaries | JSON reports are canonical enough | Strong for scan-pair tables and review queues | Strong for larger scan tables | Possible but not natural for tabular summaries | Strong if history is an application database |
| Provenance traversal | File traversal works but is manual | Good for bounded manifest joins | Good for tabular joins; less natural for graph paths | Strong when graph traversal dominates | Possible, but not publication-native |
| Analytical scans | Poor | Limited | Strong; matches ab-validator prior art | Poor unless graph-shaped | Not a columnar analytics engine |
| Canonical-file discipline | Strong | Strong if regenerated only | Strong if regenerated only | Strong if RDF is a derived view | Risky if treated as source of identity |
| Operational cost | Lowest | Low | Low-to-medium; larger binaries/files | Medium; needs RDF query tooling | Medium-to-high; database lifecycle decisions |
| Revisit trigger | Ad hoc queries become too slow/manual | Full-corpus joins or FTS exceed limits | Tiny coordinate index dominated by overhead | Provenance graph traversal becomes central | Need durable transaction-time/valid-time queries |

## Probe

This probe is disposable. Artifacts live under `/tmp/abc-query-runtime-probe`.

### Inputs

Fresh bounded history scan:

```sh
nix run .#aozora-history-audit -- \
  --aozora-repo /home/bor/Dependencies/aozorabunko \
  --scan-history \
  --from-ref 276a6cff42 \
  --to-ref 0e9ea3e586 \
  --max-pairs 3 \
  --work-dir /tmp/abc-query-runtime-probe/history-work \
  --output /tmp/abc-query-runtime-probe/aozora-history-scan.json
```

Paper demo inputs:

- `paper/demo-source-corpus-snapshot.json`
- `paper/demo-rashomon-real/*.manifest.json`
- `paper/demo-melos-real/*.manifest.json`

### Generated Tables

| Table | Rows | Source |
|---|---:|---|
| `history_pairs` | 3 | bounded `aozora-history-audit --scan-history` JSON |
| `source_snapshot_inputs` | 2 | paper source corpus snapshot |
| `artifact_manifests` | 6 | source/plaintext/TEI manifests for Rashomon and Melos |

### Loaded Stores

| Store | Path | Size |
|---|---|---:|
| SQLite | `/tmp/abc-query-runtime-probe/query-pack.sqlite` | 36 KiB |
| DuckDB | `/tmp/abc-query-runtime-probe/query-pack.duckdb` | 1.8 MiB |

### Queries

The same SQL was run against both engines:

- history summary over scan pairs,
- Melos artifact chain by `slug = 'melos'`,
- artifact counts by `corpus_snapshot_hash`.

Observed results:

| Query | Result |
|---|---|
| history summary | 3 pairs, 0 split candidates, 0 merge candidates, 0 drift-participant updates, 0 validation failures |
| Melos artifact chain | source, plaintext, and TEI artifacts resolved from one `work_content_hash` |
| snapshot artifact counts | one source snapshot, 2 works, 6 artifacts |

Timing on this tiny probe:

| Engine | Wall seconds | Max RSS |
|---|---:|---:|
| SQLite CLI | 0.01 | 3,824 KiB |
| DuckDB CLI | 0.01 | 37,800 KiB |

### Probe Interpretation

Both engines answer the small query-pack questions. SQLite has much lower
storage and memory overhead for a small coordinate/history index. DuckDB's
overhead is acceptable but only becomes attractive when the query pack includes
larger analytical tables, Parquet facts, or scan-heavy workflows.

The important result is not that SQLite "wins" universally. The result is that
ABC can keep the query pack generated and disposable. That preserves the
manifest-first model while allowing multiple query engines where each fits.

## Provisional Direction

1. Treat XTDB v1 as retired from the codebase and dependencies.
2. Keep `XTDB v2` only as a survey candidate for future bitemporal application
   storage.
3. Define a small generated query-pack schema before accepting a query-runtime
   ADR:
   - `history_pairs`
   - `source_snapshots`
   - `source_snapshot_inputs`
   - `artifact_manifests`
   - later: `artifact_edges`, `parser_adapter_evidence`,
     `tokenization_regions`
4. Use SQLite first for coordinate/history lookup if the first production-like
   query pack remains row-oriented and small.
5. Use DuckDB/Parquet for analytical facts, especially tokenization,
   parser-fidelity, and corpus-scale report tables.
6. Keep RDF/PROV-O as a publication/provenance view. Promote SPARQL only when
   graph traversal is a concrete workflow.

## Falsifiers

SQLite-first is wrong if:

- a full history/artifact query pack has unacceptable import or query time,
- FTS over publication text becomes a first-class requirement and SQLite FTS is
  insufficient,
- analytical token/fidelity joins dominate the first real consumers.

DuckDB-first is wrong if:

- the first consumers mostly need low-overhead coordinate lookup and review
  queues,
- database files are much larger than the indexed manifests for no practical
  query benefit,
- write/update semantics start to matter; DuckDB should remain generated,
  not a mutable application store.

XTDB v2 or Dolt-like storage becomes relevant if:

- ABC needs durable valid-time plus transaction-time queries, e.g. "what did
  release X know about upstream ref Y on date Z?",
- multiple writers need coordinated review state,
- query-pack regeneration is too expensive and incremental database state
  becomes necessary.

## Next Work

Before accepting a Query Runtime ADR:

1. Add a non-production query-pack probe that flattens a larger
   `aozora-history-audit --scan-history` report plus all current paper/demo
   manifests.
2. Compare SQLite and DuckDB on:
   - import time,
   - database size,
   - coordinate lookup by work/person/hash,
   - artifact chain queries,
   - history review queue queries,
   - analytical scan queries if token/fidelity tables are present.
3. Decide whether the first accepted runtime is:
   - SQLite coordinate/history index only,
   - DuckDB/Parquet analytical pack only,
   - hybrid generated query pack.
4. Reconcile docs after XTDB removal:
   - keep XTDB v1 marked as retired,
   - keep XTDB v2 only as a future bitemporal candidate,
   - point ADR 0005's query-runtime split at this handoff,
   - keep TEI generation documented as publication rendering, not as query
     runtime or storage replacement.
