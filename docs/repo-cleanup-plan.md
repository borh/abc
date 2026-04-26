# Repository Cleanup Plan

Status: Draft
Date: 2026-04-26

This plan separates cleanup from architecture work. The goal is to make the
repository reviewable without deleting potentially useful research artifacts by
accident.

## Current Decisions

- `docs/` is the canonical documentation directory.
- `/doc/` is retired.
- `docs/` should be tracked by Git.
- Generated schemas, fixtures, and example bundle files should be tracked:
  `schemas/`, `fixtures/`, and `examples/v0/`.

## Immediate Gitignore Changes

The following are local/generated/downloaded artifacts and should not appear in
normal `git status`:

| Path | Reason |
| --- | --- |
| `.cpcache/` | Clojure classpath cache |
| `.direnv/` | local direnv/Nix state |
| `.idea/`, `.lsp/`, `.clj-kondo/.cache/` | editor/tool caches |
| `cache-old/` | old downloaded/generated conversion cache |
| `dist-new/`, `gen/` | generated outputs |
| `projectFilesBackup/` | IDE backup |
| `aozora_lod_data/`, `aozora_lod_data.zip` | downloaded/generated data snapshot |
| `canopy/` | vendored/downloaded parser generator archive |
| `extern/` | external checkouts/vendor experiments |
| `data/dev/`, `data/test/` | local XTDB/RocksDB-style stores |
| `old-build-boot` | old build backup |
| `resources/*.interp`, `resources/*.tokens`, generated Java lexer/parser files | ANTLR generated outputs |

## Archive Candidates

These should be moved into an archive directory or external storage only after
one review pass:

| Path | Size Observed | Proposed Action |
| --- | ---: | --- |
| `data/` | ~167 MB | Ignore local stores now; decide whether any reduced fixture data should be extracted later |
| `references/` | ~105 MB | Keep research notes; consider moving large PDFs/data under `references/archive/` or external source pins |
| `tmp.rt` | ~97 MB | Inspect and archive/delete if generated |
| `pretty-2018-08-08.ttl` | ~93 MB | Treat as generated RDF snapshot; archive outside Git unless required as fixture |
| `tmp.ttl-` | ~59 MB | Inspect and archive/delete if generated |
| `dist/` | ~46 MB | Existing ignored build output; delete locally if rebuildable |
| root `*.ttl`, `*.xml`, `*.txt` files | varies | Classify as fixtures vs. scratch outputs |

## Review Before Ignoring Or Deleting

These may contain useful source concepts and should be reviewed before any
cleanup:

| Path | Reason |
| --- | --- |
| `aozora-parser.js`, `aozora-parser.pegjs`, `run-pegjs.js` | parser reference material |
| `resources/*.g4` | grammar experiments; likely useful |
| `scratch-abc.aozora.clj`, `refuse.clj`, `new-test-data.edn`, `tests.edn` | possible historical test or design evidence |
| `CHANGELOG.md`, `NOTES.md`, `kadai.md` | project notes; decide whether to retain under `docs/` |
| `bin/update-deps.sh` | possible useful maintenance script |

## Proposed Cleanup Phases

1. **Stabilize tracked planning artifacts**
   - Track `docs/high-level-architecture-note.md`.
   - Track `docs/adr/`.
   - Track `docs/v0-design-bundle/`.
   - Track `schemas/`, `fixtures/`, and `examples/v0/`.

2. **Reduce status noise**
   - Keep generated/downloaded directories ignored.
   - Run `git status --short --untracked-files=all` and verify that only
     review-worthy files remain.

3. **Classify root scratch files**
   - Move durable notes into `docs/archive/` or `docs/notes/`.
   - Move parser references into `references/parsers/` if they are source
     evidence.
   - Delete only files proven rebuildable or obsolete.

4. **Archive bulky generated data**
   - For large RDF/corpus outputs, prefer external archival or a local ignored
     `archive/` directory with a manifest of hashes.
   - Do not commit large generated snapshots unless they are deliberate small
     fixtures.
   - Every archived generated artifact keeps a small tracked manifest recording
     path, byte length, SHA-256, source, reason for retention, and whether it is
     reproducible.

5. **Add cleanup verification**
   - Add a script or task that validates the v0 bundle.
   - Keep `git status` small enough that new untracked files are visible.

## Open Questions

- Should `references/` stay ignored while selected research summaries move
  into tracked `docs/references/`?
- Should parser reference files be tracked or replaced by URLs/submodules?
- Is `data/test` a fixture set worth tracking in reduced form?
- Are existing root TTL/XML/TXT outputs reproducible from code?
