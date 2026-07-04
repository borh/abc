# Monorepo Component Boundaries and Naming

Status: active handoff
Date: 2026-07-04

## Purpose

This handoff integrates the Soranoha naming-migration plan into the monorepo
planning surface without making the namespace/vocabulary rename part of the
first monorepo move.

Source integrated:

- `docs/superpowers/plans/2026-07-03-soranoha-naming-migration.md`

Related historical design input:

- `docs/superpowers/specs/2026-07-03-soranoha-naming-design.md`

That plan proposed a full `abc` to `soranoha` migration across Clojure
namespaces, JSON-LD contexts, SHACL, Schematron rule IDs, Nix apps, and tests.
It is useful as a component taxonomy, but it is too broad to execute as a
monorepo prerequisite.

## Decision Boundary

The near-term monorepo migration removes physical checkout boundaries. It does
not rename the accepted ABC contract surface.

Keep stable until a separate rename/vocabulary ADR accepts a migration:

- `abc.*` Clojure namespaces;
- `abc.tools.*` command namespaces and Nix app semantics;
- `https://w3id.org/abc/...` vocabulary and artifact identity URIs;
- `abc-...` Schematron rule IDs and validation result rule references;
- existing manifest, parser-evidence, TEI, RDF, SHACL, and fixture paths.

Do not perform a repo-wide search-and-replace from `abc` to `soranoha` as part
of monorepo setup. That would braid branding, code namespaces, public
vocabulary identity, validation rule IDs, and historical artifact references
into one change.

## Component Taxonomy

The Soranoha plan's component names are useful as planning labels:

| Label | Planning role | Current concrete surface |
|---|---|---|
| `core` | Shared schemas, hashing, canonicalization, logging, NDC/text helpers | ABC schemas and `abc.tools.*` helpers |
| `yomi` | Source reading, Aozora ingest, upstream history, parser evidence intake | Aozora CSV/history tools, parser evidence citations, compatibility registry intake |
| `ori` | Publication/curation rendering | parser-IR plaintext/TEI rendering, TEI validation, RDF/Linked Art/IIIF views |
| `kura` | Storage and query packs | generated manifest indexes, future SQLite/DuckDB query packs, retention/packing decisions |
| `za` | Presentation and publication surfaces | paper demos, publication views, future UI/API surfaces |

Use these labels in planning prose when they clarify ownership. Do not map them
to Clojure namespaces, filesystem roots, or public vocabulary terms until a
separate migration ADR exists.

## Monorepo Layout Rule

The first monorepo should preserve logical producer/consumer roles:

| Logical component | Role | Identity rule |
|---|---|---|
| `ab-validator` | Producer-side parser, adapter, AAT, parser-IR conversion, and measurement evidence | Report paths and hashes remain producer evidence. |
| `abc` | Consumer-side acceptance, registry admission, publication rendering, validation, manifests, and query packs | Manifests and schemas remain the accepted publication contract. |
| `paper` | Ignored or generated paper/demo working area | Not a canonical contract surface; tracked evidence belongs under `docs/handoffs/` or `data/`. |

The physical paths may become `monorepo/ab-validator/...` and `monorepo/abc/...`,
but evidence records should continue using logical workspace-relative component
paths such as `ab-validator/docs/superpowers/reports/...`.

## Rename ADR Requirements

A future Soranoha rename is a separate high-blast-radius ADR. It must decide:

1. Whether `Soranoha` is a public project name, a namespace prefix, a vocabulary
   base, or only an internal component taxonomy.
2. Whether existing `https://w3id.org/abc/...` terms remain permanent aliases,
   are superseded by `https://w3id.org/soranoha/...`, or never move.
3. How historical manifests, validation results, Schematron rule IDs, SHACL
   reports, RDF/Turtle fixtures, and paper citations remain resolvable.
4. Whether code namespaces move in phases, and how downstream scripts and Nix
   apps keep compatibility aliases.
5. Which validation command proves the migration preserves behavior.

The old implementation plan's proposed `git mv` tasks are therefore archived
as design input, not executable instructions.

## Query-Pack Implication

Generated query packs should include both:

- physical path columns for local files; and
- logical component/path columns for durable identity and citations.

That makes the query pack resilient to monorepo path changes and avoids
treating a local database or physical checkout layout as the source of truth.

## Next Work

1. When the monorepo exists, update path-producing scripts to emit logical
   component paths in addition to local physical paths.
2. Keep `data/parser-evidence-citations.edn` as the model for evidence records:
   logical path, temporary current locator, hash, component, and evidence class.
3. Extend the first generated query-pack prototype with `logical_component` and
   `logical_path` fields.
4. Only open a Soranoha rename ADR after the monorepo migration and parser-IR
   Level 3 evidence work stop moving.
