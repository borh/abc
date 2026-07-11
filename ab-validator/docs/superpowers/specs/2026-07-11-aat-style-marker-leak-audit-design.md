# AAT Style Marker Leak Audit Design

**Date:** 2026-07-11

## Goal

Provide a reusable, read-only corpus audit that measures Aozora notation
markers surviving inside text descendants of parser-emitted AAT `style`
nodes, with compact deterministic evidence for the retained
`aozora-full-repin-1a4f864` dump.

## Scope and ownership

The audit belongs to ab-validator report tooling. It reads AAT JSON files and
writes reports only. It does not modify AAT, repair projections, change parser
or adapter behavior, regenerate a dump, or decide the later adapter fix.

The checked-in evidence consists only of a compact JSON summary and a rendered
Markdown report. Corpus-scale rows and copied AAT documents remain outside the
repository.

## Detection contract

A finding requires all of the following:

1. A recursively visited object has `kind == "style"`.
2. A recursively visited `text` descendant under that style has a string
   `value`.
3. The value contains an Aozora directive-shaped marker beginning with `［＃`
   and ending with `］`.

The audit therefore measures marker-shaped text inside a parser-classified
style container. It does not scan arbitrary prose and does not claim that
every finding can be repaired by deleting a substring. The later adapter fix
must use parser classification and source/provenance semantics.

Each finding is attributed to its nearest containing style node. Nested style
nodes are audited independently; their findings are not charged to outer
styles a second time.

## Command and outputs

Add a Python CLI under `reports/aat-fidelity/` accepting an AAT directory,
`--summary-json`, `--report-md`, and a bounded `--example-limit`. It streams
sorted `*.json` paths one file at a time.

The summary records:

- audit schema identifier and input label;
- files scanned, malformed files, affected files, style nodes inspected,
  affected style nodes, affected text nodes, and marker occurrences;
- counts by `style_type`;
- counts by normalized marker-form signature;
- bounded examples containing file stem, style type, JSON path, marker forms,
  and a length-bounded text excerpt.

Marker-form signatures preserve directive bodies but replace visible content
between paired leading/trailing markers with an ellipsis. Exact examples stay
bounded so one large work cannot dominate the artifact.

JSON keys, count tables, file traversal, and examples are deterministically
ordered. Markdown is rendered entirely from the summary model so the two
outputs cannot disagree.

Malformed JSON or structurally invalid `blocks` is recorded and makes the CLI
exit nonzero after writing the partial diagnostic summary. Ordinary AAT nodes
with missing optional fields are ignored unless they violate the specific
shape being audited.

## Verification

Python tests use small temporary AAT fixtures covering:

- clean styles and prose containing marker-like text outside styles;
- bold, emphasis, and bouten findings;
- nested styles without double attribution;
- multiple text descendants and multiple markers;
- deterministic ordering and example limiting;
- malformed JSON and invalid top-level shape;
- agreement between JSON counts and Markdown rendering.

The repository's ruff and mypy checks cover the new tracked Python. A pinned
dump run produces the checked-in summary and report, with the absolute dump
path represented only by a stable input label.

## Acceptance criteria

- Focused Python tests and `just python-quality` pass.
- The CLI processes the retained 17,886-file dump with bounded memory.
- Repeated runs over the same dump are byte-identical.
- Checked-in evidence is compact and contains no machine-local path.
- No parser, adapter, schema, registry, or Phase 3 plan file is modified.

