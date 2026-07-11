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

Complete markers are the shortest non-overlapping matches of `［＃`, followed
by zero or more characters other than `］`, followed by `］`. Scanning resumes
immediately after each closing `］`. Thus `［＃foo］x］［＃bar］` contains exactly
the complete markers `［＃foo］` and `［＃bar］`; the stray `］` is not absorbed by
the first match.

An occurrence of `［＃` in the same text value that is not the start of a
complete match is counted separately as an `unmatched_marker_open` diagnostic.
It is not promoted to a complete-marker finding. The audit does not concatenate
sibling text nodes to reconstruct a marker split across nodes; that limitation
is stated in the generated report, so zero complete findings cannot be read as
proof of zero partial or split leakage.

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
- unmatched marker-open diagnostics, by style type, with bounded examples.

Marker-form signatures preserve directive bodies but replace visible content
between paired leading/trailing markers with an ellipsis. For example,
`［＃太字］ピアノ［＃太字終わり］` has signature
`［＃太字］…［＃太字終わり］`. A quoted-target marker such as
`［＃「ピアノ」は太字］` is one marker and retains the signature
`［＃「ピアノ」は太字］`; the audit does not reinterpret its quoted target.

Example selection permits at most two examples per file across the whole
report and selects candidates in deterministic round-robin order across
sorted style types until the global `--example-limit` is reached. Within a
style type candidates sort by file stem, JSON path, and signature. Exact
examples and excerpts stay bounded so neither one work nor one style type can
consume the evidence budget.

JSON keys, count tables, file traversal, and examples are deterministically
ordered. Markdown is rendered entirely from the summary model so the two
outputs cannot disagree.

Malformed JSON, a non-array `blocks`, or a `text` node under a style whose
present `value` is not a string is recorded as malformed input. A text node
with no `value` is also malformed because the audited AAT text shape requires
it. Other missing optional fields are ignored. The CLI writes a partial
diagnostic summary and exits 1 if any malformed input is seen. Exit 0 means the
audit completed, regardless of whether leaks were found; findings are the
subject of the report, not command failure. Exit 2 is reserved for CLI usage
errors. Checked-in evidence requires exit 0.

## Verification

Python tests use small temporary AAT fixtures covering:

- clean styles and prose containing marker-like text outside styles;
- bold, emphasis, and bouten findings;
- nested styles without double attribution;
- multiple text descendants and multiple markers;
- shortest-match behavior with two markers and a stray closing `］`;
- unmatched `［＃` accounting and the documented split-node limitation;
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
