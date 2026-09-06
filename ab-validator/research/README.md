# Parser research

This directory owns parser qualification policies, source-authority and parser-IR
measurement code, schemas, and retained research evidence. Clojure namespaces use
`ab-research`; shared rendering and canonical encodings come from Soranoha.

Run checks from the monorepo root:

```sh
nix build ./ab-validator#checks.x86_64-linux.research-clojure-tests
nix build ./ab-validator#checks.x86_64-linux.research-python-tests
```

`schemas/` is the canonical schema source consumed by the Rust converter and
research reports. `AB_RESEARCH_ROOT` and the converter's `--research-root` option
select an explicit research root when running outside the packaged environment.
Historical schema identifiers and recorded capture hashes remain unchanged.

The publication system is under `soranoha/`. Historical publication captures in
`test/fixtures` and `docs/reports` are evidence for their recorded instruments;
they do not define the current release format.

`docs/reports/parser-rq/runs/` retains the captured corpus used by
source-accountability regression tests, including failed and incomplete captures.
Resource tests verify recorded reports against their captures and memory witnesses.
Recorded identities describe their original instruments and must not be rewritten
to describe a newer implementation.

## Evidence and identity boundaries

Parser-selection studies compare instruments; conversion-compatibility evidence
binds an exact adapter/version tuple. A neutral comparison must identify its frozen
study contract and cannot establish conversion compatibility. The citation index in
`data/parser-evidence-citations.edn` identifies retained reports by path and digest.

Source accountability measures the partition of source bytes independently from
parser output. Parser output alone cannot establish markup coverage. Terminal
source attribution is distinct from colophon metadata; the shared classifier in
`../reports/lib/terminal_provenance.py` defines their boundary for measurements.

A source archive hash identifies exact ZIP bytes. Its logical bundle hash identifies
normalized member paths, member contents, and the selected primary text member;
compression metadata does not affect that identity. Publication owns the bundle
inspector and TEI/plaintext rendering. Research consumers use its explicit outputs
rather than independently inferring source identity from parser text.

The qualification command `-m ab-research.parser-release-qualification` accepts
an input bundle and an output JSON path. Current reports identify schema
`abc/parser-release-qualification-report/v3`; `gate_status` is the qualification
outcome, supported by coherence, admission, and per-predicate results. The command
writes the report file and prints the gate status and verdict tally to stderr.
