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

`docs/adr/` records the research decisions under which historical measurements
were made. These records include retired publication designs; they are not
instructions for operating Soranoha. `docs/reports/parser-rq/runs/` retains the
captured corpus used by source-accountability regression tests, including failed
and incomplete captures. The resource tests also verify the recorded report
against its capture and memory witness. Do not rewrite recorded identities to
make an old capture describe a current implementation.
