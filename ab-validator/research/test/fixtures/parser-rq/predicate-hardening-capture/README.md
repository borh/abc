# Retained predicate-hardening capture

This directory is evidence, not a regenerable fixture. It holds a capture
produced by a past instrument, together with everything needed to re-derive that
instrument's verdict today.

`parser-ir-0.7.0.schema.json` is the parser-IR schema as it stood when the
capture was taken, rather than the live schema at
`ab-validator/research/schemas/parser-ir.schema.json`. The
committed instrument manifest
`ab-validator/data/parser-rq-parser-ir-conformance-validator-v1.json` names this
schema by hash, so replacing it with the current one would break the historical
policy's identity and, with it, the claim that the capture's verdict is
reproducible.

`ab-research.parser-rq-predicate-hardening-capture-test` pins the retained
schema's hash and rebuilds the aggregate from `store/` and `manifest.json`
against it. A change to the live schema must therefore leave this directory
untouched.

Do not regenerate these files or point the capture smoke's `--write` mode at
this path; `ab-validator/tests/parser-rq-predicate-hardening-capture-smoke.sh`
refuses that target for the same reason.
