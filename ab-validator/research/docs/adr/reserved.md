# Reserved / Withdrawn

The ADR sequence jumps from ADR 0018 to ADR 0020. This tombstone records that
0019 was never adopted, so the gap is documented rather than appearing to be a
silently-dropped decision.

ADR 0019 was the planned "Legacy namespace disposition" ADR from the
2026-04-29 discovery plan
(`docs/superpowers/plans/2026-04-29-legacy-namespaces-clj-nix.md`). The
candidate decision was withdrawn before being written as a standalone ADR; the
sequence gap was declared intentional in
`docs/handoffs/rfc-restructure-spec.md` ("There is no `docs/adr/0019-*.md`.
The sequence gap is intentional"). Any future legacy-namespace disposition
should take a new ADR number rather than reusing 0019.

Gaps in the ADR sequence are otherwise disallowed; see `docs/adr/README.md`
("File naming").
