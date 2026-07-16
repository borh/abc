# Classified-source provenance characterization

Status: **BLOCKED** at source commit `3316a11690125e17d07d30a5cf2d85589b236d10`.

The live tiled classifier cannot distinguish recovery provenance before
`flush_plain_up_to`. No recovery-reason vocabulary is frozen, and Task 3 is not
authorized.

## Exact blocker

`ClassifyStream` retains a plain run as a byte start plus, transiently, a pending
refmark. `flush_plain_up_to` emits only `SpanKind::Plain` and the half-open source
span (`lexer/classify/mod.rs:645-663`). It has no field containing the originating
`PairEvent`, recognizer outcome, or recovery reason.

The same state and output are reached by semantically different paths:

| Input path | Evidence | State reaching the flush |
|---|---|---|
| ordinary text, solo markers, unmatched closes | `lexer/classify/mod.rs:1039-1047` | `pending_plain_start` |
| declined gaiji plus declined bracket | `lexer/classify/mod.rs:882-900` | replay into `pending_plain_start` |
| any declined buffered recognition | `lexer/classify/mod.rs:913-925` | replay into `pending_plain_start` |
| literal quote/tortoise punctuation | `lexer/classify/mod.rs:981-1000` | `pending_plain_start` |
| unclosed frame at EOF | `lexer/classify/mod.rs:1631-1644` | replay into `pending_plain_start` |
| solo refmark at a boundary | `lexer/classify/mod.rs:645-663` | folded into `pending_plain_start` |

This is stronger than a missing downstream label: adjacent paths can coalesce
into one `Plain` interval, so the recovery subinterval boundary is also gone.
Recovering it later would require replaying token/pair history or inferring it
from AAT/Parser-IR/diagnostics, all forbidden by the governing design.

## Fail-stop decision

The requested full matrix and two-run JSON comparison cannot produce truthful
recovery reasons through the live classified-span interface. Continuing would
invent reasons not observed at the required seam. The temporary probe was
therefore not retained, no `Other` reason was introduced, and the recovery
matrix remains deliberately empty rather than falsely closed.

The required prerequisite is the behavior-preserving provenance split described
by the design: make `PlainText` and `RecoveredVerbatim(reason)` distinguishable
at the point each parser path is handled, before merging or flushing. That work
belongs to the subsequent explicitly authorized refactor, not this
characterization commit.
