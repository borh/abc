# Diagnostic gap partition characterization

Date: 2026-07-16

Parser: `ab-aozora`

Coordinate system: decoded UTF-8 half-open byte intervals

## Result

The P4A3 characterization gate passes. All 17 source diagnostics emit through
the production P4A1 capture path, all cases derive R1 semantic gaps through the
production P4A2 recognition path, and the proposed v1 dispositions do not
change R1.

The closed 21-code result is:

- 1 `authorize_exact_span`;
- 16 `observe_only`;
- 4 `reject_internal`.

Only `source-contains-pua` authorizes bytes. Its emitted diagnostic span is
`[6,9)`, and its intersection with the authenticated semantic gap is exactly
`[6,9)`. It therefore requires neither span expansion nor a claim outside the
diagnostic. Six observe-only diagnostics also intersect semantic gaps; their
diagnostics remain observations and contribute no authorization.

The four invariant codes cannot be produced by a valid source capture and are
recorded as `reject_internal`: `residual-annotation-marker`,
`unregistered-sentinel`, `registry-out-of-order`, and
`registry-position-mismatch`.

## Repro repairs

The documented repro text remains unchanged in the JSON evidence. Exactly the
three previously non-emitting executable repros were replaced:

- `unresolved-gaiji`: `未知の字※［＃「架空の外字」、第3水準99-99-99］です`
- `forward-referent-not-stylable`: `我\n｜我《われ》は［＃「我」に傍点］`
- `kaeriten-outside-kanbun`: `これは［＃レ］と書いた。`

No other executable repro changed from the earlier falsification artifact.

## Evidence shape

Each source row records the complete emitted schema-v3 diagnostic, exact R1
semantic gaps and intersections, plus recovery evidence consisting of the
parser-output SHA-256, the production classified-source ledger entries, and the
documented recovery text. Internal rows deliberately contain no manufactured
source capture or interval evidence.

The machine-readable artifact is
`2026-07-16-diagnostic-gap-partition.json`. Its SHA-256 is
`7eb7d7330f107cb720242a19bba17e6ec8efff83a5389819b07d6f23894c298c`.

## Reproduction

The disposable test was run twice with distinct output paths:

```sh
CHARACTERIZATION_OUT=/tmp/p4a3-task1/run-1.json \
  cargo test -p ab-parser-rq-source-accountability \
  --test diagnostic_gap_characterization -- \
  --ignored --exact characterize_all_live_diagnostic_codes

CHARACTERIZATION_OUT=/tmp/p4a3-task1/run-2.json \
  cargo test -p ab-parser-rq-source-accountability \
  --test diagnostic_gap_characterization -- \
  --ignored --exact characterize_all_live_diagnostic_codes

cmp /tmp/p4a3-task1/run-1.json /tmp/p4a3-task1/run-2.json
sha256sum /tmp/p4a3-task1/run-1.json
```

Both tests passed, `cmp` exited zero, and both outputs had the hash above. The
probe was then deleted; only these deterministic JSON and Markdown reports are
retained.

## Integrity conclusion

The evidence supports freezing the proposed disposition table independently in
ABC. Diagnostic authorization remains downstream of R1: it partitions
authenticated semantic gaps and never recognizes or accounts for source bytes.
