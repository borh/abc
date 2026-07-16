# Parser RQ diagnostic-gap falsification

Date: 2026-07-16

## Purpose

This report preserves the disposable P4A diagnostic-authorization
characterization that falsified diagnostics as the authority for Parser RQ
source-byte accountability. The adjacent JSON file preserves the probe output;
the checked-in text adds only a conventional terminal LF.

## Method

The ignored test `diagnostic_authorization_characterization` enumerated
`Diagnostic::ALL_CODES`, executed each source diagnostic's documented repro
(except the PUA diagnostic, whose prose repro was replaced by a concrete
U+E001-containing source), converted the resulting AAT-v2 output to Parser-IR,
and joined the emitted diagnostic span to R1 uncovered intervals for the same
authenticated bytes.

The probe was run twice with distinct `CHARACTERIZATION_OUT` paths. The two
files compared byte-identically with `cmp`; both have SHA-256
`58fc8d60b666d6fea137dafe5ddb963876afbd875ec3f6ee818bb62851171410`.
After adding the terminal LF, the checked-in JSON has SHA-256
`eff78037873d8aa9614071a2529d0371d02c5282726185f137a70f1de4352e35`.
The artifact contains 21 rows and 21 unique wire codes.

## Observations

- All 17 source rows have `r1_status = "ok"` and empty `r1_errors`.
- The sole proposed authorizer, `source-contains-pua`, has no intersection
  with an R1 uncovered interval.
- The captured artifact has 12 observe-only codes with a non-empty diagnostic
  intersection with an R1 uncovered interval.
- Three source repros do not emit their documented code:
  `unresolved-gaiji`, `forward-referent-not-stylable`, and
  `kaeriten-outside-kanbun`. Their documented repro strings are preserved
  unchanged in the JSON.
- Four codes are internal diagnostics and are proposed for rejection as source
  authorizers.

## Falsification

The only diagnostic proposed to authorize an exact source span intersects no
observed R1 gap, while many observe-only recovery diagnostics do intersect
gaps. Diagnostics therefore do not provide a sound authority for increasing R1
coverage. The parser must expose recognized-and-consumed source claims directly,
with ABC policy governing their public semantics.

## Count discrepancy

The governing design and the task brief state that eleven observe-only codes
intersect R1 gaps. Fresh execution of the disposable probe captured twelve:
`unclosed-bracket`, `accent-decomposition-applied`,
`mismatched-container-close`, `empty-ruby-reading`, `nested-ruby`,
`unrecognised-container-directive`, `tcy-target-not-found`,
`bouten-target-ambiguous`, `break-in-single-line-container`,
`bracketed-kaeriten-no-pair`, `mismatched-bouten-container`, and
`non-canonical-directive`. This report records the executed evidence rather
than rewriting it to agree with the prose count.
