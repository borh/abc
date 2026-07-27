# Q15: region partition and conjunctive clearance — implementation plan

Owner decision recorded 2026-07-27. This plan is written against that decision
and supersedes the Option A / Option B framing in
`plans/2026-07-27-next-session-handoff.md`, which offered two competing
denominators for one ratio.

Governing design:
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
`docs/adr/decisions.edn` is hand-authored and becomes authoritative once
accepted; a draft entry is proposed at the end of this plan for transfer, not
applied.

## The decision

**The body and the packaging metadata are two populations, not two candidate
denominators for one ratio.** The body is the work — prose and annotations. The
header and colophon are metadata *about* the work. A single ratio over their
union averages parser fidelity against packaging attribution and can mean
neither. They are measured separately, in their own coordinates, and a work is
cleared only when both clear.

- The decoded file is partitioned into declared regions derived from
  `aozora_body_range` alone. No separator or `底本：` heuristic maintained
  anywhere else.
- `source_span_coverage` becomes **body-projection coverage**: the existing
  classified-source ledger facts over `decoded.span_text`, divided by the body
  projection. Numerator and denominator finally inhabit one coordinate.
- A **metadata-attribution measure** covers the header and tail regions in their
  own coordinate, with its own facts. The policy's declared-but-unimplemented
  `publication_metadata` role belongs here.
- Header and tail are recorded as **distinct regions** so a failure localizes to
  one end of the file, but they qualify under a **single conjunctive metadata
  predicate**. Two regions, one predicate.
- **Clearance requires both predicates.** Neither number alone qualifies a work.

### Why this is not a denominator reduction

Q11 rejected shrinking a denominator to make a predicate easier to pass, and
Option A conceded that hazard. This design does not incur it: no byte leaves the
accounting. The metadata bytes move to a *different accounted region*, not out of
the denominator. Total accountability is preserved; it stops presenting two
populations as one.

### What the design gains that neither earlier option offered

**Conservation becomes assertable.** `header + body + tail == decoded file`,
disjoint, no gaps. That is a check that *can fail*.

Every defect traced in this area shares one property: no invariant existed that
could catch it. `analyze.rs:111` compares a coordinate label against a value the
emitter hard-codes, so it can never fire. `Interval::new` (`interval.rs:10`)
bounds-checks and nothing else. `source_span_coverage` divided quantities from
two coordinates and returned a plausible 0.9640. A declared partition with a
conservation check is the first structure here that breaks loudly instead of
returning a believable number.

**The numbers become diagnostic.** 0.9640 could not distinguish "the parser
missed prose" from "packaging is never lexed". It took three attempts to
establish it was entirely the latter. Two numbers separate those failure modes
by construction.

### Honest cost

This is Option A's work **plus** Option B's work, plus a partition schema and its
conservation checks. It is strictly larger than either option previously costed.
The extra cost is what buys the invariant; it is not a middle path.

## What is decided, and what is not

| | Status |
|---|---|
| Body and metadata are separate populations | **Decided** |
| Region authority is `aozora_body_range` alone | **Decided** |
| `source_span_coverage` = body-projection coverage | **Decided** |
| Header and tail are distinct regions under one metadata predicate | **Decided** |
| Clearance is conjunctive | **Decided** |
| The metadata predicate's threshold | **Not decided — deliberately** |
| Whether `source_span_coverage` keeps `:= 1.0` | **Not decided** (see below) |

**The metadata threshold is fixed only after the instrument exists and has been
measured.** Build the instrument, run it under an explicitly non-authoritative
exploratory campaign with its own predicate-set identity, then predeclare. This
is *Governance Path* steps 4–5 and the rule Q14 established: a threshold may be
fixed only once the instrument is known to measure what it claims. Declaring
`:= 1.0` now would set a contract against an instrument nobody has run — the
precise error Q14 rejected for `source_span_coverage`.

`source_span_coverage`'s own threshold is a second open question this plan does
not settle. Its declared `:= 1.0` was set against a whole-file denominator. Under
a body denominator the same literal is a *different* contract, and whether the
body population can reach it is unmeasured — the ≈0.9889 estimate came from the
disposable separator heuristic and is not an acceptance value. Re-measure first;
predeclare after. Do not carry `1.0` forward on the grounds that it was already
there.

## Traced constraints the implementation must satisfy

Stated with citations, per the design's rule that mechanism claims be traced
rather than inferred.

### 1. `aozora_body_range` does not currently partition the file

`ab-source-syntax/src/lib.rs:118` returns `(body_start..body_end, tail_start)`.
When a `底本：` line is found:

```rust
body_end  = source[..cursor].trim_end_matches(['\n', '\r']).len();  // :146
tail_start = cursor;                                                // :147
```

`body_end` is trim-adjusted; `tail_start` is not. **Whenever a tail exists there
is a gap of at least one byte between `body_end` and `tail_start`** — the blank
lines separating the body from the colophon — and those bytes currently belong to
no region.

This is structural, not incidental, and it is exactly what the conservation check
is for. The plan found it before implementation rather than after, which is the
argument for the check.

**The implementation must declare where those bytes go.** Do not silently widen
`body_end` to `tail_start`: that puts blank separator lines into the body
population, and body coverage then depends on how many blank lines a transcriber
left. The defensible options are a fourth declared region, or extending the tail
region backward to `body_end`. Either is fine; choosing neither is not.

Also handle: no `底本：` line (then `body_end = tail_start = source.len()`, empty
tail — reached by `if body_start == 0` fallback at `:137`); fewer than two
separator lines (`:130`, falls through to `hyoki_note_header_end`); and the
bare-CR works noted under *Part B, diagnosed*, whose line splitting differs.

### 2. `RecognitionInput` cannot express a region today

`recognition.rs:15` carries decoded source, parser output, diagnostics, ledger,
policy, manifest and identity — **no region and no taxonomy**. Eligibility is
hard-coded:

```rust
let eligible = if decoded.is_empty() { vec![] }
               else { vec![Interval::new(0, decoded.len(), decoded.len())…] };  // :474-477
```

So the region partition is a new authenticated input to the recognition path, not
a taxonomy-data edit. The v1 ignored-regions schema requires `rules` to be empty
and the reader rejects non-empty rules (`main.rs:137`), which is a separate
mechanism from the one this plan needs — do not overload it.

### 3. Conservation already exists, in the wrong frame

`recognition.rs:493-495` already asserts
`recognized + semantic_gaps == eligible` and
`accounted + unaccounted == eligible`. Those checks are sound; they are simply
stated over the whole file. **Restate them per region and add the cross-region
identity** (`header + body + tail (+ inter-region) == decoded file`, pairwise
disjoint). Do not remove the existing assertions.

### 4. The metadata regions have no facts at all today

The ledger lexes `decoded.span_text` (`classified_source.rs:585`), which is
`sanitized.body` (`ab-aozora-aat/src/lib.rs:296`). Demonstrated on a synthetic
work: the entire 216-byte header and 108-byte tail carry no entry of any kind —
not `plain_text`, not `newline`. Metadata attribution therefore needs a **fact
producer**, not just a policy rule. A rule alone still classifies nothing.

## Task sequence

Each task is separately reviewable. Do not combine them to reduce commit count;
combine identity rotations only after each constituent change is independently
reviewed.

1. **Characterize the current behaviour.** Capture the governed three-work
   control and one real work through the built binary as they stand, so any
   later movement is attributable. Add failing boundary tests *before* changing
   the instrument: no `底本：` line, fewer than two separators, bare-CR line
   endings, and the body/tail gap from constraint 1.
2. **Declare the partition.** Schema plus derivation from `aozora_body_range`,
   with the gap resolved explicitly. Assert conservation and disjointness. This
   task adds a check that can fail and changes no measurement.
3. **Move `source_span_coverage` onto the body region.** Thread the partition
   into `RecognitionInput`, replace the hard-coded whole-file eligibility, keep
   the existing per-region conservation assertions. Rotates the recognition
   schemas and `instrument_policy_hashes`; whether it rotates
   `predicate_set_hash` depends on task 6.
4. **Build metadata attribution.** A fact producer for the header and tail
   regions, then roles, dispositions and spans for packaging metadata —
   `publication_metadata` among them. New schemas, new tests.
5. **Rebuild and re-measure.** The classified-source policy is
   `include_bytes!`-embedded and
   `parser-rq-classified-source-authority-v1.json` fails closed on
   `raw_bytes_hash` and `identity_hash`. Re-run the governed control, then
   re-measure a real-source sample through the built binary. The ≈0.9889 figure
   is not an acceptance value.
6. **Exploratory campaign, then predeclare both thresholds.** Own predicate-set
   identity, explicitly non-authoritative. Only then fix the metadata threshold
   and re-fix `source_span_coverage`'s. The confirmatory campaign takes a
   distinct predicate-set identity; a post-hoc threshold must never be presented
   as preregistered confirmation.

**Separate deliverable, not blocked by any of the above:** map the twelve
unmapped `DirectiveKind` variants in `ab-aozora-pipeline/src/fold.rs::node_policy`
with their `ConstructId` values and policy rules. That is body-side — annotations
are part of the work — and it rotates `policy_hash` on its own.

## Sequencing: decide Q13 first

Q13's Option 1 (drop `:parser_ir_node_span_coverage` and retire the `NodeSpans`
path) **removes files from this plan's blast radius.** `analyze.rs` and
`aggregate.rs` also hard-code whole-file eligibility (`analyze.rs:129`); if that
path is retired, task 3 does not have to migrate it, and two schemas plus four
test files leave the change set.

If Q13 is instead resolved by keeping the analyzer, task 3 must migrate it too,
and the mixed-coordinate finding recorded in
`plans/2026-07-27-q13-node-span-coverage.md` becomes a blocker rather than a
note — a partition cannot be imposed on spans whose coordinate is undetermined.

**Recommendation: settle Q13 before starting task 3.** Tasks 1 and 2 are
unaffected and can begin now.

## Draft `decisions.edn` entry — for hand transfer, not applied

`decisions.edn` is authored by hand, never generated, so this is proposed text
rather than an edit. Slug and claim ids are suggestions; relations should be
checked against the live file before transfer.

```clojure
{:slug "parser-rq-source-region-partition"
 :release-authority :development
 :relations
 [{:class :lifecycle :type :depends-on
   :to "parser-release-instrument-bindings"}
  {:class :lifecycle :type :depends-on
   :to "custom-parser-release-qualification"}]
 :claims
 [{:id :c1
   :statement
   "The decoded source is partitioned into declared regions derived solely from `aozora_body_range`: a body projection carrying the work (prose and annotations) and metadata regions carrying the header and the `底本：` colophon. No separator or `底本：` heuristic is maintained elsewhere. The regions are pairwise disjoint and their union is the whole decoded file; the instrument asserts that conservation and fails closed when it does not hold."
   :kind :structural-invariant}
  {:id :c2
   :statement
   "`source_span_coverage` is body-projection coverage: classified-source ledger facts over `decoded.span_text`, divided by the body projection. Numerator and denominator inhabit one declared coordinate. It is not a whole-file measure and must not be read as one."
   :kind :measurement-contract}
  {:id :c3
   :statement
   "Packaging metadata is measured by a separate attribution predicate over the header and tail regions in their own coordinate. Header and tail are recorded as distinct regions so a failure localizes, and qualify under a single conjunctive predicate. This is an attribution measure over packaging, not a parser-fidelity measure."
   :kind :measurement-contract}
  {:id :c4
   :statement
   "A work is cleared only when both body-projection coverage and metadata attribution clear. Neither predicate alone qualifies a work."
   :kind :qualification-rule}
  {:id :c5
   :statement
   "Neither threshold is fixed before its instrument has been built and measured under an explicitly non-authoritative exploratory campaign. `source_span_coverage`'s prior `:= 1.0` was declared against a whole-file denominator and is not carried forward by default; under a body denominator the same literal is a different contract."
   :kind :governance-rule}]}
```

Supersedes, in the same transfer: the Q14 record must not preserve the
`publication_metadata`-missing-rule causal claim, and the Q15 record replaces the
Option A / Option B framing entirely.

## Verification gate

```sh
just comment-hygiene
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
```

The governed three-work corpus is the control at every step. Re-measure real
source through the built binary, never through a reconstruction.

## Status

Decision recorded. Implementation not started; no code changed. Tasks 1 and 2
are unblocked. Task 3 waits on Q13.
