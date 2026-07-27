# Q15: region partition and conjunctive clearance — implementation plan

Owner decision recorded 2026-07-27. This plan is written against that decision
and supersedes the Option A / Option B framing in
`plans/2026-07-27-next-session-handoff.md`, which offered two competing
denominators for one ratio.

Governing design:
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
`docs/adr/decisions.edn` is hand-authored and becomes authoritative once
accepted. The record `parser-rq-source-region-partition` was written on
2026-07-27 and is **`:proposed`**; it is the governance authority for this plan,
which carries the task sequence rather than the contract.

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
| The region set is closed: header, body, tail — no fourth | **Decided 2026-07-27** |
| The tail is `[body_end, len)`, absorbing the separating blank lines | **Decided 2026-07-27** |
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

### 1. The region set, and why it is not `aozora_body_range` verbatim

`ab-source-syntax/src/lib.rs:118` returns `(body_start..body_end, tail_start)`.
When a `底本：` line is found:

```rust
body_end  = source[..cursor].trim_end_matches(['\n', '\r']).len();  // :146
tail_start = cursor;                                                // :147
```

`body_end` is trim-adjusted; `tail_start` is not. **Whenever a tail exists there
is a gap of at least one byte between `body_end` and `tail_start`** — the blank
lines separating the body from the colophon.

**Resolved 2026-07-27: the tail region is `[body_end, len)`, not
`[tail_start, len)`.** It extends backward to meet the body, so the three regions
close the file with no fourth region and no unassigned byte:

| Region | Extent |
|---|---|
| header | `[0, body_start)` |
| body | `[body_start, body_end)` |
| tail | `[body_end, len)` |

This is symmetric with the header, which already absorbs its adjacent blank lines
because `skip_blank_lines` (`ab-source-syntax/src/lib.rs:193`) advances
`body_start` past them.

Two alternatives were rejected. Widening `body_end` *forward* to `tail_start`
puts transcriber whitespace into the body population, so body coverage would
depend on how many blank lines a transcriber left; metadata attribution over
packaging whitespace is trivially satisfiable by comparison. Changing
`aozora_body_range` itself is rejected because `sanitized.body` is the projection
handed to the parser, so moving that boundary would move `source_span_coverage`'s
numerator as a side effect of a partition fix.

An earlier draft of the ADR asserted whole-file conservation over three regions
while separately conceding a fourth was needed. Review caught the contradiction.
That the conservation identity is what exposed it — twice, once here and once in
review — is the argument for declaring it before relying on it.

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

1. **Characterize the current behaviour — DONE 2026-07-27.** See *Task 1
   result*. It changed what task 6 has to decide.
2. **Declare the partition — DONE 2026-07-27.** See *Task 2 result*.
3. **Move `source_span_coverage` onto the body region — DONE 2026-07-27.**
   See *Task 3 result*.
4. **Build metadata attribution — DONE 2026-07-27, partially by design.**
   See *Tasks 4 and 5 result*.
5. **Rebuild and re-measure — DONE 2026-07-27.** See *Tasks 4 and 5 result*.
6. **Exploratory campaign, then predeclare both thresholds.** Own predicate-set
   identity, explicitly non-authoritative. Only then fix the metadata threshold
   and re-fix `source_span_coverage`'s. The confirmatory campaign takes a
   distinct predicate-set identity; a post-hoc threshold must never be presented
   as preregistered confirmation.

**Separate deliverable, not blocked by any of the above:** map the twelve
unmapped `DirectiveKind` variants in `ab-aozora-pipeline/src/fold.rs::node_policy`
with their `ConstructId` values and policy rules. That is body-side — annotations
are part of the work — and it rotates `policy_hash` on its own.

## Task 1 result — and a finding that revises the premise

**Boundary tests first, and they all pass.** Added to `ab-source-syntax`'s own
test module, because that function is the single authority for where the body
starts and ends and a partition is only as sound as the range it derives from.
`assert_partitions` checks header/body/tail conservation and adjacency over
eight shapes: no `底本：` line, one separator, no separator, the ordinary
legend-fenced header, bare-CR line endings, empty input, blank-only input, and a
colophon with no body before it.

Two are worth naming. `the_returned_tail_start_leaves_a_gap_that_body_end_does_not`
pins the constraint-1 defect as behaviour rather than prose — on
`"本文です。\n\n\n底本：底本社\n"` the bytes between `body_end` and
`tail_start` are exactly `"\n\n\n"`, and anchoring the tail on `tail_start`
loses them while anchoring on `body_end` does not. `bare_cr_sources_do_not_find_a_colophon_line`
records that `split_inclusive('\n')` never sees a line start in a bare-CR file,
so its colophon lands in the **body** and its tail is empty. The partition still
conserves; a metadata predicate simply measures nothing there. That is a
property of the input, not a defect in the partition.

**Measured through the built binary on three real works** from the pinned
`aozorabunko` checkout (`0e9ea3e5…`, resolved through the flake input):
`hashire_merosu`, `hatsukoi`, `kokoro` — 850,980 decoded bytes together.

| Work | decoded | gap | gap @header | gap @body | gap @tail |
|---|---|---|---|---|---|
| `hashire_merosu` | 32,148 | 1,473 | 473 | **372** | 628 |
| `hatsukoi` | 259,320 | 4,062 | 461 | **2,883** | 718 |
| `kokoro` | 559,512 | 5,604 | 587 | **4,260** | 757 |
| fold | 850,980 | 11,139 | 1,521 | **7,515** | 2,103 |

Whole-file fold: **0.9869**. Body-projection fold: **0.9911**.

**The premise needs qualifying.** `parser-rq-instrument-before-threshold` c2
says the observed shortfall was *entirely* packaging never being lexed, and the
synthetic demonstration behind it showed a header and tail carrying no entry of
any kind. The header and tail do behave that way here — 3,624 of their 3,818
bytes are gap. **But two thirds of the total gap, 7,515 of 11,139 bytes, is
inside the body**, and moving to a body denominator moves the fold only from
0.9869 to 0.9911. It does not approach 1.0.

Populations differ — c2 rests on a 299-work sample and this is three works — so
this does not refute the sampled figure. What it does refute is the qualitative
claim that the coordinate mismatch is the whole story. It cannot be, for any
work whose body carries gap, and all three of these do.

**What that changes.** Task 6 was already told not to carry `:= 1.0` forward.
Now there is a measured reason rather than a precaution: on this evidence a body
denominator leaves roughly 0.9% of body bytes unrecognized, and that residue is
parser limitation, which is exactly the category c4 of
`parser-rq-instrument-before-threshold` says no instrument work will remove.
The partition remains worth building — it is what makes the two failure modes
separable and the conservation assertable — but it should not be sold as the
fix that gets `source_span_coverage` to its declared threshold.

The 0.9911 figure is a **reconstruction**, not instrument output: the region
boundaries were recomputed in Python over the decoded text, while the instrument
derives them over *sanitized* text and maps back. Roughly 100 header bytes read
as recognized, which they should not, and that discrepancy is the reconstruction
drifting from the mapping. It is a prediction for task 5 to confirm or refute
through the built instrument, and it is not an acceptance value — the same
caution this plan already applies to the ≈0.9889 separator-heuristic estimate,
which it happens to sit close to.

## Task 2 result

**`SourceRegions` lives in `ab-source-syntax`, beside the boundary authority it
restates.** Private fields and one checked constructor, `declare`, so the
conservation identity cannot be bypassed by assembling the regions field by
field. It refuses inverted ranges, out-of-bounds ends, and — the one that
matters — boundaries landing mid-character, which is the check every guard
traced in this area was missing.

**The derivation lives in `ab-aozora-aat`, as `DecodedSource::source_regions`.**
`aozora_body_range` runs over *sanitized* text, so its boundaries cannot be used
verbatim; they are mapped back through `span_ctx`. `SourceRegions::derive` is
the direct form and is correct only for a caller with no sanitize map to compose
through — which this crate never is.

**Recognition derives the regions rather than being handed them, and that is a
deliberate deviation from constraint 2.** The plan called the partition "a new
authenticated input". Carrying it in the ledger would have been that literally,
and it would have cost a ledger schema version plus regeneration of 22 capture
fixtures and 9 published ledgers. Instead `analyze_recognition` derives the
regions from `input.decoded_source`, which the generation manifest already
authenticates, by re-decoding it — already-valid UTF-8 takes the identity
branch, so the text and the sanitize map are the same ones the fact producer
used.

This is not weaker than carrying them. **A carried value can disagree with the
bytes it describes; a derived one cannot.** What it does mean is that the fact
producer and the denominator agree because they call the same code on the same
input, and that coupling is held to account by a test rather than assumed.

The regions are **published** in the recognition work record (`regions`, with
its own `region_interval` `$def` — unlike a measurement interval a region may be
empty, because a work with no editorial header or no colophon is legitimate, not
defective). So conservation is checkable from the published artifact alone,
which is the point of declaring it.

**Task 2 moves no measurement, and there is a test that says so.**
`declaring_the_partition_does_not_move_the_measurement` asserts `eligible_bytes`
is still the whole decoded file while the body is a strict subset of it. Task 3
is what changes that, and it will be visible as a change.

### The measurement that corrected two recorded claims

`crlf_sources_carry_facts_outside_the_body_and_lf_sources_do_not`.

`parser-rq-instrument-before-threshold` c2 says the header and tail carry "no
entry of any kind, not even `plain_text`", demonstrated on a synthetic work.
That holds for **LF** sources. Real Aozora sources are **CRLF**, and
`sanitizer_entries` (`classified_source.rs:444`) walks the whole sanitized text
rather than the body — so every line ending in the header and tail already
carries a `crlf_normalization` fact with a `structural_newline` role.

Measured on the three real works: 32, 31 and 34 accounted intervals fall outside
the body, all of them genuine `\r\n` pairs, 50 in the headers and 47 in the
tails — 194 of 3,818 metadata bytes.

Two consequences, both recorded in `decisions.edn`:

- **Task 3 must not assume the body region contains every accounted interval.**
  It does not, and setting eligibility to the body without handling this would
  break `recognized ⊆ eligible` on every real CRLF work.
- **Task 4 is smaller than the plan states, and has a new hazard.** The metadata
  fact producer has to cover the non-newline metadata bytes only, and it must
  not re-derive the newlines that already have facts, or two producers will
  double-count the same interval.

### Confirmed: the task 1 prediction was right

The instrument-derived regions match the Python reconstruction exactly on all
three works — header 505/495/621, body 30,985/258,077/558,100, tail
658/748/791 — so the body-projection fold of **0.9911** is now instrument
output rather than a reconstruction. It is still not an acceptance value; that
is task 6's to fix, after task 3 actually moves the denominator.

## Task 3 result

**Eligibility is the body region.** `analyze_recognition` intersects the
recognized and accounted sets against the body and takes the complement within
it, so the numerator and denominator finally inhabit one coordinate. The
metadata population — header and tail folded, since they qualify under one
conjunctive predicate — is measured in its own frame and published as
`metadata`. No byte leaves the accounting; the cross-region identity
`body + metadata == decoded` is asserted and fails closed.

**Measured on the three real works**, through the built binary:

| | eligible | accounted/recognized | fold |
|---|---|---|---|
| body | 847,162 | 839,647 recognized | **0.9911** |
| metadata | 3,818 | 194 accounted | **0.0508** |
| sum | 850,980 | — | equals decoded |

The two numbers are diagnostic in exactly the way the single number was not.
0.9911 says the parser leaves ~0.9% of body bytes unrecognized; 0.0508 says
packaging is almost entirely unattributed. The old 0.9869 said neither, and
could not distinguish them.

### The same defect, found inside the fix

Moving the denominator made the aggregate validator reject every record, and
the reason is worth recording because it is this plan's own subject matter
reappearing one level down.

`validate_record` bounded intervals by `eligible_bytes` and subtracted them
from `[0, eligible_bytes)`. But **an interval is an absolute offset into the
decoded file and `eligible_bytes` is a count** — those coincide only while the
measured region starts at zero, which the whole file did and the body does not.
The validator had the shape of a conservation check and the strength of an
assumption.

The same conflation was in the Clojure validators —
`canonical-intervals? intervals eligible_bytes` and
`interval-complement recognized eligible_bytes`. Both now take the region's
absolute bounds; the count-based arities are retained with a docstring saying
what they assume, because callers measuring from zero are still correct.

This is the third instance of one defect class in this area, and the first that
a check caught rather than a measurement. That is the partition earning its
cost: the conservation identity failed loudly instead of returning a believable
number.

### Versioning

The work and aggregate wire versions go to **v2**. `eligible_bytes` keeps its
name and changes its contract, which is precisely the case that must be
versioned rather than allowed to drift — a v1 artifact read as a v2 measurement
would silently compare a whole-file denominator against a body one. The v1
schemas are frozen alongside, with sample documents and a test asserting neither
version validates the other's records. The **index** schema does not move: it
carries provenance, not measurement.

## Tasks 4 and 5 result

**`metadata_entries` classifies exactly one metadata form: the `key：value`
colophon line.** That is what the Aozora colophon is built from — `底本：`,
`入力：`, `校正：`, `初出：` — a genuinely typed shape, so attributing it says
something. The rule is
`publication_metadata_line / publication_metadata / structural_control /
structural_token`, filling the role the policy declared and never used.

**Everything else in the header and tail is left unattributed on purpose.**
Blanket-claiming every metadata byte would drive the measure to 1.0 by
construction, and a predicate that cannot fail measures nothing. The editorial
legend block and the separator rules need their own classification before they
can be counted, and until they have one the number should show them missing.

The span excludes the line terminator, which already carries a normalization or
newline fact — two producers must not claim the same byte.

### Measured, 2026-07-27

| | before task 4 | after | **corrected** |
|---|---|---|---|
| body fold | 0.9911 | 0.9911 (unchanged) | **0.991129** |
| metadata fold | 0.0508 | 0.3002 (1,146 / 3,818) | **0.126768** (484 / 3,818) |
| cross-region | 850,980 | 850,980 | 850,980 |

The body number not moving is the check that the producer stayed in its own
population. The metadata number is meaningfully below 1.0, which is the honest
state of the instrument and the reason task 6 cannot proceed as written.

**The 0.3002 figure is superseded, 2026-07-27.** Review found it counted two
things that are not attribution: the sanitizer's line-ending normalizations,
which are true of every line in the file whether or not anything understands the
packaging and which made the number depend on a work's line endings; and header
lines matching the colophon field shape, including the standard notation legend
`《》：ルビ`, which is a `key：value` line by shape and is not publication
metadata. With attribution confined to the tail and to policy-named roles the
fold is **0.126768**, and roughly seven eighths of packaging bytes have no
classifier. Body recognition is unchanged across the correction. The reproducible
measurement is
`docs/reports/parser-rq-region-partition-exploratory-v1.md`.

### The identity rotation this cost

Adding a rule rotates `policy_hash`, which is bound into
`instrument_policy_hashes[:source_recognition]` and therefore rotates
`qualification_identity_ref` — exactly the attribution route
`parser-rq-classified-source-policy-binding` was written to create, now
exercised for the first time. The policy identity moved
`defc1c9b…` → `36c5197b…`, and the ledger schema `f508dfee…` → `07197d26…`
because the witness enum gained a member. Both `raw_bytes_hash` and
`identity_hash` in `parser-rq-classified-source-authority-v1.json` were resealed;
the capture chain fails closed on either.

`production_fixture_regenerates_byte_identically` had **no regeneration flag**,
alone among the capture fixtures, so a policy rotation had to be hand-applied to
bytes nobody can read. It has one now.

One test had to be repaired rather than re-blessed:
`structural witnesses are role-specific and span-bound` substituted the literal
role `publication_metadata` to prove the rule lookup is role-specific. That role
now has a rule, so the substitution became a no-op and the assertion stopped
testing anything. It now picks any role other than the rule's own. A hard-coded
negative case silently stops being a test the moment the thing it names becomes
valid.

## Task 6: not executed, and why

**The precondition this plan sets for itself is not met.** Task 6 predeclares
both thresholds after an exploratory campaign. The rule it inherits from Q14 —
and restates in c5 of `parser-rq-source-region-partition` — is that a threshold
may be fixed only once its instrument is known to measure what it claims.

The metadata instrument does not yet measure what its predicate would claim. It
classifies one metadata form and leaves ~70% of packaging bytes unattributed,
so any threshold fixed now would encode "colophon fields are classified and
nothing else is" as a permanent governed allowance. That is precisely the error
Q14 rejected for `source_span_coverage`, and running a campaign first would not
change it: a campaign measures the instrument you have.

`source_span_coverage`'s own threshold is in better shape but still not ready.
Its instrument is now correct — numerator and denominator in one coordinate,
conservation asserted — and it reads **0.9911** on three real works. But three
works is not a population, the design's own figure is over 299, and the residue
is parser limitation rather than instrument error, which c4 of
`parser-rq-instrument-before-threshold` says no instrument work will remove.

**What is owed before task 6 can run**, in order:

1. A classifier for the editorial legend block and separator rules, so metadata
   attribution can reach a number that means something.
2. An exploratory campaign over a real-source sample with its own
   predicate-set identity, explicitly non-authoritative.
3. Then, and only then, predeclaration of both thresholds — by the predicate's
   owner. Fixing them is a governance act, not an implementation detail, and
   `:= 1.0` must not be carried forward for either.

The measurements above are the input to that decision, not the decision.

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
unaffected and can begin now — task 2 became executable only once the region set
was closed on 2026-07-27; before that it carried an undecided fourth region.

## Governance records — written 2026-07-27

Applied, not drafted. Three `:proposed` records with narrative files:

| Slug | Covers |
|---|---|
| `parser-rq-instrument-before-threshold` | The rule and the traced coordinate mismatch |
| `parser-rq-source-region-partition` | This contract |
| `parser-rq-classified-source-policy-binding` | The identity binding, `:amends parser-release-instrument-bindings` |

`parser-rq-source-region-partition` carries the contract as claims c1–c6; c1
declares the closed three-region set and the conservation identity, c6 records
the tail-extends-backward derivation and the two rejected alternatives.

Adding a record invalidates two generated files — `docs/adr/INDEX.md` and
`docs/adr/adr-graph.mmd` — and requires an authored narrative at
`docs/adr/<slug>.md`:

```sh
clojure -M:abc/adr-governance --write-index
clojure -M:abc/diagrams
clojure -M:abc/adr-governance
```

Promotion to `:accepted` additionally requires `:kind` and non-empty `:evidence`
on **every** claim, with paths under `test/`, `fixtures/`, `nix/`, or
`docs/evidence/external/`. Only the policy-binding record satisfies that today;
the other two have nothing implemented to point at.

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

**Tasks 1–5 done, 2026-07-27. Task 6 deliberately not executed** — its own
precondition is unmet; see *Task 6: not executed, and why*.

The partition is declared, published and asserted; `source_span_coverage` is
body-projection coverage; metadata attribution exists and is measured.
Body fold **0.991129**, metadata fold **0.126768**, cross-region conservation
holds. Reproducible measurement:
`docs/reports/parser-rq-region-partition-exploratory-v1.md`.

Neither threshold is fixed, and neither should be carried forward from `:= 1.0`.
Both wire versions moved to v2 with the v1 schemas frozen, and the
classified-source policy rotation moved `qualification_identity_ref`.

**Reviewed and corrected, 2026-07-27**, in four places. Malformed region sets
underflowed the aggregate validator instead of being rejected; the colophon
producer also classified the header's notation legend; metadata attribution
counted `preserved_opaque` facts and line-ending normalizations; and the changed
measure was still shipping as `parser-rq-source-recognition-v1`, so the gate
scored a body denominator against a threshold predeclared for a whole-file one.
The instrument is now v2 while the predicate set stays at v1, which makes the
observation `:unavailable` until the predicate's owner declares a contract for
what is now measured.
