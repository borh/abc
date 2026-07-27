# Q13: `:parser_ir_node_span_coverage` — scoped plan

Scope: decide the fate of the retained supporting observation
`:parser_ir_node_span_coverage` and of the analyzer that produces it. This is one
of the streams the corpus-tiering handoff lists as available before the Q15
coordinate decision, and it stays independent of that decision.

Governing design:
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
Governing checkpoint:
`docs/superpowers/plans/2026-07-27-next-session-handoff.md`.
`docs/adr/decisions.edn` records are hand-authored and become authoritative once
accepted.

> **This plan does not authorize code changes.** It records a traced diagnosis
> and three options. The owner picks one; only then is an implementation plan
> written. The plan is deliberately separable from Q15 — but see *Relation to
> Q15*, because the two share a root cause and the ordering matters.

## Q13 as previously stated, and why it needs restating

The design asks whether the observation is "safe to retain", and offers
"re-derive what it means, rename it, or drop it". Its stated objection is
naming: a retained number that looks like a ratio and is named like a ratio
measures something else, which invites the misreading D6 warns against. The
handoff adds a cost argument: 9.80 ms/work — 4.2% of a capture layer that is
itself 9.25× all of `ab-check` — for a number authoritative for no predicate.

Both are true and both understate it. Traced to source, the number is not a
mislabelled ratio of the right quantities. **It is computed by reading an offset
in one coordinate system as if it were an offset in a different coordinate system
that happens to share its name.**

## The traced finding

Stated with line citations, per the design's own rule that mechanism claims must
be traced rather than inferred from correlation.

**1. Parser-IR node spans never carry decoded-source offsets.**
`ab-aat-to-parser-ir/src/convert.rs:2056` `map_node_span` receives the AAT node's
own span and uses it **only** for `line`. The emitted `start`/`end` always come
from the caller's running accumulator, and the emitted `coordinate_system` is the
hard-coded string `"decoded_utf8"` (`convert.rs:2081`). Leaf callers advance that
accumulator by the node's **own emitted text**: `let end = offset +
utf8_len(text)` (`convert.rs:949`, and the same shape at 1044, 1080, 1156, 1200,
1262). `paragraph_span` (`convert.rs:779`) derives paragraph extents from those
child spans, and its comment states the intent outright — the AAT block's
`byte_start`/`byte_end` are "raw-source offsets (they include ruby/gaiji markup)"
and are deliberately **not** used.

**2. The pipeline's own name for that coordinate is "decoded_utf8", and it means
the visible-text projection.** `ab-aat-to-parser-ir/src/sentences.rs:260`:

> INVARIANT: `paragraph_text.len()` equals `paragraph_end - paragraph_start`.
> Both the visible-text projection (ruby → base, gaiji → resolved char) and the
> node/paragraph spans are in the same decoded_utf8 coordinate system […] a
> historical bug there copied raw AAT source offsets (which include ruby/gaiji
> markup) for some nodes, breaking the invariant […]

That invariant *is* the statement that spans index emitted visible text. Note
what the comment classifies as the bug: using real decoded-source offsets. Within
this crate that is correct and load-bearing — the sentence splitter indexes
`paragraph_text` with them, and using source offsets caused sentence-projection
failures on ruby-heavy corpora. **Nothing here is broken.**

**3. The accountability analyzer reads the same string as the decoded source
file.** `ab-parser-rq-source-accountability/src/analyze.rs:111` accepts a span
only when `coordinate_system == "decoded_utf8"`, then bounds it with
`Interval::new(span.start, span.end, decoded_len)` (`analyze.rs:114`) — against
the **decoded source**, validating `end <= decoded.len()` and char boundaries in
that file. It unions the spans (`analyze.rs:128`), sets `eligible` to the whole
decoded file (`analyze.rs:129`), and publishes the result with
`coordinate_system: DecodedUtf8` and `coverage_basis: NodeSpans`
(`analyze.rs:182-183`, wire string `parser_ir.nodes[*].span`,
`model.rs:27`).

**So one string names two coordinates in two crates, and the guard cannot catch
it.** The check at `analyze.rs:111` compares against a value the converter
hard-codes, so it can never fail. Bounds validation cannot catch it either:
emitted visible text is shorter than its source, so the running offsets stay
inside the decoded length and every interval validates.

**4. What the number therefore is.** Ignoring the small tail where nodes overlap
or are absent, the union of contiguous emitted-text extents has total length
≈ the emitted visible text, and the denominator is the decoded source. So:

> `:parser_ir_node_span_coverage` ≈ **visible-text bytes ÷ decoded-source bytes**
> — a markup-density ratio of the Aozora source, reported as source coverage.

This predicts the measurements the design already recorded, which is the check
that matters: `hatsukoi.txt` at 0.865 (8,137 nodes running contiguously to
224,010 of 259,320 bytes — the shortfall being ruby, gaiji and directive markup),
and the three governed works at 0.58 / 0.43 / 0.33, which are markup-dense
fixtures. A work whose source contained no markup at all would read 1.0 — which
is why 1.0 looks like a healthy coverage result and is not one.

**5. It is authoritative for nothing.** `parser_release_qualification.clj:190`
`install-source-recognition-observation` installs the ledger envelope as
`:source_span_coverage` and moves the former value to
`:parser_ir_node_span_coverage`. No predicate in
`data/parser-release-qualification-predicates.edn` declares that key; the
recognition predicate declares instrument `parser-rq-source-recognition-v1`,
which is `recognition.rs`, not `analyze.rs`. Consumers found:
`parser_release_qualification.clj:196` (the demotion),
`parser_release_qualification_test.clj:146` (asserts the demotion),
`abc/schemas/parser-rq-source-accountability-work.schema.json` (the analyzer's
published work record), and the crate's own `tests/analyze_work.rs`.

## Confirmed through the built binary, 2026-07-27

The characterization above was traced from source. It has now been measured
through the built `ab-parser-rq-source-accountability` binary
(`analyze-work`), on the governed three-work corpus and on one real work. Every
record returned `status: "ok"` with an empty `errors` array — the instrument
reports no problem with any of this.

Method and its limits are recorded under *Probe method* below. The qualification
identity is **synthesized** and authenticates nothing.

### The governed corpus

| Work | source bytes | `eligible_bytes` | `covered_eligible_bytes` | ratio |
|---|---|---|---|---|
| `000001_1` (ruby) | 43 | 43 | **25** | 0.5814 |
| `000002_2` (gaiji) | 58 | 58 | **25** | 0.4310 |
| `000003_3` (both) | 76 | 76 | **25** | 0.3289 |

The design's reconstruction recorded 0.58 / 0.43 / 0.33. Confirmed through the
instrument.

**The decisive column is `covered_eligible_bytes`: it is 25 for all three.**
Three works of 43, 58 and 76 source bytes produce identical covered totals,
because all three emit the same eight visible characters plus a newline — 25
bytes of UTF-8. The numerator does not move with the source at all. It moves
with the emitted text, which is the claim under test.

In each work the union is a single interval `[0, 25)` and every node's span
extent equals the UTF-8 length of that node's own emitted text (2/2, 3/3, 4/4),
with every adjacent pair contiguous.

### One real work — `cards/000005/files/53194_ruby_44732.zip` (`hatsukoi.txt`)

| Quantity | Value |
|---|---|
| `eligible_bytes` (decoded, windows-31j) | 259,320 |
| `covered_eligible_bytes` | 224,205 |
| ratio | **0.8646** |
| nodes | 8,137 |
| adjacent span pairs contiguous | 8,135 / 8,136 |
| span extent == own emitted text length | 8,136 / 8,137 |
| **decoded source under span == node's own text** | **7 / 8,137** |
| sum of nodes' own emitted text bytes | 224,201 |

The design recorded 0.865. Confirmed.

**The falsifying test is the bolded row.** If `coordinate_system: "decoded_utf8"`
meant what the analyzer takes it to mean, then slicing the decoded source at each
node's span would reproduce that node's own text — 8,137 of 8,137. It reproduces
it 7 times, 0.086%, which is the coincidence rate for short spans near offset 0.
Node 8135 ends at offset 224,010; the decoded source at 224,010 is mid-sentence
in unrelated body text, and 224,010 is **not a UTF-8 character boundary** in the
decoded source.

That last point is worth separating, because it shows a second guard is absent:
`Interval::new` (`interval.rs:10`) validates only `start <= end <= bound`. Unlike
the recognition path, `analyze.rs` never checks `decoded.is_char_boundary`. A
span that lands mid-character in the decoded file is accepted without comment.

### An unplanned finding: the parser-IR mixes two coordinates in one document

Nodes 0–8135 run contiguously from 0 to 224,010 in accumulator coordinates. Node
8136 is a single `source-note` (`note_type: "source-attribution"`, the `底本：`
colophon) whose span is `[258580, 258775)` — and that span is a **genuine
decoded-source span**: slicing the decoded file there yields exactly the colophon
text, differing from the node's own `text` only by CRLF versus LF. That 4-byte
difference is the whole gap between `covered_eligible_bytes` (224,205) and the
sum of the nodes' own emitted text (224,201).

So a single parser-IR document carries spans in **two different coordinate
systems, both labelled `decoded_utf8`**, and the analyzer unions across them
without noticing — adding a projection-coordinate run to a real source interval
and reporting the total as coverage. This is the residue of exactly the defect
`sentences.rs:260` describes as historical ("copied raw AAT source offsets […]
for some nodes"): it is not fully historical, it survives in the source-note arm.

Whether that is a bug in the source-note arm or the only arm doing the right
thing depends on which coordinate parser-IR spans are *supposed* to inhabit —
which is a question for the emitter's owner, and is outside Q13's scope. Q13 only
needs the consequence: the analyzer cannot tell the two apart, and neither can
any consumer of the published record.

### What this changes in the options below

Nothing in the recommendation. It strengthens the case against a rename-only
fix: a renamed field would still be computed by unioning intervals from two
coordinate systems. It also adds a fact the owner should weigh — Option 3
(re-derive over real source offsets) is larger than stated, since the emitter
does not have one coordinate to correct but two to reconcile.

### Probe method

Non-authoritative. Recorded here so the numbers can be reproduced or refuted.

- Binaries: `cargo build --release` in `ab-validator`, at tree
  `6d317103`, giving `target/release/{ab-aozora, ab-aat-to-parser-ir,
  ab-parser-rq-source-accountability}`.
- Path: `ab-aozora --mode aat` → `ab-aat-to-parser-ir convert --mapping
  ab-validator/data/aat-to-parser-ir-mapping-v2.json` (the mapping
  `parser_rq_campaign.clj:257` selects) → `analyze-work`.
- Taxonomy: `abc/data/parser-rq-ignored-regions-v1.json` unmodified, rules empty.
- The **qualification identity was synthesized** from each parser-IR's own
  `derived_from` so that `analyze-work` returns `ok`, with placeholder
  `parser_git_rev`, corpus and predicate-set hashes. It authenticates nothing and
  is meaningless outside this probe. The measured coverage quantities do not
  depend on it: the identity is compared for equality and recorded, never used in
  the arithmetic.
- Real work: `cards/000005/files/53194_ruby_44732.zip` from the pinned
  `aozorabunko` checkout, revision `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`,
  verified before use.
- The probe script was written to session scratch and is not tracked, per the
  standing rule against committing one-off capture probes. The comparisons it
  performs are: span contiguity, span extent versus the node's own emitted text
  length, and decoded-source-slice versus node text. All three are recomputable
  from the parser-IR and the decoded source alone.
- **Not measured:** the 9.80 ms/work cost was not re-timed, and no second real
  work was analyzed. One real work plus the governed control is enough to
  confirm the characterization; it is not a population claim.

## Why "rename it" is not sufficient

A rename fixes what a reader of the qualification record concludes. It leaves
three things standing:

- `analyze.rs` still computes a union of projection offsets against source
  bytes. Renaming the field does not make that arithmetic mean anything.
- The published work record still declares `coordinate_system: DecodedUtf8` for
  intervals that are not in that coordinate. That is a false claim in a governed
  artifact, independent of the field name downstream.
- The collision stays live for the next consumer. `decoded_utf8` will keep
  reading as "offsets into the decoded file" to anyone who has not read
  `sentences.rs:260`.

## Options

### Option 1 — Drop the observation and retire the `NodeSpans` analyzer path

Remove `:parser_ir_node_span_coverage` from the measurements map, and retire
`analyze.rs`'s node-span coverage as a produced artifact.

Recovers the full 9.80 ms/work — 4.2% of the capture layer, ≈12 minutes of a
78–85 minute serial full-snapshot pass — and removes a governed artifact that
asserts a coordinate it does not inhabit. Cost: deletes the only structured
record of parser-IR spans against source, and touches published schemas, so it
is a protocol change with its own migration and staleness consequences.

Against dropping: it is the fastest way to stop publishing a false coordinate
claim, but it discards the analyzer without first establishing that nothing
wanted it. The evidence says nothing does — no predicate, no other consumer —
so this objection is weak.

### Option 2 — Rename the field and correct the artifact's declared coordinate

Keep the analyzer, rename the measurement to something that states the quantity
(for example `:parser_ir_visible_text_ratio`), and correct the work record so
`coordinate_system` names the projection rather than the decoded file — which
requires a name for that coordinate distinct from `decoded_utf8`, and therefore
touches the parser-IR emitter, not just the analyzer.

Keeps a cheapish signal that would in fact detect a regression in the emitter
(a sudden jump toward 1.0 would mean visible text stopped being stripped). Costs
the 9.80 ms/work, and is the larger change: it requires introducing a coordinate
name and propagating it, whereas the collision is currently confined to one
consumer.

### Option 3 — Re-derive it as real source coverage

Make the analyzer measure what its name claims, by giving parser-IR nodes a
second span in true decoded-source coordinates and unioning those.

This is the only option that yields a genuine parser-IR source-coverage number.
It is also by far the largest: the converter deliberately does not carry source
offsets (`convert.rs:787-793`), the sentence splitter's invariant depends on
spans being projection offsets, and the history quoted at `sentences.rs:260`
records that mixing the two caused corpus-wide sentence-projection failures.
Adding a parallel source-coordinate span is a parser-IR schema change with
publication-pipeline blast radius, in service of a number no predicate consumes.

**Recommendation: Option 1**, with the naming work of Option 2 folded in only if
the owner wants the emitter regression signal kept. Nothing consumes the number;
its cost is measured and non-trivial; and the cheapest honest thing to do with an
artifact that declares a coordinate it does not inhabit is to stop publishing it.
Option 3 should not be undertaken on Q13's motivation alone — if a real
parser-IR source-coverage measure is ever wanted, it should be justified by a
predicate that needs it.

## Relation to Q15

These are two instances of one defect class: **a quantity computed across a
coordinate boundary that no guard checks.** Q15 divides a body-projection
numerator by a whole-file denominator. Q13 reads visible-text-projection offsets
as decoded-source offsets. Neither errors; both produce plausible numbers.

They stay separable, and Q13 does not need the Q15 decision. But the ordering
is worth stating: if Q15 selects body-projection coverage (handoff Option A),
the codebase will then carry **three** distinct byte coordinates — decoded file,
`aozora_body_range` body projection, and parser-IR visible-text projection — of
which two are currently both called `decoded_utf8`. Whichever Q15 option is
chosen, resolving Q13 first removes one coordinate from that set, or at minimum
gives it a distinct name.

Worth recording as a governance observation independent of both: **the guard
that compares a coordinate label against a hard-coded constant is not a guard.**
`analyze.rs:111` has the shape of a coordinate check and the strength of a
comment.

## Acceptance conditions for whichever option is chosen

- A `decisions.edn` record states the chosen option and the quantity the retained
  or removed observation actually measured. Do not record "renamed for clarity";
  the reason is a coordinate collision.
- No governed artifact declares `coordinate_system` for intervals that do not
  inhabit that coordinate.
- The governed three-work corpus is run as a control before and after. Its
  node-span values (0.58 / 0.43 / 0.33 per the design's reconstruction) are
  re-measured **through the built binary** first, because those figures are a
  Python reconstruction and the design's own failure log warns against treating
  reconstructions as instrument output.
- Schema and identity effects are stated explicitly: which schema hashes move,
  whether `instrument_policy_hashes` moves, and that `predicate_set_hash` does
  **not**, since no predicate declares this key.
- Existing capture evidence is classified as stale or protocol-incompatible, as
  appropriate, rather than silently reinterpreted.
- The 9.80 ms/work saving is re-measured rather than assumed, if it is claimed.

## Work required before implementation, whichever option wins

1. ~~Confirm the characterization through the built binary.~~ **Done 2026-07-27;
   see *Confirmed through the built binary*.**
2. Enumerate every consumer of the analyzer's work record and index, including
   any published manifest, before removing or renaming anything. Published
   manifests are immutable. The enumeration recorded above is grep-level, not
   exhaustive against published artifacts.
3. Decide whether the mixed-coordinate finding — the `source-note` arm emitting
   genuine source spans while every other arm emits accumulator offsets — is
   raised as its own item against the parser-IR emitter. It is not Q13's to fix,
   but Q13 should not be the only place it is written down.
4. Write the implementation plan against the chosen option.

## Verification gate

Standard branch gate; Rust checks are in scope because any option touches
`ab-parser-rq-source-accountability`, and Options 2 and 3 also touch
`ab-aat-to-parser-ir`:

```sh
just comment-hygiene
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
```

## Status

Diagnosis traced to source and **confirmed through the built instrument** on the
governed three-work corpus and one real work, 2026-07-27. Awaiting an owner
selection among Options 1–3; no code changed.

The instrument returned `status: "ok"` and no errors on every work measured. That
is the finding, not an aside: an analyzer can be fully green while unioning
intervals from two different coordinate systems and dividing by a third
quantity's denominator.
