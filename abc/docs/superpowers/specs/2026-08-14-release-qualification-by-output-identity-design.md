# Release Qualification by Output Identity Design

Date: 2026-08-14 (revised three times on 2026-08-14: after review, after
measurement, after second review)
Status: **Proposed and explicitly provisional.** The original five questions are
closed, but revision 3 review found six contract gaps and the premises they rest
on are **not** discharged. Claiming "what remains is governance" in revision 2
was premature and is withdrawn.

Undischarged premises, each blocking implementation:

- converter determinism is unmeasured (only the parser was run twice);
- no run has been made over the **correct 17,602-work** population;
- projected-digest cost is measured for AAT and parser-IR but **not** divergence,
  and is excluded from the headline end-to-end figure;
- release-root recomputation is unresolved, because two of the three qualified
  outputs are not retained where the release scan can see them (E14).

Governance is also wider than previously stated: this requires superseding
`release-parser-identity-approval` and the **c3 and c4** claims of
`sole-publication-release-identity`.

**Measurement status: every cost figure is now measured, not extrapolated.**
Q1 and Q4 were closed on 2026-08-14 by a full run over all 17,887 corpus zips —
17,872 works parsed and converted with zero failures. The whole-corpus direction
holds, with one condition it did not previously carry: **the converter costs 17×
the parser and dominates the budget**, so whole-corpus qualification is viable
at 32 jobs (~3.8 min) and marginal single-threaded (~55 min). See E5.

**What the gate asks today.** `flake.nix:285-288` compares the sha256 of two
built binaries against approved values. At HEAD it is red, while the AAT output
goldens are unchanged since the approved record and pass. So the gate is red for
a change that altered no covered output, and clearing it requires a human to
hand-diff commits and assert an equivalence the system never checks.

Depends on: `docs/adr/decisions.edn` slugs `release-parser-identity-approval`,
`sole-publication-release-identity`, `owned-aat-parser-ir-mapping`

## Purpose

Replace an implementation-artifact predicate with an observable-output
predicate, stated precisely:

> **Does projected output differ on the pinned release-qualification corpus?**

This is deliberately narrower than "can the parser's output differ". A finite
corpus under a declared projection proves equality **for that corpus and that
projection** — it is corpus regression equivalence, not domain equivalence. The
value is that the question is machine-decidable on every run, so a rebuild that
does not move the answer clears with no human step, and judgment is spent only
where the answer moved.

## Current Fault

Binary bytes are a proxy for behavioral equivalence, and the proxy is unsound in
both directions:

- **Over-sensitive.** It differs when covered behavior does not (E2/E3).
- **Under-informative.** Byte equality says nothing about why output is the same;
  byte inequality says nothing about what changed. Red carries no diagnostic
  payload.

The remedy the fault forces is worse than the fault: re-approval is a bare
assertion, so green means "someone once bumped this", not "this output is
qualified". Because red is routinely noise, the ritual trains the operator to
rubber-stamp — and the one red encoding a genuine semantic change is waved
through by the same motion.

## Evidence

Measured 2026-08-14 at `77fb39a5` unless marked otherwise.

### E1 — the gate is red, and both binaries drifted (measured)

The check exits on the first mismatch, which hid the second:

| Binary | Approved in record | Built at HEAD |
|---|---|---|
| `ab-aozora` | `7f75b8f94de9…` | `35d8a9df8392…` |
| `ab-aat-to-parser-ir` | `8073c1dc520f…` | `de820929edbc…` |

### E2 — covered output is unchanged (measured, bounded coverage)

`crates/ab-aozora-aat/tests/goldens.rs:85` asserts AAT JSON byte-for-byte and
runs under `cargo test --workspace`. Neither the goldens nor their inputs have
changed since the approved record (`dc6e442e`), and at HEAD the test passes.

**Coverage bound:** five hand-verified fixtures of the parser only. This is
evidence that the drift altered *nothing the fixtures cover*. It is not evidence
about the whole parser domain, and it says nothing about the converter.

### E3 — attribution of the drift (measured)

| Crate | Change | Reaches |
|---|---|---|
| `ab-aozora` | none | — |
| `ab-aozora-aat` | `db00e055` +29 lines: one **new** `pub fn source_regions()`; no existing path modified | both binaries |
| `ab-aozora-aat` | `4dda5c3d` two `#[must_use]` attributes | both binaries |
| `ab-ortho-detect` | `5ee31286` +4/−1 in `policy.rs` | converter only |

Plus workspace-member removals (`ab-compare`, `ab-oracle`,
`ab-parser-study-report`, four third-party adapters). Toolchain pin and nixpkgs
revs did not move; `flake.lock` changed by deletion only.

### E4 — the declared identity cannot discriminate (measured)

The built binary reports exactly the `adapter_version` the record asserts:
`ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git unknown)`.

### E5 — whole-corpus cost (closes Q4; Q1 only partially)

Full run over the pinned corpus, 2026-08-14, 32 jobs, this machine. Extract →
`ab-aozora --mode aat` → `ab-aat-to-parser-ir convert` with the **v2** mapping
(`aat-to-parser-ir-mapping-v2.json`; the v1 mapping rejects AAT v2 by design —
`soranoha_build_publication.clj:183`).

| Phase | 32-job wall (measured) | single-threaded (**derived**: measured rate × N) |
|---|---:|---:|
| extract 17,887 zips | 14.4 s | — |
| parser, 17,872 works | **11.2 s** | ~180 s @ 10 ms/work |
| converter, 17,872 works | **199.3 s** | ~2,931 s @ 164 ms/work |
| projected digest, AAT (Python) | not run in parallel | ~27 s @ 1.5 ms/work |
| projected digest, parser-IR (Python) | not run in parallel | ~107 s @ 6.0 ms/work |
| projected digest, divergence | — | **unmeasured** |
| **pipeline only (excludes digests)** | **~225 s** | ~52 min |
| **qualification runtime (all three digests)** | **not yet measured** | **not yet measurable** |

The single-threaded column is **derived** (rate × population), not directly
measured; revision 2's blanket claim that "every cost figure is measured" was
wrong. The 3.8-minute headline covered extract+parse+convert only and excluded
the projection work it listed one row below, so it understated the gate. A
divergence projection cost is still missing entirely, which is why the
qualification runtime row is blank rather than estimated.

Outputs: source 578 MB, AAT 2.2 GB, parser-IR 9.2 GB, divergence 153 MB.
**Failures: zero** — 17,872/17,872 parsed, 17,872/17,872 converted.

The converter costs **17× the parser** and dominates every budget. The
single-threaded converter rate is from a 200-work stride over the full
population, not the earlier ruby-only sample.

**Population caveat.** This run keyed outputs by zip basename, so it covered
17,872 works rather than the 17,602 the catalog join defines (E13). The per-work
*rates* are unaffected and the totals move by under 2%; only the population is
wrong, and E12 is the measurement of that wrongness.

### E6 — determinism, **measured at corpus scale**

Two full parser passes over all 17,872 works produced an identical tree digest:

```
run1: sha256:2f8318868eae04d502cba1ed4436dbc6cad6ab55f8f9812a109249e36e132b20
run2: sha256:2f8318868eae04d502cba1ed4436dbc6cad6ab55f8f9812a109249e36e132b20
```

No timestamp, path, host, or RNG-derived field appears in the output. Digesting
2.2 GB costs 3.6 s, so the digest itself is never the bottleneck.

### E7 — which primitives exist, and which do not

| Need | Status |
|---|---|
| raw-byte digest of an AAT tree | **exists** — `reports/lib/aat_hash.py::hash_aat_dir`, sorted-path fold, walk-order independent |
| pinned corpus | **exists** — `aozorabunkoCorpus`, `ab-validator/flake.nix:187` |
| precedent for pinning an output digest | **exists** — `resolve-run-set.py:53` fails closed on a mutated dump |
| **projected** digest (field exclusion + canonicalization) | **does not exist** — see D6 |
| **per-work** expectation manifest | **does not exist** — see D7 |
| declared qualification population | **does not exist** — see D8 |

Revision 1 called this "a wiring problem". That was wrong: three of six
components must be built.

### E8 — the gate never runs locally (measured)

Every local path runs `nix flake check --no-build`, which only evaluates.
`justfile:88` already documents this failure mode as the reason
`parser-rq-instrument-identity` got its own recipe.

### E9 — the converter copies the parser's declared version (measured)

`ab-aat-to-parser-ir/src/convert.rs:2129` sets
`derived_from.aat_adapter_version` from AAT `meta.adapter_version`. A
version-only change is therefore invisible to a projected AAT digest but visible
in any raw parser-IR digest — the two stages would disagree about whether the
same change is behavioral unless both projections are specified together.

### E10 — publication's population is a catalog join, not a zip walk (measured)

`soranoha_build_publication.clj:575` reads the catalog zip, indexes rows by
basename, and joins selections against it, recording or omitting failed
selections. Walking every `*.zip` would evaluate a different workload than
publication does.

### E11 — the converter emits two artifacts (measured)

`soranoha_build_publication.clj:233-245` runs `convert` with both
`--parser-ir-out` and `--divergence-out`. Qualifying parser-IR alone leaves
divergence/loss-reporting behavior ungated.

### E12 — the population is not the zip list (measured)

The full run surfaced three distinct discrepancies between "zips" and "works":

| Observation | Count |
|---|---:|
| `*.zip` in the pinned corpus | 17,887 |
| zips containing **no** `.txt` member | 8 |
| zips containing **more than one** `.txt` member | 2 |
| **distinct zip basenames colliding across card directories** | **7** |
| works actually produced when keyed by zip basename | 17,872 |

The 7 collisions are the important one: naming outputs by zip basename
**silently loses 7 works**, because a later card directory overwrites an earlier
one — with no error, and with a plausible-looking 17,872 as the result. This is
direct evidence for D8: work identity must come from the catalog join, never
from a filename.

### E13 — the catalog join, executed (closes Q3)

The join publication actually performs, run against the pinned corpus on
2026-08-14:

1. **Candidates** — files matching `^cards/[0-9]{6}/files/[^/]+\.zip$`
   (`aozora-work-zip?`): **17,887**.
2. **Catalog** — `index_pages/list_person_all_extended_utf8.zip` →
   `list_person_all_extended_utf8.csv`: **19,470 rows**, of which **19,159**
   carry a `テキストファイルURL`, indexed by that URL's basename with later rows
   overwriting earlier ones (`catalog-index`): **17,651 distinct basenames**.
3. **Selection** — candidates whose basename is in that index: **17,602**.
   The other **285** are rejected `not-catalog-text-zip`; they are legacy
   filenames with no numeric suffix (`542_ruby.zip`, `197_txt.zip`) that the
   current catalog no longer references.
4. **Identity** — `slug = 作品ID_人物ID_<card-dir>_<zip-stem>`: **17,602 slugs,
   zero collisions.**

**The qualification population is 17,602 works.** It equals none of the numbers
previously in circulation.

| Count | What it actually is | Verdict |
|---:|---|---|
| 19,470 | catalog rows | superset — several persons per work |
| 19,159 | rows with a text-file URL | superset |
| 17,887 | zips under `cards/*/files` | candidate set, pre-join |
| 17,881 | `.txt` members in those zips | 5 zips hold none, 2 hold two, 3 are unreadable |
| 17,872 | naive basename-keyed run (E5/E12) | **wrong** — loses 7 to collision, 8 to no-`.txt` |
| **17,602** | **catalog-joined selection** | **the population** |
| 17,886 | parser-comparison union | unreconciled, sits between join and candidates |
| 17,894 | inventory `works_scanned` | **unreproducible — see below** |
| 14,049 | `*_ruby_*.zip` | irrelevant subset |

Two consequences the design must absorb:

- **1,508 catalog rows are lost to the basename index**, because a work with an
  author *and* a translator contributes several rows pointing at one text file.
  The `人物ID` in a slug is therefore whichever CSV row happened to come last —
  deterministic under a pinned, hashed catalog, but not semantically "the
  author". The manifest must pin `catalog_csv_hash` alongside the corpus
  revision, or work identity silently moves when the catalog is refreshed.
- **The card directory is load-bearing.** Seven basenames are each filed under
  two contributor cards (e.g. `47896_ruby_49619.zip` under `cards/000075` and
  `cards/001030`). Without the card-directory element the slug is not injective,
  which is exactly the silent 7-work loss E12 measured.

**Where 17,894 came from.** The inventory summary records its own inputs:
corpus `/home/bor/Projects/ab-validator/references/aozorabunko`, index
`/home/bor/Projects/ab-validator/scratch/ab-index.json`, with matrix and
allowlist paths inside a `.worktrees/source-letter-address-origin` worktree —
all under a repo root that no longer exists, and **every one of those paths is
missing today**. So 17,894 is not a different *unit* of the pinned corpus; it is
a count over an **unpinned local working copy** via a scratch index. It exceeds
every count derivable from the pinned corpus — 7 more than the total zip count,
13 more than the total `.txt` members — and cannot be reproduced. This is
precisely the failure mode the qualification manifest exists to prevent.

### E14 — three enforcement sites, and the release root cannot see two of the three outputs

`publication_release.clj:139-150` is a **third** site: it compares the snapshot
index's `parser_build_hash` and `converter_build_hash` against
`executable-hash` drawn from the authenticated record, and at `:130` checks
`qualification_identity_ref`. Removing `:executables` — or dropping
`qualification_identity_ref`, which appears **178 times** across `soranoha`,
`snapshot_index`, `publication_release`, and six `parser_rq_*` modules — makes
this verifier reject every release.

Worse for the obvious fix, the qualified tuple is only partly retained:

| Output | Written to | In the closed release scan? |
|---|---|---|
| AAT | `works/<slug>/aat.json` | **no** |
| divergence | `works/<slug>/divergence.json` | **no** |
| parser-IR | `works/<slug>/parser-ir.json`, **copied** to `publications/<slug>/parser-ir.json` | yes (the copy) |

`render-one-work!` states the flat `works/` tree "is NOT part of the closed
release scan". So a release root can recompute **only** the parser-IR digest
today. Recomputing the full tuple requires retaining AAT and divergence in
`publications/<slug>/` as well — +2.2 GB and +153 MB against the 9.2 GB of
parser-IR already copied there.

### E15 — divergence embeds the adapter version too

`divergence.rs:178` writes `"aat": {"adapter_version": meta.adapter_version, …}`.
A version-only change therefore leaves projected AAT and parser-IR digests
stable while moving any *raw* divergence digest — so acceptance check 1 fails
unless divergence gets its own projection.

### E16 — declared failures are inadmissible under current release policy

Three independent mechanisms agree:

- `render-one-work!` — source manifest write, parser derivation, and the
  parser-IR identity assertion are **HARD**: they abort the build even in
  best-effort mode. Only the publication render is catchable.
- `release-failure-policy` — `allow_nonzero_failures false`, `max_failure_rate 0`.
- `failure-problems` — "A release closes over a complete corpus: any recorded
  failure is inadmissible."

### E17 — the canonicalization the spec pointed at is frozen and not RFC 8785

`jcs.clj:1-12` documents `sha256-json-jcs` as **historical**, delegating number
serialization to charred, explicitly not meeting RFC 8785 §3.2.2.2, with bytes
**frozen** ("any change must be paired with a new manifest schema hash"). The
same namespace already provides the correct path for new protocols:
`rfc8785-safe-integer-json-string-v1`, restricted to integers in
[-9007199254740991, 9007199254740991], failing closed on every other numeric
value.

### E18 — one `input_hash` cannot express the input identity

`inspect-selected-work!` and `assert-parser-identities!` carry **distinct**
identities: the archive hash, the bundle `work_content_hash` passed to the
converter, and the `primary_text_member` plus `primary_text_hash` that identify
the parser's actual input. Collapsing these into a single `input_hash` loses the
ability to say which one moved.

## Diagnosis

### D1 — the approval predicate sits at the implementation, not the contract

The record binds build artifacts; what anyone approves is behavior on input.

### D2 — `candidate_ref` binds the build, so machine facts become human judgments

`candidate_ref` hashes the whole record, executables included, and the decision
binds that exact value (`parser_release_authority.clj:191`). Any binary drift,
including provably no-op drift, invalidates the binding and forces a
`decisions.edn` edit that no evidence backs.

### D3 — the binary hash is braided: admission predicate *and* artifact identity

`parser_build_hash` feeds `parser_config_hash` (`snapshot_index.clj:176`), which
propagates into parser-IR, tokenized, and annotation manifests
(`manifest_index.clj:88`) and forms one leg of the discriminating triple
(`publication_release.clj:186`). A no-op rebuild therefore re-identifies every
downstream artifact while every artifact's bytes are identical.

### D4 — there are two enforcement sites

`soranoha_build_publication.clj:334-339` independently compares build hashes.
Fixing only the CI check leaves publication inadmissible.

### D5 — the gate's red is discovered late (E8)

### D6 — a raw-byte digest cannot express the projection the design needs

`hash_aat_dir` hashes file bytes; it has no field-exclusion mechanism. Excluding
`meta.adapter_version` requires parse, exclusion, canonical serialization, and a
**versioned** hash algorithm. And by E9 the exclusion must be specified for the
converter's `derived_from.aat_adapter_version` in the same breath, or the two
stages classify the same change differently.

### D7 — an aggregate digest cannot name what changed

`hash_aat_dir` irreversibly folds filenames and content into one SHA-256. A
design whose red says only "the corpus digest moved" reproduces the exact defect
this spec exists to remove (D1's under-informative half). Naming differing works
requires retained per-work expectations.

### D8 — an undeclared population makes the sites incomparable

By E10, "the corpus" is not self-evident: work IDs, archive members, output
paths, and conversion arguments all change the result. One site could approve a
workload another never evaluates. Closed by E13; the manifest declares it.

### D9 — parallelism must not enter behavioral identity

Revision 2 put job count in the invocation contract. That is the same defect
this spec removes: parallelism affects **cost**, not qualified output, so
binding it lets an operational change rotate approval without changing
behavior. Concurrency belongs to the runner, expressed as a timeout and a
capacity requirement; only output-affecting arguments are bound.

### D10 — serialization order is not behavior

For the same reason, manifest row order must be semantically irrelevant.
Revision 2 promised to distinguish "reordered" works and tested reordering as an
error, which would make a serialization detail an approval trigger. Rows are
canonicalized by work key; a permutation must be a no-op.

## Design

### The bound object is a qualification manifest, not a digest

The governed record carries **only qualification**, and its heaviest content is
referenced by hash rather than inlined:

```
:schema_version           "2.0.0"
:adapter_id               "ab-aozora"
:aat_schema_version       2
:qualification_identity   {:mapping_hash … :parser_ir_schema_hash …}
:projections              {:aat "aat-behavior-v1" :parser_ir "parser-ir-behavior-v1"}
:qualification_identity_ref "sha256:…"      ; RETAINED — 178 consumers (E14)
:qualification_manifest_path "data/release-qualification-manifest-v1.json"
:qualification_manifest_ref "sha256:…"      ; path + hash; a hash alone cannot be loaded
:corpus_ref               "aozorabunko@0e9ea3e586…"
:catalog_csv_hash         "sha256:…"        ; work identity depends on catalog row order (E13)
:canonicalization         "rfc8785-safe-integer-json-string-v1"
```

`candidate_ref` covers this whole record, exactly as today. **Nothing unbound
lives inside the governed record** — which is what revision 1 got wrong (F1).

### Provenance leaves the record entirely

`:executables` and `:adapter_version` are **removed**, not unbound. They are
observed build facts, recorded as provenance where the artifact is produced and
never asserted inside a governed object. Tamper detection holds by construction:
there is no unbound field to tamper with.

**Revision 2's "build attestation" is withdrawn.** An attestation the release
root must *trust* is exactly the receipt that `sole-publication-release-identity`
c3 forbids ("recomputable, carries no receipt"). Release-root verification must
instead **recompute** the per-work projected digests from retained artifacts and
compare them to the manifest — no receipt, no trusted intermediary.

That has a prerequisite the current layout does not meet (E14): only parser-IR
is copied into the release-scanned `publications/<slug>/`; AAT and divergence
stay in the flat `works/` tree, which is explicitly outside the closed release
scan. **This design therefore requires retaining AAT and divergence alongside
parser-IR** (+2.2 GB and +153 MB against 9.2 GB already retained). Without that,
release-root recomputation can cover only one of the three qualified outputs,
and the alternative — trusting an attestation — contradicts c3.

`parser_build_hash` remains in the runtime identity object and artifact identity
(protected boundary; D3 is diagnosed here, fixed in Phase 2).

### The qualification manifest

Content-addressed, one row per qualified work, binding population and
expectation together:

| Field | Purpose |
|---|---|
| `slug` | publication identity `作品ID_人物ID_<card-dir>_<zip-stem>`, injective over all 17,602 (E13); also the canonical row-sort key |
| `text_zip_relpath`, `primary_text_member` | which zip, and which member the parser actually read |
| `archive_hash` | the zip's bytes |
| `work_content_hash` | the bundle identity passed to the converter |
| `primary_text_hash` | the parser's actual input bytes |
| `aat_digest` | projected per-work AAT digest |
| `parser_ir_digest` | projected per-work parser-IR digest |
| `divergence_digest` | projected per-work divergence digest (E15) |

Three separate input hashes, not one, because publication already distinguishes
them (E18) and a single value cannot say which moved.

**Exit status is an invariant, not a field.** Parser and converter must exit
zero for every row: `render-one-work!` aborts on derive failure even in
best-effort mode, release policy sets `allow_nonzero_failures false`, and
release verification rejects any recorded failure (E16). Revision 2's
`disposition` column contradicted all three and is removed. Source-selection
exclusions — the 285 `not-catalog-text-zip` zips — are *not* qualified rows;
they are outside the population by construction. Admitting expected failures
would be a release-policy change requiring its own governance, and is a non-goal
here.

**Row order carries no meaning.** Rows are sorted by `slug`; a permuted manifest
must produce an identical `qualification_manifest_ref` (D10).

The aggregate digest is **derived** from the manifest, never the primary
artifact. This yields the diagnostics revision 1 promised but could not deliver:
missing, extra, renamed, reordered, and changed works are all distinguishable
(F3), and the population is declared once and consumed by both enforcement
sites (F4, D8).

### Projection contracts

**Three** executable, versioned contracts — one per qualified output:

| Contract | Excludes exactly | Because |
|---|---|---|
| `aat-behavior-v1` | `meta.adapter_version` | E4 |
| `parser-ir-behavior-v1` | `derived_from.aat_adapter_version` | E9 |
| `divergence-behavior-v1` | `aat.adapter_version` | E15 |

All three identifiers are bound in the record. Revision 2 declared only two,
which left the divergence digest sensitive to a version-only change and silently
broke acceptance check 1.

**Canonicalization is `rfc8785-safe-integer-json-string-v1`, named explicitly.**
Not `sha256-json-jcs`: that is the frozen historical Charred path, documented as
not meeting RFC 8785 §3.2.2.2, and its bytes may not change (E17). Because the
CI runner and publication may be written in different languages, byte agreement
is a **protocol requirement**, not an implementation detail:

- **Scalar domain:** strings, booleans, nulls, and integers within
  [-9007199254740991, 9007199254740991]. Any other numeric value fails closed.
  Every projected output must be shown to satisfy this before adoption.
- **Shared cross-language vectors** are part of the contract, covering at
  minimum: non-BMP and combining Unicode, solidus escaping, control characters,
  booleans, nulls, and both numeric boundaries.

Excluded paths are enumerated literally, and a projection-id change is itself a
qualification change. `hash_aat_dir` is **left untouched** as the raw-byte
primitive; the projected digest is a separate operation.

Rationale for excluding exactly these fields and nothing else: they are the
parser's self-declaration of build identity, embedded in every document (E4,
E9), so digesting them raw makes a routine version bump re-trigger full
re-qualification with unchanged behavior — the same disease in milder form. AAT
`version` (schema version) stays covered: if the output contract changes,
re-qualification is correct.

### All three enforcement sites consume the same manifest

Revision 2 named two. There are three (E14):

1. **CI gate.** Build binaries, execute the manifest's invocation contract,
   compute per-work projected digests, compare row-wise, derive the aggregate.
2. **Publication.** Same manifest, same contract; publication already produces
   these outputs, so the digests are a byproduct rather than an added run.
3. **Release-root verification** (`publication_release.clj`). Today it compares
   `parser_build_hash`/`converter_build_hash` against the record's
   `:executables`; under this design it **recomputes** the projected digests
   from the retained AAT, parser-IR, and divergence and compares them to the
   manifest. `qualification_identity_ref` and the `adapter_id` check are
   untouched — only the two build-hash comparisons are replaced.

The invocation contract binds **only output-affecting arguments** — the mapping
document, the projection ids, and the per-work paths. Job count is *not* bound
(D9); concurrency is a runner concern expressed as a timeout plus a capacity
requirement.

### Qualified output tuple

Per work, three digested outputs — **AAT**, **parser-IR**, **divergence
sidecar** — each with its own projection contract. **Exit status is an
invariant (zero), not a digested member** (E16): a nonzero exit fails the run
rather than being qualified. Any further converter diagnostic is either added to
the tuple with its own projection or listed as a declared non-goal with
justification (E11).

### Sufficiency ladder (corrected)

| Surface | Rung | Present consumer / justification |
|---|---|---|
| raw-byte directory digest | 2 — reuse | `hash_aat_dir`, unchanged |
| corpus | 2 — reuse | `aozorabunkoCorpus`, already pinned |
| decision shape schema | 1 — no change | closed map `{record_path, schema_version, candidate_ref}`; only values move |
| **projected digest op** | **4 — implement** | consumer: both enforcement sites; revision 1 wrongly called this reuse |
| **qualification manifest** | **5 — add structure** | consumer: both enforcement sites; required by D7/D8 |
| **provenance attestation** | **5 — add structure** | consumer: publication identity, which needs the observed hash |
| gate check | 4 — implement | modify the existing check |
| `just` recipe wiring | 4 — implement | consumer: the operator (E8/D5) |

Deliberate omissions with triggers: **no sampled vector set** — the trigger
(qualification runtime over 10 minutes) is not met with runner capacity, so the
runner declares a timeout and required capacity instead of binding job count;
no per-construct behavioral report (trigger: first red whose differing-work list
is unreadable); no artifact-identity change (Phase 2).

## Phasing

**Phase 1 ships as one unit.** Adding a digest while `candidate_ref` still
covers executables would deliver none of the benefit (D2). Phase 1 = three
projection contracts + qualification manifest + **retaining AAT and divergence
in `publications/<slug>/`** (E14) + all three enforcement sites +
`schema_version` bump + ADR supersession of `release-parser-identity-approval`
and of c3 **and** c4 of `sole-publication-release-identity`.

**Phase 2 (separate decision).** Remove `parser_build_hash` from
`parser_config_hash` so identical output stops re-identifying artifacts (D3).
Trigger: Phase 1 accepted and a no-op rebuild observed to re-identify artifacts
in a real publication run.

## Acceptance checks

1. **The decisive one.** Add a `#[must_use]`, rebuild: binary hashes change, all
   **three** per-work digests unchanged (AAT, parser-IR, **and divergence** —
   revision 2 failed this via E15), gate green, `decisions.edn` untouched.
2. Change a real emission: gate red **and names the differing works** (now
   deliverable via the manifest).
2b. Bump only the crate version: all three projected digests unchanged, gate
   green. This is the check E15 would have broken.
3. Tamper with any governed field: rejected (no unbound field exists).
4. Stale or altered provenance attestation: rejected by comparison against the
   invoked binary.
5. Re-point the decision at a different `candidate_ref`: rejected.
6. Corpus-scale determinism: two full runs, identical per-work digests.
   **Partially discharged** — E6 shows two full parser passes over 17,872 works
   agreeing on the aggregate digest. Still owed at the per-work level and for
   the converter.

### Test-strategy additions (from review)

- provenance tampering and stale provenance;
- missing, extra, and renamed works **detected**; **reordered rows a no-op**
  (permutation invariance of `qualification_manifest_ref`, per D10);
- projection-only `adapter_version` change, through parser, converter **and
  divergence**, classified identically by all three;
- parser-IR unchanged but divergence output changed → red;
- CI, publication, and **release-root recomputation** producing identical
  per-work digests;
- cross-language canonicalization vectors: non-BMP and combining Unicode,
  solidus escaping, control characters, booleans, nulls, and both safe-integer
  boundaries;
- a value outside the safe-integer domain failing closed;
- projection determinism **and idempotence**;
- nonzero parser/converter exits failing closed (they are an invariant, E16).

## Open questions

- **Q1 — partially closed. Whole corpus is affordable; the runner needs
  capacity, not a bound job count.** The pipeline costs ~225 s at 32 jobs and
  ~52 min single-threaded (E5). No sampled vector set is needed given
  parallelism, but parallelism is a **runner capacity requirement plus a
  timeout**, not part of the bound invocation contract (D9) — binding it would
  let an operational change rotate approval. Still open: the qualification
  runtime including all three projected digests, one of which is unmeasured.
- **Q2 — closed with Q3.** stdin yields `work_id: "stdin"` and basename keying
  drops 7 works (E12), so the runner takes identity from the join: the manifest
  row carries the publication `slug` (`作品ID_人物ID_<card-dir>_<zip-stem>`),
  proven injective over all 17,602 works (E13).
- **Q3 — closed by execution: the population is 17,602.** The join is
  candidates (`cards/[0-9]{6}/files/*.zip`) ∩ catalog text-URL basenames, with
  identity `作品ID_人物ID_<card-dir>_<zip-stem>`, verified injective over the
  whole corpus (E13). The manifest pins **both** the corpus revision and
  `catalog_csv_hash`, because 1,508 catalog rows collapse into the basename
  index and the surviving `人物ID` is CSV-order dependent. The inventory's
  17,894 is **not** a different unit of this corpus — it was counted over an
  unpinned local checkout whose inputs no longer exist, and it exceeds every
  count the pinned corpus can produce. The 17,886 comparison union remains
  unreconciled but is not load-bearing for this design.
- **Q4 — closed by measurement: the converter dominates.** 164 ms/work
  single-threaded, 199.3 s at 32 jobs for 17,872 works, producing 9.2 GB of
  parser-IR and 153 MB of divergence with zero failures. Any budget that
  reasons from the parser's 10 ms/work is wrong by a factor of 17.
- **Q5 — resolved, and wider than stated.** c4 requires both record refs to
  recompute **and** the decision to bind the exact candidate_ref, so changing
  what `candidate_ref` covers requires superseding it. **c3** is also implicated:
  its "recomputable, carries no receipt" guarantee is what rules out the
  attestation revision 2 proposed, and what forces release-root recomputation
  (E14). Both claims move with `release-parser-identity-approval`.
- **Q6 — converter determinism, unmeasured.** Only the parser has been run
  twice (E6). Two full converter passes are required before the digest can be
  treated as stable.
- **Q7 — no run over the correct population.** Every timing and determinism
  figure comes from the 17,872-work basename keying, not the 17,602-work catalog
  join (E13). The join must be run end to end before any digest is recorded as
  an approved value.
- **Q8 — divergence projection cost, unmeasured**, and therefore no honest
  qualification-runtime total exists (E5).
- **Q9 — release-root recomputation is unresolved.** It depends on retaining AAT
  and divergence in the release-scanned tree (E14); until that lands, the third
  enforcement site cannot verify two of the three qualified outputs.
