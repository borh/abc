# snh protocol v1 — live specification

Status: **NORMATIVE DRAFT — the sole normative source, effective now
(F84).** The D16.1 freeze changes stability (no further changes without
a decision-log entry), not precedence. Open items marked inline: the
owner's O3 choice, the assessment-snapshot content fields, and the F83
naming ratification. Per F80, the FROZEN objects are the executable
JSON Schemas plus the conformance vectors (§11). Authority split
(F88): the JSON Schemas govern STRUCTURE; this document governs
SEMANTIC and STATE invariants; the conformance vectors demonstrate
both. Any disagreement among the three BLOCKS the freeze — and after
it, is a defect resolved by a decision-log entry, never by silently
preferring one artifact.

This document contains only the live protocol. Rationale, decision
history, and superseded designs live in the design ledger
(`2026-08-24-publication-rearchitecture.md`); nothing there overrides
this document. Contingency designs (key rotation, recovery statements,
signature envelopes) are NOT part of v1 — see the ledger's contingency
appendix for their activation triggers.

## 1. Canonical form and identity

- Canonicalizer: `rfc8785-safe-integer-json-string-v1` (test vectors:
  `abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json`).
- No floats anywhere; integers within the safe range; object keys sorted
  per RFC 8785; array sort orders as specified per field. Canonical
  bytes are unique for a given value.
- Every content id is `sha256` lowercase hex over canonical bytes (for
  JSON objects) or exact published bytes (for artifacts).
- Artifact id string form: `snh:1:<type>:<sha256hex>`.
- `manifest_id` = sha256 hex over the manifest's canonical bytes.
  External citation form: `snh:1:release-manifest:<hex>`.
- Resolver: artifact id → `/blobs/sha256/<hex>` (+ type-suffixed
  convenience path).

## 2. Type registry (closed)

Per-work types (the only types permitted in `works[].artifacts`):
`tei`, `plaintext`, `tei-validation`.
Release-level types: `release-manifest`, `assessment-snapshot`,
`admission-report`, `governance-event`.
Any addition or removal is wire version `snh-manifest/2`; closed-schema
v1 consumers must never meet unknown members.

## 3. `snh-manifest/1`

Required top-level fields; closed schema — no other members. No dates
of any kind appear in manifest bytes.

| Field | Contract |
|---|---|
| `schema` | literal `"snh-manifest/1"` |
| `corpus` | `{upstream_origin (URL string), upstream_rev (commit hex)}` |
| `toolchain` | object: stage-id (matches `^[0-9a-z][0-9a-z-]*$`, F80) → `{nix_closure_hash (string), stage_code_version (string)}`; keys sorted. Provenance/derivation-key input only — never an input to artifact identity |
| `selection_params` | object; string keys sorted; values strings or safe-range integers; `{}` when the inclusion rule takes no parameters |
| `admission` | `{policy_id (string), policy_hash (hex), inclusion_rule_id (string), inclusion_rule_hash (hex), assessment_snapshot (artifact id), admission_report (artifact id)}` |
| `works` | array sorted by `slug` as raw UTF-8 bytes ascending; slugs match `^[0-9a-z_-]+$` (non-empty, F80), unique. Each `{slug, source_content_hash (hex of upstream source bytes), artifacts}`; `artifacts` sorted bytewise by `type`, each `{type, id, bytes}` with `bytes` = exact byte length (non-negative safe integer). Every work has exactly one artifact per per-work registry type |
| `withdrawn` | array sorted by `slug`; each `{slug, event}` with `event` a `governance-event` artifact id — the GOVERNING event carrying the public reason |
| `validation_summary` | `{invalid_count (integer), invalid_slugs (sorted array of slugs)}` |
| `governance_event` | `null`, or the `governance-event` artifact id this manifest executes; non-null exactly when the manifest performs a withdrawal or event-amendment |
| `prev_manifest` | hex of the predecessor manifest's canonical bytes; genesis = 64×"0" |

## 4. `snh-governance-event/1`

Public, sanitized, canonical-bytes artifact; published in the release
commit; GC-rooted; resolvable by hash.

```
{schema: "snh-governance-event/1",
 kind: "withdrawal" | "event-amendment",
 entries: [{slug, reason_code, statement, amends?}]}  // sorted by slug
```

(F82: `authority` and `evidence_hash` are REMOVED from v1 — `authority`
duplicated the pinned governance key's role; `evidence_hash` had no
frozen evidence serialization, retention policy, or verifier consumer,
and publicly committed potentially sensitive material. Private
request/evidence stays in the publisher's operational record; a public
evidence commitment returns only with a concrete audit consumer and
encoding.)

- `reason_code` ∈ {"rights", "takedown-request", "data-defect", "other"}.
- `statement`: string, may be empty. No dates in event bytes.
- `entries` is non-empty with unique slugs. In a `withdrawal` event NO
  entry has `amends`; in an `event-amendment` event EVERY entry has
  `amends` (the superseded event's artifact id).
- **[O3-PENDING]** The `event-amendment` kind exists only under owner
  option (b). Under option (a) it does not exist and corrections
  require wire v2.
- Signed by the GOVERNANCE key (see §7). Stored in the release commit at
  `governance/<hex>.json` (convenience copy; CAS blob authoritative)
  with detached signature `governance/<hex>.sig`.

## 5. Admission evidence

Two retained public content-addressed artifacts; both are permanent GC
roots; both resolve by hash.

**`snh-assessment-snapshot/1`** — assessment FACTS only, for every
candidate in the selected population, per rights-relevant contribution:
status ∈ {public-domain, in-copyright, undetermined, not-evaluated},
jurisdiction, effective date, recorded basis. `not-evaluated` (no
completed assessment) and `undetermined` (assessment performed,
inconclusive) are distinct and never collapsed; absence of a completed
assessment is an explicit `not-evaluated` fact, never an omitted
contribution. No admission decisions appear here.
**[OPEN until the pre-Slice-2 freeze: exact field names/shape — the
minimal content schema, exercised on the Slice-2 fixture.]**

**`snh-admission-report/1`** — the inclusion rule's TOTAL PARTITION:

```
{schema: "snh-admission-report/1",
 assessment_snapshot: <full artifact id>,    // F80: typed id, matching
 policy_hash: <hex>,                         // the manifest's admission
 inclusion_rule_id, inclusion_rule_hash: <hex>,
 admitted: [slug...],                        // sorted
 excluded: [{slug, reason_code}...],         // sorted by slug
 quarantined: [{slug, reason_code}...]}      // sorted by slug
```

Every candidate appears exactly once across the three sets. Report
`reason_code` values match `^[0-9a-z-]+$`; their DOMAIN is defined by
the content-addressed inclusion rule (`inclusion_rule_id` +
`inclusion_rule_hash`). The hash BINDS that vocabulary but does not
resolve it (F87 — v1 publishes no resolver for rule bytes): the
schema constrains syntax publicly; the semantic domain is checked by
the ASSEMBLER against the rule bytes it holds.

## 6. Encodings (wire contracts; conformance vectors required)

- Signed MESSAGE: the exact ASCII bytes of the domain-separated string.
  No trailing newline, no BOM, no framing.
  - manifests: `snh-manifest-sig/1:<manifest_id>`
  - governance events: `snh-governance-event-sig/1:<event hex>`
- `.sig` file: EXACTLY 64 raw Ed25519 signature bytes.
- `.pub` file: EXACTLY 65 bytes — 64 lowercase ASCII hex characters
  (the 32 raw Ed25519 public-key bytes) + one LF.
- Key FINGERPRINT: lowercase sha256 hex over the DECODED 32 raw key
  bytes (never over `.pub` file bytes).
- `releases/HEAD` file: EXACTLY 65 bytes — 64 lowercase ASCII hex + one
  LF. Present in every commit of the publication branch including the
  initial one; 64 zeros before genesis; the current raw manifest_id
  after each publication.

## 7. Keys and verification

Two directly pinned, disjoint Ed25519 keys; no key-manifest, no in-band
rotation, no envelope in v1:

- **RELEASE key** (online, held by CI): signs release manifests —
  authenticates that Soranoha issued the release.
- **GOVERNANCE key** (offline, owner-held): signs governance events —
  authenticates withdrawal/amendment authority. A compromised release
  key cannot withdraw works.

The verifier selects the key from the signed object's kind, constructs
the domain-separated message from the artifact it holds, and verifies
the raw signature. Public keys: `keys/release.pub`,
`keys/governance.pub` (encoding §6) — convenience copies; the trust
anchor is the out-of-band fingerprints, obtained from the owner-named
pre-release discovery channel, which carries the Zenodo concept DOI and
both fingerprints before the first signed release.

## 8. Verifier invariants

Structural:
- `works[].slug` unique; `withdrawn[].slug` unique; the sets DISJOINT.
- Every artifact id hash is 64 lowercase hex; `bytes` matches the
  stored blob's length.
- Type-prefix and hash checks are EXPLICIT (F80): every artifact id's
  `<type>` component must match its field context (`withdrawn[].event`
  and `governance_event` are `governance-event`; `admission.*` ids are
  `assessment-snapshot`/`admission-report`; `works[].artifacts[].id`
  type equals its `type` member); for every artifact the verifier
  FETCHES, it recomputes sha256 over the bytes and requires equality
  with the id's hash component.
- `invalid_count == count(invalid_slugs)`; `invalid_slugs` ⊆ works'
  slugs, sorted; summary re-derivable from the per-work `tei-validation`
  artifacts.

Admission (fetch both evidence artifacts by hash):
- The report's fields match `admission` field-for-field over the
  fields the report actually carries (F89 — the report has NO
  `policy_id`): report `assessment_snapshot` ==
  `admission.assessment_snapshot`; report `policy_hash`,
  `inclusion_rule_id`, `inclusion_rule_hash` equal the corresponding
  `admission` values.
- admitted ∪ excluded ∪ quarantined partitions the snapshot's
  candidates exactly.
- **Totality binding (F81/F87 — snapshot and report could otherwise
  omit the same work undetected):** the snapshot's candidate set must
  equal the selected candidate population derived from `corpus` +
  `selection_params`. In v1 this is an ASSEMBLER invariant: candidate
  selection PRECEDES assessment (it cannot run "under the inclusion
  rule", which consumes the snapshot's facts — the round-15 wording
  was circular), and the assembler checks set equality against the
  transactionally consistent selection it derived before emitting the
  manifest. v1 makes NO public-recomputation claim — rule and policy
  bytes are not publicly resolvable. Public verifiers still check the
  partition and field-binding invariants above. Upgrade path, if
  independent totality verification is ever wanted: a
  candidate-selection definition independent of rights inclusion plus
  hash-resolvable selector/policy/rule bytes.
- `works[].slug` set = admitted − withdrawn slugs. Excluded and
  quarantined slugs never appear in `works`.

Chain (walk `prev_manifest` from `releases/HEAD` to the zero genesis;
reject a HEAD not matching a valid chain head):
- **Linear history (F85 — a merge could otherwise bypass the
  round-15 first-parent rule: the old authoritative head rides the
  MERGE's second parent while its first-parent line carries a
  replacement chain from zero, and every round-15 check passes):**
  every commit on the publication branch except the initial one has
  EXACTLY ONE parent — the previously accepted head. Merge commits on
  the publication branch are INVALID; the verifier rejects them.
- **Append-onlyness (F78):** for every commit transition where
  `releases/HEAD` changes from H to M, `M.prev_manifest == H` MUST
  hold. Only the branch's initial commit may contain the zero HEAD.
  With F85's linearity this makes "the unique HEAD-advancing commit"
  (§10) demonstrable.
- **Genesis (F85 — the predecessor-relative rules below are otherwise
  undefined without a predecessor):** the genesis manifest has
  `prev_manifest` = 64×"0", `governance_event` = `null`, and
  `withdrawn` = `[]` — explicit values replacing the ordinary-build
  predecessor rules at the chain's start.
- The `withdrawn` slug set is a monotonic extension of the
  predecessor's; entries are never silently dropped; reinstatement does
  not exist in v1.
- `governance_event = null` ⇔ ordinary build ⇔ `withdrawn` equals the
  predecessor's verbatim.
- kind `withdrawal` ⇒ added `withdrawn` slugs equal the event's
  `entries` slugs exactly; each added `withdrawn[slug].event` equals
  this manifest's `governance_event`; unaffected `works` entries
  verbatim; publication coordinates (corpus, toolchain, admission,
  selection_params) equal the predecessor's; the only `works` changes
  are the affected slugs' removals.
- kind `event-amendment` (O3(b) only) ⇒ changed `withdrawn` slugs equal
  the event's `entries` slugs exactly; for each, `amends ==
  predecessor.withdrawn[slug].event` (linear — no skipped or
  overwritten corrections); the superseded event has exactly one entry
  for that slug; each changed `withdrawn[slug].event` equals this
  manifest's `governance_event`; withdrawn slug set, `works`, and
  coordinates verbatim-unchanged.
- Mixed build/governance changes prohibited in one manifest.
- Each `withdrawn` entry's slug appears in its governing event's
  `entries`.

## 9. Publication transaction

Authority: the protected publication branch of the authoritative public
origin; fast-forward-only push is the compare-and-swap.

1. Fetch Git commit C (branch head).
2. Read manifest head H = C:`releases/HEAD`.
3. Assemble manifest M with `prev_manifest` = H.
4. Create commit C′ with EXACTLY ONE parent, C (F85 — never a merge):
   M's blobs, `releases/<manifest_id>.json` + `.sig`,
   `releases/HEAD` = M's manifest_id.
5. Push with C as the expected ref value.
6. UNKNOWN result: if M is on the accepted manifest chain (walked from
   the current `releases/HEAD`) — success. If M is ABSENT, proceed
   exactly as for REJECTION (step 7); the two cases converge (F79).
7. REJECTION — CURRENT-STATE reconciliation (F86 — the round-15
   pairwise race taxonomy assumed exactly one intervening operation
   and left "derived state" undefined; the loser now consults only
   the CURRENT head, so any number and ordering of intervening
   commits reconciles identically):
   - Fetch and FULLY VERIFY the new accepted head; DISCARD the
     assembled M.
   - **Build:** recompute the desired projection `{corpus, toolchain,
     selection_params, admission}`.
     - Projection DIFFERS from the head's → REQUEUE an ordinary build
       from the head. (This subsumes round-15's build-vs-governance
       reassembly: the fresh build inherits the head's `withdrawn` by
       construction; a stale loser is never blindly published.)
     - Projection EQUAL → recompute the expected derived content
       under that projection and the head's `withdrawn` (`works` =
       admitted − withdrawn, artifact ids, `validation_summary`).
       Equal to the head's → SUCCESS (the desired state is already
       published, whoever published it). Different → DETERMINISM
       FAILURE — halt (same coordinates, different output; consistent
       with the halt rule below).
   - **Governance:** the event's artifact id already appears as some
     chain manifest's `governance_event` → SUCCESS (already applied).
     Otherwise validate the UNCHANGED signed event against the current
     head under the §8 transition invariants and append; if it no
     longer validates (a slug already withdrawn by another event, or
     `amends` no longer naming the head's governing event) → HALT for
     fresh offline governance authorization. Never rewrite or re-sign
     an event.

Scheduled-build no-op: build iff the projection
`{corpus, toolchain, selection_params, admission}` differs from the
current head's. Governance state is inherited and excluded.

Nondeterministic output under identical coordinates HALTS as a
determinism defect.

## 10. Naming, time, and archival

- Naming (F83, **[OWNER-RATIFICATION PENDING — amends D13's dated
  form]**): the canonical, citable identity is the full typed manifest
  id `snh:1:release-manifest:<hex>`. Publication dates are
  presentation/citation metadata only (from the accepted commit and the
  Zenodo record) — never part of the name, because a Git committer
  timestamp is unsigned: the same signed manifest would otherwise
  acquire a different derived name when repackaged in another commit
  history. Display conventions (e.g. a short hash prefix such as
  `r<manifest_id[0:12]>`) are presentation concerns OUTSIDE this
  protocol (round 16) — they carry no identity semantics.
- Stored lifecycle state: `published` only.
- archive-verified is an OBSERVED reproducible predicate: SWH full
  visit + expected publication commit + all referenced blobs present +
  byte hashes equal. Latest result stored as a disposable report/CI
  status; citation eligibility is computed from it.
- Independent authorship checkpoints: Zenodo deposits containing the
  ACTUAL canonical manifest bytes + signature, made with credentials
  unavailable to release CI. Compromise semantics: chain freezes at the
  last checkpoint; later signatures contested until an out-of-band
  cutoff notice; release-key compromise halts publication;
  governance-key compromise/loss halts governance operations.

## 11. Executable schemas and conformance vectors (the FROZEN objects, F80)

The four JSON Schemas and these vectors are what the freeze review
approves — authored BEFORE that review, not transcribed after it.

1. Canonicalization: existing shared vectors (§1).
2. A complete valid manifest → canonical bytes → manifest_id.
3. A governance event (each kind) → canonical bytes → id.
4. Signature: key bytes, message bytes, 64-byte signature for one
   manifest and one event.
5. `.pub` and `releases/HEAD` byte-exact fixtures (65 bytes each),
   including the pre-genesis zero HEAD.
6. Invariant fixtures: each §8 rule with one passing and one failing
   case — including the F85 linearity rule (a MERGE commit carrying
   the old head on its second parent must FAIL), the F78
   HEAD-transition rule (a chain-replacement attempt with
   `prev_manifest` = 0 must FAIL), an explicit genesis fixture (zero
   `prev_manifest`, null `governance_event`, empty `withdrawn`), and
   the F87 assembler-side totality check; the §9 current-state
   reconciliation cases — build: state-already-published success,
   same-projection-different-content determinism failure,
   changed-projection requeue (including after an intervening
   withdrawal, and after MULTIPLE intervening commits per F86);
   governance: already-applied success, revalidate-and-append,
   conflicting-withdrawal halt, stale-`amends` halt.
