# snh protocol v1 — live specification

Status: DRAFT — becomes AUTHORITATIVE for builders and verifiers at the
D16.1 freeze. Two items remain explicitly open and are marked inline:
the owner's O3 choice and the assessment-snapshot content fields.

This document contains only the live protocol. Rationale, decision
history, and superseded designs live in the design ledger
(`2026-08-24-publication-rearchitecture.md`); nothing there overrides
this document once frozen. Contingency designs (key rotation, recovery
statements, signature envelopes) are NOT part of v1 — see the ledger's
contingency appendix for their activation triggers.

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
| `toolchain` | object: stage-id (ASCII `[0-9a-z-]`) → `{nix_closure_hash (string), stage_code_version (string)}`; keys sorted. Provenance/derivation-key input only — never an input to artifact identity |
| `selection_params` | object; string keys sorted; values strings or safe-range integers; `{}` when the inclusion rule takes no parameters |
| `admission` | `{policy_id (string), policy_hash (hex), inclusion_rule_id (string), inclusion_rule_hash (hex), assessment_snapshot (artifact id), admission_report (artifact id)}` |
| `works` | array sorted by `slug` as raw UTF-8 bytes ascending; slugs ASCII `[0-9a-z_-]`, unique. Each `{slug, source_content_hash (hex of upstream source bytes), artifacts}`; `artifacts` sorted bytewise by `type`, each `{type, id, bytes}` with `bytes` = exact byte length (non-negative safe integer). Every work has exactly one artifact per per-work registry type |
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
 entries: [{slug, reason_code, statement, amends?}],  // sorted by slug
 authority: <deciding role, e.g. "owner">,
 evidence_hash: <hex of private evidence record, or 64×"0">}
```

- `reason_code` ∈ {"rights", "takedown-request", "data-defect", "other"}.
- `statement`: string, may be empty. No dates in event bytes.
- `entries` is non-empty with unique slugs. In a `withdrawal` event NO
  entry has `amends`; in an `event-amendment` event EVERY entry has
  `amends` (the superseded event's artifact id).
- **[O3-PENDING]** The `event-amendment` kind exists only under owner
  option (b). Under option (a) it does not exist and corrections
  require wire v2.
- The private evidence record (may contain personal data) stays
  access-controlled, bound by `evidence_hash`.
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
 assessment_snapshot: <hex>, policy_hash: <hex>,
 inclusion_rule_id, inclusion_rule_hash: <hex>,
 admitted: [slug...],                        // sorted
 excluded: [{slug, reason_code}...],         // sorted by slug
 quarantined: [{slug, reason_code}...]}      // sorted by slug
```

Every candidate appears exactly once across the three sets.

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
- `invalid_count == count(invalid_slugs)`; `invalid_slugs` ⊆ works'
  slugs, sorted; summary re-derivable from the per-work `tei-validation`
  artifacts.

Admission (fetch both evidence artifacts by hash):
- The report's embedded snapshot/policy/rule hashes match `admission`.
- admitted ∪ excluded ∪ quarantined partitions the snapshot's
  candidates exactly.
- `works[].slug` set = admitted − withdrawn slugs. Excluded and
  quarantined slugs never appear in `works`.

Chain (walk `prev_manifest` from `releases/HEAD` to the zero genesis;
reject a HEAD not matching a valid chain head):
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
4. Create commit C′: M's blobs, `releases/<manifest_id>.json` + `.sig`,
   `releases/HEAD` = M's manifest_id.
5. Push with C as the expected ref value.
6. UNKNOWN result: success ⇔ M is on the accepted manifest chain
   (walked from the current `releases/HEAD`) — not merely in Git
   history.
7. REJECTION — operation-specific reconciliation:
   - Fetch and FULLY VERIFY the new accepted head.
   - Intended state already satisfied → succeed without publishing.
   - Build losing to governance → reassemble against the new governance
     state (inherit `withdrawn`; recompute `works`,
     `validation_summary`); retry.
   - Governance event losing to a build/disjoint governance → revalidate
     the UNCHANGED signed event against the new predecessor; re-chain;
     retry.
   - Slug conflict or stale `amends` → HALT for fresh offline
     governance authorization. Never rewrite or re-sign an event.

Scheduled-build no-op: build iff the projection
`{corpus, toolchain, selection_params, admission}` differs from the
current head's. Governance state is inherited and excluded.

Nondeterministic output under identical coordinates HALTS as a
determinism defect.

## 10. Naming, time, and archival

- Release name (derived, never embedded):
  `r<YYYYMMDD>-<manifest_id[0:12]>`, date = UTC calendar date of the
  COMMITTER timestamp on the unique commit advancing `releases/HEAD` to
  that manifest.
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

## 11. Conformance vectors (required before the freeze is exercised)

1. Canonicalization: existing shared vectors (§1).
2. A complete valid manifest → canonical bytes → manifest_id.
3. A governance event (each kind) → canonical bytes → id.
4. Signature: key bytes, message bytes, 64-byte signature for one
   manifest and one event.
5. `.pub` and `releases/HEAD` byte-exact fixtures (65 bytes each),
   including the pre-genesis zero HEAD.
6. Invariant fixtures: each §8 rule with one passing and one failing
   case; the §9 reconciliation matrix (build/build, build/governance,
   disjoint withdrawals, same-slug withdrawal race, competing
   amendments).
