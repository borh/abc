# snh protocol v1

This specification defines the publication protocol's semantic and state
invariants. The four executable JSON Schemas govern structure; conformance
vectors demonstrate both. Changes to these contracts require a permanent
architectural decision record. A disagreement among the specification, schemas
and vectors is a defect; no artifact silently overrides another.

Key rotation, successor-chain recovery statements and signature envelopes are
outside this version. Assessment snapshots use payload version 2 under the
existing `snh:1:assessment-snapshot` artifact identity, and the release
catalog uses payload version 1 under the `snh:1:catalog` identity. The
manifest is at wire version 2; `adr/0001-snh-manifest-2.md` records why it
moved there and what that cost.

## 1. Canonical form and identity

- Canonicalizer: `rfc8785-safe-integer-json-string-v1` (test vectors:
  `soranoha/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json`).
- No floats anywhere; integers within the safe range; object keys sorted
  per RFC 8785; array sort orders as specified per field. Canonical
  bytes are unique for a given value.
- Every content id is `sha256` lowercase hex over canonical bytes (for
  JSON objects) or exact published bytes (for artifacts).
- BOUNDARY DECODE (ONE reusable operation applied to EACH
  OF THE FIVE PROTOCOL JSON OBJECTS (the §2 registry) and to NOTHING
  ELSE, on both the assembler and verifier sides):
  reject duplicate object keys; parse WITHOUT coercion; validate the
  parsed value against its frozen JSON Schema; apply the type's
  SINGLE-OBJECT semantic boundary rules (EVERY rule
  a lone object must satisfy: sortedness/uniqueness of each object's
  slug- and id-keyed lists, list disjointness within the object, real
  calendar dates, absolute origin; cross-object and transition
  invariants stay in §8); canonicalize that same value; require the
  STORED bytes to
  EQUAL the canonical bytes; recompute the id from those bytes.
  Equivalent-but-noncanonical stored JSON (same value, different
  bytes) is INVALID.
- Artifact id string form: `snh:1:<type>:<sha256hex>`.
- `manifest_id` = sha256 hex over the manifest's canonical bytes.
  External citation form: `snh:1:release-manifest:<hex>`.
- Resolver route: artifact id → `/blobs/sha256/<hex[0:2]>/<hex>`.
  The static serving tree exposes the same sharded blob layout as the repository.
- In-repo blob layout: `blobs/sha256/<hex[0:2]>/<hex>` (sharded).
  A flat directory of tens of thousands of entries would
  rewrite one giant tree object per release. Fixed here so verifiers
  and the archival predicate can locate any blob in any commit or
  archive by hash alone.

## 2. Type registry (closed)

Per-work types (the only types permitted in `works[].artifacts`):
`markdown`, `tei`, `plaintext`, `tei-validation`.
Release-level types: `release-manifest`, `assessment-snapshot`,
`admission-report`, `catalog`, `governance-event`.
Any addition or removal is wire version `snh-manifest/3`; closed-schema
v2 consumers must never meet unknown members.

## 3. `snh-manifest/2`

Required top-level fields; closed schema (no other members). No dedicated date or timestamp fields appear in manifest bytes.

| Field | Contract |
|---|---|
| `schema` | literal `"snh-manifest/2"` |
| `corpus` | `{upstream_origin (URL string), upstream_rev (commit hex)}` |
| `toolchain` | object: stage-id (matches `^[0-9a-z][0-9a-z-]*$`) → `{nix_closure_hash (string), stage_code_version (string)}`; keys sorted. Provenance/derivation-key input only; never an input to artifact identity. `nix_closure_hash` carries the stage's toolchain identity EXACTLY as the build's derivation keys carry it: the wrapper-supplied Nix closure hash for nix-provisioned stages, the hashed binary/profile identity for subprocess stages; a constant placeholder is prohibited: the build fails closed without a supplied identity |
| `selection_params` | object; string keys sorted; values strings or safe-range integers; `{}` when the inclusion rule takes no parameters |
| `admission` | `{policy_id (string), policy_hash (hex), inclusion_rule_id (string), inclusion_rule_hash (hex), assessment_snapshot (artifact id), admission_report (artifact id)}` |
| `catalog` | the `catalog` artifact id (§13) describing exactly this manifest's `works` |
| `rights` | `{works (string), encoding (string), statement_url (absolute URL)}` (the grant under which the published bytes may be reused). `works` states the standing of the underlying texts, `encoding` the licence over Soranoha's own encoding and derived artifacts. Carried by the rights policy whose `policy_hash` this manifest already records, so the terms published and the terms authorized cannot diverge |
| `works` | array sorted by `slug` as raw UTF-8 bytes ascending; slugs match `^[0-9a-z_-]+$` (non-empty), unique. Each `{slug, source_content_hash (hex of the CANONICAL SOURCE-BUNDLE IDENTITY: sha256 over the canonical bytes of the abc-source-bundle-v1 identity object `{construction, members: [{path, member_hash}...], primary_text_member}`, stable across archive-level repackaging that preserves members, unlike a raw archive hash), artifacts}`; `artifacts` sorted bytewise by `type` (`markdown`, `plaintext`, `tei`, `tei-validation`); each `{type, id, bytes}` with `bytes` = exact byte length (non-negative safe integer). Every work has exactly one artifact per per-work registry type |
| `withdrawn` | array sorted by `slug`; each `{slug, event}` with `event` a `governance-event` artifact id (the GOVERNING event carrying the public reason) |
| `validation_summary` | `{invalid_count (integer), invalid_slugs (sorted array of slugs)}` |
| `governance_event` | `null`, or the `governance-event` artifact id this manifest executes; non-null exactly when the manifest performs a withdrawal or event-amendment |
| `prev_manifest` | hex of the predecessor manifest's canonical bytes; genesis = 64×"0" |

## 4. `snh-governance-event/1`

Public, sanitized, canonical-bytes artifact; published in the release
commit; GC-rooted; resolvable by hash.

```
{schema: "snh-governance-event/1",
 kind: "withdrawal" | "event-amendment",
 entries: [{slug, reason_code, statement, amends?}]} // sorted by slug
```

Private requests and supporting evidence remain in the publisher's operational
record; governance events contain only the public statement.

- `reason_code` ∈ {"rights", "takedown-request", "data-defect", "other"}.
- `statement`: string, may be empty. No dedicated date or timestamp
  FIELD exists in event bytes; substantive dates may appear
  inside the free-form `statement` text: the enforceable rule is
  structural, not a prohibition on prose content.
- `entries` is non-empty with unique slugs. In a `withdrawal` event NO
  entry has `amends`; in an `event-amendment` event EVERY entry has
  `amends` (the superseded event's artifact id).
- A governing event changes only through an `amends` chain. Amending its
  statement never reverses the withdrawal.
- Signed by the GOVERNANCE key (see §7). Stored in the release commit at
  `governance/<hex>.json` (convenience copy; CAS blob authoritative)
  with detached signature `governance/<hex>.sig`.

## 5. Admission evidence

Two retained public content-addressed artifacts; both are permanent GC
roots; both resolve by hash.

**`snh-assessment-snapshot/2`** records every candidate in the selected
population as either independent assessment facts or edition-level reliance
(see the payload section below). An independent candidate records both the
exact work/edition and every rights-relevant contribution. `not-evaluated`
means no currently applicable assessment; `undetermined` means an applicable
assessment was inconclusive. Unavailable assessment is explicit, never an
omitted contribution. Neither is an admission decision.

Independent candidate shape:

```
{schema: "snh-assessment-snapshot/2",
 candidates: [{slug,
               work_assessment: {status, jurisdiction,
                                 effective_date, basis},
               contributions: [{contribution_id, status,
                                jurisdiction, effective_date,
                                basis}]}]} // candidates sorted by slug;
                                             // contributions sorted by
                                             // contribution_id; both unique
```

- `work_assessment`: the assessment fact for the exact
  work/edition itself, same shape as a contribution fact minus
  `contribution_id`; required for every independent-assessment candidate.
- `contribution_id`: string matching `^[0-9a-z][0-9a-z:_-]*$`, naming
  the rights-relevant contribution (e.g. `author:000035`,
  `annotator:001357`); unique within its candidate; every independent-assessment candidate has
  at least one contribution.
- Fact rule (both levels): status `not-evaluated` ⇔ `jurisdiction`,
  `effective_date`, and `basis` are all `null`. Every other status
  (including `undetermined`: the assessment was performed) carries all
  three non-null: `jurisdiction` lowercase (`^[a-z][a-z0-9-]+$`, e.g.
  `jp`), `effective_date` = the effective date of the recorded facts
  (`YYYY-MM-DD`; a REAL calendar date per the semantic rule below;
  dates are permitted here because the structural no-date-field rule binds
  manifest and event bytes), `basis` a non-empty recorded-basis string.

Semantic boundary rules (enforced inside the §1 boundary
decode after structural validation, so assembler and verifier
inherit them from the one shared operation; never JSON Schema
`format`, whose enforcement is inconsistent across validators):
- every non-null `effective_date` must be a real proleptic-Gregorian
  calendar date (schema syntax alone admits e.g. `2026-99-99`);
- `corpus.upstream_origin` (§3) must be an absolute URI with a scheme
  and a non-empty host.

**`snh-admission-report/1`**: the inclusion rule's TOTAL PARTITION:

```
{schema: "snh-admission-report/1",
 assessment_snapshot: <full artifact id>, // typed id, matching
 policy_hash: <hex>, // the manifest's admission
 inclusion_rule_id, inclusion_rule_hash: <hex>,
 admitted: [slug...], // sorted
 excluded: [{slug, reason_code}...], // sorted by slug
 quarantined: [{slug, reason_code}...]} // sorted by slug
```

Every candidate appears exactly once across the three sets. Report
`reason_code` values match `^[0-9a-z-]+$`; their DOMAIN is defined by
the content-addressed inclusion rule (`inclusion_rule_id` +
`inclusion_rule_hash`). The hash binds that vocabulary but does not resolve rule bytes. The schema
constrains syntax; assembler and verifier check the rule identity and reproduce
the partition with the executable inclusion rule.

## 6. Encodings (wire contracts; conformance vectors required)

- Signed MESSAGE: the exact ASCII bytes of the domain-separated string.
  No trailing newline, no BOM, no framing.
  - manifests: `snh-manifest-sig/1:<manifest_id>`
  - governance events: `snh-governance-event-sig/1:<event hex>`
- `.sig` file: EXACTLY 64 raw Ed25519 signature bytes.
- Key-bytes file (`.pub`; the encoding for key bytes in the
  trust-anchor deposit and pinned verifier configuration; v1's
  PUBLICATION repositories and serving trees contain NO key copies;
  deployment source MAY carry public keys as NON-AUTHENTICATING
  pinned verifier configuration; only the trust anchor authenticates
  the role assignment): EXACTLY 65 bytes:
  64 lowercase ASCII hex characters (the 32 raw Ed25519 public-key
  bytes) + one LF.
- Key FINGERPRINT: lowercase sha256 hex over the DECODED 32 raw key
  bytes (never over `.pub` file bytes).
- `releases/HEAD` file: EXACTLY 65 bytes (64 lowercase ASCII hex + one
  LF). Present in every commit of the publication branch including the
  initial one; 64 zeros before genesis; the current raw manifest_id
  after each publication.

## 7. Keys and verification

Two disjoint ROLES, each with exactly one fixed, directly pinned Ed25519 key. FOR A GIVEN PUBLICATION CHAIN, the pinned sets are
established at genesis and NEVER change; changing them ENDS that
chain; successor continuity is OUTSIDE v1 (the verifier
takes ONE `pinned_keys` argument over the FULL chain: removing a
member fails historical signatures, retaining it authorizes future
events, and no epoch/window machinery exists to distinguish the
cases). No key-manifest, no in-band rotation, no envelope in v1:

- **RELEASE role** (online, held by CI; v1 set size 1): signs release
  manifests (authenticates that Soranoha issued the release).
- **GOVERNANCE role** (offline, owner-held; v1 set size 1; one
  SOFTWARE Ed25519 keypair generated offline, held as TWO authorized
  persistent copies on separately controlled encrypted offline media
  under the ceremony-declared copy inventory): signs governance
  events (authenticates withdrawal/amendment authority). A
  compromised release key cannot withdraw works.

The verifier selects the ROLE from the signed object's kind,
constructs the domain-separated message from the artifact it holds,
and accepts the raw signature iff it verifies against that role's pinned key.

Degradation and halt ("lost without compromise" is not
normally observable; for SOFTWARE keys accountability attaches to the
DECLARED COPY INVENTORY, not a physical token): the ceremony declares
the governance key's COMPLETE AUTHORIZED PERSISTENT-COPY INVENTORY:
v1: exactly two encrypted offline media, separately controlled; the
copy operation itself is recorded in the inventory. Governance
operation continues only while EVERY surviving inventoried medium
remains accounted for and controlled. One medium verifiably destroyed
or failed leaves governance operating on the remaining copy: the
pinned set never changes. An UNEXPLAINED COPY, LOST CUSTODY of any
medium, or POSSIBLE DISCLOSURE is SUSPECTED COMPROMISE and triggers
the halt rule: a copyable file has no "lost but unread" state.
COMPROMISE of any role member halts the role's operations
(fail-closed: a valid signature no longer proves authority).
Custody of the media's encryption secrets is an operational ceremony
matter, OUTSIDE the wire protocol.

Key distribution: pinned key BYTES and fingerprints live ONLY
in the independent trust anchor and the verifier's pinned
configuration, obtained via the owner-named pre-release discovery
channel (the owner's ORCID record, which lists the FIRST anchor
deposit's specific Zenodo VERSION DOI as a work before the first
signed release; ONE pointer to the ONE immutable role-bound
anchor; a version DOI's files are fixed, unlike the concept DOI's;
no fingerprints are duplicated outside the anchor, so no counts can
drift). The anchor
authenticates the ROLE ASSIGNMENT, never a flat key list:
RELEASE = {K_release}; GOVERNANCE = {K_governance}. `pinned_keys`
PRESERVES that partition; the role
sets MUST be disjoint, and an un-roled/overlapping configuration is
INVALID: otherwise an accidental flat configuration could authorize
the online release key for governance. v1's PUBLICATION repositories
and serving trees contain NO key copies (a copy hosted
in the publication channel cannot authenticate itself and has no
consumer; even an "optional non-normative" copy invites
synchronization questions). Deployment source MAY carry the public
keys as NON-AUTHENTICATING pinned verifier configuration; only the
Zenodo/ORCID anchor authenticates the role assignment.

Governance signing uses plain Ed25519 over the §6 message on an offline
machine. The key material must never reside on a network-connected host. The
ceremony creates the two authorized encrypted copies, records the copy operation
in the custody inventory, and signs a fixed conformance vector as disposable
evidence. Ceremony signatures never replace the checked-in fixture signatures.

## 8. Verifier invariants

THE verifier primitive:
`verify_repository_at(repository_view, C, pinned_keys)`. Every read
(commits, trees, manifests, signatures, events, artifacts,
`releases/HEAD`) goes through the single NON-FALLBACK
`repository_view`; there is no secondary data source, so an incomplete
view FAILS instead of silently completing from elsewhere (an
implementation must not be able to fetch a missing signature or blob
from the live resolver and declare an incomplete archive complete).
Instantiations: live verification supplies the fetched authoritative
repository; archive verification supplies ONLY the SWH snapshot;
mirrors and local clones supply themselves. The invariants below are
what the primitive checks, with C:`releases/HEAD` as the target head.

The view's contract is COMMIT-SCOPED and NON-SUBSTITUTING: reads must ignore object-replacement mechanisms and must not
import objects from outside the view's single object store; for a
git-backed view, replacement refs are disabled, promisor/lazy fetches
are disabled, and alternate object directories are rejected outright.
Discovery and reads run under a SANITIZED environment bound to the
view's own git directory: every `GIT_`-prefixed variable is
stripped and the git directory resolved at construction is passed
explicitly on every invocation, so an inherited `GIT_DIR`,
`GIT_OBJECT_DIRECTORY`, or alternates override cannot point reads at
a foreign object store. Linked worktrees are REJECTED at construction: a linked worktree's per-worktree git directory hides the
common directory's object store and alternates file, so the view
requires the resolved common directory to equal the git directory
(linked worktrees have no v1 consumer).
It exposes reads of the
form `read_at(C, required_path)` and `parent_of(C)`. Every manifest,
signature, event, and artifact must be REACHABLE AT ITS PRESCRIBED
PATH from C's tree; presence anywhere else in the same object graph
(another branch, a later commit, a dangling object) is INSUFFICIENT
(the co-presence error one level lower). Pre-genesis base case: the
initial commit with the zero `releases/HEAD` is a valid EMPTY
repository state; every later valid target C must be a publication
commit.

PUBLICATION COMMIT (a LOCAL definition; uniqueness is a
consequence of the linear-history invariants below, not a separate
search): C is a publication commit iff
- C has exactly one parent P;
- P:`releases/HEAD` = H and C:`releases/HEAD` = M with M ≠ H;
- M.prev_manifest = H;
- C contains M and its required verification closure.

Structural single-object rules (works/withdrawn/invalid_slugs
sortedness, uniqueness, and disjointness; event entries sortedness;
snapshot candidate/contribution ordering; report list ordering and
partition-list disjointness) are enforced by the §1 boundary decode
each manifest/evidence/event fetch passes through; the bullets
below are the verifier's cross-object additions:
- Every artifact id hash is 64 lowercase hex; `bytes` matches the
  stored blob's length.
- Type-prefix and hash checks are EXPLICIT: every artifact id's
  `<type>` component must match its field context (`withdrawn[].event`
  and `governance_event` are `governance-event`; `admission.*` ids are
  `assessment-snapshot`/`admission-report`; `works[].artifacts[].id`
  type equals its `type` member; `catalog` is `catalog`); for every artifact the verifier
  FETCHES, it recomputes sha256 over the bytes and requires equality
  with the id's hash component; each of the FIVE PROTOCOL JSON
  objects additionally passes the §1 BOUNDARY DECODE (stored bytes must equal the canonical bytes of the validated
  value). All other artifacts (e.g. `tei-validation` JSON bytes)
  are exact published bytes checked by hash; they have no frozen schema or
  canonical form. Validation records additionally satisfy the consumed contract below.
- Validation-summary RE-DERIVATION: the verifier consumes
  a fixed projection of each per-work `tei-validation` record under a
  minimal consumed contract: the record parses as strict JSON with
  duplicate keys rejected, `status` is exactly one of `passed`,
  `warning`, `failed`, and `validated_artifact` is exactly
  `sha256:<hex>` with `<hex>` equal to that work's `tei` artifact
  hash. It requires `invalid_slugs` to EQUAL the sorted slugs whose
  `status` is `failed`. No other field is consumed or constrained;
  tei-validation bytes remain exact published bytes checked by hash,
  with no frozen schema and no canonical form.
- Semantic boundary rules (applied inside the §1 boundary
  decode, never via JSON Schema `format`): `corpus.upstream_origin`
  is an absolute URI with a scheme and non-empty host, as are
  `rights.statement_url` and every catalog entry's `card_url`; every non-null
  `effective_date` in the assessment snapshot is a real
  proleptic-Gregorian calendar date.

Admission (fetch both evidence artifacts by hash):
- The report's fields match `admission` field-for-field over the
  fields the report actually carries (the report has NO
  `policy_id`): report `assessment_snapshot` ==
  `admission.assessment_snapshot`; report `policy_hash`,
  `inclusion_rule_id`, `inclusion_rule_hash` equal the corresponding
  `admission` values.
- admitted ∪ excluded ∪ quarantined partitions the snapshot's
  candidates exactly.
- **Totality binding (snapshot and report could otherwise
  omit the same work undetected):** the snapshot's candidate set must
  equal the selected candidate population derived from `corpus` +
  `selection_params`. In v1 this is an ASSEMBLER invariant: candidate
  selection PRECEDES assessment (it cannot run "under the inclusion
  rule", which consumes the snapshot's facts), and the assembler checks set equality against the
  transactionally consistent selection it derived before emitting the
  manifest. v1 makes NO public-recomputation claim: rule and policy
  bytes are not publicly resolvable. Public verifiers still check the
  partition and field-binding invariants above.
- `works[].slug` set = admitted − withdrawn slugs. Excluded and
  quarantined slugs never appear in `works`.

Catalog (fetch by hash and boundary-decode):
- The catalog's `works[].slug` sequence EQUALS the manifest's, element
  for element and in the same order, and each entry's
  `source_content_hash` equals that work's. A manifest may therefore not
  name a catalog describing a different release: including one that
  still describes a work this release withdrew, which is exactly what a
  takedown must remove. A withdrawal consequently publishes a new
  catalog; an event-amendment, which changes no work, does not.

Chain (walk `prev_manifest` from `releases/HEAD` to the zero genesis;
reject a HEAD not matching a valid chain head):
- **Linear history:**
  every commit on the publication branch except the initial one has
  EXACTLY ONE parent (the previously accepted head). Merge commits on
  the publication branch are INVALID; the verifier rejects them.
- **Append-onlyness:** for every commit transition where
  `releases/HEAD` changes from H to M, `M.prev_manifest == H` MUST
  hold. Only the branch's initial commit may contain the zero HEAD.
  With linearity this makes "the unique HEAD-advancing commit"
  (§10) demonstrable.
- **Genesis (the predecessor-relative rules below are otherwise
  undefined without a predecessor):** the genesis manifest has
  `prev_manifest` = 64×"0", `governance_event` = `null`, and
  `withdrawn` = `[]` (explicit values replacing the ordinary-build
  predecessor rules at the chain's start).
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
- kind `event-amendment` ⇒ changed `withdrawn` slugs equal
  the event's `entries` slugs exactly; for each, `amends ==
  predecessor.withdrawn[slug].event` (linear: no skipped or
  overwritten corrections); each changed `withdrawn[slug].event`
  equals this manifest's `governance_event`; withdrawn slug set,
  `works`, and coordinates verbatim-unchanged. (A superseded event
  with more than one entry per slug is unrepresentable: §1 boundary
  decode rejects duplicate entry slugs, so no such event can enter a
  valid chain.)
- Mixed build/governance changes prohibited in one manifest.
- Each `withdrawn` entry's slug appears in its governing event's
  `entries`.

## 9. Publication transaction

Authority: the protected publication branch of the authoritative public
origin; fast-forward-only push is the compare-and-swap. What each
mechanism establishes (the ref is UNSIGNED and authenticated
freshness is not established by v1, so the origin DESIGNATES the tip
operationally; it cannot cryptographically prove it is not serving a
stale or equivocated head):
- release signature → Soranoha issuance;
- manifest `prev_manifest` links → logical ordering;
- content hashes → byte integrity;
- the authoritative ref → the operationally designated current tip
  and the compare-and-swap serialization point;
- Zenodo checkpoints → independently recorded historical cutoffs.
"Completeness" names THREE distinct claims, none an origin
property: REPOSITORY-CLOSURE completeness (every manifest, signature,
event, and blob required for verification is present; a verifier
result); ADMISSION-PARTITION completeness (the published
snapshot/report partition checks internally; §8, a verifier result);
CANDIDATE-SELECTION totality (assembler-only in v1; §8, not
publicly recomputable). A non-authoritative carrier (mirror, archive,
clone) can present an internally valid chain from genesis; whether
that chain is a prefix of the authoritative one is decidable only by
comparison against an independently obtained head or checkpoint.

1. Fetch Git commit C (branch head).
2. FULLY VERIFY C with the §8 primitive and take the manifest head H
   and the decoded head manifest from its result: the fetched
   state is trusted only after verification; an accepted-but-invalid
   tip (e.g. a permitted fast-forward that leaves `releases/HEAD`
   unchanged) must fail here, never satisfy the no-op decision in
   step 3. The same rule governs reconciliation (step 7).
3. Assemble manifest M with `prev_manifest` = H. BEFORE creating any
   commit, apply the projection/derived-state decision against
   the current head: projection equal and derived content equal →
   SUCCESS without publishing (the scheduled no-op); projection equal
   and derived content different → DETERMINISM FAILURE: halt (the
   halt rule applies uncontended, not only after losing a race);
   projection different → proceed.
4. Create commit C′ with EXACTLY ONE parent, C (never a merge):
   M's blobs, `releases/<manifest_id>.json` + `.sig`,
   `releases/HEAD` = M's manifest_id.
5. VERIFY C′ with the §8 primitive BEFORE pushing: an invalid
   candidate (a bad signature, a missing blob, or any violated
   invariant) must never reach the origin ref. Then push with C as
   the expected ref value.
6. UNKNOWN result: if M is on the accepted manifest chain (walked from
   the current `releases/HEAD`): success. If M is ABSENT, proceed
   exactly as for REJECTION (step 7); the two cases converge.
7. REJECTION: reconcile against the current accepted state:
   - Fetch and FULLY VERIFY the new accepted head; DISCARD the
     assembled M.
   - **Build:** recompute the desired projection `{corpus, toolchain,
     selection_params, admission}`.
     - Projection DIFFERS from the head's → REQUEUE an ordinary build
       from the head. (The fresh build inherits the head's `withdrawn` by
       construction; a stale loser is never blindly published.)
     - Projection EQUAL → recompute the expected derived content
       under that projection and the head's `withdrawn` (`works` =
       admitted − withdrawn, artifact ids, `validation_summary`).
       Equal to the head's → SUCCESS (the desired state is already
       published, whoever published it). Different → DETERMINISM
       FAILURE: halt (same coordinates, different output; consistent
       with the halt rule below).
   - **Governance:** the event's artifact id already appears as some
     chain manifest's `governance_event` → SUCCESS (already applied).
     Otherwise validate the UNCHANGED signed event against the current
     head under the §8 transition invariants and append; if it no
     longer validates (a slug already withdrawn by another event, or
     `amends` no longer naming the head's governing event) → HALT for
     fresh offline governance authorization. Never rewrite or re-sign
     an event.

Scheduled-build no-op: publish only when the projection
`{corpus, toolchain, selection_params, admission}` differs from the current
head. The repeated build still checks inputs and determinism before converging.
Governance state is inherited and excluded from this projection.

Nondeterministic output under identical coordinates HALTS as a
determinism defect.

## 10. Naming, time, and archival

- Naming: the canonical, citable identity is the full typed manifest
  id `snh:1:release-manifest:<hex>`. Publication dates are
  presentation/citation metadata only (from the accepted commit and the
  Zenodo record), never part of the name, because a Git committer
  timestamp is unsigned: the same signed manifest would otherwise
  acquire a different derived name when repackaged in another commit
  history. Display conventions (e.g. a short hash prefix such as
  `r<manifest_id[0:12]>`) are presentation concerns OUTSIDE this
  protocol; they carry no identity semantics.
- Stored lifecycle state: `published` only.
- Withdrawal semantics (the PROTOCOL guarantees exactly
  three things): after a withdrawal manifest, the slug is ABSENT from
  current `works`, PRESENT in `withdrawn`, and the transition is
  authorized by its governance event (§8). Removal from discovery and
  work-facing serving routes is a SERVICE obligation of soranoha.za;
  the service tests exercise that obligation because this protocol defines no
  work-facing routes. NEITHER layer promises byte erasure or
  hash-level suppression: bytes remain in chain history, clones,
  mirrors, and archives, and identical bytes may be shared by another
  admitted work. Hash-level suppression is outside this protocol.
- Archive verification:
  `archive_verification(archived_view, C, pinned_keys) → report`.
  The result contract is TOTAL over READABLE views (bounded so acquisition failures and programmer defects are never
  turned into claims about archival validity): acquiring or
  materializing `archived_view` may fail OPERATIONALLY, and that is a
  failure to PERFORM the observation, not an observation. Given a
  readable view, verification ALWAYS returns a report:
  `report.result` =
  SUCCESS iff C is present in the archived view, C is a PUBLICATION
  COMMIT (§8), and
  `verify_repository_at(archived_view, C, pinned_keys)` succeeds with
  the archived SWH snapshot as the SOLE repository view (nothing is read from the live origin or resolver;
  co-presence of C and a valid head is not binding); OTHERWISE a
  FAILED report recording the reason. The DISPOSABLE
  report records the SWH snapshot identifier, C, the pinned key
  fingerprints, and the verifier version + result (no signed
  receipt, no frozen schema). The claim is repository-closure
  completeness plus the public §7–§8 invariants, nothing more. CITATION ELIGIBILITY requires a SUCCESSFUL observation
  satisfying the CURRENT citation policy (disposable reports
  have no ordering contract, so "the latest report" is undefined, and
  a later failed observation does not necessarily invalidate an
  earlier successful one).
- Archive resolution recipe (a documented recipe, no new wire
  format): the published promise documents how to map a manifest id +
  artifact id to the archived publication commit and the sharded
  in-repo path (§1), hence to an SWHID, so a citation stays
  resolvable if the live resolver disappears.
- Independent authorship checkpoints: Zenodo deposits containing the
  ACTUAL canonical manifest bytes + signature, made with credentials
  unavailable to release CI. Compromise semantics: chain freezes at the
  last checkpoint; later signatures contested until an out-of-band
  cutoff notice; release-key compromise halts publication. The
  governance key, exactly per §7: one inventoried medium
  verifiably destroyed → continue on the remaining copy; an
  unexplained copy, lost custody, or possible disclosure → suspected
  compromise → HALT; compromise of any role member → HALT.

## 11. Executable schemas and conformance vectors

Schemas are at `soranoha/resources/snh/schemas/*.schema.json`, one per §2
release-level type (five of them). Vectors are at `soranoha/resources/snh/vectors/`, with
`expected.json` as their index. Accept vectors contain exact canonical bytes;
reject vectors contain exact rejected bytes and their rejection reasons.
`soranoha.snh.conformance-test` checks items 1–5 below. State and transaction
fixtures in `soranoha.snh.verify-test` and `soranoha.snh.transact-test` exercise
item 6 against a local fixture origin.

1. Canonicalization: existing shared vectors (§1).
2. A complete valid manifest → canonical bytes → manifest_id; the
   boundary-decode NEGATIVE vector: EQUIVALENT but NONCANONICAL
   JSON (same value; reordered keys or altered whitespace) must be
   REJECTED; and the DUPLICATE-KEY negative vector: JSON
   carrying a repeated object key must be REJECTED at parse, BEFORE
   schema validation (canonicality vectors cannot exercise this
   parser behavior).
3. A governance event (each kind) → canonical bytes → id.
4. Signature: key bytes, message bytes, 64-byte signature for one
   manifest and one event; and the table-driven CROSS-ROLE
   tests: the release key signing a governance event FAILS, a
   governance key signing a manifest FAILS, a non-member key FAILS,
   and an overlapping/un-roled `pinned_keys` configuration is
   REJECTED. All vectors use FIXTURE keys: the key
   ceremony's smoke signing with the ACTUAL governance key is
   pre-release DISPOSABLE evidence, never a frozen fixture or schema.
5. `.pub` and `releases/HEAD` byte-exact fixtures (65 bytes each),
   including the pre-genesis zero HEAD.
6. Invariant fixtures: each §8 rule with one passing and one failing
   case, including the linearity rule (a MERGE commit carrying
   the old head on its second parent must FAIL), the
   HEAD-transition rule (a chain-replacement attempt with
   `prev_manifest` = 0 must FAIL), an explicit genesis fixture (zero
   `prev_manifest`, null `governance_event`, empty `withdrawn`), and
   the assembler-side totality check; the §9 current-state
   reconciliation cases (build: state-already-published success,
   same-projection-different-content determinism failure,
   changed-projection requeue, including after an intervening
   withdrawal, and after MULTIPLE intervening commits;
   governance: already-applied success, revalidate-and-append,
   conflicting-withdrawal halt, stale-`amends` halt); and the
   archive-binding NEGATIVE fixture (an archived snapshot whose
   current head verifies but whose expected commit C is invalid, or
   is not a publication commit, must produce a FAILED
   `archive_verification` report); the view-isolation NEGATIVE
   fixture (an archived view lacking a required signature/blob that
   the live origin still has must FAIL without fallback reads); and the
   tree-reachability NEGATIVE fixture (a
   required blob present ELSEWHERE in the same archived object graph
   such as another branch, a later commit, or dangling, but absent at its
   prescribed path under C's tree must FAIL).

## 12. Assessment snapshot payload version 2

`snh-assessment-snapshot/2` uses the registered `assessment-snapshot` artifact
type and `snh:1:assessment-snapshot:<sha256>` identity over canonical stored
bytes. The decoder accepts this explicit payload discriminator and rejects
unknown versions. Manifest, admission-report and signature formats retain their
own version 1 discriminators.

The normative structure is
`soranoha/resources/snh/schemas/snh-assessment-snapshot-2.schema.json`.
Candidates remain sorted and unique by slug. Each candidate is exactly one
of the existing independent-assessment object or `{slug, reliance}`. The
reliance payload has no duplicate slug, work assessment, or contribution
list. It records reliance on Aozora Bunko's `copyright-expired`
classification for Japan; it does not assert an independently established
public-domain fact.

The payload retains the canonical source content hash, source revision,
observation and decision dates, basis, catalog/card/file/rules digests,
and exception. Dates must be real calendar dates and the observation date
must not follow the decision date. `relied-upon` requires null reason and
exception. `unavailable` requires a nonempty reason and is quarantined.

Version 2 releases use `za-assessment-or-aozora-reliance-v2`. Its executable
value and partition are shared by assembler and verifier in
`soranoha.snh.admission`. Reliance candidates are admitted exactly when
`relied-upon`; independent candidates retain the unanimous public-domain,
in-copyright exclusion, and otherwise quarantine rule. A verifier requires
the exact rule identity/hash and reproduces the partition. The assembler
binds relied-upon source content hashes to built work inputs; the verifier
checks that published works carry the same canonical source content hash.
Withdrawal handling still subtracts the withdrawn set from admission.


Internal RDF places attributed source classification and the relying
decision in their own record graphs. Current reliance links appear only in
`accepted-reliance`, separately from independently evaluated facts in
`accepted`; no synthetic contribution or independent fact is introduced.

## 13. Release catalog payload version 1

`snh-catalog/1` uses the registered `catalog` release-level type and the
`snh:1:catalog:<sha256>` identity over its canonical stored bytes. ONE
release-level object, not one per work: a reader looking for a text should
need one fetch, and a manifest that cannot name its own contents is a weak
archival object.

`{schema, works}`. `works` is sorted by `slug` ascending, unique, and equal to
the manifest's `works` (§8). Each entry is closed:

`{slug, source_content_hash, title, title_reading, subtitle, original_title,
first_published, orthographic_style, ndc, card_url, archive_stem,
contributors, source_editions}` (nullable where Aozora's catalog leaves the
field empty; `title`, `orthographic_style`, `card_url` and `archive_stem` are
always present). `contributors` is sorted by `person_id`, unique, non-empty,
each `{person_id, family_name, given_name, family_name_romaji,
given_name_romaji, relation_to_work}`. `source_editions` entries are
`{title, publisher, first_edition_year}`.

`archive_stem` is the Aozora archive's own name for the work's primary text
member, without its extension. Every published work has exactly one such
member (the source bundle fails closed on none and on several), so the value
is always well defined and needs no fallback rule.

FACTS, NOT RENDERINGS. The catalog carries no download filename, citation
string, DOI, manifest id or release ordinal.

- A filename or citation rendered from these fields and stored beside them
  could disagree with them, in a record that can never be corrected. The
  rendering rules belong to the serving layer, which can be.
- A catalog naming its own manifest would be circular: the manifest names the
  catalog. Content addressing also lets consecutive releases with an unchanged
  corpus share one catalog blob, which an embedded release ordinal defeats.
- Archive-provider references such as a Zenodo DOI stay out of the closed
  schema, which keeps it provider-neutral; serving injects them from
  deployment configuration.
