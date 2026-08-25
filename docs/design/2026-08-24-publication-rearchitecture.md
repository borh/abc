# Publication Rearchitecture — Design Ledger

Status (post review round 11, 2026-08-25):
- **Slice 0: READY** (F4 pinned procedure).
- **Slice 1: READY NOW** (F56 — the D20 gate was stale: F52 removed
  manifests/admission from Slice 1, so the publication-schema ownership
  transfer has no Slice-1 consumer; ABC keeps that ownership during
  Slice 1; soranoha/ owns only kernel, CAS, trace store, copied
  renderers; TEI profiles via flake input as already decided). The
  build index is a derived trace-store view, not a persisted schema
  (F57).
- **Before Slice 2:** owner O3(b) + O5a-as-amended ratification with
  reviewer boundary validation; the D16.1 freeze — six formats at their
  first consumer (manifest, tombstone, admission-report,
  governance-event, sig, AND the minimal assessment-snapshot content
  schema per F58 — Slice 2's assessed fixture is its first consumer);
  the manifest-related ownership transfer in AGENTS.md (D20 as amended —
  moved from Slice 1 to Slice 2).
- **Before Slice 3:** O1 (approved conditional on committed assessment
  evidence) + full-corpus assessment data; O2 concrete host + F12
  probes; `snh-archive-receipt/1` schema (the only pre-Slice-3 format);
  the F54/F59 independent trust-anchor deposit (key bytes + fingerprints
  + the genesis manifest_id, on Zenodo), whose quarterly successors are
  the independent authorship checkpoints.
- **Compromise semantics per F55/F59:** artifact ids always identify
  exact bytes; the official chain freezes at the last INDEPENDENT
  pre-incident checkpoint (Zenodo deposits — archive receipts are signed
  by the very release key under investigation and cannot establish the
  cutoff); post-checkpoint signatures are CONTESTED until an out-of-band
  notice names the accepted cutoff; release-key compromise halts
  publication; governance-key compromise/loss halts governance
  operations and any publication requiring them.
- **O3: facilitator and reviewer both recommend option (b)**; owner
  ratification pending. **O5a: direction approved; ratification withheld
  pending F58/F59 — both now applied; owner ratification pending.**
D18.1 approved conditional on F12. Authoritative record: decision log +
reviews F1–F9, F10–F15, F16–F23, F24–F28, F29–F33, F34–F37, F38–F42,
F43–F47, F48–F51, F52–F55, F56–F59 + dev handoff below.
Owner: Bor Hodošček
Process: hammock-driven-design decision interview, session of 2026-08-24

## Problem statement

The corpus-publication system (aozorabunko revision → parser → TEI/plaintext)
has grown intertwined: identity, qualification, and governance concerns are
woven into the build's hot path, the build is all-or-nothing, and "incremental"
exists as three unrelated mechanisms, none serving the main pipeline. The goal
is a principled, simpler architecture in which building TEI/plaintext (later
pretokenized) outputs for arbitrary aozorabunko revisions is easy, releases are
automatic and incremental, and comparison between revisions/toolchains falls
out of the design rather than being computed after the fact.

## Evidence (established from the repository, 2026-08-24)

- Pipeline core is small: renderers ~719 LOC, render from parser-IR only.
- `build-publication!` re-derives all 17,602 works per run; the 2026-07-09
  workflow-cache spec was never implemented (`workflow.clj` has no cache).
- Revision is a flake input pin, not a runtime parameter.
- ADR governance (`decisions.edn`, 2,342 lines) is parsed and semantically
  validated on every publication build; `parser-release-authority/authenticate`
  binds builds to ADR records (commit 3f6aff77 documents the friction).
- ≥7 identity schemes; canonicalizer implemented 3× (Clojure/Python/Rust)
  pinned by shared vectors; documented past divergence outage.
- Parser-RQ subsystem ~5,008 LOC + 47 schemas ≈ 7× the renderers it qualifies.
- 81 env vars, 7 competing "root" variables; 81 schemas with mirror/symlink
  sync machinery; docs ~278k lines vs ~54k Clojure.
- Prior art inside the repo: the 2026-08-14 release-qualification-by-output-
  identity spec (provisional) already moves the release gate from binary-hash
  identity to observable-output identity. Measured there: converter is
  deterministic over 17,602 works; full digest pass exists; Clojure aggregate
  regeneration 413.9s vs Python 11.7s (open Q11).

## Diagnosis (confirmed with owner)

1. Interleaving, not size, is the disease: governance/qualification on the
   build hot path blocks unattended releases and couples ADR edits to renders.
2. Incrementality failed because identity was only corpus-wide; the natural
   key is per-work.
3. Past failure (owner-reported): keying "unchanged" on parser *source/binary*
   hash over-invalidated — output-irrelevant code changes looked like changes.

## Keystone principle (agreed)

Separate two identities that were previously one:

- **Derivation key** (may we skip work?): (work source content hash ×
  toolchain identity). Conservative; over-invalidation only costs compute.
- **Artifact identity** (what is cited/stored/compared): hash of the output
  bytes. Cannot over-distinguish; identical output ⇒ identical id ⇒ automatic
  dedup and automatic "unchanged" signaling across releases. (= early cutoff,
  as in Nix CA-derivations / build-systems-à-la-carte constructive traces.)

A **release** = an immutable manifest mapping slug → artifact id (+ provenance:
upstream revision, toolchain identities). Comparison of releases = manifest
set-difference. Daily releases are cheap: mostly pointers to existing artifacts.

## Decision log

| # | Decision | Basis | Reversibility |
|---|---|---|---|
| D1 | Consumers: public online release, graduate courses, owner's research, broader linguistics community. Reproducibility guarantee is archival: any cited identifier must resolve to exact bytes indefinitely. | owner | fixed intent |
| D2 | Distribution: hybrid — archived immutable artifacts are the contract; on-demand regeneration is best-effort bonus. Start archive-first on owner's NixOS servers; lean on outside infra (e.g., Zenodo) later. | owner, 2026-08-24 | revisit at first public release |
| D3 | Identifier shape: per-artifact content id (machine layer) + human release tags; releases possibly multiple/day. Resolved by D13: `r<date>-<manifesthash12>` tags + quarterly snapshot DOIs. | owner; superseded-into D13 | resolved |
| D4 | Tokenization = just another renderer lane: consumes parser-IR/plaintext, identity folds in tokenizer+dictionary versions, may lag main release. Not required for first release. vibrato-pipe (rename pending) will supersede current tokenizers later; JADH2026 uses existing. | owner | easy |
| D5 | Auto-release runs on Forgejo CI (this repo). Unattended ⇒ no human/ADR step on hot path, no machine-local hardcoded paths. | owner | easy |
| D6 | Non-goals confirmed: parser-RQ is JADH2026 paper support — keep working until conference, then archive, do not migrate. ab-validator measurement/warehouse Python stays as-is outside new architecture (later supplemented by vibrato-pipe). aozora-history-audit stays as-is. | owner | fixed |
| D7 | ADRs: important but move off build hot path (CI governance only); corpus needs review, pruning, and removals. Greenfield mindset — early decisions are not binding. | owner | — |
| D8 | Preserve: slug injectivity check, TEI profile validation (Jing+Schematron), fail-closed provenance; leverage schemas/validation where they pay. | owner + repo history (silent-loss bug) | fixed |
| D9 | **Direction: candidate A — greenfield publication kernel** (constructive-trace table + CAS + static release manifests), lane-by-lane retirement of old machinery. Rejected: B (fix-in-place — would inherit god-namespace coupling and governance-on-hot-path structure; the kernel is small enough that porting the 719-LOC renderers is cheaper than untangling 22k LOC); C (Nix-native — eliminated on evidence, see prior-art findings); D (status quo — cannot meet unattended auto-release or per-work incrementality). | owner, after incubation, 2026-08-24 | Revisit trigger: if the kernel's first slice cannot reproduce the rehearsal build's TEI/plaintext byte-for-byte, reassess before retiring any abc lane. |
| D10 | Kernel language: **Clojure** — renderers/TEI-header/whitespace policy (~900 LOC) port rather than rewrite. "<1 s incremental" counts work-skipped, not JVM startup; babashka fast path only if CI latency ever warrants. | owner ratified rec, 2026-08-24 | easy |
| D11 | Artifact classes: **publish** TEI, plaintext, per-work tei-validation-result; **archive privately** parser-IR; **regenerate on demand** AAT + preservation sidecars (14.2 GB class eliminated from storage). | owner ratified rec, 2026-08-24 | easy per class |
| D12 | Placement: new top-level component, sibling of abc/ and ab-validator/, own flake, zero requires into abc namespaces (renderers copied in). abc/ and ab-validator/ frozen until after JADH2026, then retired lane by lane. | owner ratified rec, 2026-08-24 | revisit if kernel needs >renderers from abc |
| D13 | Identifiers ratified: artifact id = typed content hash; release name = `r<date>-<manifesthash12>`; snapshot DOIs **quarterly** (Zenodo concept DOI + version DOIs). Prefix harmonized with 2026-07-03 naming spec: **`snh:1:<type>:<sha256>`** (spec already reserves `snh:` + w3id.org/soranoha — matches storage rec). | owner, 2026-08-24 | prefix trivially renameable pre-first-release; frozen after |
| D14 | Slug scheme kept: `作品ID_人物ID_carddir_zipstem` as stable human-facing work name; qualifier, never identity. | owner ratified rec, 2026-08-24 | frozen after first release |
| D15 | Naming ratified per 2026-07-03 Model B spec: new top-level `soranoha/` component; namespaces `soranoha.core` (shared types/config/the one canonicalizer), `soranoha.yomi` (source-acquirer+selector), `soranoha.kura` (trace store+CAS+verifier), `soranoha.ori` (stages/renderers/validation), `soranoha.za` (release assembly+publishing). Vocabulary reused, NOT the old spec's scope (no LOD/IIIF/XTDB in v1). `snh:` id prefix per D13. | owner, 2026-08-24 | dir rename trivial pre-first-release |
| D16 | **Manifest v1 frozen before slice 1** (external review F3): canonicalizer = `rfc8785-safe-integer-json-string-v1` (legacy c14n-v0 excluded from kernel); strict schema `snh-manifest/1`; detached signature; derived release name; type registry {tei,txt,val,manifest}; full spec in F3 section. **VETOED by review round 2 → superseded by D16.1 draft (F13): types renamed (plaintext/tei-validation), visibility field removed (public-only manifest + private index), receipts excluded, admission field added, intent_id added, nested shapes + sort rules specified. Freeze awaits owner O1/O2 + reviewer re-approval.** **Round 3 (F16–F20) applied: intent_id = pre-build operation identity; withdrawn.since removed (self-reference); admission binds inclusion_rule_id+hash.** **Round 4 (F24–F26) applied: admission gains assessment snapshot commitment (in intent_id); D16.1 rewritten as a STANDALONE normative spec (wire version stays `snh-manifest/1`; "v1.1" naming retired); tombstone chain invariants added. Freeze gate decoupled from O1/O2 per F28.** **Round 5 (F30–F31, F33) applied: admission evidence = two retained PUBLIC artifacts — `snh-assessment-snapshot/1` (facts) + `snh-admission-report/1` (the rule's total partition), both registry types added pre-freeze; tombstone mutability surfaced as O3 options (immutable vs amends-chain) — freeze now ALSO gated on the owner's O3 choice; signature wire format `snh-sig/1` defined.** **Round 6 (F34–F36) applied: `release_intent {kind, governance_event_hash}` added as manifest field 12 and intent_id coordinate (withdrawals/amendments are distinct operations); snapshot fact model gains `not-evaluated` (distinct from `undetermined`; absence is an explicit fact); signature verification split — operational keys via key-manifest with chain-position validity windows, root key via out-of-band pinned fingerprint only.** **Round 7 (F38–F40) applied: admission invariant corrected to `works = admitted − withdrawn` (withdrawal is governance, not inclusion); strict predecessor→successor transition invariants (added/changed slugs = the event's affected_slugs exactly; mixed build/governance prohibited per manifest); governance event promoted to a public sanitized `snh-governance-event/1` artifact signed by a governance-role key (registry type added pre-freeze).** **Round 8 (F43–F46) applied: key windows corrected to chain-ancestry semantics (bᵢ, bᵢ₊₁]; key sets disjoint with globally unique key_ids; the governance event authorizes exact `{slug, tombstone}` pairs (`changes`), with a frozen signature filename/resolution rule; root public-key bytes published at `root.pub`, accepted only against the out-of-band fingerprint; Ed25519 encodings fixed (32 bytes / 64 hex; key_id = sha256 of raw bytes).** **Round 9 (F48/O5a) applied: the frozen-v1 schema set is exactly SIX year-one-exercised formats (manifest, assessment-snapshot, admission-report, tombstone, governance-event, sig); key-manifest and recovery are NON-NORMATIVE contingency, not frozen; v1 signing = two directly pinned disjoint keys; reviewer: "manifest core ready for freeze after O3(b) ratification".** **Round 10 (F52–F53) applied: Slice 1 emits NO public manifest (build ≠ admission — kernel depends only on D20); manifest assembly + round-trip move to Slice 2 on a fully assessed fixture; freezes STAGED — five formats at D16.1, assessment-snapshot content schema + `snh-archive-receipt/1` (the restored seventh format, signed by the release key) before Slice 3.** **Round 11 (F57–F58) applied: the build index is a derived trace-store view, never a persisted schema; staging corrected — SIX formats freeze before Slice 2 (the minimal assessment-snapshot schema's first consumer is Slice 2's fixture), only the receipt before Slice 3; "freeze at first consumer" is the rule, the count is incidental.** | review rounds 2–11, 2026-08-24/25 | draft — cheap to change until frozen |
| D17 | **Publication = atomic compare-and-append transaction** (F1): flock + head re-check + complete-write + atomic HEAD advance; idempotent by manifest_id; CI concurrency is optimization only. **Amended by D17.1 (F10): the remote protected git ref is the authority (fast-forward-only push, re-chain on rejection); idempotency by intent_id, not manifest_id; local flock is a single-host optimization.** | review round 2, 2026-08-24 | internal protocol, revisable |
| D18 | **Retention/lifecycle** (F2): all published manifests are permanent GC roots; states built→published→archived→citable; public git repo carries published artifact bytes (SWH archives real bytes); archival receipts in subsequent manifests; indefinite promise advertised only when archive-verified. **Reviewer sign-off: conditionally APPROVED; conditions adopted as D18.1 (F11/F12): receipts are separate signed attestations keyed by manifest_id (never in later manifests); states published → archive-verified, citation-eligible is a policy projection; archival latency treated as unbounded until measured; SWH completeness proven by the four F12 checks on a real public origin.** | owner-endorsed principle + review round 2 | promise text frozen at first public release |
| D19 | **Trust** (F8): offline root key → key-manifest → operational signing keys; revocation procedure; fork/equivocation consumer rule anchored in SWH-archived checkpoint history. **F22: fail-closed freeze rule. F27: signed recovery statement (`snh-recovery/1`) specified — incident record with rollback protection (recovery_seq + prev_recovery chain), key revocation/transition, and defined discovery paths; see F8 section. F29: the incident_id self-hash removed — recovery_id is DERIVED (sha256 of canonical bytes, like manifest_id), signed as `snh-recovery-sig/1:<recovery_id>`. F32: consumer bootstrap rules (chaining = rollback protection for stateful consumers only; full-chain fetch; cross-channel fail-closed); recovery lives on a protected `recovery` BRANCH, not refs/meta. F33: all detached signatures use the `snh-sig/1` envelope. F36: verification split by trust object — recovery signatures verify ONLY against the out-of-band pinned root fingerprint (never through a key-manifest, which recovery itself replaces); operational validity windows are chain-position facts (`effective_after` boundaries), never manifest-declared dates. F37: w3id is a discovery route, not a copy; bootstrap accepts the longest non-conflicting valid chain, tolerating lagging archives with valid prefixes. F40: key ROLES (release vs governance) — withdrawal authority separated from the unattended-CI release key. F41: `snh-key-manifest/1` append-only history specified (derived id, seq + prev chain, effective_after boundary, roles, cumulative revocations, root-only signature, trust-branch publication, same-boundary/gap = fail-closed root incident). F43: windows are chain-ancestry (bᵢ, bᵢ₊₁], boundaries strict descendants validly signed under the predecessor. F44: role sets disjoint, key_ids globally unique; governance signers bound to the successor manifest's window. F46: `root.pub` on the trust branch, accepted only against the pinned fingerprint; Ed25519 encodings fixed. F47: the protected branch is named `trust`.** **Round 9 (O5a/F48/F49): the key-manifest/recovery/trust-branch protocol is DEMOTED to a non-frozen contingency appendix — v1 trust = two directly pinned disjoint keys (online release, offline governance) stated in the published promise; on compromise, publication HALTS (archived releases remain valid; out-of-band notice; next epoch designed deliberately). Activation trigger: BEFORE a second release key, a cryptographic-continuity promise, or an authenticated-freshness consumer — never after an incident begins. F50 decomposition: hashes = content identity; SWH/Zenodo = availability; pinned release key = authorship; pinned governance key = withdrawal authority; freshness deferred. Round 10: F54 — a minimal trust anchor (key bytes + fingerprints, independent immutable channel, pinned verifier config) precedes the first signed release; F55 — compromise semantics: chain freezes at the last uncontested checkpoint, post-compromise signatures contested until an out-of-band cutoff notice, byte identity never conflated with authorship, governance-key loss halts governance operations.** | review-adopted 2026-08-24; amended rounds 4–10 | contingency: activate per O5a trigger; upgradeable to Tessera log |
| D20 | **Ownership** (F6): TEI profile schemas consumed as explicit flake input from abc during migration (no schema copies). **Amended F56 (round 11): the publication-schema/manifest-identity transfer has NO Slice-1 consumer after F52 — ABC keeps that ownership during Slice 1; soranoha/ owns only kernel/CAS/trace-store/copied renderers; the AGENTS.md transfer moves to the first Slice-2 work that assembles manifests. Slice 1 waits on nothing administrative.** | review-adopted 2026-08-24; amended round 11 | owner action: AGENTS.md edit, before Slice 2 |
| D21 | **Rights/registry admission restored** (F14): kernel ports the fail-closed value-plus-hash rights authority (policy hash recorded in manifest `admission`); per-work inclusion governed by named rule (O1, owner); admission responsibility consumed from abc or transferred in AGENTS.md before Slice 3. Currently `publication-policy.edn` BLOCKS release — the kernel inherits that block until the rights assessment migrates. **F17: admission is assessment-based per O1 (adopted reviewer rule); catalog flags seed, never authorize; private archiving needs its own authorization policy.** **F24: the assessment data itself is cryptographically committed — bound in `admission` and included in intent_id, so newly completed assessments change the operation identity. F30: the commitment is two retained public artifacts, separating domain roles — the assessment SNAPSHOT commits facts (public-domain / in-copyright / undetermined / not-evaluated per contribution, F35/F47); the admission REPORT records the inclusion rule's total partition (admitted/excluded/quarantined with reasons). Both published, permanently rooted, hash-resolvable. F38: works = admitted − withdrawn.** | review rounds 2–8 | policy content owner-governed; mechanism fixed |

## Constraints & success metrics

- Full from-scratch build: ≤ a few minutes (currently "pretty fast"; is O(minutes)).
- Incremental release (new/changed works only): target < 1s decision + only
  changed work compute. Tokenization exempt (slower, separate lane).
- Must run unattended on Forgejo CI.
- Open to replacing any tool/language choice if simpler (Nix/Clojure not sacred).

## Open questions (ranked by design impact)

- Q1 **RESOLVED 2026-08-24** — storage is a non-problem. Measured on the full
  rehearsal tree `/db/ab-validator/publication/ab-aozora-full-20260724-rehearsal`
  (17,594 works; predates the injectivity fix) + zstd -9 sampling (40 works/class):

  | Class | Total | Avg/work | zstd ratio | Compressed/generation |
  |---|---|---|---|---|
  | tei.xml | 1.74 GB | 101 KB | 5.9× | ~295 MB |
  | plain.txt | 688 MB | 40 KB | 3.4× | ~202 MB |
  | parser-ir.json | 9.1 GB | 517 KB | 13.5× | ~675 MB |
  | aat.json | 2.1 GB | 119 KB | ~10× est | ~210 MB |
  | preservation.json | 14.2 GB | 828 KB | 40.5× | ~351 MB |

  Researcher payload (TEI+plaintext) ≈ **0.5 GB compressed per full toolchain
  generation**; append-only daily releases add KB–MB. Keeping *every* toolchain
  generation forever is trivially self-hostable. Timings (2026-08-14 summary):
  parse 12.7 s, convert ~233 s (32 jobs), projections ~10 s → converter is the
  from-scratch bottleneck (~4 min), consistent with the ≤ few-minutes target.

  Side-finding: `preservation.json` sidecars (14.2 GB) are 6× the payload they
  document — new design must decide which artifact *classes* are published/
  archived vs. regenerable QA byproducts (feeds Q4/manifest schema). Owner
  note: intermediates (IR/AAT) are not necessarily published; the only class
  expected to take significant space is the future pretokenized lane (D4) —
  size it when the tokenizer lands, but even 10× plaintext stays self-hostable.
- Q2 **RESOLVED — ratified as D13** (typed content-hash artifact ids,
  `r<date>-<manifesthash12>` release names, quarterly snapshot DOIs).
- Q3 **RESOLVED by prior art**: static files only — immutable digest paths +
  thin mutable pointer layer (Go-proxy/OCI/Debian-by-hash pattern); no
  resolver service. w3id.org indirection for host-move survival.
- Q4 **RESOLVED by D16** (manifest v1): toolchain identity recorded as
  provenance fields only; used solely in trace keys; full schema frozen in
  external-review section F3.
- Q5 Migration path: which existing namespaces become the new kernel vs are
  retired; how the JADH2026-frozen machinery is quarantined meanwhile.
- Q6 **RESOLVED by R6**: TEI profile hash sits in the render/validate stage
  keys; a profile change recomputes all TEI (~minutes, accepted).
- Q7 **RESOLVED by D2**: the archive is the contract; regeneration stays
  best-effort and is never publicly promised (reaffirmed in Dev Handoff).

### Identifier side (researched; load-bearing sources inlined under F9)

- **SOTA consensus across ARK/Crossref/DataCite/W3C-TAG/McMurry-2017**: never
  encode a *mutable* fact in an identifier; immutable facts of the *naming
  event* pass the test ("indisputable, unchangeable, useful"). In a
  content-addressed immutable-artifact system, **release date and artifact
  type are naming-event facts** — embedding them is defensible even by the
  opacity camp's own criterion. W3C "Cool URIs" explicitly endorses creation
  date; excludes topic/status/mechanism.
- Every mature post-2015 system (SWHID/ISO-18670, OCI, Go modules, npm, HF)
  uses the same architecture: **verifiable hash core = identity; human
  semantics = qualifiers/tags/aliases that never participate in equality**
  (SWHID rule: "compare core SWHIDs, ignoring all qualifiers"). DTA is the
  cautionary inverse: semantic primary IDs, opaque PIDs bolted on later.
- High-cadence datasets: **nobody reputable mints a DOI per release** (GBIF:
  DOI per download-event; ERA5: one DOI + access date; Crossref: annual
  snapshot DOI; ESIP: time-slice DOIs "unwieldy"). RDA dynamic-data rec:
  versioned timestamped store + PID per citation event.
- Comparable corpora: UD/CLARIN = opaque Handle per 6-monthly release +
  version-chain metadata (strongest promises); CLARIN policy: "PIDs should
  not include semantics"; Wikimedia dumps = date-semantic paths, zero
  permanence (outsourced to IA).
- **Recommended shape (agent synthesis, matches D3)**:
  - Artifact identity: content hash, period. Type may be embedded SWHID-style
    (`srn:1:tei:<sha256>`); work-id/date/release are qualifiers or path
    context, never part of equality.
  - Release names: Go-pseudo-version style **date + manifest-hash fused**,
    e.g. `r20260824-<hash12>` — sortable, human-dateable, self-verifying.
    Nothing else in the name (no counts/sizes/titles — rot risk).
  - Citability overlay: Zenodo concept-DOI + version-DOI per *periodic*
    snapshot (quarterly/semester); optionally an ARK NAAN with suffix
    passthrough later. Daily releases cited by intrinsic id + resolver URL.

## Candidate directions (compared; A chosen — see D9)

- A. **Publication kernel (greenfield core, incremental retirement):** new
  small component: content-addressed artifact store + derivation cache +
  manifest/release builder; parser as black box; port the 719-LOC renderers;
  old machinery retired lane by lane.
- B. **Fix in place:** implement the spec'd workflow cache inside current abc,
  de-couple governance, unify identity — keep overall structure.
- C. **Nix-native:** per-work (dynamic/CA) derivations; Nix store as the
  artifact store. **Effectively eliminated by prior-art evidence 2026-08-24**
  (ca-derivations immature/mid-redesign; deep-trace model has no early
  cutoff; ~0.28 s/drv overhead ≈ 80 min for 17.6k works). Nix's role:
  toolchain pinning/naming only.
- D. **Status quo + minimal patches** (baseline for honest comparison).

Owner chose A after incubation (D9), with the researched shape —
constructive-trace table (SQLite) + sharded CAS + static release manifests
(hash-chained, signed tags) + Nix-pinned toolchain ids + SWH ingestion per
release + quarterly Zenodo snapshot DOIs.

## Design (ratified via D10–D15; component names per D15)

Components (each: purpose / in / out / state):

1. **source-acquirer** — manage aozorabunko clone; in: git rev (runtime
   parameter, NOT flake pin); out: catalog snapshot + per-work source bundles;
   state: managed clone under configured root (no hardcoded paths). Identity:
   work source content hash.
2. **selector** — catalog + cards → work list + slugs; injectivity asserted
   before any write (port of existing logic, D8).
3. **build engine** — generic constructive-trace store: SQLite WAL trace
   table keyed (stage-id, stage-version, toolchain-id, input-hashes) →
   output-hashes; sharded CAS `objects/sha256/..`; blob-before-trace commit
   order; append-only history ledger (determinism monitor). Owns all build
   state. No stage-specific knowledge.
4. **stages** (pure functions registered with engine): parse (ab-aozora),
   convert (ab-aat-to-parser-ir), render-tei, render-plaintext, validate-tei
   (Jing+Schematron), tokenize (later, D4). Toolchain-id per stage = hash of
   its Nix closure + stage code version.
5. **release assembler** — manifest {slug → artifact ids} + provenance
   (upstream rev, toolchain ids, prev-manifest sha256 = hash chain); release
   name `r<date>-<manifesthash12>`; signed tag.
6. **publisher** — static tree (blobs/, releases/, per-work history.json);
   SWH save-code-now trigger; periodic Zenodo snapshot bundling.
7. **verifier** — ledger uniqueness query, ~1% sampled rebuilds, canary set,
   fixity sweep. Runs in CI, not on build hot path.

Nix: pins toolchains and names them (toolchain-ids); does not drive per-work
builds. Forgejo CI: polls upstream → kernel run → publish. ADR governance:
CI check only, never consulted by the kernel.

Build systems à la carte (early cutoff/constructive traces); Nix CA
derivations; OCFL (archival layout); Software Heritage SWHIDs; Go module
sumdb-style transparency log (verifiable "reproduce forever"); Zenodo
versioned DOIs; casync/OSTree content stores; Hugging Face dataset versioning.

## Review findings (2026-08-24; classified Accepted/Mitigated/Unknown/Blocking)

Simplicity:
- R1 **Mitigated** — kura bundles storage (trace+CAS) and verification;
  distinct concerns. Rule: verifier is a separate entry point
  (`soranoha.kura.verify`), runs only in CI, never on the build path.
- R2 **Mitigated** — renderer copies duplicate abc code until retirement.
  Divergence prevented by D12 freeze + byte-equivalence acceptance test.
- R3 **Mitigated (rule)** — config sprawl (81 env vars) must not recur: the
  kernel reads exactly ONE root (`SORANOHA_ROOT` or a single config file);
  every other path derives from it. A second env var is a design smell.

Correctness / determinism:
- R4 **Mitigated (probed 2026-08-24; confirm in slice 1)** — grep of
  tei_header/parser_ir_tei/parser_ir_plaintext/materialize_publication found
  NO wall-clock usage; all TEI dates derive from upstream metadata
  (`aozora_modified`, `first_edition_year`) — compliant with the rule
  (provenance dates in manifests, never artifact bytes). Residual risk =
  map-ordering in JSON artifacts → covered by the single canonicalizer.
  Slice-1 double-build is the confirming check.
- R5 **SUPERSEDED by D17/D17.1 (F1, F10)** — Forgejo concurrency groups are
  best-effort and a local flock does not span runners; the remote protected
  git ref is the compare-and-append boundary. Build stages may still run
  concurrently (SQLite WAL + atomic blob writes).
- R6 **Accepted** — toolchain-id = Nix closure hash over-invalidates on
  nixpkgs bumps; costs minutes of compute only, artifact ids unaffected
  (early cutoff). Q6 resolved the same way: TEI profile hash sits in the
  render/validate stage keys; a profile change recomputes all TEI (~min).
- R7 **Decided (owner, 2026-08-24): include-and-flag** — policy when a work's
  TEI fails profile validation in an unattended release: include-and-flag
  (publish work with validation-result marked invalid + release-level
  summary; never silently exclude — slug silent-loss lesson; never block
  the release on per-work validation). Structural failures (parse/convert
  crash on a work) still fail closed.
- R8 **Decided (owner, 2026-08-24): tombstone policy** — withdrawal/takedown
  exception to "reproduce forever": retain past-release bytes unless
  legally impossible; later manifests mark slug `withdrawn`; public
  tombstone note records what/when/why; promise text published with this
  exception stated.

Operability:
- R9 **Amended by D18.1 (F11/F12)** — upstream malformed revision → fail
  closed, no release, CI alert; archival pushes remain non-blocking for
  *publication*, but archive-verified status requires a separate signed
  receipt proven per the F12 acceptance checks; disk/fixity health via
  verifier sweep. Observability = CI logs + release chain + receipts +
  verifier reports.
- R10 **Accepted** — single-operator bus factor; structurally mitigated by
  public git manifests, mirrors, SWH (corpus survives the operator).
- R11 **SUPERSEDED by D19 (F8)** — offline root key → key-manifest →
  operational keys, revocation procedure, checkpoint-anchored fork rule.

Migration / acceptance:
- R12 **Mitigated (important)** — the July rehearsal tree is NOT a valid
  byte-equivalence target (predates injectivity fix: 17,594 works; possible
  renderer drift). Regenerate the acceptance reference with CURRENT abc at
  the pinned revision; compare kernel output work-by-work.

Performance/cost: **Accepted** — converter ~4 min dominates from-scratch
(meets target); incremental = upstream git diff + trace lookups; storage
trivial (Q1). Security/privacy: **Mitigated** — public-domain data, no PII
beyond public author metadata; threats = cache poisoning (only CI writes,
R5), key compromise (R11), equivocation (hash chain + SWH).

R7/R8 decided by owner 2026-08-24. R4 remains OPEN (probed clean; closes only
on slice-1 double-build). R5 and R9-GC were superseded by the external review
(see F1/F2 → D17/D18). No other Blocking findings from this pass.

## External design review (2026-08-24) — findings F1–F9 and resolutions

Reviewer verdict accepted: "Direction ratified; Slice 0 ready after
clarification; release architecture still has blocking protocol and
lifecycle decisions." Resolutions below are adopted as D16–D20 (owner may
veto; flagged where the choice is product intent, not engineering).

### F1 → D17: Publication is an atomic compare-and-append transaction
Forgejo concurrency groups are best-effort (forgejo.org/docs/v15.0/user/
actions/reference/) and cannot be a correctness boundary. Protocol:
1. Read head H (`releases/HEAD` = manifest_id of current head).
2. Assemble manifest with `prev = H`; compute manifest_id M.
3. Acquire exclusive lock (flock on `releases/.lock`).
4. Re-read head; if ≠ H → release lock, restart from 1 (rebase onto new head).
5. Write blobs to CAS (idempotent), write `releases/<name>/` completely
   (manifest + detached sig), fsync.
6. Atomically rename temp HEAD → `releases/HEAD` advancing to M; unlock.
Crash recovery: a release dir not referenced by HEAD and not on the
prev-chain is incomplete → quarantine; retry is idempotent by manifest_id.
CI concurrency groups remain an optimization only.

### F2 → D18: Retention roots, lifecycle states, and real byte archival
**[PARTIALLY SUPERSEDED by D18.1 (F11/F12): receipts are separate signed
attestations, never fields of later manifests; "typically hours" retracted
(latency unbounded until measured); "citable" stored state dropped. The GC
roots, lifecycle direction, and bytes-in-git decision below remain live.]**
- GC roots = **every manifest on the prev-chain from HEAD, plus tombstoned
  manifests — i.e., all manifests ever published, permanently.** GC may
  collect only blobs unreachable from any manifest and older than N days
  (failed/temp builds). Private classes (parser-IR) are rooted by a private
  archive index with the same rule.
- Lifecycle states per release: **built** (in CAS, no manifest) →
  **published** (manifest on chain, served) → **archived** (independent-copy
  receipt verified) → **citable** (advertised for scholarly citation).
- SWH archives *submitted repositories*, not our CAS (SWH save-code-now API
  docs); w3id is redirect-only. Therefore the **public git repo contains the
  published artifact bytes themselves** (git packs dedup content-addressed
  small files well; ~2.4 GB raw/generation is in-budget), and SWH ingestion
  of that repo archives actual bytes, not just manifests.
- **Archival receipts** (SWH snapshot id; Zenodo DOI for quarterly bundles)
  are recorded in a *subsequent* manifest (no self-reference). The
  indefinite-resolution promise is advertised only for releases in state
  ≥ archived. [Product intent, owner may veto: citation guidance thus lags
  publication by one SWH visit — typically hours, not days.]

### F3 → D16: Manifest v1 frozen BEFORE slice 1
**[SUPERSEDED by D16.1 (F13; rewritten as a STANDALONE normative spec per
F25, round 4): types renamed (plaintext/tei-validation), visibility field
removed, receipts and archival_receipts removed, admission + intent_id
added, withdrawn shape corrected, assessment snapshot bound (F24),
tombstone chain invariants added (F26). This section is historical and no
longer load-bearing for ANY field; the D16.1 section is complete on its
own.]**
- Canonicalization: **`rfc8785-safe-integer-json-string-v1`** — the named,
  test-vectored canonicalizer already in-tree (vectors:
  `abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json`).
  The legacy `abc-legacy-json-c14n-v0` is NOT used by the kernel. This *is*
  "the one canonicalizer" (D15) — now selected by name.
- Schema id `snh-manifest/1`, strict (closed) schema, no floats, integers in
  safe range only.
- Fields: `schema`; `corpus {upstream_origin, upstream_rev}`;
  `snapshot_date` (release-event date, naming-event fact); `toolchain
  {stage-id → {nix_closure_hash, stage_code_version}}` (provenance ONLY —
  never inputs to artifact identity); `works` (array sorted bytewise by
  slug; each `{slug, artifacts: [{type, id, bytes, visibility}] sorted by
  type}`); `validation_summary {invalid_count, invalid_slugs}` (R7);
  `withdrawn` (R8 tombstone refs); `prev_manifest` (sha256 of predecessor's
  canonical bytes; genesis = 64×"0"); `archival_receipts` (for *earlier*
  releases).
- `manifest_id` = sha256 over canonical bytes. **Signature is detached**
  (`manifest.json.sig` over manifest_id) — never inside hashed bytes.
  **Release name is derived** (`r<YYYYMMDD>-<manifest_id[0:12]>`) — never
  embedded (no self-reference).
- Artifact ids `snh:1:<type>:<sha256hex>`; type registry v1 = `{tei, txt,
  val, manifest}`; additions are a minor schema rev, removals prohibited.
  `visibility ∈ {public, archived}`. Resolver: id →
  `/blobs/sha256/<hex>` (+ type-suffixed convenience path).
- All arrays sorted (bytewise lexicographic) — canonical bytes are unique.

### F4: Slice-0 exclusions restricted (amends Slice 0 below)
Exclusions may cover **non-artifact diagnostics only** (logs, reports,
build metadata). Any diff in TEI, plaintext, validation-result, or
identity-bearing bytes between the two abc runs is a **defect**: fix in abc,
or normalize via a *versioned transformation applied identically to both
systems* — never an ad-hoc exclusion. Slice 1 pauses on such a defect;
that is the probe doing its job. Slice-0 pin: `nix run .#soranoha --
build-publication --aozora-root <checkout@flake-pinned-rev> --config
<build-config> --snapshot-date <FIXED-DATE> --output-root <ref-root-N>
--concurrency 0`, run twice with **identical `--snapshot-date`** and
identical config; full-corpus scope; both trees retained.

### F5: Slice-2 delta oracle corrected (amends Slice 2 below)
"Manifest set-difference = upstream delta" is wrong under early cutoff
(source edits can preserve output bytes; catalog edits fan out; unrelated
files change nothing). Replace with three explicit sets and invariants:
(a) source/selection delta; (b) stages invalidated & executed; (c)
artifact-byte/manifest delta. Invariants: every artifact change corresponds
to a byte change; unchanged bytes retain ids; every executed stage is
explained by a changed declared input. Test separately: addition, deletion,
withdrawal, and an output-preserving source edit (fixture).

### F6 → D20: Ownership vs AGENTS.md (abc owns publication schemas today)
During migration the kernel consumes the TEI profile artifacts
(`tei-profile.rng/.sch/.odd`) as an **explicit flake input from abc/** —
one source of truth, no copied schemas (renderer *code* copies remain per
D12/R2). **[Amended F56, round 11: the transfer timing below is stale.]**
The ownership transfer (publication schemas, manifest identity →
`soranoha/`) is recorded in AGENTS.md **before the first Slice-2 work
that assembles manifests** — NOT before Slice 1, whose F52-narrowed scope
(kernel, CAS, trace store, copied renderers) has no publication-schema
consumer; abc retains ownership of everything not yet migrated. If
AGENTS.md acknowledges the new component earlier, it records only that
narrower kernel ownership. [Owner action: AGENTS.md edit, before
Slice 2.]

### F7: Performance acceptance protocol (amends Slice 1 below)
Fixed corpus = pinned revision; cold = empty trace store + CAS; warm =
fully populated. Metrics, each median of 5 runs on recorded hardware (CPU
model, RAM, storage, kernel noted in the report): (1) engine no-op:
process-ready → plan-complete with 0 stages, < 1 s; (2) end-to-end no-op
incl. JVM startup, < 15 s; (3) cold full build < 10 min; guardrails: peak
RSS < 8 GB, temp disk < 2× output. Numbers are acceptance bounds, revisable
only by decision-log entry.

### F8 → D19: Root of trust, revocation, and fork rule
**[CONTINGENCY per O5a/F48 (round 9): everything below except the
fail-closed principle is NON-NORMATIVE and NOT frozen. v1 has two
directly pinned static keys; on compromise or observed equivocation, v1
consumers/publisher HALT (F49) — archived releases stay valid; there is
no in-band recovery in v1. Known open defect if ever activated (F48):
recovery's key_manifest_hash lacks consistency requirements binding the
replacement key-manifest's seq/predecessor/effective_after/revocations
to accepted_head. See the contingency appendix for triggers.]**
Offline root key (kept off-server) signs a key-manifest naming operational
signing key(s) + validity windows; releases are signed by operational keys.
Compromise/rotation: root publishes revocation + new key-manifest (root
compromise = declared integrity incident; new root distributed out-of-band
via the published paper/docs fingerprint). Equivocation: consumer rule
(fail-closed per F22) — on conflicting signed chains both extending an
archived checkpoint, consumers FREEZE at the last uncontested
archive-verified checkpoint and accept NEITHER fork until an
offline-root-signed recovery statement names the canonical continuation;
the incident is published.

**Recovery statement (F27, round 4; corrected F29/F32, round 5 — the
implementable record behind the F22 policy).** A recovery statement is a
canonical-bytes document (`rfc8785-safe-integer-json-string-v1`), signed
by the OFFLINE ROOT key (never an operational key), with required fields:

- `schema`: `"snh-recovery/1"` (closed schema).
- `incident_label`: OPAQUE owner-assigned string (e.g.
  `"incident-2026-001"`) cross-referencing the published human-readable
  incident notice. A naming fact, never a hash of this document — the
  round-4 `incident_id` (sha256 of the statement containing it) was the
  same unsolvable self-reference H = sha256(document containing H) that
  F19 removed from withdrawals (F29).
- `recovery_seq`: integer ≥ 1, strictly increasing across all recovery
  statements ever issued.
- `prev_recovery`: the previous statement's DERIVED recovery_id (below);
  64×"0" for the first.
- `last_uncontested_manifest`: manifest_id of the freeze point (must be
  archive-verified).
- `accepted_head`: manifest_id of the canonical continuation; MUST be a
  chain descendant of `last_uncontested_manifest`.
- `rejected_heads`: array of manifest_ids (sorted, bytewise) of every known
  contested head; consumers permanently reject these and their descendants.
- `revoked_keys`: array of operational-key fingerprints revoked by this
  incident (sorted; may be empty).
- `key_manifest_hash`: sha256 hex of the replacement key-manifest naming
  the operational keys valid AFTER the incident (key transition is part of
  recovery, not a separate step).

**Identity and signature (F29 — the manifest pattern):** `recovery_id` is
NOT a member of the document; it is DERIVED as sha256 lowercase hex over
the statement's canonical bytes. The root key signs the domain-separated
string `snh-recovery-sig/1:<recovery_id>`, carried in an `snh-sig/1`
envelope (F33, defined in D16.1) at `recovery/<recovery_seq>.json.sig`
beside `recovery/<recovery_seq>.json`.

**Discovery (F32):** the statement's authority is the root signature, not
its channel, so distribution is redundant by design. Authoritative path: a
**protected `trust` branch** (`refs/heads/trust`; F47 fixed the name —
rounds 5–7 alternated between "recovery branch" and "trust branch"; one
branch hosts ALL trust material: `recovery/`, `keys/`, `root.pub`) on the
O2 origin — an ordinary branch, chosen over `refs/meta/*` because clones,
branch protection, the Forgejo UI, and archival tooling already
understand it,
and SWH's git loader preserves advertised refs (its ignore list does not
cover custom refs, but whether a given origin ADVERTISES one is exactly
what F12 must test). Submitted to SWH (an independent COPY). The
w3id.org/soranoha well-known URL is a **discovery route only** — w3id is
redirect-only (per the inlined sources), so it is never an independent
copy and never counts as a channel when comparing chains (F37).

**Consumer acceptance and bootstrap (F32):** `recovery_seq` +
`prev_recovery` chaining is **rollback protection for STATEFUL consumers
only** — it is NOT proof that any client has discovered the latest
statement. Rules:
- A consumer retains the highest recovery_id it has ever accepted, locally
  and durably.
- A stateful consumer accepts a statement iff the root signature verifies,
  `recovery_seq` strictly exceeds its retained sequence, `prev_recovery`
  matches its retained recovery_id chain, and `accepted_head` descends
  from `last_uncontested_manifest`; then it resumes following the accepted
  branch under the new key-manifest.
- A BOOTSTRAP consumer (no retained state) MUST (F37 ordering): compare
  its CONFIGURED INDEPENDENT SOURCES (the origin's recovery branch, SWH —
  not w3id, which merely redirects); fetch and verify EVERY observed
  chain in full from genesis (seq 1, prev = 64×"0"), never merely a
  "latest" endpoint; ACCEPT the longest non-conflicting valid chain;
  TOLERATE an archive lagging with a valid prefix of the longest chain
  (normal SWH lag is thereby distinguishable from a fork); and FAIL
  CLOSED on root-signed statements that conflict at the same sequence
  (that is a root-key incident, not a tie to break). This yields no
  global freshness guarantee — none exists for a bootstrap client — but
  it makes lag and equivocation distinguishable.

Tessera/tlog-tiles static log remains the upgrade if external verifiers
materialize.

### F9: Ledger durability & reconciliation
Q2/R4/status inconsistencies reconciled (this revision); load-bearing
sources inlined below; ledger committed to git (branch
`spec/publication-rearchitecture`) — presenting an untracked file as
authoritative was an error.

### Load-bearing sources (inlined per F9)
- Build Systems à la Carte (JFP 2020): ndmitchell.com/downloads/paper-build_systems_a_la_carte_theory_and_practice-21_apr_2020.pdf
- Nix ca-derivations status: github.com/NixOS/nix/issues/4087; milestone 35; jade.fyi/blog/the-postmodern-build-system/
- Per-derivation overhead & eval scale: fzakaria.com/2026/08/05/super-mario-derivations
- Bazel AC/CAS + poisoning: jmmv.dev/2025/09/bazel-remote-caching.html; github.com/bazelbuild/bazel/issues/4276
- OCFL inventory pathology: github.com/OCFL/spec/issues/367; spec: ocfl.io/1.1/spec/
- SWH save-code-now API: docs.softwareheritage.org/devel/apidoc/swh.web.save_code_now.api_views.html; SWHID ISO/IEC 18670:2025: iso.org/standard/89985.html
- SWH git-loader ref filtering (F32): docs.softwareheritage.org/_modules/swh/loader/git/utils.html
- Transparency: research.swtch.com/tlog; c2sp.org/tlog-tiles; github.com/transparency-dev/tessera
- Zenodo versioning & limits: zenodo.org/help/versioning; support.zenodo.org (50 GB/100-file caps)
- Identifier guidance: datatracker.ietf.org/doc/html/draft-kunze-ark-42; w3.org/Provider/Style/URI; w3.org/2001/tag/doc/metaDataInURI-31.html; datacite.org/blog/cool-dois/; McMurry et al. 10.1371/journal.pbio.2001414; RDA dynamic data 10.15497/RDA00016
- Forgejo concurrency best-effort: forgejo.org/docs/v15.0/user/actions/reference/
- w3id redirect-only: github.com/perma-id/w3id.org

## External design review round 2 (2026-08-24) — findings F10–F15

Reviewer sign-offs: **D18 conditionally approved** (conditions adopted
below); **D16 vetoed** — revised as D16.1 draft, requiring owner + reviewer
re-approval before freeze.

### F10 → D17.1: The remote git ref is the compare-and-append boundary
flock coordinates only one filesystem; Forgejo runners don't share it.
Amended protocol: the **protected branch of the public repository is the
authority**. Publish = fetch expected head → create one complete commit
(blobs + manifest + detached sig) → **fast-forward-only push**; rejection →
fetch, re-chain (`prev_manifest` := new head, recompute manifest_id),
retry. The local flock survives only as a single-host optimization.
**Operation identity = `intent_id`**, not manifest_id (rebasing changes
manifest_id): intent_id per the F18 definition below, recorded in the manifest;
a publisher MUST abort if intent_id already appears on the chain — a lost
push response cannot publish the same operation twice.
**Amended by F18 (round 3): intent_id is OPERATION identity, computable
BEFORE any build effect, from the requested publication coordinates.
Artifact ids are payload, NOT operation identity (they are unknowable
pre-build and unstable under nondeterministic retry — the exact case
duplicate suppression must catch; they also let origin collisions and
provenance-only releases slip).**
**Amended by F24 (round 4): the coordinates include the assessment
snapshot — otherwise newly completed assessments (same upstream, toolchain,
policy, rule) change the admitted set while intent_id stays constant, and
the publication is wrongly suppressed as a duplicate. Normative
definition (matches the D16.1 spec):
`intent_id = sha256(canonical {schema: "snh-manifest/1", upstream_origin,
upstream_rev, toolchain, admission_policy_hash, inclusion_rule_hash,
assessment_snapshot_hash, selection_params, release_intent})` — where
`toolchain`, `selection_params`, and `release_intent` are the exact
canonical objects defined in D16.1, and the schema literal is hashed as
written.**
**Amended by F34 (round 6): `release_intent` {kind ∈ build | withdrawal |
tombstone-amendment, governance_event_hash} added as a pre-effect
governance coordinate — without it, a withdrawal or O3(b) tombstone
correction changes NO other coordinate, inherits the published release's
intent_id, and is rejected as a duplicate, making the correction
unpublishable.**

### F11 → D18.1: Archival receipts are separate signed attestations
Receipts leave the release manifest entirely (a later release must never be
required for an earlier one to become verifiable, and archival timing must
not perturb release identity). Receipt = immutable signed record keyed by
manifest_id (`receipts/<manifest_id>.<venue>.json` + sig) containing venue,
venue-side identifiers (SWH snapshot id / DOI), verification method, and
result. **F53 (round 10): this is the SEVENTH v1 wire format,
`snh-archive-receipt/1` — canonical bytes, derived receipt_id, snh-sig/1
envelope over `snh-archive-receipt-sig/1:<receipt_id>`, signed by the
pinned RELEASE key. It was missing from the O5a six-format inventory
while D18.1 required signed receipts — a contradiction. Its exact field
schema is frozen BEFORE SLICE 3 (staged freeze; its first consumer is
Slice 3's archive-verification acceptance), per the F51 rule. F59
narrows the receipt's MEANING: SWH + the receipt prove archival
completeness and that Soranoha's release key REPORTED verifying it —
they do NOT establish a compromise cutoff, because the receipt is signed
by the same release key whose compromise would be under investigation;
the cutoff comes only from independent pre-incident Zenodo checkpoints
(see the F55/F59 compromise semantics).** Derived states: **published** (on accepted chain) →
**archive-verified** (complete independent-copy receipt exists);
**citation-eligible** is a policy *projection* of archive-verified, not a
stored state (no owner-controlled transition exists to justify one).

### F12 → D18.1: SWH completeness/latency are unproved — probe required
SWH docs: large repos fail more often; >100 MB objects not archived; visits
may be full/partial/failed (docs.softwareheritage.org/user/using_data/;
save_code_now API). Current repo origin is SSH on a .ts.net host — **no
publicly reachable archival origin exists yet** (owner decision O2 below).
Receipt acceptance for SWH must prove, on the real public repository:
(1) HTTPS origin reachable and request accepted; (2) visit status **full**
(not partial) naming the expected commit/snapshot; (3) every public
artifact referenced by the manifest present as SWH content (checked via
content-by-hash lookups, all new artifacts of the release); (4) retrieved
bytes match manifest hashes. Latency is **unbounded until measured**
("typically hours" was unsupported — retracted). Add a repo-growth probe:
fetch/clone/repack behavior of a multi-generation artifact repo, before
committing to a forge/origin (quota limits on public forges are real).
**Extended round 5 (F32), demoted to CONTINGENCY by O5a (round 9): if
the contingency trust branch is ever activated, verify on the chosen
origin that it is advertised to clients and appears in the SWH
snapshot** — SWH's git loader preserves advertised refs (its ignore list
does not cover the branch), but whether a given Forgejo origin
advertises and protects it is origin-specific. Not a v1 probe.

### F13 → D16.1: Manifest `snh-manifest/1` — standalone normative specification

**(Rewritten in full per F25, round 4. This section is self-contained: it
depends on NO superseded section. There is exactly ONE wire version:
`schema: "snh-manifest/1"`. "D16.1" names the decision amendment, not a
wire version — the "v1.1" label is retired. Status: draft awaiting
reviewer re-review + owner approval; per F28 the freeze is NOT gated on
O1/O2, which block Slice 3 only.)**

**Canonical form.** Canonicalizer = `rfc8785-safe-integer-json-string-v1`
(test vectors:
`abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json`).
No floats anywhere; all integers within the safe range; all arrays sorted
as specified per field; object keys sorted per RFC 8785. Canonical bytes
are unique for a given value. `manifest_id` = lowercase sha256 hex over
the manifest's canonical bytes. External citation form
`snh:1:release-manifest:<hex>`.

**Required top-level fields (closed schema — no other members permitted):**

1. `schema` — the literal string `"snh-manifest/1"`.
2. `intent_id` — lowercase sha256 hex; MUST equal
   `sha256(canonical {schema: "snh-manifest/1", upstream_origin,
   upstream_rev, toolchain, admission_policy_hash, inclusion_rule_hash,
   assessment_snapshot_hash, selection_params, release_intent})`, each
   value drawn from this same manifest (`admission_policy_hash` =
   `admission.policy_hash`; `assessment_snapshot_hash` = the sha256 hex
   component of `admission.assessment_snapshot`; `release_intent` = the
   field-12 object verbatim). Re-derivable by the verifier from the
   manifest alone. The admission REPORT's hash is deliberately NOT an
   intent input — the report is derived evidence (rule applied to facts),
   not a publication coordinate. The `release_intent` coordinate (F34) is
   what lets a withdrawal or tombstone amendment — which changes NO other
   coordinate — form a distinct operation instead of colliding with the
   already-published release's intent_id and being rejected as a
   duplicate.
3. `corpus` — `{upstream_origin (URL string), upstream_rev (commit hex)}`.
4. `snapshot_date` — `"YYYY-MM-DD"` release-event date (naming-event fact;
   provenance dates live here, never in artifact bytes).
5. `toolchain` — object mapping stage-id (ASCII `[0-9a-z-]` string) →
   `{nix_closure_hash (string), stage_code_version (string)}`. Keys
   sorted. Provenance and derivation-key input ONLY — never an input to
   artifact identity. This exact canonical object is the "toolchain
   identity set" hashed into intent_id.
6. `selection_params` — object (string keys sorted; values strings or
   safe-range integers only; `{}` when the inclusion rule takes no
   parameters). The exact canonical object hashed into intent_id.
7. `admission` — `{policy_id (string), policy_hash (sha256 hex of the
   rights-policy bytes, value-plus-hash authority per D21),
   inclusion_rule_id (string), inclusion_rule_hash (sha256 hex of the
   rule's governing bytes), assessment_snapshot (full artifact id
   `snh:1:assessment-snapshot:<hex>`), admission_report (full artifact id
   `snh:1:admission-report:<hex>`)}`.
   **Admission evidence (F24, restructured per F30):** two retained,
   PUBLIC, content-addressed artifacts, separating domain roles —
   assessments are FACTS; admitted/excluded/quarantined are the inclusion
   RULE'S decisions:
   - **`snh-assessment-snapshot/1`** commits, for every candidate in the
     selected population, the versioned assessment FACTS per the
     2026-07-11 assessment model (per rights-relevant contribution:
     status ∈ {public-domain, in-copyright, undetermined,
     **not-evaluated**}, jurisdiction, effective date, recorded basis;
     the model's source_knowledge_state distinction retained where
     applicable). **F35: `not-evaluated` (no completed assessment exists)
     and `undetermined` (an assessment was performed and could not reach
     a conclusion) are distinct knowledge states and must never be
     collapsed. For every required contribution, the ABSENCE of a
     completed assessment appears as an explicit `not-evaluated` fact —
     never as an omitted contribution — so the admission report can prove
     totality.** No admission decisions appear here. Its canonical-bytes
     hash is the intent_id input (facts are a publication coordinate).
   - **`snh-admission-report/1`** records the rule's TOTAL PARTITION of
     the snapshot's candidate population: `{schema, assessment_snapshot
     (hex), policy_hash, inclusion_rule_id, inclusion_rule_hash, admitted
     (sorted slugs), excluded ([{slug, reason_code}], sorted), quarantined
     ([{slug, reason_code}], sorted)}` — every candidate appears exactly
     once (anti-silent-loss at the admission boundary).
   Both are published in the release commit as public admission evidence,
   are permanent GC roots like every manifest-referenced artifact, and
   resolve via `/blobs/sha256/<hex>`. Verifier obligations: fetch both by
   hash; the report's embedded snapshot/policy/rule hashes match this
   `admission` block; admitted ∪ excluded ∪ quarantined partitions the
   snapshot's candidates exactly; and **(F38) `works[].slug` set =
   admitted − withdrawn slugs** — NOT plain equality with admitted, since
   withdrawal is a governance act, not an inclusion-rule decision: an
   unchanged assessment/rule still ADMITS a withdrawn work, while the
   work must leave `works`. Excluded and quarantined slugs must NEVER
   appear in `works`. This keeps rights eligibility (admission evidence)
   and governance withdrawal (release_intent) separate and both
   verifiable.
8. `works` — array sorted by `slug` as raw UTF-8 bytes ascending; slugs
   ASCII `[0-9a-z_-]`, unique, no normalization. Each element:
   `{slug, source_content_hash (sha256 hex of upstream source bytes),
   artifacts}`. `artifacts` is an array sorted bytewise by `type`; each
   element `{type, id, bytes}` where `type` ∈ the registry below, `id` is
   the full artifact id string `snh:1:<type>:<sha256hex>` (lowercase hex;
   the hash is over the artifact's exact published bytes), and `bytes` is
   the artifact's exact byte length (non-negative safe integer). Every
   work has exactly one artifact per applicable registry type — for
   `snh-manifest/1` that is exactly `tei`, `plaintext`, `tei-validation`.
9. `withdrawn` — array sorted by `slug` (same ordering rule), each
   `{slug, tombstone}` with `tombstone` a full id
   `snh:1:tombstone:<hex>`. No `since` field (F19: `since = current
   manifest_id` is the unsolvable self-reference H = sha256(manifest
   containing H); consumers derive "since" as the first chain manifest
   containing the tombstone). Chain rules per the invariants below.
10. `validation_summary` — `{invalid_count (integer), invalid_slugs
    (array of slugs, sorted)}`.
11. `prev_manifest` — lowercase sha256 hex of the predecessor manifest's
    canonical bytes; genesis = 64×"0".
12. `release_intent` (F34) — `{kind, governance_event_hash}` with `kind` ∈
    `{"build", "withdrawal", "tombstone-amendment"}`.
    - `kind: "build"` — an ordinary publication driven by upstream/
      toolchain/admission coordinates; `governance_event_hash` MUST be
      64×"0". (Upstream file deletions are builds — a work simply leaves
      `works`; withdrawal is a governance act, not a source event.)
    - `kind: "withdrawal"` — the manifest executes a governance decision
      to withdraw work(s) (takedown request, rights finding, data
      defect); `governance_event_hash` MUST be the sha256 hex of the
      canonical governance-event record (below), non-zero.
    - `kind: "tombstone-amendment"` (exists only under O3 option (b)) —
      the manifest corrects existing tombstone(s) via the `amends` chain;
      `governance_event_hash` non-zero as above.
    Withdrawal governance is NOT an inclusion-rule parameter — it must
    never be smuggled into `selection_params`.
    **Governance-decision object (`snh-governance-event/1`, F40 — an
    authorization protocol, not a private nonce):** a PUBLIC, sanitized,
    canonical-bytes artifact (registry type `governance-event`, published
    in the release commit, GC-rooted, resolvable via
    `/blobs/sha256/<hex>`): `{schema: "snh-governance-event/1", kind
    (matching release_intent.kind), changes (array sorted by slug of
    `{slug, tombstone}` where `tombstone` is the AUTHORIZED target
    tombstone artifact id `snh:1:tombstone:<hex>`), authority (the
    deciding role, e.g. "owner"), evidence_hash (sha256 hex of the
    private request/evidence record, or 64×"0" when none)}`.
    **F45: the event authorizes the exact tombstone RESULT, not merely
    slugs — under a slugs-only event, a compromised release key holding a
    legitimate event could substitute a false reason_code, statement, or
    amendment target. Affected slugs are DERIVED from `changes` (never
    stored separately), and the verifier requires the manifest transition
    to match the authorized tombstone ids exactly** (withdrawal: the
    added `{slug, tombstone}` pairs equal `changes`; amendment: the
    changed pairs equal `changes`, each via a valid `amends` chain).
    It is signed (snh-sig/1) by the **directly pinned offline GOVERNANCE
    key** (O5a; the F44 role separation kept, the key-manifest
    indirection dropped — in v1 the pinned key list IS the authority).
    NOT the online release key: a
    compromised unattended-CI release key cannot withdraw works;
    irreversible withdrawal authority is deliberately separated from
    routine publishing. (Revisit trigger: the owner may instead ratify
    release-key withdrawal authority explicitly; the distinct role is the
    fail-closed default, reviewer-ratified round 8.) The private
    request/evidence (which may contain personal data) stays
    access-controlled, bound by `evidence_hash`. `governance_event_hash`
    = sha256 hex over the PUBLIC object's canonical bytes.
    **Signature filename/resolution (F45):** the event's snh-sig/1
    envelope is stored in the release commit at
    `governance/<governance_event_hash>.sig`, beside a convenience copy
    of the object at `governance/<governance_event_hash>.json`
    (authoritative bytes remain the CAS blob; the verifier resolves
    `release_intent.governance_event_hash` → blob → signature at that
    fixed path).

**Type registry (closed, permanent names):** per-work types `tei`,
`plaintext`, `tei-validation` (the only types permitted in
`works[].artifacts`); release-level types `tombstone`, `release-manifest`,
`assessment-snapshot`, `admission-report`, `governance-event` (referenced
from `withdrawn`, the chain, `admission`, and `release_intent` — never
from `works[].artifacts`). Key-manifests and recovery statements are
contingency designs outside v1 entirely (O5a/F48) and are not registry
artifacts. ANY addition
or removal is a new wire version `snh-manifest/2` — closed-schema
consumers of `snh-manifest/1` must never meet unknown members.
(`assessment-snapshot`/`admission-report` added pre-freeze per F30 — the
round-4 registry omission would have forced a v2 the moment the evidence
became retrievable.) The public manifest
lists public artifacts only; private classes (parser-IR) live in a
separate private archive index with its own schema, never part of public
identity. ("archived" exclusively names a release lifecycle state, not a
visibility.) No receipt fields exist in the manifest (F11 — receipts are
separate signed attestations keyed by manifest_id).

**Tombstone artifact shape (F26):** a tombstone is itself a
canonical-bytes artifact: `{schema: "snh-tombstone/1", slug (the withdrawn
slug), reason_code (∈ {"rights", "takedown-request", "data-defect",
"other"}), statement (string, may be empty)}`; under O3 option (b) only,
an additional optional `amends` member (the corrected predecessor's full
tombstone id) — absent on an original tombstone. Its id is
`snh:1:tombstone:<sha256 of canonical bytes>`. No dates in tombstone bytes
(withdrawal time is derived from chain position).

**Uniqueness and cross-field invariants (verifier-enforced, chain-level
where marked):**
- `works[].slug` unique; `withdrawn[].slug` unique; the two slug sets are
  DISJOINT.
- Chain: the `withdrawn` SLUG SET is a monotonic extension of the
  predecessor's — a withdrawn slug never leaves the map, and a tombstone
  can never be silently dropped (silent drop ≠ reinstatement;
  reinstatement in v1 does not exist — it would require an explicit
  reinstatement event in a future wire version). **Whether a slug's
  TOMBSTONE ID may ever change is owner decision O3 (F31 — the round-4
  text was silently deciding it), to be resolved BEFORE freeze:**
  - **Option (a) — fully immutable:** the `{slug, tombstone}` pair is
    carried verbatim forever; correcting a mistaken reason_code or
    statement requires wire v2.
  - **Option (b) — amendable but permanent (facilitator's
    recommendation):** a slug's tombstone id may change ONLY when the new
    tombstone's `amends` field equals the previous tombstone id (an
    audited correction chain); withdrawal itself remains permanent.
  The verifier enforces whichever the owner ratifies.
- Chain: `intent_id` unique across the accepted chain (D17.1 duplicate
  suppression).
- `intent_id` re-derives from the manifest's own fields (rule in field 2).
- `release_intent` transition invariants (F34, strengthened per F39 —
  predecessor→successor, verifier-enforced; "grew" alone proved nothing
  about WHAT changed):
  - `kind = "build"` ⇔ `governance_event_hash` = 64×"0"; `withdrawn`
    equals the predecessor's verbatim.
  - `kind = "withdrawal"` ⇒ the ADDED `withdrawn` entries `{slug,
    tombstone}` equal EXACTLY the governance event's `changes` pairs
    (F45 — authorized tombstone ids, not just slugs); every unaffected
    work's entry is verbatim-unchanged; all publication coordinates
    (corpus, toolchain, admission, selection_params) equal the
    predecessor's; the only `works` changes are the removals of the
    affected slugs (derived from `changes`).
  - `kind = "tombstone-amendment"` (O3(b) only) ⇒ the CHANGED `{slug,
    tombstone}` pairs equal exactly the event's `changes`, each new
    tombstone via a valid `amends` chain; the withdrawn slug set, all of
    `works`, and all publication coordinates are verbatim-unchanged.
  - Mixed build/governance changes are PROHIBITED in one manifest —
    consecutive manifests, in any order. The cost is stricter
    serialization, which D17.1's single-chain compare-and-append already
    requires.
- Each tombstone artifact's `slug` field equals its `withdrawn` entry's
  slug.
- `invalid_count == count(invalid_slugs)`; `invalid_slugs` ⊆ works'
  slugs, sorted; the summary re-derivable from the per-work
  `tei-validation` artifacts.
- Every artifact `id` hash is 64 lowercase hex chars; `bytes` matches the
  stored blob's length.

**Signature binding, wire format, and filenames (F33).** The manifest's
canonical bytes are stored at `releases/<manifest_id>.json`. The signature
is DETACHED at `releases/<manifest_id>.json.sig`, made by a current
operational key (D19) over the domain-separated string
`snh-manifest-sig/1:<manifest_id>` — never over re-serialized JSON, and
never inside the hashed bytes.

Every `.sig` file in the system (in v1: release manifests, governance
events, and archival receipts — F53 restored receipts to the signed set,
since D18.1 requires them signed; contingency formats reuse the same
envelope if ever activated) is the
**`snh-sig/1` envelope**: canonical bytes
(`rfc8785-safe-integer-json-string-v1`) of `{schema: "snh-sig/1", key_id
(lowercase sha256 hex fingerprint of the signing PUBLIC key itself),
algorithm ("ed25519" — the only value in v1), signed_context (the exact
domain-separated string signed), signature (128 lowercase hex chars,
Ed25519)}`. A raw signature without this envelope is invalid — with
multiple operational keys a bare `.sig` cannot name its key.

**v1 verification rules (O5a — two DIRECTLY PINNED disjoint keys; no
key-manifest indirection):**
- **Manifest signature:** `key_id` MUST equal the pinned **RELEASE key**
  fingerprint (online, held by CI — authenticates that Soranoha issued
  the release).
- **Governance-event signature:** `key_id` MUST equal the pinned
  **GOVERNANCE key** fingerprint (offline, owner-held — authenticates
  withdrawal/amendment authority; disjoint from the release key per
  F44's ratified separation, so a compromised CI key cannot withdraw
  works).
Both keys are pinned in the published promise document AND in a
machine-readable key list in the repository; there is no in-band key
rotation in v1 (see F49 below).

**Key bootstrap material (F46, restated for O5a — a fingerprint alone
cannot verify anything; the verifier needs public-key BYTES):** the raw
public keys (each 32 Ed25519 bytes as 64 lowercase hex) are published at
the fixed paths **`keys/release.pub`** and **`keys/governance.pub`** in
the publication repository, with their sha256 fingerprints stated
out-of-band in the published promise/paper. A verifier accepts key bytes
ONLY when their sha256 equals the corresponding out-of-band fingerprint —
the files are convenience copies; the fingerprints are the trust anchor.
System-wide: `key_id` = sha256 hex of the raw 32 public-key bytes.

**Key-compromise rule (F49; failure semantics made precise by F55 —
"archived releases remain valid" conflated BYTE IDENTITY with
AUTHORSHIP: during an unknown-duration compromise an attacker may have
signed and archived releases before detection; their bytes and artifact
ids verify, but their status as AUTHORIZED Soranoha releases is
contested):**
- Content-addressed artifact identifiers ALWAYS continue to identify
  exact bytes — that guarantee never depends on any key.
- The official release chain FREEZES at the last INDEPENDENT
  pre-incident checkpoint. **F59: an archive-verified release is NOT
  automatically such a checkpoint — the archive receipt is signed by the
  very release key under investigation, and SWH proves preservation of
  bytes, not Soranoha's independent authorization of them. Independent
  checkpoints are the Zenodo deposits: the initial F54 trust-anchor
  deposit names the genesis manifest_id, and each quarterly deposit
  (Slice 4 cadence, already decided in D13) names the then-current chain
  head. If compromise occurs before the first post-genesis checkpoint,
  everything after genesis may remain authorship-contested — that is the
  honest v1 exposure.**
- Release signatures after that checkpoint are CONTESTED until an
  out-of-band notice names the accepted cutoff.
- Release-key compromise HALTS publication.
- Governance-key compromise or loss HALTS governance operations and any
  publication requiring them.
No recovery wire protocol is needed for any of this — only these
semantics. An out-of-band incident notice is published, and the next
trust epoch is designed deliberately — never improvised mid-incident.
"First incident" is NOT a tooling trigger: recovery machinery, if ever
wanted, is built and drilled BEFORE any promise that requires it (see
the contingency appendix triggers).

**[MOVED TO CONTINGENCY per O5a/F48.]** Key-manifest histories,
chain-ancestry validity windows (F43), root-of-trust bootstrap, and
recovery statements are NOT part of normative v1 — v1 has exactly two
directly pinned static keys and no in-band rotation, so there is nothing
for windows to govern. The full designs (F27–F46) are preserved,
explicitly NON-FROZEN, in the contingency appendix with their activation
triggers.

In all cases the verifier recomputes `signed_context` from the artifact
it holds, requires equality with the envelope's copy, and verifies.

The release name `r<YYYYMMDD>-<manifest_id[0:12]>` is DERIVED (date =
`snapshot_date`), never embedded.

**Resolver:** artifact id → `/blobs/sha256/<hex>` (+ type-suffixed
convenience path).

### F14 → D21: Rights/registry admission restored to the boundary
abc's fail-closed rights authority exists and currently blocks release
(`abc/data/publication-policy.edn` = `:blocked-pending-assessment-migration`;
only `:assessment-required` authorizes — `abc/src/abc/tools/
publication_policy.clj`). The catalog carries per-work copyright flags
(作品著作権フラグ; `aozora_csv.clj:258`) — Aozora includes in-copyright
works under rights-holder conditions. Resolution: (a) the kernel ports the
value-plus-hash rights-authority pattern (`load-rights-authority!`) —
release-level admission is a fail-closed input, its policy hash recorded in
the manifest `admission` field; (b) per-work inclusion is governed by a
named inclusion rule (owner decision O1); (c) before Slice 3 the admission
responsibility is either consumed from abc or explicitly transferred in
AGENTS.md alongside D20's schema transfer. Moving ADR governance off the
hot path does NOT bypass rights admission — admission is data-driven and
fail-closed, not interactive.

### F15: Ledger reconciliation round 2
D3 marked resolved-by-D13; Q6/Q7 marked resolved; R5/R11 marked Superseded
(D17.1/D19); R9 amended (receipt separation per D18.1); Slice 3 rewritten
around remote compare-and-append + verified receipts + admission; "sources
in agent report" headings redirected to the inlined sources list.

### Owner decisions OPEN (O1/O2 block Slice 3, not Slices 0–2; O3 blocks the D16.1 freeze per F31)
- **O1 — admission rule.** Round-3 review VETOED the two-flag (なし/なし)
  rule: the in-repo rights-remediation design (abc/docs/superpowers/specs/
  2026-07-11-rights-assessment-remediation-design.md) states the catalog
  Boolean is a source assertion that must not be promoted to a legal
  assessment, work and person assessments are independent facts, and
  Aozora itself warns translations can retain independent rights.
  **Adopted rule (reviewer text, owner ratification pending):** public
  admission requires a versioned assessment for the exact work/edition
  and every rights-relevant contribution, yielding public-release-allowed
  for the declared jurisdiction and effective date, with recorded basis;
  missing, undetermined, in-copyright, or permission-specific cases are
  excluded. Aozora flags may SEED assessments, never authorize. Private
  archiving is itself conditional on a separate authorization/access
  policy. Consequence (product): the first public release's scope is the
  set of works with completed assessments — the rights-assessment
  migration is now on the critical path to Slice 3.
  **Round-4 sign-off: direction APPROVED, conditional on committed
  assessment evidence — the assessments must exist as versioned, committed
  data, bound into each release via the F24 assessment snapshot. The
  owner's ratification must state that `public-release-allowed` is the
  INCLUSION RULE'S decision, not a new assessment_status value: the
  assessment model's statuses remain facts (public-domain, in-copyright,
  undetermined, not-evaluated — F35/F42); the rule maps facts to
  admission.**
- **O2 — authoritative public origin (wording approved round 4; host
  unreviewed).** "An owner-controlled public HTTPS Git origin, anonymously
  readable and writable only by authenticated publishers, hosts the
  authoritative protected publication branch. Other forges are downstream
  mirrors." A mirror cannot be the D17.1 serialization authority. No
  concrete host can be approved until one is named and passes F12 on THAT
  origin: object-size limits, full clone/repack behavior, force-push
  prevention, branch-deletion protection, and recovery from a rejected
  concurrent push. **Per F28, O2/F12 gate Slice 3 only — they are removed
  from the D16.1 freeze gate (the host does not affect the wire schema).**
- **O3 — tombstone mutability (F26; PROMOTED by F31 to a D16.1 freeze
  blocker).** Round 5 caught that the round-4 invariant ("tombstone id can
  never change") was silently DECIDING this while O3 was recorded as open:
  freezing that text would ratify permanent, uncorrectable tombstones by
  accident. Settled either way in v1: withdrawn slugs are monotonic
  forever, silent tombstone drop never means reinstatement, and
  reinstatement (corrected assessment, restored permission) requires an
  explicit event in a future wire version. The OPEN choice, owner's call
  before freeze:
  - **(a) Fully immutable tombstones** — simplest invariant; correcting a
    mistaken reason_code or statement requires wire v2.
  - **(b) Amendable-but-permanent (facilitator's recommendation)** — a
    tombstone may be superseded only by a new tombstone whose `amends`
    field names it (audited correction chain, verifier-enforced);
    withdrawal itself stays permanent. Recommended because takedown
    paperwork and rights findings DO get corrected, and a wire-version
    bump for a typo'd reason_code is disproportionate.
  **Round-6 reviewer sign-off on O3: RECOMMENDS option (b)** — "option (a)
  makes an ordinary typo a wire-version event" — conditional on F34,
  which is now applied (tombstone amendments have their own operation
  identity via `release_intent`, so O3(b) is executable). Owner
  ratification remains the freeze gate.
- **O5a — v1 trust boundary (PROPOSED, reviewer-drafted round 9;
  supersedes O5, whose freeze-all-formats clause F48 rejected as
  contradicting the ratchet; amended round 10 per F52–F55).**
  (1) STAGED freezes; the durable rule is **freeze at first consumer**
  (F53, corrected F58 — postponing the assessment-snapshot schema to
  Slice 3 contradicted Slice 2, which constructs an assessed fixture,
  hashes the snapshot into intent_id, and exercises an assessment-only
  delta: an object cannot be normative, hashed, and cross-checked while
  its content contract is unfrozen). Freeze BEFORE SLICE 2: `snh-
  manifest/1`, tombstone, admission-report, governance-event,
  `snh-sig/1`, and the MINIMAL `snh-assessment-snapshot/1` content
  schema (exercised on the fixture). Freeze BEFORE SLICE 3: only
  `snh-archive-receipt/1`. Full-corpus assessment MIGRATION (data, not
  schema) stays a Slice-3 prerequisite. (2) Two directly pinned disjoint keys: online CI
  RELEASE key; offline GOVERNANCE key — with the F54 trust anchor
  (public-key bytes + fingerprints on an independent immutable channel,
  loaded as pinned verifier config) published BEFORE the first signed
  release; the Slice-4 promise document is not the first pin. (3)
  Implement the complete data verifier, a governance-withdrawal fixture,
  atomic publication, and archive verification. (4) `snh-key-manifest/1`,
  `snh-recovery/1`, the trust branch, root ceremony, and cross-channel
  recovery are REMOVED from normative v1 — retained as an explicitly
  non-frozen contingency note. (5) Activation trigger: BEFORE adding a
  second release key, promising cryptographic continuity, or serving an
  external consumer requiring authenticated freshness — never after an
  incident begins (F49). (6) Compromise semantics per F55: artifact ids
  always identify exact bytes; the official chain freezes at the last
  uncontested independently recorded checkpoint; post-compromise release
  signatures are CONTESTED until an out-of-band notice names the
  accepted cutoff; release-key compromise halts publication;
  governance-key compromise or loss halts governance operations and any
  publication requiring them.

## External design review round 3 (2026-08-24) — findings F16–F23

Reviewer decisions: D18.1 approved conditional on F12; O1 two-flag rule
VETOED (assessment-based rule adopted in its place); O2 conditionally
approved with authoritative-origin wording; D16.1 veto maintained pending
F16–F20, which are now applied in place (see amended sections above);
Slice 0 ready; Slices 1–2 not ready.

- **F16 (status honesty)** — "Slices 0–2 ready" was internally false while
  D16.1 was a draft and AGENTS.md still assigns manifest identity to abc.
  Status header rewritten with per-slice blockers.
- **F17 (O1)** — two-flag admission vetoed on in-repo evidence (2026-07-11
  rights-assessment-remediation design: flags are source assertions;
  work/person assessments independent; translations retain rights).
  Assessment-based admission rule adopted; see O1. Rights-assessment
  migration is now on the Slice-3 critical path.
- **F18 (intent_id)** — was payload identity (artifact ids: post-build,
  retry-unstable, origin-blind). Redefined as pre-build operation identity
  over requested publication coordinates; artifact ids stay in the
  manifest only. Applied in D17.1 and D16.1.
- **F19 (withdrawn.since)** — `since: <manifest_id>` was either the
  unsolvable self-reference H = sha256(manifest containing H) or
  unspecified. Field removed; tombstones carried forward; "since" derived
  from first chain manifest containing the tombstone. Applied in D16.1.
- **F20 (rule binding)** — inclusion rule now bound by its own
  `inclusion_rule_id` + `inclusion_rule_hash` in `admission`; the
  release-level policy hash alone binds nothing about selection. Applied.
- **F21 (O2)** — a downstream mirror cannot be the D17.1 serialization
  authority; O2 reworded (authoritative owner-controlled origin; forges
  downstream). F12 probe list extended to that exact origin.
- **F22 (fork resolution)** — detection ≠ resolution. Fail-closed consumer
  rule added to D19: freeze at last uncontested archive-verified
  checkpoint; accept neither fork until an offline-root-signed recovery
  statement names the continuation.
- **F23 (superseded text)** — F2/F3 sections banner-marked superseded;
  "mechanical transcription" claim corrected; builders no longer need
  amendment-order reasoning.

## External design review round 4 (2026-08-25) — findings F24–F28

Reviewer verdicts: round 3 resolved all eight prior findings; **D16.1 veto
MAINTAINED** (three new blockers, F24–F26, all applied in place); O1
direction approved conditional on committed assessment evidence; O2
wording approved (host unreviewed); D19 direction approved, protocol
completed per F27; Slice 0 ready; Slice 1 still blocked on D16.1 + D20.
Commit 8423938d verified present, ledger-only, clean `git diff --check`.

- **F24 (assessment commitment — Blocker)** — admission bound the policy
  and the rule but NOT the assessment data. Failure scenario: upstream,
  toolchain, policy, rule, and selection unchanged; more works receive
  completed assessments; the admitted set (and possibly TEI metadata)
  changes; intent_id is unchanged, so publication is wrongly suppressed as
  a duplicate. Fix applied: `admission` gains `assessment_snapshot_id` +
  `assessment_snapshot_hash`; `assessment_snapshot_hash` is an intent_id
  input; the snapshot gives total accounting of the candidate population
  (every candidate admitted / excluded / quarantined with reason),
  preserving the anti-silent-loss invariant. Applied in D16.1, D17.1/F18,
  D21. *(Round 5, F30 restructured this commitment into two retained
  artifacts — facts snapshot + admission report; the D16.1 section is
  current.)*
- **F25 (standalone spec — Blocker)** — D16.1 was an amendment list
  leaning on the explicitly superseded F3 for essential fields. Missing:
  complete required field set, artifact object shapes and hash encodings,
  signature binding + filenames, uniqueness/cross-field invariants,
  canonical shapes of `selection_params` and the toolchain identity set,
  and the literal schema value; plus a naming conflict ("manifest v1.1"
  vs `snh-manifest/1` vs Slice 1's "manifest v1 per D16"). Fix applied:
  the D16.1 section rewritten as a complete self-contained normative
  specification; ONE wire version `"snh-manifest/1"` (hashed as written
  into intent_id); "v1.1" retired as a label; Slice 1 wording corrected.
- **F26 (tombstone transition invariants — Blocker)** — the carried-
  forward representation permitted contradictory states. Fix applied,
  verifier chain rules in D16.1: works/withdrawn slug sets unique and
  disjoint; withdrawn is a monotonic extension of its predecessor; an
  existing slug's tombstone id can never silently change; the tombstone
  artifact shape is specified (`snh-tombstone/1`, no dates in bytes).
  Owner decision O3 opened: permanence vs explicit reinstatement events —
  silent tombstone drop must never mean reinstatement.
- **F27 (recovery record — Slice-3 Blocker)** — D19 had a freeze policy
  but no implementable recovery record: discovery, replay protection, and
  key transition were unspecified. Fix applied in the F8/D19 section: the
  root-signed `snh-recovery/1` statement (incident_id, recovery_seq +
  prev_recovery chain for replay protection, last_uncontested_manifest,
  accepted_head, rejected_heads, revoked_keys, key_manifest_hash) with
  defined authoritative path (`refs/meta/recovery` on the O2 origin) and
  channel-independent discovery (w3id well-known URL, SWH) — validity
  comes from the root signature, not the channel.
- **F28 (decouple O2 from the freeze — Strong suggestion, adopted)** —
  the concrete publication host does not affect the manifest wire schema.
  O2 + F12 now block Slice 3 only; the D16.1 freeze gate is reviewer
  re-review + owner approval. Kernel work no longer waits on a deployment
  choice.

## External design review round 5 (2026-08-25) — findings F29–F33

Reviewer verdicts: F24–F26 amendments substantively accepted; **D16.1
veto MAINTAINED** (F30, F31); **D19/F27 recovery protocol VETOED pending
the F29 fixed-point correction**; O1 still conditionally approved; O2
wording approved (host unreviewed); Slice 0 ready. Commit 67c0c642
verified present, ledger-only, clean `git diff --check`. Re-approval
waits on F29–F31; F32–F33 must close before Slice-3 publication.

- **F29 (recovery self-hash — Blocker)** — `incident_id` (sha256 over
  "the canonical statement minus signature context") reintroduced the
  exact H = sha256(document containing H) fixed point that F19 removed
  from withdrawals; "minus signature context" excluded nothing, since the
  detached signature was never in the canonical bytes. Fix applied (the
  manifest pattern): no self-hash member; `recovery_id` DERIVED as
  sha256(canonical statement); root signs
  `snh-recovery-sig/1:<recovery_id>`; cross-referencing uses an OPAQUE
  `incident_label` naming fact.
- **F30 (assessment evidence not retained/verifiable — Blocker)** — the
  snapshot was bound by hash but had no ID syntax, no publication or
  resolution path, no GC-root status, no public-evidence stance, no
  verifier access route — and the closed registry omitted its type, so
  freezing v1 risked forcing v2 the moment the evidence became real. A
  domain-model conflict was also latent: a "snapshot" that contains
  admitted/excluded/quarantined mixes assessment FACTS with inclusion-rule
  DECISIONS. Fix applied (reviewer's second option): two content-addressed
  public artifacts — `snh-assessment-snapshot/1` (facts only; its hash is
  the intent_id input) and `snh-admission-report/1` (the rule's total
  partition; derived evidence, NOT an intent input) — both published in
  the release commit, permanent GC roots, resolvable by hash, with
  verifier cross-checks (partition exactness; admitted == works slugs;
  embedded hashes match `admission`). Registry types added pre-freeze.
- **F31 (O3 implicitly decided — Blocker for owner approval)** — the
  round-4 invariant "tombstone id can never change" silently chose
  permanent-immutable while O3 was recorded open, and would have made
  even a mistaken reason_code uncorrectable without v2. Fix applied: the
  invariant is now explicitly O3-gated with two owner options —
  (a) fully immutable vs (b) amendable-but-permanent via an `amends`
  audit chain (facilitator recommends b). O3 promoted into the D16.1
  freeze gate.
- **F32 (bootstrap replay protection — Slice-3 Blocker)** — seq/chaining
  was described as general replay protection, but it only protects
  STATEFUL consumers; a bootstrap client shown an old-but-valid chain has
  no trusted fact that a later sequence exists. Fix applied: retained
  highest-accepted recovery_id; bootstrap = full-chain fetch from genesis
  (never a "latest" endpoint alone); cross-channel comparison with
  fail-closed on same-sequence conflicts; freshness language corrected to
  "rollback protection for stateful consumers". Recovery moved from
  `refs/meta/recovery` to a **protected `recovery` branch** (ordinary
  clones, branch protection, forge UI, and archival tools all understand
  branches); F12 extended: verify the chosen origin advertises the branch
  and SWH archives it (SWH's git-loader ref ignore list does not cover it,
  but advertisement is origin-specific).
- **F33 (signature wire format — Blocker for public signing)** — the
  signed message and filename were defined but not algorithm, encoding, or
  key identification; with multiple D19 operational keys a raw `.sig`
  cannot name its key. Fix applied: the versioned `snh-sig/1` envelope
  (schema, key_id = the signing PUBLIC KEY'S own fingerprint [wording
  corrected per F42], algorithm ed25519, signed_context, hex signature)
  is now the only valid signature carrier, for manifests and recovery
  statements alike.

## External design review round 6 (2026-08-25) — findings F34–F37

Reviewer verdicts: F29 approved; F30 architecture approved (fact model
incomplete); F31/O3 — reviewer recommends option (b) after the intent
fix; F32 approved with one terminology correction; F33 envelope syntax
approved (root-key resolution defective); **D16.1 veto MAINTAINED**
(F34, F35; F36 blocks public signing); Slice 0 ready. Commit 491677c6
verified present, ledger-only, clean `git diff --check`.

- **F34 (operation identity for governance acts — Blocker)** — intent_id
  coordinates (upstream, toolchain, admission inputs, selection) are all
  UNCHANGED by a takedown withdrawal or an O3(b) tombstone correction, so
  the corrective manifest would inherit the published release's intent_id
  and D17.1 would reject it as a duplicate — the correction becomes
  unpublishable. Fix applied: new required manifest field 12
  `release_intent {kind ∈ build|withdrawal|tombstone-amendment,
  governance_event_hash}` hashed into intent_id; builds carry the zero
  hash; governance kinds carry the sha256 of a canonical signed
  governance-event record (retained, hash-public, disclosure per event);
  explicitly NOT smuggled into selection_params (withdrawal governance is
  not an inclusion-rule parameter). Kind-consistency invariants added
  (build ⇒ withdrawn unchanged; withdrawal ⇒ withdrawn grew; amendment ⇒
  only tombstone ids changed via valid amends chains; one kind per
  manifest).
- **F35 (missing knowledge state — Blocker)** — the snapshot's fact set
  {public-domain, in-copyright, undetermined} could not represent "no
  completed assessment exists", though the 2026-07-11 model has
  not-evaluated and O1 excludes exactly that case; mapping missing →
  undetermined would collapse two deliberately distinct knowledge states.
  Fix applied: `not-evaluated` added; absence of a completed assessment
  for any required contribution MUST appear as an explicit fact, never an
  omitted contribution, so the admission report can prove totality;
  source_knowledge_state distinction retained.
- **F36 (root-key resolution circular — Blocker for public signing)** —
  resolving every key_id through "the governing key-manifest" is wrong
  for recovery: the recovery statement is root-signed and itself
  introduces the replacement key-manifest — resolving the root through it
  is circular and weakens the out-of-band anchor. Fix applied: split
  rules (operational keys via the applicable root-signed key-manifest;
  the root key ONLY via the independently pinned out-of-band
  fingerprint); operational validity windows defined as chain-position
  facts via each key-manifest's `effective_after` boundary — never
  manifest-declared dates (a compromised key must not date itself into
  validity). Also corrected: key_id is the signing public key's own
  fingerprint (the round-5 session summary misstated this as the
  key-manifest fingerprint; the ledger text was already correct).
- **F37 (terminology + bootstrap ordering — Strong suggestion,
  adopted)** — w3id is redirect-only: a discovery route, never an
  independent copy or comparison channel. Bootstrap ordering specified:
  compare configured independent sources; verify every observed chain in
  full; accept the longest non-conflicting valid chain; tolerate archives
  lagging with a valid prefix (normal SWH lag ≠ fork); fail closed on
  same-sequence equivocation. No global freshness is claimed.

## External design review round 7 (2026-08-25) — findings F38–F42

Reviewer verdicts: F35/F37 approved; F34's operation-identity correction
approved but integration incomplete (F38–F40); F36 direction approved,
protocol incomplete (F41); O3(b) "still the right choice and technically
approvable" but does not clear the freeze; **D16.1 veto MAINTAINED**;
Slice 0 ready; Slice 2 additionally awaits the governance-event protocol
(now folded into D16.1). Commit 20639cca verified as HEAD, ledger-only,
clean `git diff --check`, clean worktree.

- **F38 (withdrawal vs admission evidence — Blocker)** — the round-5
  verifier rule `admitted == works[].slug` cannot hold after any
  withdrawal: withdrawal is a governance act, not an inclusion-rule
  decision, so an unchanged assessment/rule still ADMITS the withdrawn
  work while it must leave `works` — every post-withdrawal manifest would
  fail verification. Fix applied: `works = admitted − withdrawn`, plus
  excluded/quarantined slugs never appear in `works`. Rights eligibility
  and governance withdrawal stay separate AND separately verifiable.
- **F39 (transitions underconstrained — Blocker)** — "withdrawn set
  strictly grew" passed manifests whose added tombstones had nothing to
  do with the governance event, alongside arbitrary simultaneous changes
  — the "one kind per manifest" rule was stated but not enforced. Fix
  applied, strict predecessor→successor invariants: withdrawal ⇒ added
  tombstone slugs = the event's `affected_slugs` exactly, unaffected
  entries verbatim, all publication coordinates unchanged; amendment ⇒
  changed tombstone ids = `affected_slugs` exactly, everything else
  verbatim; mixed build/governance = consecutive manifests. Cost is
  stricter serialization, which D17.1's single chain already imposes.
- **F40 (governance event not an authorization protocol — Blocker)** —
  as designed the event could be fully private, schema-less, and signed
  by the same operational key that publishes — so verifiers could not
  compare affected slugs with the transition, "authority" was
  self-asserted, and the hash was a mere commitment nonce. Fix applied:
  public sanitized `snh-governance-event/1` artifact (kind,
  affected_slugs, authority, optional evidence_hash of the
  access-controlled private record), registry type added pre-freeze,
  signed by a distinct GOVERNANCE-role key (F41) — a compromised CI
  release key cannot withdraw works. Revisit trigger recorded: the owner
  may instead explicitly ratify release-key withdrawal authority.
- **F41 (key-manifest protocol undefined — Blocker for public
  signing)** — "applicable key-manifest" had no schema, identity,
  signature context, publication path, history linkage, or conflict rule;
  two root-signed key-manifests could claim the same boundary with no
  deterministic selection or omission detection. Fix applied:
  `snh-key-manifest/1` append-only history (derived key_manifest_id, seq
  + prev chain, `effective_after` boundary, role-tagged keys, cumulative
  revocations, root-only `snh-key-manifest-sig/1` signature, `keys/` on
  the trust branch, bootstrap full-history verification) with
  same-seq/same-boundary/gap = fail-closed root incident resolved only
  via recovery; release signatures bound to the unique covering window.
- **F42 (stale statements — Strong suggestion, adopted)** — O1's fact
  enum now includes `not-evaluated`; the round-5 F33 summary's "key_id =
  key-manifest fingerprint" corrected to the signing public key's own
  fingerprint (the normative text was already correct).

## External design review round 8 (2026-08-25) — findings F43–F47

Reviewer verdicts: F38/F39 approved; F40 architecture approved and the
**distinct governance role RATIFIED** subject to F44–F45; F41 history
design approved, verifier semantics blocked by F43/F46; O3(b) still
recommended; **D16.1 veto MAINTAINED**; Slice 0 ready. Commit ba94dad0
verified as HEAD, ledger-only, clean `git diff --check`, clean worktree.

- **F43 (window off-by-one — Blocker)** — `effective_after` = "last
  release governed by the predecessor", but the round-7 interval
  `[effective_after, successor's effective_after)` assigned that boundary
  manifest to the WRONG key-manifest (KM2 with effective_after=M1 would
  claim M1, which belongs to KM1). Fix applied: chain-ancestry semantics
  — KMᵢ governs (bᵢ, bᵢ₊₁], latest window unbounded; every new boundary
  must be a strict descendant of the previous AND validly signed under
  the predecessor key-manifest; numeric interval notation abandoned.
- **F44 (role separation unenforced — Blocker)** — "a key may appear
  once per role entry" let one key hold BOTH roles, defeating the
  cryptographic separation, and made sort order ambiguous for duplicate
  key_ids; governance signatures also weren't window-bound, so a revoked
  governance key could present an old key-manifest. Fix applied: key_ids
  globally unique, release/governance key sets disjoint; a governance
  signer must be active and unrevoked in the key-manifest governing the
  SUCCESSOR release manifest itself (window per F43 — determined by
  chain position, never by the signer's choice of document).
- **F45 (slugs-only authorization — Blocker)** — the event committed
  kind + slugs while the release key chose the actual tombstone bytes: a
  compromised release key holding a legitimate event could substitute a
  false reason_code, statement, or amendment target. Fix applied: the
  event carries `changes: [{slug, tombstone}]` (authorized target
  artifact ids, sorted); affected slugs are derived, never stored; the
  F39 transition invariants now match the authorized ids exactly;
  signature filename/resolution frozen
  (`governance/<governance_event_hash>.json` + `.sig` in the release
  commit; authoritative bytes = the CAS blob).
- **F46 (fingerprint ≠ key — Blocker for public verification)** — the
  envelope carries only key_id + signature and the design published only
  a fingerprint, so a verifier had no root public-key BYTES to verify
  with. Fix applied: `root.pub` at a fixed path on the trust branch
  (raw 32 Ed25519 bytes as 64 lowercase hex), accepted ONLY when its
  sha256 matches the out-of-band pinned fingerprint; encodings fixed
  system-wide (public_key = 64 hex chars of the raw 32 bytes; key_id =
  sha256 hex of those bytes).
- **F47 (reconciliation — Strong suggestion, adopted)** — D21's summary
  gains `not-evaluated`; Slice 2's "immutable tombstone id" corrected to
  "immutable except via valid O3(b) amends chains"; the protected branch
  is definitively named **`trust`** (hosting `recovery/`, `keys/`,
  `root.pub`); the snh-sig/1 scope sentence now names all four signed
  object kinds.

## Simplicity re-review (owner-triggered, 2026-08-25): are we recreating the removed complexity?

Audit against the original diagnosis (interleaving; corpus-wide identity;
over-defensive qualification):

**Cured — each original symptom checked:**
- 81 env vars / 7 root variables → 1 (SORANOHA_ROOT).
- ≥7 identity schemes, canonicalizer ×3 → ONE canonicalizer, ONE hash,
  ONE id pattern, ONE signature envelope; every derived id uses the same
  manifest pattern. Uniformity, not proliferation.
- ADR corpus parsed on every build → governance is data files hashed into
  manifests; verifier is CI-only. Nothing interactive on the hot path.
- Corpus-wide identity → per-work derivation keys + artifact ids.
- The build kernel design (~50 ledger lines + ~900 LOC ported renderers)
  has attracted ZERO findings in 8 review rounds. Slices 0–2 — the part
  serving the original goals (arbitrary-revision TEI/plaintext,
  incremental comparison) — are untouched by rounds 5–8.

**The rhyme with the old disease (flagged honestly):** the accreted
complexity is entirely in the PUBLICATION TRUST protocol. Measured: trust
spec ≈ 200 ledger lines (~4× the entire build-kernel design); 5 of 8 wire
schemas (sig, key-manifest, recovery, governance-event, tombstone) exist
to defend against compromised signing keys; ~15 of 47 findings — and
essentially every round-5–8 blocker — were defects in trust machinery WE
INVENTED (self-hash ×2, window off-by-one, circular root resolution,
role non-disjointness). Old system: qualification 7× the renderers it
qualified. New risk: a hand-rolled TUF for a verifier population of
approximately one. **[Corrected by F50, round 9: the claim "signatures
only protect the latest pointer" overreached. The honest decomposition:
content identity = hashes; availability = SWH/Zenodo; OFFICIAL RELEASE
AUTHORSHIP = the pinned release key; WITHDRAWAL AUTHORITY = the pinned
governance key — both signature roles are load-bearing in v1 even
though freshness/continuous recovery is deferred.]**

**[O5 as first proposed — "freeze all formats, stage operations" — was
REJECTED by round 9 (F48): a frozen, unimplemented schema is still v1
complexity and compatibility debt, contradicting the ratchet below; and
F49 showed "first incident" is a dishonest tooling trigger. Superseded
by O5a (see Owner decisions): freeze and implement ONLY the six
year-one-exercised formats; key-manifest/recovery/trust-branch designs
demoted to a NON-FROZEN contingency appendix; compromise halts
publication rather than triggering improvised recovery.]**

**Ratchet guard (process rule, amended F51):** 8 review rounds each
ADDED mechanism; none removed any. Before the D16.1 freeze, every
mechanism must name (a) the concrete failure it prevents and (b) who
exercises it in YEAR ONE; no year-one exerciser → contingency appendix,
not v1 scope. Ongoing: every PUBLIC schema needs a current consumer, a
concrete prevented failure, and an exercised conformance path — schema
count is a signal, not a quota (the earlier one-in/one-out rule was
rejected as forcing unrelated deletions).

**Explicitly NOT cut** (load-bearing for the original goals or legally
required): per-work identity/early cutoff; the manifest chain; one
canonicalizer/one root; fail-closed admission + assessment evidence
(rights exposure is real); the F4/F5 acceptance oracles.

## External design review round 9 (2026-08-25) — findings F48–F51

Reviewer verdicts: F43–F47 approved; **"Manifest core: ready for freeze
after O3(b) ratification"**; O5 staging INTENT approved,
"freeze-all-formats" REJECTED — replaced by the reviewer-drafted O5a;
recommended next review validates the reduced O5a boundary rather than
hardening dormant trust machinery. Commits 9ca7d330 + d3cc456f verified
ledger-only, clean `git diff --check`, HEAD d3cc456f, clean worktree.

- **F48 (O5 self-contradiction — Blocker to O5 ratification)** — O5
  froze every trust format while its own ratchet said unexercised
  mechanisms belong in a contingency appendix: a frozen, unimplemented
  schema is still v1 complexity and compatibility debt. Concrete proof
  the freeze was premature: recovery's `key_manifest_hash` never
  required the replacement key-manifest's seq/predecessor/
  effective_after/revocations to agree with accepted_head — execution
  would have uncovered more post-"freeze" corrections. Fix applied:
  key-manifest and recovery formats carry NO frozen /1 contract; moved
  to the non-normative contingency appendix with the defect recorded.
- **F49 (late triggers — Blocker)** — "first incident" cannot be the
  moment recovery tooling gets its first implementation and integration
  test, nor "first rotation" the first exercise of window semantics. Of
  the two honest options (implement-and-drill everything before first
  publication, or drop the continuous-recovery promise from v1), the
  second is proportionate and adopted: compromise HALTS publication;
  archived content-addressed releases remain valid; out-of-band notice;
  the next trust epoch is designed deliberately.
- **F50 (signatures ≠ just freshness — Strong suggestion, adopted)** —
  the facilitator's "signatures only protect the latest pointer"
  overreached: hashes prove integrity and archives preserve
  availability, but neither authenticates that SORANOHA issued a release
  or AUTHORIZED a withdrawal. Adopted decomposition: content identity =
  hashes; availability = SWH/Zenodo; release authorship = pinned online
  release key; withdrawal authority = pinned offline governance key;
  freshness/continuous recovery = explicitly deferred. The simplicity
  re-review section is corrected in place.
- **F51 (ratchet rule — Strong suggestion, adopted)** — one-in/one-out
  can force unrelated deletions; replaced with the stronger rule: every
  public schema needs a current consumer, a concrete prevented failure,
  and an exercised conformance path. Schema count is a signal, not a
  quota.

## External design review round 10 (2026-08-25) — findings F52–F55

Reviewer verdicts: F48–F51 approved; O5a's simplified direction
approved, ratification withheld pending F52–F55 (all four now applied);
O3(b) still recommended; the round-9 "manifest core ready for freeze"
statement self-corrected as "correct about its internal transition model
but too broad as an integration verdict". Slice 0 ready; **with F52,
Slice 1 depends only on D20.** Commit 084d1301 verified as HEAD,
ledger-only, clean `git diff --check`, clean worktree.

- **F52 (build/admission conflation — Blocker)** — Slice 1 was required
  to reproduce all 17,602 works AND emit `snh-manifest/1`, but that
  manifest is public-only with `works = admitted − withdrawn`, while the
  rights policy blocks release and assessments are unmigrated — so
  Slice 1 had to admit 17,602 unassessed works illegally or fail its own
  count. Fix applied: Slice 1 outputs CAS/trace results and an internal
  build index only; 17,602/17,602 measured through the trace store;
  manifest assembly + round-trip acceptance moved to Slice 2 on a fully
  assessed fixture. "Can build" is cleanly separated from "may publish";
  the kernel no longer waits on the publication protocol, O1, or O3.
- **F53 (freeze inventory not closed — Blocker)** — two contradictions:
  the assessment-snapshot was counted frozen while its content schema is
  explicitly unknown until the rights migration; and D18.1 requires
  SIGNED archival receipts while receipts were absent from the six
  formats and outside snh-sig/1's stated scope. Fix applied: staged
  freezes replace the artificial count — five formats freeze at D16.1;
  the assessment-snapshot content schema and the restored seventh format
  `snh-archive-receipt/1` (signed by the release key, per the existing
  D18.1 decision — the honest choice over unsigned recomputed evidence)
  freeze before Slice 3, each at its first consumer per F51.
- **F54 (key pinning after its first consumer — Blocker for Slice 3)** —
  Slice 3 verified signatures against fingerprints in the published
  promise, but the promise is a Slice-4 deliverable, and repo-hosted
  keys cannot authenticate themselves. Fix applied: a minimal trust
  anchor (both public-key bytes + fingerprints, independent immutable
  channel, loaded as pinned verifier configuration) precedes the first
  signed release; the fuller promise document stays in Slice 4; no new
  wire schema.
- **F55 (byte identity ≠ authorship — Blocker for the compromise
  runbook)** — "archived releases remain valid" glossed over
  unknown-duration compromises: an attacker may have signed and archived
  releases before detection; those bytes verify, but their status as
  AUTHORIZED releases is contested. Fix applied, precise failure
  semantics (no recovery wire protocol needed): artifact ids always
  identify exact bytes; the official chain freezes at the last
  uncontested independently recorded checkpoint; later signatures are
  contested until an out-of-band notice names the accepted cutoff;
  release-key compromise halts publication; governance-key compromise or
  loss halts governance operations and any publication requiring them.

## External design review round 11 (2026-08-25) — findings F56–F59

Reviewer verdicts: F52–F55 remain; O5a-as-amended ratification withheld
pending F58/F59 (both now applied); the largest remaining simplification
was DELETION, not protocol: drop the obsolete Slice-1 governance gate,
make the build index a trace-store view, reuse Zenodo as both trust
anchor and independent checkpoint channel.

- **F56 (stale D20 gate — simplification)** — F52 removed manifests and
  admission from Slice 1, so the "publication schemas, manifest identity
  → soranoha before Slice 1" transfer had no Slice-1 consumer and
  conflicted with AGENTS.md's current boundary. Fix applied: ABC keeps
  that ownership through Slice 1; soranoha/ owns only kernel, CAS, trace
  store, copied renderers; the transfer moves to the first Slice-2 work
  that assembles manifests. **Slice 1 is READY NOW** — the administrative
  dependency was unnecessary.
- **F57 (shadow manifest risk — simplification)** — a separately
  persisted "internal build index" would be a second schema with its own
  synchronization, lifecycle, and authority questions: a shadow
  publication format. Fix applied: the build index is a QUERY/disposable
  export over the trace store (CAS bytes ← trace records → derived
  view), with no independent identity, retention promise, or canonical
  schema; Slice 2 consumes a transactionally consistent trace snapshot.
- **F58 (snapshot freeze timing — correction)** — postponing the
  assessment-snapshot content schema to pre-Slice-3 contradicted
  Slice 2, which constructs an assessed fixture, publishes
  snapshot/report, hashes the snapshot into intent_id, and exercises an
  assessment-only delta — that IS the schema's first consumer. Fix
  applied: minimal snapshot schema frozen before Slice 2 and exercised
  on the fixture; full-corpus assessment MIGRATION (data) stays a
  Slice-3 prerequisite; only `snh-archive-receipt/1` freezes before
  Slice 3. Count de-emphasized: "freeze at first consumer" is the
  durable rule.
- **F59 (receipt ≠ independent checkpoint — correction)** — F55's
  parenthetical equated the freeze point with D18.1 archive
  verification, but the archive receipt is signed by the very release
  key whose compromise is being investigated: an attacker controlling it
  could publish, archive, and sign the receipt; SWH proves byte
  preservation, not Soranoha's independent authorization. Fix applied:
  the receipt's meaning is narrowed (archival completeness + release-key
  self-report); the compromise cutoff comes only from independent
  pre-incident records — the F54 Zenodo trust-anchor deposit (which now
  also names the genesis manifest_id) and the quarterly Zenodo deposits
  thereafter, each naming the then-current chain head. No new wire
  format. Honest v1 exposure recorded: compromise before the first
  post-genesis checkpoint may leave everything after genesis
  authorship-contested.

## Contingency appendix (NON-NORMATIVE, NOT FROZEN — per O5a/F48)

The following designs are preserved for deliberate future activation;
none carries a frozen wire contract, and all would require fresh review
before use: the `snh-key-manifest/1` append-only key history with
chain-ancestry windows (F41/F43/F44/F46 sections above), the
`snh-recovery/1` statement + bootstrap/cross-channel rules
(F27/F29/F32/F37 sections above), the protected `trust` branch, and the
root-key ceremony. Known open defect recorded by F48: recovery's
`key_manifest_hash` lacks consistency requirements binding the
replacement key-manifest (seq, predecessor, effective_after, cumulative
revocations) to `accepted_head` and the recovery record. **Activation
trigger (O5a): BEFORE adding a second release key, promising
cryptographic continuity, or serving an external consumer that requires
authenticated freshness — never after an incident begins.** Until then,
v1's posture is F49: halt on compromise; archives carry the permanence
promise.

## Dev Handoff (2026-08-24; slices 0–2 amended by F4/F5/F7, D16–D18; slice 3 rewritten per round 2; slice gating per F16)

### Slice 0 — prerequisite probe (existing machinery, disposable)
Regenerate the acceptance reference with the CURRENT abc pipeline at the
pinned revision (R12): full `build-publication!` run → expect 17,602 works.
Run it TWICE with the pinned invocation in F4 (identical `--snapshot-date`
both runs). Diff handling per F4: artifact-class diffs (TEI/plaintext/
validation-result/identity-bearing bytes) are defects — fix or apply a
versioned normalization to both systems; only non-artifact diagnostics may
be excluded. Keep the tree; it is the golden reference.

### Slice 1 — kernel end-to-end at the pinned revision
Build `soranoha/` (own flake, D12/D15 layout): `core` (config: one
SORANOHA_ROOT; the one canonicalizer, reusing existing shared test vectors),
`kura` (SQLite WAL trace table + sharded CAS, blob-before-trace commit,
append-only history ledger), `yomi` (clone management + catalog/selector
port with injectivity assert), `ori` (stages: parse, convert, render-tei,
render-plaintext, validate-tei; renderers copied from abc). **Output
(F52, round 10): CAS + trace-store results — NOT a public
`snh-manifest/1`.** The public manifest is public-only, requires
admission evidence, and enforces `works = admitted − withdrawn` — with
the rights policy blocking release and assessments unmigrated, Slice 1
emitting one would have to either admit 17,602 unassessed works
illegally or omit them and fail its own count. "Can build" is hereby
separated from "may publish". **The build index is a QUERY/disposable
export over the trace store (F57): CAS bytes ← trace-store records →
derived view. It has NO independent identity, no canonical schema, no
retention promise — a separately persisted index would be a shadow
publication format with its own synchronization and authority questions.
Slice 2 consumes a transactionally consistent trace snapshot when
assembling manifests.** **Slice 1 is READY NOW (F56)** — it depends on
neither the D16.1 freeze, O1, O3, nor any ownership transfer (D20 as
amended moves to Slice 2; ABC keeps publication-schema/manifest-identity
ownership during this slice). No za (publishing, signing, CI) in this
slice. TEI profile artifacts consumed as flake input from abc per D20.

Acceptance criteria (all measurable):
1. Work-by-work byte equality of TEI + plaintext vs the slice-0 reference
   (F4 rules: no artifact-class exclusions), 17,602/17,602 — **counted
   through the trace store** (F52).
2. Double-build byte-identical (closes R4); ledger uniqueness query returns
   0 violations.
3. Performance per the F7 protocol (cold/warm definitions, median-of-5,
   recorded hardware, RSS/disk guardrails).
4. Trace keys include stage-code-version AND toolchain-id (inspect schema —
   the incomplete-key trap).
(The former criterion 5 — manifest round-trip — moves to Slice 2 per
F52.)

### Slice 2 — release semantics
Preconditions (round 11): O3(b) + O5a ratified with reviewer boundary
validation; the six pre-Slice-2 formats frozen (incl. the minimal
assessment-snapshot schema, F58); the D20-as-amended AGENTS.md
manifest-ownership update (F56).
**Manifest assembly enters HERE (F52), on a FULLY ASSESSED FIXTURE
corpus** — not the unassessed full corpus: `snh-manifest/1` emission per
the frozen D16.1 spec, with real admission evidence
(snapshot/report) for every fixture work. Acceptance inherited from
Slice 1's former criterion 5: manifest round-trip — canonical bytes →
manifest_id stable across re-serialization; validates against the strict
schema; intent_id re-derives.
Chain mechanics on that manifest: prev-manifest hash chain, the D17
compare-and-append publication transaction, D18 GC roots + lifecycle
states. Build at a SECOND (newer) upstream revision; acceptance = the F5
three-set oracle: (a) source/selection delta, (b) stages
invalidated/executed, (c) artifact-byte/manifest delta, with invariants
(artifact change ⇔ byte change; unchanged bytes retain ids; every executed
stage explained by a changed declared input). Fixture tests: addition,
deletion, withdrawal (R8 path — must exercise the F26 chain invariants:
monotonic withdrawn map, disjoint slug sets, tombstone ids immutable
except via valid O3(b) `amends` chains — F47),
output-preserving source edit, the R7 include-and-flag path with an
invalid work, an assessment-only delta (same upstream/toolchain, enlarged
assessment snapshot → NEW intent_id, per F24), and the F34 governance
operations: a withdrawal with NO upstream change (distinct intent_id via
release_intent) and — if O3(b) is ratified — a tombstone amendment
(amends chain verified; withdrawn set unchanged; distinct intent_id).

### Slice 3 — za publishing + CI (rewritten per D17.1/D18.1/D21)
Preconditions: O1 ratified (with the round-4 wording: public-release-
allowed is the inclusion rule's decision) AND the assessment evidence
committed as versioned data (F24 snapshot source); O2 host named and
F12-probed; rights admission consumed-from-abc or transferred (F14c);
F12 repo-growth probe run against the chosen origin; O5a signing in
place: two directly pinned disjoint Ed25519 keys generated (online CI
RELEASE key; offline GOVERNANCE key), public keys at `keys/release.pub`
+ `keys/governance.pub`; **the F54 minimal trust anchor published BEFORE
the first signed release: both public-key bytes + fingerprints on an
independent, immutable channel (repo-hosted keys cannot authenticate
themselves; the Slice-4 promise document arrives too late to be the
first pin), loaded as pinned verifier configuration**; the
pre-Slice-3 staged freezes done (assessment-snapshot content schema,
`snh-archive-receipt/1` — F53); all signatures emitted as `snh-sig/1`
envelopes (F33); the F49/F55 compromise runbook documented (chain
freezes at last uncontested checkpoint; post-compromise signatures
contested until an out-of-band cutoff notice; artifact ids always
identify exact bytes). No key-manifest, trust branch, or recovery
tooling in this slice (O5a — contingency appendix only). Additional
acceptance: a governance-withdrawal fixture executed end-to-end (event
signed by the governance key; F39/F45 transition invariants verified by
the published checker).
Publication = the D17.1 remote compare-and-append: one complete commit
(blobs + manifest + sig) fast-forward-pushed to the protected branch;
rejection → re-chain and retry; intent_id duplicate check enforced.
Forgejo auto-release polls upstream; admission is a fail-closed input
(policy hash in manifest). Serving tree (blobs/, releases/, history.json)
derives from the repo. Archival: SWH save-code-now per release,
non-blocking; receipts written only after the F12 four-point verification
passes. Acceptance: (1) two consecutive automated releases from real
upstream movement, chain verified end-to-end by the published checker;
(2) a forced concurrent-publish attempt loses the push race and correctly
re-chains without duplicate intent; (3) at least one release reaches
archive-verified via a receipt passing all four F12 checks, with measured
(not assumed) archival latency recorded in the ledger.

### Slice 4 — citability layer
Quarterly Zenodo snapshot (concept DOI + first version DOI) — **each
deposit names the then-current chain head, doubling as an independent
authorship checkpoint (F59)**; w3id.org/soranoha registration; published
promise document incl. R8 tombstone policy + R7 validation policy +
restated key fingerprints (the FIRST pin is the earlier F54 anchor
deposit, not this document).

### Post-JADH2026 — retirement lanes (D6/D7)
Archive parser-RQ; retire abc build path lane-by-lane (each lane removed
only after kernel covers it); ADR corpus review/prune (off hot path);
governance becomes CI-only check.

### Test strategy
Golden canonicalizer vectors (existing); unit: trace-store round-trip,
commit-order, injectivity, manifest chain verification; small fixture corpus
for fast CI; full-corpus byte-equivalence is operator-run (not a CI gate).
TDD pins: trace-key composition, blob-before-trace, R7 flag path.

### Observability & rollback
CI logs + release chain + kura.verify reports (ledger uniqueness, ~1%
sampled rebuilds, canary set, fixity sweep). Releases are immutable: a bad
release is never deleted — it is superseded by the next one; consumers roll
back by citing the previous release tag.

### Remaining unknowns / owners
- Slice-0 diagnostics-only exclusion list (F4 rules) — dev, slice 0.
- `snh-manifest/1` JSON Schema file authored from the D16.1 normative
  section — dev, before slice 1, only AFTER the D16.1 freeze is
  re-approved (now a mechanical transcription of a standalone spec, per
  F25).
- `snh-assessment-snapshot/1` MINIMAL content schema (per-contribution
  facts per the 2026-07-11 model) — dev with owner, **frozen before
  Slice 2 and exercised on the assessed fixture (F58)**. Its identity,
  retention, resolution, and verifier semantics are already normative in
  D16.1 (F30). Full-corpus assessment DATA migration remains a Slice-3
  prerequisite — schema and migration are separate obligations.
- O3 (tombstone mutability: immutable vs amends-chain) — owner, BEFORE
  the D16.1 freeze (F31). Facilitator and round-6 reviewer both recommend
  (b); its F34 precondition is applied.
- ~~Governance-event record content schema~~ RESOLVED round 7: the public
  `snh-governance-event/1` object is frozen in D16.1 (F40). Remaining:
  the access-controlled private evidence record's handling/retention
  policy — owner, before the first real withdrawal.
- Pinned-key setup (O5a/F54): generate the two disjoint Ed25519 keys
  (release online for CI; governance offline), publish
  `keys/release.pub` + `keys/governance.pub`, AND publish the minimal
  trust anchor on Zenodo (F59: the deposit carries key bytes +
  fingerprints + the GENESIS manifest_id, making it both the first pin
  and the first independent authorship checkpoint) BEFORE the first
  signed release — owner, before Slice 3. Quarterly Zenodo deposits
  thereafter double as the subsequent independent checkpoints; the
  fuller promise/paper restates the anchor at Slice 4.
  (Replaces the round-7/8 root-ceremony and fingerprint-venue items; no
  root key or key-manifest exists in v1.)
- `snh-archive-receipt/1` field schema (F53, seventh format) — dev,
  frozen before Slice 3 alongside its first consumer.
- Tokenizer lane (vibrato-pipe) identity design — owner, post-JADH2026 (D4).
- Zenodo record metadata + first snapshot timing — owner, slice 4.
- w3id.org PR — owner, slice 4.
- Forgejo runner sizing/secrets — owner, before slice 3.
- Q7 (public on-demand-regeneration promise): resolved by D2 — archive is
  the contract; regeneration stays best-effort and unadvertised.

## Prior art findings (2026-08-24)

### Build-system side (researched; load-bearing sources inlined under F9)

- In Build-Systems-à-la-Carte taxonomy, the chosen principle = **constructive
  traces**: a trace table `(stage-id, stage-code-version, toolchain-id, params,
  input-hashes) → output-hashes` + a separate content-addressed blob store
  (CAS). This is the Bazel/CloudBuild/Cloud-Shake cell — the industry-standard
  realization of early cutoff (paper §5.3 endorses exactly this split).
- **Nix-native per-work derivations (candidate C) contraindicated in 2026**:
  ca-derivations still experimental/mid-redesign (Lix removed them); classic
  Nix is *deep* constructive traces — no early cutoff by construction; ~0.28 s
  fixed overhead per derivation build ⇒ ~80 min pure overhead for 17.6k works;
  no sub-second no-op check. Use Nix to pin/name the **toolchain** (toolchain-id
  in trace keys = hash of Nix closure), not per-work builds.
- Minimal sound store: CAS blobs (hash-named, temp+rename atomic writes,
  mark-and-sweep GC from live traces) + SQLite WAL trace table; commit blob
  before trace row; treat missing blob as trace miss; trace table is the trust
  boundary (only CI/pipeline writes it — Bazel cache-poisoning lesson).
- Top correctness traps in the wild: **incomplete cache keys** (DVC keys on
  command string not tool version; Gatsby's coarse keys destroyed user trust →
  "always clean" culture) and **untrusted/concurrent cache writers**. Include
  per-stage code version AND toolchain id in keys (Cloud Shake's two Vers).
- Determinism verification for the reproducibility promise, essentially free:
  (1) append-only history ledger with uniqueness invariant — same action key
  mapping to 2 output hashes = proven nondeterminism (SQL query); (2) sampled
  ~1% cache-ignoring double-builds per CI run; (3) a canary work-set rebuilt
  under varied env (locale, tmp dir, enumeration order).
- Non-determinism under constructive traces degrades efficiency only; under
  Nix-style deep traces it breaks correctness (frankenbuilds) — another reason
  the app-layer ledger is the right home for per-work incrementality.

### Archival storage & verification side (researched; load-bearing sources inlined under F9)

- **OCFL: export target, not live layout.** Its versioned-object model
  mismatches a CAS+manifests design; corpus-as-one-object is quantifiably
  pathological (inventory copies exceed corpus bytes at daily cadence, OCFL
  spec issue #367). Digest-addressed store makes later OCFL/BagIt export a
  mechanical transform if an institution ever requires it.
- **Canonical store: plain sharded CAS** `objects/sha256/ab/<hash>` on
  ZFS(zstd)/XFS; bytes stored exactly as hashed (no format between citation
  and storage). A bare git repo of the tree doubles as delta-compressed
  mirror/DR. git itself is master-mirror, not citation authority (blob-header
  hashing, SHA-1→256 transition unfinished).
- **Serving convention converges across Go proxy / OCI / Debian by-hash /
  IPFS**: immutable digest paths + thin mutable pointer layer, all static:
  `/blobs/sha256/<hex>`, `/releases/<tag>/manifest.json`,
  `/releases/latest` (short cache), per-work `history.json` (static
  Memento-style TimeMap), slug symlinks per release for human URLs. nginx:
  `immutable` cache headers, precompressed siblings, no body-modifying
  filters. w3id.org namespace in front so cited URLs survive host moves.
- **Verifiable "reproduce forever" promise, near-zero ops**: manifests in a
  public git repo; **each manifest embeds the previous manifest's sha256**
  (append-only hash chain independent of git); signed tag per release;
  mirrors; **Software Heritage Save-Code-Now per release** — the only venue
  where daily cadence is native, content-dedup makes deltas free, and SWHIDs
  are now ISO/IEC 18670:2025; cosign offline bundles committed alongside
  (not Rekor URLs — shards get turned down). Tessera/tlog-tiles static
  transparency log is the optional gold-plated upgrade.
- **External venues**: Zenodo = monthly/quarterly snapshot DOIs (concept DOI
  + version DOIs; 50 GB & 100-file caps force tarball bundling — fine);
  Hugging Face = community mirror only (no permanence guarantee; Xet dedup
  suits daily deltas); CLARIN B-centre / TextGrid = annual reference edition
  for disciplinary discoverability; Internet Archive = optional immutable
  item per snapshot. This resolves the D2 hybrid concretely and Q3 (static
  files only — no resolver service needed).
