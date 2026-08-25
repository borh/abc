# Publication Rearchitecture — Design Ledger

Status (post review round 7, 2026-08-25):
- **Slice 0: READY** (F4 pinned procedure).
- **Slice 1: BLOCKED** on the D16.1 freeze (round-7 veto: F38 — withdrawal
  contradicted the admission-report equality, fixed; F39 — governance
  transitions underconstrained, strict predecessor→successor invariants
  added; F40 — governance event promoted to a public sanitized
  authorization object signed by a distinct governance-role key). Freeze
  gate = reviewer re-review + owner O3 choice + owner approval. Plus the
  D20 AGENTS.md ownership transfer (not yet edited).
- **Slice 2: BLOCKED** transitively on the D16.1 freeze (which now contains
  the governance-event protocol round 7 required for Slice 2).
- **Slice 3: additionally BLOCKED** on O1 (approved conditional on committed
  assessment evidence — owner ratification pending), O2 concrete host
  (wording approved; none named or F12-probed), and the F12 probes. Before
  public signing: the F41 `snh-key-manifest/1` protocol (now specified) and
  F32/F36/F37 rules must be implemented.
- **O3: facilitator and reviewer both recommend option (b)**; owner
  ratification pending; O3(b) "technically approvable" per round 7 but does
  not clear the freeze.
D18.1 approved conditional on F12. Authoritative record: decision log +
reviews F1–F9, F10–F15, F16–F23, F24–F28, F29–F33, F34–F37, F38–F42 + dev
handoff below.
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
| D16 | **Manifest v1 frozen before slice 1** (external review F3): canonicalizer = `rfc8785-safe-integer-json-string-v1` (legacy c14n-v0 excluded from kernel); strict schema `snh-manifest/1`; detached signature; derived release name; type registry {tei,txt,val,manifest}; full spec in F3 section. **VETOED by review round 2 → superseded by D16.1 draft (F13): types renamed (plaintext/tei-validation), visibility field removed (public-only manifest + private index), receipts excluded, admission field added, intent_id added, nested shapes + sort rules specified. Freeze awaits owner O1/O2 + reviewer re-approval.** **Round 3 (F16–F20) applied: intent_id = pre-build operation identity; withdrawn.since removed (self-reference); admission binds inclusion_rule_id+hash.** **Round 4 (F24–F26) applied: admission gains assessment snapshot commitment (in intent_id); D16.1 rewritten as a STANDALONE normative spec (wire version stays `snh-manifest/1`; "v1.1" naming retired); tombstone chain invariants added. Freeze gate decoupled from O1/O2 per F28.** **Round 5 (F30–F31, F33) applied: admission evidence = two retained PUBLIC artifacts — `snh-assessment-snapshot/1` (facts) + `snh-admission-report/1` (the rule's total partition), both registry types added pre-freeze; tombstone mutability surfaced as O3 options (immutable vs amends-chain) — freeze now ALSO gated on the owner's O3 choice; signature wire format `snh-sig/1` defined.** **Round 6 (F34–F36) applied: `release_intent {kind, governance_event_hash}` added as manifest field 12 and intent_id coordinate (withdrawals/amendments are distinct operations); snapshot fact model gains `not-evaluated` (distinct from `undetermined`; absence is an explicit fact); signature verification split — operational keys via key-manifest with chain-position validity windows, root key via out-of-band pinned fingerprint only.** **Round 7 (F38–F40) applied: admission invariant corrected to `works = admitted − withdrawn` (withdrawal is governance, not inclusion); strict predecessor→successor transition invariants (added/changed slugs = the event's affected_slugs exactly; mixed build/governance prohibited per manifest); governance event promoted to a public sanitized `snh-governance-event/1` artifact signed by a governance-role key (registry type added pre-freeze).** | review rounds 2–7, 2026-08-24/25 | draft — cheap to change until frozen |
| D17 | **Publication = atomic compare-and-append transaction** (F1): flock + head re-check + complete-write + atomic HEAD advance; idempotent by manifest_id; CI concurrency is optimization only. **Amended by D17.1 (F10): the remote protected git ref is the authority (fast-forward-only push, re-chain on rejection); idempotency by intent_id, not manifest_id; local flock is a single-host optimization.** | review round 2, 2026-08-24 | internal protocol, revisable |
| D18 | **Retention/lifecycle** (F2): all published manifests are permanent GC roots; states built→published→archived→citable; public git repo carries published artifact bytes (SWH archives real bytes); archival receipts in subsequent manifests; indefinite promise advertised only when archive-verified. **Reviewer sign-off: conditionally APPROVED; conditions adopted as D18.1 (F11/F12): receipts are separate signed attestations keyed by manifest_id (never in later manifests); states published → archive-verified, citation-eligible is a policy projection; archival latency treated as unbounded until measured; SWH completeness proven by the four F12 checks on a real public origin.** | owner-endorsed principle + review round 2 | promise text frozen at first public release |
| D19 | **Trust** (F8): offline root key → key-manifest → operational signing keys; revocation procedure; fork/equivocation consumer rule anchored in SWH-archived checkpoint history. **F22: fail-closed freeze rule. F27: signed recovery statement (`snh-recovery/1`) specified — incident record with rollback protection (recovery_seq + prev_recovery chain), key revocation/transition, and defined discovery paths; see F8 section. F29: the incident_id self-hash removed — recovery_id is DERIVED (sha256 of canonical bytes, like manifest_id), signed as `snh-recovery-sig/1:<recovery_id>`. F32: consumer bootstrap rules (chaining = rollback protection for stateful consumers only; full-chain fetch; cross-channel fail-closed); recovery lives on a protected `recovery` BRANCH, not refs/meta. F33: all detached signatures use the `snh-sig/1` envelope. F36: verification split by trust object — recovery signatures verify ONLY against the out-of-band pinned root fingerprint (never through a key-manifest, which recovery itself replaces); operational validity windows are chain-position facts (`effective_after` boundaries), never manifest-declared dates. F37: w3id is a discovery route, not a copy; bootstrap accepts the longest non-conflicting valid chain, tolerating lagging archives with valid prefixes. F40: key ROLES (release vs governance) — withdrawal authority separated from the unattended-CI release key. F41: `snh-key-manifest/1` append-only history specified (derived id, seq + prev chain, effective_after boundary, roles, cumulative revocations, root-only signature, trust-branch publication, same-boundary/gap = fail-closed root incident).** | review-adopted 2026-08-24; amended rounds 4–7 | upgradeable to Tessera log |
| D20 | **Ownership** (F6): TEI profile schemas consumed as explicit flake input from abc during migration (no schema copies); AGENTS.md ownership transfer recorded before slice-1 implementation. | review-adopted 2026-08-24 | owner action: AGENTS.md edit |
| D21 | **Rights/registry admission restored** (F14): kernel ports the fail-closed value-plus-hash rights authority (policy hash recorded in manifest `admission`); per-work inclusion governed by named rule (O1, owner); admission responsibility consumed from abc or transferred in AGENTS.md before Slice 3. Currently `publication-policy.edn` BLOCKS release — the kernel inherits that block until the rights assessment migrates. **F17: admission is assessment-based per O1 (adopted reviewer rule); catalog flags seed, never authorize; private archiving needs its own authorization policy.** **F24: the assessment data itself is cryptographically committed — bound in `admission` and included in intent_id, so newly completed assessments change the operation identity. F30: the commitment is two retained public artifacts, separating domain roles — the assessment SNAPSHOT commits facts (public-domain / in-copyright / undetermined per contribution); the admission REPORT records the inclusion rule's total partition (admitted/excluded/quarantined with reasons). Both published, permanently rooted, hash-resolvable.** | review rounds 2–5 | policy content owner-governed; mechanism fixed |

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
D12/R2). The ownership transfer (publication schemas, manifest identity →
`soranoha/`) is recorded in AGENTS.md **before slice-1 implementation
begins**; abc retains ownership of everything not yet migrated. [Owner
action: AGENTS.md edit.]

### F7: Performance acceptance protocol (amends Slice 1 below)
Fixed corpus = pinned revision; cold = empty trace store + CAS; warm =
fully populated. Metrics, each median of 5 runs on recorded hardware (CPU
model, RAM, storage, kernel noted in the report): (1) engine no-op:
process-ready → plan-complete with 0 stages, < 1 s; (2) end-to-end no-op
incl. JVM startup, < 15 s; (3) cold full build < 10 min; guardrails: peak
RSS < 8 GB, temp disk < 2× output. Numbers are acceptance bounds, revisable
only by decision-log entry.

### F8 → D19: Root of trust, revocation, and fork rule
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
**protected `recovery` branch** (`refs/heads/recovery`) on the O2 origin —
an ordinary branch, chosen over `refs/meta/*` because clones, branch
protection, the Forgejo UI, and archival tooling already understand it,
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
result. Derived states: **published** (on accepted chain) →
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
**Extended round 5 (F32): also verify on the chosen origin that the
protected `recovery` branch is advertised to clients and appears in the
SWH snapshot** — SWH's git loader preserves advertised refs (its ignore
list does not cover the branch), but whether a given Forgejo origin
advertises and protects it is origin-specific.

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
    (matching release_intent.kind), affected_slugs (sorted, the exact
    slugs this decision covers), authority (the deciding role, e.g.
    "owner"), evidence_hash (sha256 hex of the private request/evidence
    record, or 64×"0" when none)}`. It is signed (snh-sig/1) by a
    **GOVERNANCE-role key** from the F41 key-manifest — NOT the
    operational release key, so a compromised unattended-CI release key
    cannot withdraw works; irreversible withdrawal authority is
    deliberately separated from routine publishing. (Revisit trigger: the
    owner may instead ratify release-key withdrawal authority explicitly;
    the distinct role is the fail-closed default.) The private
    request/evidence (which may contain personal data) stays
    access-controlled, bound by `evidence_hash`. `governance_event_hash`
    = sha256 hex over the PUBLIC object's canonical bytes — so public
    verifiers can fetch it, check its signature and role, and compare
    `affected_slugs` against the manifest transition (F39).

**Type registry (closed, permanent names):** per-work types `tei`,
`plaintext`, `tei-validation` (the only types permitted in
`works[].artifacts`); release-level types `tombstone`, `release-manifest`,
`assessment-snapshot`, `admission-report`, `governance-event` (referenced
from `withdrawn`, the chain, `admission`, and `release_intent` — never
from `works[].artifacts`). Key-manifests and recovery statements are NOT
registry artifacts — they live on the trust branch, referenced by raw
hash, outside release identity. ANY addition
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
  - `kind = "withdrawal"` ⇒ the ADDED tombstone slugs equal EXACTLY the
    governance event's `affected_slugs`; every unaffected work's entry
    is verbatim-unchanged; all publication coordinates (corpus,
    toolchain, admission, selection_params) equal the predecessor's; the
    only `works` changes are the removals of the affected slugs.
  - `kind = "tombstone-amendment"` (O3(b) only) ⇒ the CHANGED tombstone
    ids equal exactly the event's `affected_slugs`, each via a valid
    `amends` chain; the withdrawn slug set, all of `works`, and all
    publication coordinates are verbatim-unchanged.
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

Every `.sig` file in the system (manifest AND recovery) is the
**`snh-sig/1` envelope**: canonical bytes
(`rfc8785-safe-integer-json-string-v1`) of `{schema: "snh-sig/1", key_id
(lowercase sha256 hex fingerprint of the signing PUBLIC key itself),
algorithm ("ed25519" — the only value in v1), signed_context (the exact
domain-separated string signed), signature (128 lowercase hex chars,
Ed25519)}`. A raw signature without this envelope is invalid — with
multiple operational keys a bare `.sig` cannot name its key.

**Verification rules are SPLIT by trust object (F36 — resolving every
key through the key-manifest was circular for recovery, since the
recovery statement is what introduces the replacement key-manifest):**
- **Manifest signature:** `key_id` MUST be an operational key authorized
  by the applicable root-signed key-manifest, unrevoked and within its
  validity window.
- **Recovery signature:** `key_id` MUST equal the independently pinned
  OFFLINE-ROOT fingerprint — distributed out-of-band (published
  paper/docs fingerprint per F8) — and is NEVER authorized through any
  operational key-manifest.

**Operational validity windows are CHAIN-POSITION facts, not dates
(F36):** each root-signed key-manifest declares `effective_after` — the
manifest_id of the last release manifest governed by its predecessor
(genesis key-manifest: 64×"0"). A release manifest is validly signed iff
its signing key is authorized by the key-manifest whose window covers its
chain position; the verifier walks the `prev_manifest` chain and switches
key-manifests exactly at the declared boundaries. Manifest-declared dates
(`snapshot_date`) play no role in key validity — a compromised key must
not be able to date itself into validity.

**Key-manifest protocol (`snh-key-manifest/1`, F41 — "applicable
key-manifest" is otherwise undefined):** an append-only, root-signed
history. Each key-manifest is a canonical-bytes document
(`rfc8785-safe-integer-json-string-v1`), closed schema:
- `schema`: `"snh-key-manifest/1"`.
- `key_manifest_seq`: integer ≥ 1, strictly increasing, gap-free.
- `prev_key_manifest`: the predecessor's DERIVED key_manifest_id;
  64×"0" for the genesis (manifest pattern — no self-hash member;
  `key_manifest_id` = sha256 hex over canonical bytes).
- `effective_after`: manifest_id of the last release manifest governed
  by the predecessor key-manifest; 64×"0" in the genesis.
- `keys`: array (sorted by key_id) of `{key_id (sha256 fingerprint of
  the public key), public_key (hex), role ∈ {"release", "governance"}}`
  — release keys sign release manifests (D17.1/CI); governance keys sign
  `snh-governance-event/1` objects (F40). Roles are disjoint
  authorizations; a key may appear once per role entry.
- `revoked_keys`: array of key_ids revoked as of this key-manifest
  (sorted; cumulative — once listed, forever listed).

Signature: OFFLINE ROOT only, `snh-sig/1` envelope over
`snh-key-manifest-sig/1:<key_manifest_id>`, key_id = the pinned root
fingerprint (F36 — never resolved through any key-manifest).
Publication/discovery: `keys/<key_manifest_seq>.json` + `.sig` on the
same protected trust branch that carries `recovery/` (F32); the F12
probe covers it; recovery statements reference key-manifests by
`key_manifest_hash` = key_manifest_id.
Conflict rule (fail-closed): two root-signed key-manifests with the same
`key_manifest_seq`, or claiming the same `effective_after` boundary, or
a gap in the sequence, is a ROOT-KEY INCIDENT — verifiers freeze exactly
as for release-chain equivocation (F22) and wait for a recovery
statement; there is no deterministic tie-break by design. Stateful
consumers retain the highest accepted key_manifest_id (rollback
protection); bootstrap consumers fetch and verify the full `keys/`
history from genesis alongside the recovery chain (F37 ordering).
Binding: a release manifest at chain position p is validly signed iff
its envelope's key_id is a `release`-role key, unrevoked, in the unique
key-manifest whose [effective_after, successor's effective_after)
window covers p.

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
  **Round-6 reviewer sign-off: RECOMMENDS option (b)** — "option (a)
  makes an ordinary typo a wire-version event" — conditional on F34,
  which is now applied (tombstone amendments have their own operation
  identity via `release_intent`, so O3(b) is executable). Owner
  ratification remains the freeze gate.

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
render-plaintext, validate-tei; renderers copied from abc). Output: local
release manifest {slug → artifact ids} + provenance. No za (publishing,
signing, CI) in this slice.

The manifest emitted in this slice is **`snh-manifest/1` per the D16.1
normative specification** (frozen before implementation; not a slice-2
invention; "v1.1" is not a wire version). TEI profile artifacts consumed
as flake input from abc per D20.

Acceptance criteria (all measurable):
1. Work-by-work byte equality of TEI + plaintext vs the slice-0 reference
   (F4 rules: no artifact-class exclusions), 17,602/17,602.
2. Double-build byte-identical (closes R4); ledger uniqueness query returns
   0 violations.
3. Performance per the F7 protocol (cold/warm definitions, median-of-5,
   recorded hardware, RSS/disk guardrails).
4. Trace keys include stage-code-version AND toolchain-id (inspect schema —
   the incomplete-key trap).
5. Manifest round-trip: canonical bytes → manifest_id stable across
   re-serialization; validates against the strict v1 schema.

### Slice 2 — release semantics
Chain mechanics on the D16 manifest: prev-manifest hash chain, the D17
compare-and-append publication transaction, D18 GC roots + lifecycle
states. Build at a SECOND (newer) upstream revision; acceptance = the F5
three-set oracle: (a) source/selection delta, (b) stages
invalidated/executed, (c) artifact-byte/manifest delta, with invariants
(artifact change ⇔ byte change; unchanged bytes retain ids; every executed
stage explained by a changed declared input). Fixture tests: addition,
deletion, withdrawal (R8 path — must exercise the F26 chain invariants:
monotonic withdrawn map, disjoint slug sets, immutable tombstone id),
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
F12 repo-growth probe run against the chosen origin; D19 recovery-record
machinery (F27/F29/F32) in place: root-signed `snh-recovery/1`
verification — including bootstrap full-chain verification and
cross-channel fail-closed — in the published checker, plus the protected
`recovery` branch on the origin; all signatures emitted as `snh-sig/1`
envelopes (F33); the F41 genesis key-manifest published on the trust
branch (root ceremony done, release + governance roles populated).
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
Quarterly Zenodo snapshot (concept DOI + first version DOI), w3id.org/
soranoha registration, published promise document incl. R8 tombstone
policy + R7 validation policy + key fingerprint.

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
- `snh-assessment-snapshot/1` FACTS content schema (per-contribution
  assessment fields per the 2026-07-11 model) — dev with owner, alongside
  the rights-assessment migration, before slice 3. Its identity,
  retention, resolution, and verifier semantics are already normative in
  D16.1 (F30); only the domain field content remains.
- O3 (tombstone mutability: immutable vs amends-chain) — owner, BEFORE
  the D16.1 freeze (F31). Facilitator and round-6 reviewer both recommend
  (b); its F34 precondition is applied.
- ~~Governance-event record content schema~~ RESOLVED round 7: the public
  `snh-governance-event/1` object is frozen in D16.1 (F40). Remaining:
  the access-controlled private evidence record's handling/retention
  policy — owner, before the first real withdrawal.
- Offline-root fingerprint publication venue (the F36 out-of-band pin:
  paper/docs) — owner, before Slice 3.
- Root-key ceremony: generate offline root, genesis `snh-key-manifest/1`
  (release + governance keys, F40/F41), publish `keys/1.json` on the
  trust branch — owner, before Slice 3.
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
