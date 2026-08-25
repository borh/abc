# Publication Rearchitecture — Design Ledger

Status (post review round 16, 2026-08-25):
- **`docs/design/snh-protocol-v1.md` is the SOLE NORMATIVE source,
  effective now (F84)** — the freeze will change stability, not
  precedence; this ledger is the decision/rationale record and nothing
  in it overrides the protocol spec. Per F80 the FROZEN objects are the
  executable JSON Schemas + conformance vectors, authored BEFORE the
  freeze review; per F88 the schemas govern structure, the spec governs
  semantic/state invariants, the vectors demonstrate both, and any
  disagreement blocks the freeze.
- Round-16 fixes applied in the protocol: F85 (publication commits
  have exactly ONE parent — the previously accepted head; merges
  rejected, closing the second-parent bypass of F78; genesis state
  made explicit), F86 (current-state reconciliation replaces the
  pairwise race taxonomy — the loser consults only the current head,
  handling any number of intervening operations; build-vs-governance
  reassembly subsumed by requeue), F87 (totality is an ASSEMBLER
  invariant against the transactionally consistent selection; the
  public-recomputation claim was circular and non-executable and is
  withdrawn; upgrade path recorded), F88 (authority split defined;
  this ledger's D16.1 body replaced by decision summaries — Git
  history holds the archaeology), F89 (report-field binding corrected
  — the report carries no `policy_id`). Reviewer RECOMMENDS ratifying
  F83; the `r<hash12>` display alias moved out of the protocol.
- Round-15 fixes applied in the protocol: F78 (first-parent HEAD
  transitions must satisfy `M.prev_manifest == H` — closes silent
  chain replacement), F79 (complete build/build reconciliation:
  same-projection-same-state succeeds; same-projection-different-state
  is a determinism failure; different projections requeue; unknown+absent
  = rejection path), F80 (regex quantifiers, reason_code domains, typed
  ids, explicit type/hash checks), F81 (snapshot candidate set bound to
  the recomputed selection — totality independently checkable), F82
  (`authority`/`evidence_hash` removed from the governance event), F83
  (dateless canonical naming — OWNER ratification pending, amends D13),
  F84 (normative-now).
- Round-14 fixes applied: F73 (operation-specific rejection
  reconciliation — never blind re-chain; stale `amends`/slug conflicts
  HALT for fresh governance authorization), F74 (`releases/HEAD`
  protocol: always present, 64 hex + LF, zero genesis, full-chain
  verification), F75 (concrete PRE-RELEASE discovery channel required —
  the Slice-4 paper postdates the first release and cannot be the first
  pin), F76 (release-name date = UTC committer date of the
  HEAD-advancing commit; reviewer's dateless alternative recorded as an
  owner option), F77 (D11/registry/Slice-3 stale-state reconciliation).

Superseded status detail (round 13):
- **Slice 0: READY**; **Slice 1: READY NOW** (F56; kernel is
  policy-blind per F52/F60 — rights admission lives entirely in
  soranoha.za).
- **Normative v1 (honest inventory per F69): four canonical JSON object
  schemas** — `snh-manifest/1`, `snh-assessment-snapshot/1`,
  `snh-admission-report/1`, `snh-governance-event/1` — **plus fixed raw
  signature and key encodings** (exact bytes specified; conformance
  vectors before Slice 2). Round-13 corrections applied: F66 (git head
  vs manifest head — `releases/HEAD`, explicit transaction), F67 (no-op
  projection = {corpus, toolchain, selection_params, admission}), F68
  (frozen linear event-amendment invariants), F72 (snapshot_date
  deleted — publication date derives from the accepted commit; no dates
  in manifest bytes).
- **Before Slice 2:** owner O3(b) + O5a-as-amended ratification with
  reviewer boundary validation; the D16.1 freeze (four formats + F69
  encoding vectors); the AGENTS.md manifest-ownership update (D20 as
  amended, F56).
- **Before Slice 3:** O1 + full-corpus assessment data; O2 concrete
  host + F12 probes. **Before the first signed release (F70):** the
  Zenodo trust-anchor deposit (key bytes + fingerprints + actual genesis
  manifest bytes and signature) under CREDENTIAL SEPARATION — deposit
  credentials unavailable to release CI (manual owner action or MFA
  workflow).
- **Lifecycle (F71):** stored state is `published` only;
  archive-verified is an observed reproducible predicate; citation
  eligibility is computed from a successful archival observation.
- **Compromise semantics (F55/F59/F70):** artifact ids always identify
  exact bytes; the chain freezes at the last credential-separated Zenodo
  checkpoint; post-checkpoint signatures contested until an out-of-band
  cutoff notice; release-key compromise halts publication;
  governance-key compromise/loss halts governance operations.
- **O3 (event-amendment form): facilitator and reviewer both recommend
  (b)**; owner ratification pending. **O5a-as-amended: owner
  ratification pending (reviewer: wait for F66–F69 — all four now
  applied).**
D18.1 approved conditional on F12. Authoritative record: decision log +
reviews F1–F9, F10–F15, F16–F23, F24–F28, F29–F33, F34–F37, F38–F42,
F43–F47, F48–F51, F52–F55, F56–F59, F60–F65, F66–F72, F73–F77,
F78–F84, F85–F89 + dev handoff below.
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
| D11 | Artifact classes: **publish** TEI, plaintext, per-work tei-validation-result; **parser-IR: retain operationally in CAS/trace — private archival EXPORT deferred pending an authorization policy (amended F77/F65, round 14; the original "archive privately" promise had no authorized consumer)**; **regenerate on demand** AAT + preservation sidecars (14.2 GB class eliminated from storage). | owner ratified rec, 2026-08-24; amended round 14 | easy per class |
| D12 | Placement: new top-level component, sibling of abc/ and ab-validator/, own flake, zero requires into abc namespaces (renderers copied in). abc/ and ab-validator/ frozen until after JADH2026, then retired lane by lane. | owner ratified rec, 2026-08-24 | revisit if kernel needs >renderers from abc |
| D13 | Identifiers ratified: artifact id = typed content hash; release name = `r<date>-<manifesthash12>`; snapshot DOIs **quarterly** (Zenodo concept DOI + version DOIs). Prefix harmonized with 2026-07-03 naming spec: **`snh:1:<type>:<sha256>`** (spec already reserves `snh:` + w3id.org/soranoha — matches storage rec). **PROPOSED AMENDMENT (F83, round 15; owner ratification pending): drop the date from release names — canonical identity = the full typed manifest id, display alias `r<hash12>`, dates presentation-only — because the Git committer timestamp is UNSIGNED: the same signed manifest could acquire a different derived name when repackaged. This reverses the date-inferable-name preference the owner stated at D3; the reviewer's technical argument is recorded for the owner's call.** | owner, 2026-08-24; F83 amendment proposed | prefix trivially renameable pre-first-release; frozen after |
| D14 | Slug scheme kept: `作品ID_人物ID_carddir_zipstem` as stable human-facing work name; qualifier, never identity. | owner ratified rec, 2026-08-24 | frozen after first release |
| D15 | Naming ratified per 2026-07-03 Model B spec: new top-level `soranoha/` component; namespaces `soranoha.core` (shared types/config/the one canonicalizer), `soranoha.yomi` (source-acquirer+selector), `soranoha.kura` (trace store+CAS+verifier), `soranoha.ori` (stages/renderers/validation), `soranoha.za` (release assembly+publishing). Vocabulary reused, NOT the old spec's scope (no LOD/IIIF/XTDB in v1). `snh:` id prefix per D13. | owner, 2026-08-24 | dir rename trivial pre-first-release |
| D16 | **Manifest v1 frozen before manifest assembly — i.e., before SLICE 2 per F52/F56 (headline updated round 12; the original "before slice 1" predates the F52 build/publish split)** (external review F3): canonicalizer = `rfc8785-safe-integer-json-string-v1` (legacy c14n-v0 excluded from kernel); strict schema `snh-manifest/1`; detached signature; derived release name; type registry {tei,txt,val,manifest}; full spec in F3 section. **VETOED by review round 2 → superseded by D16.1 draft (F13): types renamed (plaintext/tei-validation), visibility field removed (public-only manifest + private index), receipts excluded, admission field added, intent_id added, nested shapes + sort rules specified. Freeze awaits owner O1/O2 + reviewer re-approval.** **Round 3 (F16–F20) applied: intent_id = pre-build operation identity; withdrawn.since removed (self-reference); admission binds inclusion_rule_id+hash.** **Round 4 (F24–F26) applied: admission gains assessment snapshot commitment (in intent_id); D16.1 rewritten as a STANDALONE normative spec (wire version stays `snh-manifest/1`; "v1.1" naming retired); tombstone chain invariants added. Freeze gate decoupled from O1/O2 per F28.** **Round 5 (F30–F31, F33) applied: admission evidence = two retained PUBLIC artifacts — `snh-assessment-snapshot/1` (facts) + `snh-admission-report/1` (the rule's total partition), both registry types added pre-freeze; tombstone mutability surfaced as O3 options (immutable vs amends-chain) — freeze now ALSO gated on the owner's O3 choice; signature wire format `snh-sig/1` defined.** **Round 6 (F34–F36) applied: `release_intent {kind, governance_event_hash}` added as manifest field 12 and intent_id coordinate (withdrawals/amendments are distinct operations); snapshot fact model gains `not-evaluated` (distinct from `undetermined`; absence is an explicit fact); signature verification split — operational keys via key-manifest with chain-position validity windows, root key via out-of-band pinned fingerprint only.** **Round 7 (F38–F40) applied: admission invariant corrected to `works = admitted − withdrawn` (withdrawal is governance, not inclusion); strict predecessor→successor transition invariants (added/changed slugs = the event's affected_slugs exactly; mixed build/governance prohibited per manifest); governance event promoted to a public sanitized `snh-governance-event/1` artifact signed by a governance-role key (registry type added pre-freeze).** **Round 8 (F43–F46) applied: key windows corrected to chain-ancestry semantics (bᵢ, bᵢ₊₁]; key sets disjoint with globally unique key_ids; the governance event authorizes exact `{slug, tombstone}` pairs (`changes`), with a frozen signature filename/resolution rule; root public-key bytes published at `root.pub`, accepted only against the out-of-band fingerprint; Ed25519 encodings fixed (32 bytes / 64 hex; key_id = sha256 of raw bytes).** **Round 9 (F48/O5a) applied: the frozen-v1 schema set is exactly SIX year-one-exercised formats (manifest, assessment-snapshot, admission-report, tombstone, governance-event, sig); key-manifest and recovery are NON-NORMATIVE contingency, not frozen; v1 signing = two directly pinned disjoint keys; reviewer: "manifest core ready for freeze after O3(b) ratification".** **Round 10 (F52–F53) applied: Slice 1 emits NO public manifest (build ≠ admission — kernel depends only on D20); manifest assembly + round-trip move to Slice 2 on a fully assessed fixture; freezes STAGED — five formats at D16.1, assessment-snapshot content schema + `snh-archive-receipt/1` (the restored seventh format, signed by the release key) before Slice 3.** **Round 11 (F57–F58) applied: the build index is a derived trace-store view, never a persisted schema; staging corrected — SIX formats freeze before Slice 2 (the minimal assessment-snapshot schema's first consumer is Slice 2's fixture), only the receipt before Slice 3; "freeze at first consumer" is the rule, the count is incidental.** **Round 12 (F61–F65) applied — normative v1 is FOUR formats (manifest, assessment-snapshot, admission-report, governance-event): intent_id DELETED (retained-manifest retry protocol instead); `snh-tombstone/1` and `release_intent` DELETED (the governance event carries reason/statement directly; `withdrawn` maps slug → event id; manifest field `governance_event: null | id`); `snh-archive-receipt/1` DELETED (archive-verified = reproducible predicate, disposable report); `snh-sig/1` envelope DELETED (raw Ed25519 detached signatures; key selected by object kind); private archive index DELETED (CAS/trace state only).** **Round 13 (F66–F69, F72) applied: retry transaction made explicit (git commit head ≠ manifest head; `releases/HEAD`; success = on the accepted manifest chain); no-op projection = {corpus, toolchain, selection_params, admission} exactly; linear event-amendment invariants frozen (amends == predecessor's current event; changed entries point at THIS manifest's event); exact signature/key byte encodings + conformance vectors (honest inventory: four JSON schemas PLUS raw encodings); snapshot_date DELETED from manifest identity (publication date derives from the accepted commit).** **Round 14 (F73–F74 + extraction): rejection handling is operation-specific reconciliation (already-satisfied → succeed; build → reassemble under new governance; governance → revalidate unchanged event; conflict/stale amends → HALT, never re-sign); `releases/HEAD` fully specified (65 bytes, zero genesis, chain-verified); the live protocol extracted to `docs/design/snh-protocol-v1.md`.** **Round 15 (F78–F84): the protocol spec is the SOLE NORMATIVE source effective immediately; first-parent HEAD transitions must satisfy M.prev_manifest == H (append-onlyness); complete build/build reconciliation rules; schemas made freezeable (regexes, reason_code domains, typed ids, explicit type/hash checks; executable JSON Schemas + vectors are THE frozen objects); snapshot totality bound to the recomputed selection; governance event slimmed to {schema, kind, entries}; dateless naming proposed (F83, owner pending).** | review rounds 2–15, 2026-08-24/25 | draft — cheap to change until frozen |
| D17 | **Publication = atomic compare-and-append transaction** (F1): flock + head re-check + complete-write + atomic HEAD advance; idempotent by manifest_id; CI concurrency is optimization only. **Amended by D17.1 (F10): the remote protected git ref is the authority (fast-forward-only push, re-chain on rejection); local flock is a single-host optimization. Round 12 (F61): intent_id DELETED — idempotency via the retained-manifest retry protocol. Round 13 (F66/F67): the transaction distinguishes the git commit head (fast-forward CAS token) from the manifest head (`releases/HEAD` → `prev_manifest`); unknown-result success requires M on the ACCEPTED manifest chain; the scheduled-build no-op projection is exactly {corpus, toolchain, selection_params, admission}; nondeterminism halts as a defect.** | review rounds 2–13 | internal protocol, revisable |
| D18 | **Retention/lifecycle** (F2): all published manifests are permanent GC roots; states built→published→archived→citable; public git repo carries published artifact bytes (SWH archives real bytes); archival receipts in subsequent manifests; indefinite promise advertised only when archive-verified. **Reviewer sign-off: conditionally APPROVED; conditions adopted as D18.1 (F11/F12): receipts are separate signed attestations keyed by manifest_id (never in later manifests); states published → archive-verified, citation-eligible is a policy projection; archival latency treated as unbounded until measured; SWH completeness proven by the four F12 checks on a real public origin.** **Round 13 (F63/F71): receipts DELETED — archive-verified is an OBSERVED reproducible predicate (disposable report), not a stored lifecycle state; the stored lifecycle is `published` only; citation eligibility is COMPUTED from a successful archival observation. Independent authorship checkpoints are the F70 credential-separated Zenodo deposits, not archival status.** | owner-endorsed principle + review rounds 2–13 | promise text frozen at first public release |
| D19 | **Trust** (F8): offline root key → key-manifest → operational signing keys; revocation procedure; fork/equivocation consumer rule anchored in SWH-archived checkpoint history. **F22: fail-closed freeze rule. F27: signed recovery statement (`snh-recovery/1`) specified — incident record with rollback protection (recovery_seq + prev_recovery chain), key revocation/transition, and defined discovery paths; see F8 section. F29: the incident_id self-hash removed — recovery_id is DERIVED (sha256 of canonical bytes, like manifest_id), signed as `snh-recovery-sig/1:<recovery_id>`. F32: consumer bootstrap rules (chaining = rollback protection for stateful consumers only; full-chain fetch; cross-channel fail-closed); recovery lives on a protected `recovery` BRANCH, not refs/meta. F33: all detached signatures use the `snh-sig/1` envelope. F36: verification split by trust object — recovery signatures verify ONLY against the out-of-band pinned root fingerprint (never through a key-manifest, which recovery itself replaces); operational validity windows are chain-position facts (`effective_after` boundaries), never manifest-declared dates. F37: w3id is a discovery route, not a copy; bootstrap accepts the longest non-conflicting valid chain, tolerating lagging archives with valid prefixes. F40: key ROLES (release vs governance) — withdrawal authority separated from the unattended-CI release key. F41: `snh-key-manifest/1` append-only history specified (derived id, seq + prev chain, effective_after boundary, roles, cumulative revocations, root-only signature, trust-branch publication, same-boundary/gap = fail-closed root incident). F43: windows are chain-ancestry (bᵢ, bᵢ₊₁], boundaries strict descendants validly signed under the predecessor. F44: role sets disjoint, key_ids globally unique; governance signers bound to the successor manifest's window. F46: `root.pub` on the trust branch, accepted only against the pinned fingerprint; Ed25519 encodings fixed. F47: the protected branch is named `trust`.** **Round 9 (O5a/F48/F49): the key-manifest/recovery/trust-branch protocol is DEMOTED to a non-frozen contingency appendix — v1 trust = two directly pinned disjoint keys (online release, offline governance) stated in the published promise; on compromise, publication HALTS (archived releases remain valid; out-of-band notice; next epoch designed deliberately). Activation trigger: BEFORE a second release key, a cryptographic-continuity promise, or an authenticated-freshness consumer — never after an incident begins. F50 decomposition: hashes = content identity; SWH/Zenodo = availability; pinned release key = authorship; pinned governance key = withdrawal authority; freshness deferred. Round 10: F54 — a minimal trust anchor (key bytes + fingerprints, independent immutable channel, pinned verifier config) precedes the first signed release; F55 — compromise semantics: chain freezes at the last uncontested checkpoint, post-compromise signatures contested until an out-of-band cutoff notice, byte identity never conflated with authorship, governance-key loss halts governance operations.** | review-adopted 2026-08-24; amended rounds 4–10 | contingency: activate per O5a trigger; upgradeable to Tessera log |
| D20 | **Ownership** (F6): TEI profile schemas consumed as explicit flake input from abc during migration (no schema copies). **Amended F56 (round 11): the publication-schema/manifest-identity transfer has NO Slice-1 consumer after F52 — ABC keeps that ownership during Slice 1; soranoha/ owns only kernel/CAS/trace-store/copied renderers; the AGENTS.md transfer moves to the first Slice-2 work that assembles manifests. Slice 1 waits on nothing administrative.** | review-adopted 2026-08-24; amended round 11 | owner action: AGENTS.md edit, before Slice 2 |
| D21 | **Rights/registry admission restored** (F14): the fail-closed value-plus-hash rights authority (policy hash recorded in manifest `admission`); per-work inclusion governed by named rule (O1, owner); admission responsibility consumed from abc or transferred in AGENTS.md before Slice 3. Currently `publication-policy.edn` BLOCKS release. **F60 (round 12): admission lives ENTIRELY in soranoha.za — `load-rights-authority!`, inclusion rules, and the fail-closed block are RELEASE-ASSEMBLER concerns, never kernel concerns. The kernel builds any selected inputs, policy-blind (F52); Slice 2 uses fixture admission inputs; Slice 3 connects the production authority. The earlier "kernel ports/inherits" wording was a leak that could have reintroduced the build/publication coupling F52 removed.** **F17: admission is assessment-based per O1 (adopted reviewer rule); catalog flags seed, never authorize; private archiving needs its own authorization policy.** **F24 (as amended by F61/F71 — intent_id no longer exists): the assessment data itself is cryptographically committed — bound in `admission` and part of the F67 build projection, so newly completed assessments change the manifest and are never no-op'd as duplicates. F30: the commitment is two retained public artifacts, separating domain roles — the assessment SNAPSHOT commits facts (public-domain / in-copyright / undetermined / not-evaluated per contribution, F35/F47); the admission REPORT records the inclusion rule's total partition (admitted/excluded/quarantined with reasons). Both published, permanently rooted, hash-resolvable. F38: works = admitted − withdrawn.** | review rounds 2–8 | policy content owner-governed; mechanism fixed |

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
**[intent_id DELETED by F61 (round 12) — the F18/F24/F34 amendment
chain (rounds 3–6) built an ever-larger operation-identity coordinate
as COMPENSATION for the retry algorithm. The replacement uses existing
facts:]**
**Retry protocol (F61; transaction made explicit per F66, round 13 —
the round-12 text conflated the GIT BRANCH HEAD (a commit id, the
fast-forward CAS token) with the MANIFEST HEAD (a manifest_id, the
`prev_manifest` value); assigning the former to the latter produces an
invalid chain. The current manifest head lives at the fixed path
`releases/HEAD` inside the publication commit):**
1. Fetch Git commit C (the current branch head).
2. Read manifest head H = C:`releases/HEAD`.
3. Assemble manifest M with `prev_manifest` = H.
4. Create commit C′ containing M's blobs, `releases/<manifest_id>.json`
   + `.sig`, and `releases/HEAD` = M's manifest_id.
5. Push with C as the EXPECTED Git ref value (fast-forward CAS).
6. After an UNKNOWN push result, succeed ONLY if M is on the current
   ACCEPTED MANIFEST CHAIN (walk `prev_manifest` from the current
   `releases/HEAD`) — not merely somewhere in Git history.
7. On rejection: **operation-specific reconciliation (F73, round 14 —
   blind "re-chain against H₂" is not valid for every operation, since
   rebasing changes more than `prev_manifest`):**
   - Fetch AND FULLY VERIFY the new accepted head (chain, signatures,
     invariants).
   - If the intended state is ALREADY SATISFIED (a build whose F67
     projection matches the new head; a withdrawal whose slugs are
     already withdrawn under the same event), SUCCEED without
     publishing.
   - For a BUILD losing to a governance change: REASSEMBLE against the
     new governance state — inherit the new `withdrawn`, recompute
     `works` and `validation_summary`, then retry.
   - For a GOVERNANCE event losing to a build or a disjoint governance
     change: revalidate the UNCHANGED signed event against the new
     predecessor's transition invariants, re-chain, retry.
   - If affected slugs CONFLICT (same-slug withdrawal already applied
     differently) or an amendment's `amends` is now STALE (an
     intervening correction changed the slug's governing event): HALT
     for fresh offline governance authorization. NEVER rewrite or
     automatically re-sign a governance event.
   Slice-2 test matrix: build/build, build/governance, disjoint
   withdrawal/withdrawal, same-slug withdrawal race, competing
   amendments.

**`releases/HEAD` protocol (F74, round 14 — F66 made it load-bearing
without specifying it):**
- ALWAYS present, including in the publication branch's INITIAL commit.
- Exact bytes: 64 lowercase ASCII hex characters + one LF (in F69's
  conformance vectors).
- Before genesis: 64 zeros. After each publication: the current raw
  manifest_id.
- A verifier requires the referenced manifest + signature to exist,
  verifies the ENTIRE `prev_manifest` chain down to the zero genesis,
  and REJECTS a pointer that does not match a valid chain head. No
  special missing-file genesis branch exists.
**Scheduled-build no-op (F67 — the round-12 projection omitted
upstream_origin and selection_params, so the same commit hash from a
different origin, or a selection-only change, would be wrongly
suppressed):** the build projection is EXACTLY
`{corpus, toolchain, selection_params, admission}` (corpus =
{upstream_origin, upstream_rev}); a fresh scheduled build NO-OPs iff
this projection equals the current head's. Governance state is
inherited from the head and deliberately EXCLUDED from the projection.
CAS construction is repeatable; the only externally meaningful effect is
the fast-forward push, whose result is queryable. **Nondeterministic
output under identical coordinates HALTS as a determinism defect** — it
is never normalized into an "intent". Withdrawals and amendments change
manifest CONTENT, so their manifest_ids are already distinct; no wire
identity is needed. A publisher-local request token may be introduced
later if operational evidence requires one — publisher state, never wire
format. The traded cost, accepted: precise lost-ack/reachability,
origin-only-change, and selection-only-change integration tests in
Slice 2/3.

### F11 → D18.1: Archival receipts are separate signed attestations
**[SUPERSEDED by F63 (round 12): after F59 narrowed its meaning, the
signed receipt proved only that the release key REPORTED a successful
SWH check — it authenticated neither the archive nor the compromise
cutoff, and no longer earned a wire protocol. `snh-archive-receipt/1`
is DELETED. **archive-verified is a REPRODUCIBLE PREDICATE**: SWH full
visit + expected publication commit + all referenced blobs present +
byte hashes equal. Its latest result is stored as a disposable verifier
report or Forgejo status — unsigned, not content-addressed, not frozen;
SWH remains the authority and citation pages recompute or refresh it.
What survives from this section: receipts/status must NEVER live inside
the release manifest (archival timing must not perturb release
identity), and a later release is never required for an earlier one to
become verifiable.]**
Historical shape (pre-F63): immutable signed record keyed by manifest_id
(`receipts/<manifest_id>.<venue>.json` + sig) containing venue,
venue-side identifiers, verification method, and result. Derived states: **published** (on accepted chain) →
**archive-verified** (complete independent-copy receipt exists);
**citation-eligible** is a policy *projection* of archive-verified, not a
stored state (no owner-controlled transition exists to justify one).

### F12 → D18.1: SWH completeness/latency are unproved — probe required
SWH docs: large repos fail more often; >100 MB objects not archived; visits
may be full/partial/failed (docs.softwareheritage.org/user/using_data/;
save_code_now API). Current repo origin is SSH on a .ts.net host — **no
publicly reachable archival origin exists yet** (owner decision O2 below).
Predicate acceptance for SWH ("receipt" retired per F63/F71) must
prove, on the real public repository:
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

**[EXTRACTED per round 14 (F84): `docs/design/snh-protocol-v1.md` is
the SOLE NORMATIVE source. Body REPLACED per F88 (round 16): this
section previously carried a full, exact-looking copy of the wire
protocol that had drifted from the extracted spec — including
governance fields removed by F82 — under a heading still reading
"standalone normative specification". Git history preserves every
superseded revision; what remains here is the decision trail.]**

Decision trail (normative text: the spec section cited):
- **Canonical form, manifest field table, type registry** — spec
  §1–§3. Deletions on the way: `intent_id` (F61 — the retry protocol
  uses existing facts), `snapshot_date` (F72 — no dates in manifest
  bytes), the standalone tombstone artifact (F62 — one fact, one
  representation), `since` (F19 — unsolvable self-reference), the
  private archive index (F65).
- **Governance event `{schema, kind, entries}`** — spec §4. Origin
  F40; F62 collapsed event+tombstone; F82 removed
  `authority`/`evidence_hash`; F68 froze the linear amendment binding;
  O3 (a: immutable / b: event-amendment) still pending — facilitator
  and reviewer recommend (b).
- **Admission evidence pair** — spec §5. F24/F30 facts-vs-decisions
  separation; F35 `not-evaluated` ≠ `undetermined`; F38 `works` =
  admitted − withdrawn; F81→F87 totality as an assembler invariant.
- **Raw signature/key/HEAD encodings** — spec §6. F33 origin; F64
  deleted the `snh-sig/1` envelope; F69 exact byte contracts;
  `key_id` retired.
- **Two directly pinned disjoint keys** — spec §7. O5a; F44 role
  disjointness; F46 bootstrap bytes; F54 pinning sequence.
  Key-manifest/rotation/recovery designs live in the contingency
  appendix, non-frozen.
- **Verifier invariants and chain rules** — spec §8. F34/F39/F68
  governance transitions; F78 append-onlyness; F85 single-parent
  linearity + explicit genesis; F89 report-field binding.
- **Publication transaction** — spec §9. D17.1/F10 CAS boundary; F66
  git-head/manifest-head separation; F73/F79 superseded by F86
  current-state reconciliation.
- **Naming, compromise semantics, checkpoints** — spec §10.
  F49/F55/F59/F70 compromise semantics; F75 pre-release discovery
  channel; F76 dated name superseded by F83 dateless canonical id
  (owner ratification pending).

### F14 → D21: Rights/registry admission restored to the boundary
abc's fail-closed rights authority exists and currently blocks release
(`abc/data/publication-policy.edn` = `:blocked-pending-assessment-migration`;
only `:assessment-required` authorizes — `abc/src/abc/tools/
publication_policy.clj`). The catalog carries per-work copyright flags
(作品著作権フラグ; `aozora_csv.clj:258`) — Aozora includes in-copyright
works under rights-holder conditions. Resolution **(as amended by F60,
round 12 — admission is a soranoha.za RELEASE-ASSEMBLER concern, never
a kernel concern)**: (a) `soranoha.za` ports the value-plus-hash
rights-authority pattern (`load-rights-authority!`) — release-level
admission is a fail-closed input to MANIFEST ASSEMBLY, its policy hash
recorded in the manifest `admission` field; the kernel (Slice 1) builds
any selected inputs, policy-blind; (b) per-work inclusion is governed by
a named inclusion rule (owner decision O1), evaluated at assembly;
(c) Slice 2 uses fixture admission inputs; before Slice 3 the admission
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
- **O3 — withdrawal-record mutability (F26/F31; recast by the F62
  collapse, round 12 — "tombstone" no longer exists; the question now
  concerns a slug's GOVERNING EVENT id in `withdrawn`).** Settled either
  way in v1: withdrawn slugs are monotonic forever, silent entry drop
  never means reinstatement, and reinstatement (corrected assessment,
  restored permission) requires an explicit event in a future wire
  version. The OPEN choice, owner's call before freeze:
  - **(a) Fully immutable** — the `{slug, event}` pair is carried
    verbatim forever; no "event-amendment" kind exists; correcting a
    mistaken reason_code or statement requires wire v2.
  - **(b) Amendable-but-permanent (facilitator's AND reviewer's
    recommendation)** — a slug's governing event id may change only via
    an event-amendment whose entry's `amends` names the superseded event
    (audited correction chain, verifier-enforced); withdrawal itself
    stays permanent. Recommended because takedown paperwork and rights
    findings DO get corrected, and a wire-version bump for a typo'd
    reason_code is disproportionate ("option (a) makes an ordinary typo
    a wire-version event" — round 6). Amendments produce distinct
    manifests by content (F61), so no operation-identity machinery is
    needed. Owner ratification remains the freeze gate.
- **O5a — v1 trust boundary (PROPOSED, reviewer-drafted round 9;
  supersedes O5, whose freeze-all-formats clause F48 rejected as
  contradicting the ratchet; amended round 10 per F52–F55).**
  (1) STAGED freezes; the durable rule is **freeze at first consumer**
  (F53/F58; pruned round 12 per F61–F65). **Normative v1 is FOUR
  formats, all frozen BEFORE SLICE 2: `snh-manifest/1`,
  `snh-assessment-snapshot/1` (minimal content schema, exercised on the
  fixture), `snh-admission-report/1`, `snh-governance-event/1`.**
  Deleted round 12: intent_id (F61 — retained-manifest retry protocol),
  `snh-tombstone/1` + `release_intent` (F62 — collapsed into the
  governance event), `snh-archive-receipt/1` (F63 — reproducible
  predicate + disposable report), `snh-sig/1` (F64 — raw detached
  Ed25519, key selected by object kind), the private archive index
  (F65 — CAS/trace state only). No ADDITIONAL format freezes exist
  between Slice 2 and Slice 3 (F84 drift fix — all four formats freeze
  before Slice 2; nothing further ever freezes for Slice 3);
  full-corpus assessment MIGRATION (data, not schema) stays a Slice-3
  prerequisite. **Round 13: the honest inventory is "four canonical
  JSON object schemas PLUS fixed raw signature and key encodings" (F69
  — encodings are wire contracts too; conformance vectors before
  Slice 2); snapshot_date deleted from manifest identity (F72); Zenodo
  checkpoint deposits require F70 credential separation (unavailable to
  release CI) — a blocker before the first signed release, not before
  Slice 2.** (2) Two directly pinned disjoint keys: online CI
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

## External design review round 12 (2026-08-25) — findings F60–F65

Reviewer verdicts: round 11 accurately applied; Slices 0–1 genuinely
ready; F60 is a blocker to the dev handoff (not to Slice-1
implementation); F61–F65 strong suggestions before freeze — all
adopted. Result: **normative v1 shrinks from seven wire formats to
FOUR** (manifest, assessment-snapshot, admission-report,
governance-event); validation_summary retained (include-and-flag has a
plausible user-facing consumer). None of this delays kernel work.

- **F60 (rights admission leaked into the kernel — dev-handoff
  blocker)** — D21 still said "the kernel ports" the rights authority
  and "inherits" the publication block, contradicting F52's policy-blind
  kernel; a Slice-1 implementer could have reasonably reintroduced the
  exact build/publication coupling F52 removed. Fix applied:
  `load-rights-authority!`, inclusion rules, and the fail-closed block
  live entirely in soranoha.za (kernel: build any selected inputs;
  release assembler: decide what enters a public manifest; Slice 2:
  fixture admission inputs; Slice 3: production authority).
- **F61 (intent_id was compensation for the retry algorithm — deleted)**
  — the F18→F24→F34 coordinate kept growing because re-chaining changes
  manifest_id. The replacement uses existing facts: retain the assembled
  manifest while pushing; on an unknown push result check reachability
  on the authoritative ref; re-chain only if absent; no-op a scheduled
  build when desired state matches the head. Nondeterminism under
  identical coordinates HALTS as a determinism defect. Withdrawals and
  amendments change content, so their manifest_ids are already distinct.
  Traded cost accepted: a precise lost-ack/reachability integration
  test.
- **F62 (governance event + tombstone were one fact twice — collapsed)**
  — two hashes, two schemas, two resolution paths, and cross-object
  equality rules for one withdrawal decision. Now: the signed event
  carries `{slug, reason_code, statement, amends?}` entries directly;
  `withdrawn` maps slug → governing event id; the event's `kind`
  distinguishes withdrawal from correction; the manifest carries only
  `governance_event: null | id`. Deletes `snh-tombstone/1` and all of
  `release_intent`. The offline governance signature still authorizes
  the exact public result (F45's principle survives); O3(b) becomes the
  event-amendment rule; withdrawal permanence unchanged.
- **F63 (the receipt no longer earned a wire protocol — deleted)** —
  after F59 the signed receipt proved only that the release key reported
  a successful SWH check. archive-verified is now a reproducible
  predicate (SWH full visit + expected commit + blobs present + hashes
  equal) stored as a disposable verifier report/Forgejo status; SWH is
  the authority; citation pages recompute. Zenodo checkpoints deposit
  the ACTUAL canonical manifest + signature (or repo snapshot), never a
  record that merely names a head.
- **F64 (three redundant envelope fields — envelope deleted)** — with
  one pinned key per role and one algorithm, key_id, algorithm, and the
  copied signed_context were all determined by object type and pinned
  configuration. A `.sig` is now the raw 64-byte Ed25519 signature over
  the domain-separated message; the verifier selects the key by object
  kind. A versioned envelope returns only at O5a's second-key/rotation
  trigger.
- **F65 (private archive index = the F57 shadow-manifest problem —
  deleted)** — no authorized private-archive consumer or settled access
  policy exists, so a schema'd index fails the current-consumer ratchet.
  Private parser artifacts stay in CAS/trace state; an export is derived
  only when an authorized archival destination exists.
- **Reconciliation (round 12):** D16's "frozen before slice 1" headline
  corrected to "before Slice 2"; the unknowns list no longer schedules
  the manifest JSON Schema before Slice 1; Slice 3 lists no pre-Slice-3
  format freezes.

## External design review round 13 (2026-08-25) — findings F66–F72

Reviewer verdicts: round 12's deletions directionally sound but not
freeze-ready; Slices 0–1 unaffected and ready; O3(b)'s product intent
reasonable; O5a and the freeze wait for F66–F69 (now applied). The
reviewer also corrected the facilitator's "three rounds of net
deletion" claim: rounds 10–12 were net ADDITIONS by line count
(+108/+94/+43); the meaningful claim — normative MECHANISMS shrank — is
the one recorded.

- **F66 (git head vs manifest head — Blocker)** — the round-12 retry
  said `prev_manifest := head`, conflating the git branch head (a
  COMMIT id, the fast-forward CAS token) with the manifest head (a
  MANIFEST id). Assigning one to the other produces an invalid chain.
  Fix applied: explicit seven-step transaction with `releases/HEAD` as
  the manifest-head pointer inside the publication commit;
  unknown-result success requires M on the ACCEPTED manifest chain, not
  merely somewhere in git history.
- **F67 (no-op projection omitted coordinates — Blocker)** — comparing
  only upstream_rev/toolchain/admission would wrongly suppress the same
  commit hash from a DIFFERENT ORIGIN, or a selection-only change. Fix
  applied: the projection is exactly {corpus, toolchain,
  selection_params, admission}; governance state inherited from the
  head and deliberately excluded; origin-only and selection-only
  fixtures added to Slice 2.
- **F68 (amendment binding underspecified — Blocker)** — "each amends
  chain valid" never required the changed entry to point at the CURRENT
  manifest's event. Fix applied, frozen invariants: entries non-empty
  with unique slugs; withdrawal entries have no `amends`, amendment
  entries all do; for each amended slug `amends ==
  predecessor.withdrawn[slug].event` (linear — no skipping or
  overwriting an intervening correction); the superseded event has
  exactly one entry for that slug; every added/changed
  `withdrawn[slug].event` equals this manifest's `governance_event`.
- **F69 (raw signatures are still a wire format — Blocker before
  freeze)** — deleting the envelope removed a JSON schema, not the
  signature protocol; and the encodings were incomplete/contradictory
  (`.pub` called both raw and hex). Fix applied, exact bytes: message =
  exact ASCII bytes of the domain string, no trailing newline; `.sig` =
  exactly 64 raw bytes; `.pub` = 64 lowercase hex chars + one LF;
  fingerprint = sha256 over the DECODED 32 raw key bytes; `key_id`
  retired from normative v1 (no consumer). Conformance vectors before
  Slice 2. Honest inventory adopted: four canonical JSON object schemas
  PLUS fixed raw signature and key encodings.
- **F70 (checkpoint control-plane independence — Blocker before the
  first signed release)** — a separate domain is not automatically
  independent: if release CI holds the Zenodo credential, CI + key
  compromise publishes a malicious release AND its matching
  "independent" checkpoint. Fix applied: deposits use credentials
  unavailable to release CI (manual owner action or separately
  controlled MFA workflow); verifiers obtain the initial DOI +
  fingerprints out of band from the published paper/docs.
- **F71 (reconciliation — Blocker to the authoritative handoff,
  mechanical)** — five stale contracts fixed: D21's intent_id
  reference; D18's normative receipts (lifecycle stored state is now
  `published` only; archive-verified is an OBSERVED predicate; citation
  eligibility computed from a successful archival observation); F12's
  "Receipt acceptance" heading; Slice 2's "six formats"; Slice 4's
  "names the head".
- **F72 (snapshot_date — reviewer's further-simplification suggestion,
  adopted)** — a push retried across midnight either carried an
  inaccurate date or changed identity for a non-content reason. Deleted
  from the manifest; the publication date derives from the accepted
  publication commit (and the Zenodo record); no dates remain in
  manifest bytes; content state and publication time are fully
  separated.

## External design review round 14 (2026-08-25) — findings F73–F77 + extraction

Reviewer verdicts: round 13's F66–F72 fixes correct; boundary
validation still failing on retry transitions, HEAD bootstrap, and the
initial trust anchor — "not in the four JSON schemas themselves";
Slices 0–1 ready; O3(b) reasonable; O5a and the Slice-2 freeze wait for
F73/F74 + handoff reconciliation. All applied, plus the requested
extraction of a clean protocol spec.

- **F73 (blind re-chain invalid — Blocker before Slice 2)** — rebasing
  changes more than `prev_manifest`: a build losing to a withdrawal
  must inherit the new withdrawn state and recompute
  works/validation_summary; a duplicate build should no-op; a same-slug
  withdrawal race is no longer applicable; a losing amendment has a
  STALE `amends` that cannot be repaired without fresh offline
  authorization. Fix applied: step 7 is operation-specific
  reconciliation (verify new head fully; already-satisfied → succeed;
  build → reassemble; governance → revalidate the unchanged signed
  event; conflict/stale → HALT — never rewrite or auto-re-sign an
  event). Slice-2 matrix: build/build, build/governance, disjoint
  withdrawals, same-slug race, competing amendments.
- **F74 (releases/HEAD unspecified — Blocker before Slice 2)** — F66
  made it load-bearing without an encoding or genesis rule. Fix
  applied: always present (including the initial commit), 64 lowercase
  hex + LF, 64 zeros before genesis, current raw manifest_id after;
  verifier requires the referenced manifest + signature, walks the full
  chain to zero, and rejects a non-head pointer; included in the F69
  vectors. No missing-file genesis special case.
- **F75 (discovery channel circular — F70 stays blocked)** — "the
  published paper/docs" postdates the first signed release. Fix
  applied: the owner names a concrete already-trusted PRE-RELEASE
  channel (institutional page / ORCID-linked record / distributed
  verifier config) carrying the concept DOI + both fingerprints before
  publication. Credential separation protects checkpoint CREATION;
  trusted discovery protects checkpoint SELECTION; both required.
- **F76 (commit date ambiguous — Strong suggestion)** — resolved as the
  UTC calendar date of the COMMITTER timestamp on the unique
  HEAD-advancing commit. The reviewer's fuller simplification
  (`r<manifesthash12>`, dates as separate display metadata) is recorded
  as an owner option before first release; the dated form is kept
  because D3/D13's owner intent was date-inferable identifiers. Zenodo
  deposit dates stay citation metadata only.
- **F77 (contradicting decisions — Blocker to handoff, mechanical)** —
  D11's "archive privately" amended to "retain operationally in
  CAS/trace; private archival export deferred pending authorization"
  (F65); the registry's "archived is a lifecycle state" line corrected
  (published is the only stored state); Slice 3's "reaches
  archive-verified" reworded to "archival predicate succeeds".
- **Extraction (adopted)** — the ledger passed 2,000 lines with deleted
  fields and amendment banners inside the "standalone" spec. The live
  protocol now lives in **`docs/design/snh-protocol-v1.md`**: four
  schemas, raw signature/key/`releases/HEAD` encodings, invariants,
  publication reconciliation, and the conformance-vector list —
  authoritative for builders and verifiers at the freeze; this ledger
  stays the decision and rationale record.

## External design review round 15 (2026-08-25) — findings F78–F84

Reviewer verdicts: extraction is a real simplification; Slices 0–1
ready; O3(b) and O5a's boundary sound; F75 correctly gated; F77
reconciled; Slice-2 freeze waits on F78–F81 — all four now applied,
plus both simplifications and the cleanup.

- **F78 (append-onlyness — Blocker)** — walking HEAD→zero verifies a
  chain but not THE chain: a fast-forward commit could point
  `releases/HEAD` at a fresh manifest with `prev_manifest` = 0,
  silently replacing the logical chain while preserving Git ancestry.
  Fix: every first-parent HEAD transition H → M must satisfy
  `M.prev_manifest == H`; only the initial commit may hold the zero
  HEAD. Also closes the genesis invariant and makes "the unique
  HEAD-advancing commit" demonstrable.
- **F79 (build/build undefined — Blocker)** — "already satisfied" could
  have blessed a nondeterministic twin (same projection, different
  bytes), contradicting the determinism halt; different-coordinate
  losers could regress upstream state if blindly published. Fix: same
  projection + same derived state → success; same projection +
  different state → determinism failure; different projections →
  halt/requeue from the new head; only build-vs-governance reassembles
  automatically. Unknown-result-with-M-absent now explicitly follows
  the rejection path.
- **F80 (prose shapes aren't freezeable schemas — Blocker)** —
  ambiguous character-class repetition, undefined report reason_code
  domain, untyped report snapshot reference, implicit type/hash checks.
  Fixes applied in the spec (anchored regexes; report reason_code
  syntax fixed with semantics owned by the content-addressed rule;
  typed artifact id in the report; explicit type-prefix and
  hash-recomputation invariants) — and the ORDER reversed: the
  executable JSON Schemas + vectors are the objects the freeze review
  approves, authored before it.
- **F81 (totality not independently checkable — Blocker)** — snapshot
  and report could omit the same work undetected; the verifier only
  proved a partition of whatever the snapshot contained. Fix: the
  snapshot's candidate set must equal the selection recomputed from
  `corpus` + `selection_params` under the bound rule — an invariant,
  not a wire field.
- **F82 (unconsumed governance metadata — adopted)** — `authority`
  duplicated the pinned key's role; `evidence_hash` had no frozen
  serialization, retention policy, or verifier consumer, and publicly
  committed potentially sensitive material. v1 event = {schema, kind,
  entries} + signature; evidence stays operational.
- **F83 (dateless naming — adopted in the spec, OWNER ratification
  pending)** — the round-14 dated name depended on an UNSIGNED
  committer timestamp: the same signed manifest could acquire a
  different name when repackaged. Canonical identity = the full typed
  manifest id; display alias `r<hash12>`; dates purely
  presentation/citation metadata. This amends owner-ratified D13 and
  reverses the D3 date-inferable preference — recorded for the owner's
  explicit call.
- **F84 (normative-now — adopted)** — authority transferring only at
  freeze left duplicated normative prose that had already drifted
  ("four formats freeze before Slice 2" vs "NOTHING freezes before
  Slice 3"). The protocol spec is the sole normative source
  immediately; the freeze changes stability, not precedence; the
  ledger's D16.1 section is explicitly non-normative rationale; the
  drifted phrasing corrected to "no additional freezes between Slice 2
  and Slice 3".

## External design review round 16 (2026-08-25) — findings F85–F89

Reviewer verdicts: round 15 substantially cleaner but F78/F79/F81 not
fully closed; Slices 0–1 remain ready; the protocol should not freeze
yet. Reviewer RECOMMENDS ratifying F83 (the signed typed manifest id is
the authentic identity; dates stay citation metadata) and moving the
optional `r<hash12>` display convention out of the protocol — both
applied/recorded.

- **F85 (merge commits bypass F78 — Blocker)** — F78 checked only
  FIRST-PARENT transitions: a fast-forwarded MERGE places the old
  authoritative head on its second parent while its first-parent
  history carries a replacement chain from zero — every round-15 check
  passes, yet history is replaced. Fix: publication commits have
  exactly ONE parent (the fetched expected commit); merges on the
  publication branch are invalid and rejected. Genesis also made
  explicit (`prev_manifest` = 0, `governance_event` = null,
  `withdrawn` = []) — the ordinary-build predecessor rule was
  undefined at genesis.
- **F86 (reconciliation assumed ONE intervening operation — Blocker)**
  — the round-15 pairwise cases overlap once several publications land
  before the loser refetches (identical build wins, THEN a withdrawal
  lands: same projection + different state matches both "determinism
  failure" and "build vs governance"), and "derived state" was
  undefined (it cannot mean manifest bytes — `prev_manifest` changes).
  Fix: CURRENT-STATE reconciliation. Build: recompute the desired
  projection and expected derived content under the current head's
  `withdrawn`; equal → success; equal coordinates with unexplained
  content differences → determinism failure; otherwise requeue.
  Governance: exact event already applied → success; else validate the
  unchanged event against the current head and append, or halt on
  conflict. Handles any number/ordering of intervening commits;
  build-vs-governance reassembly is subsumed by requeue; the pairwise
  race taxonomy leaves production logic.
- **F87 (F81 circular and non-executable — Blocker)** — candidate
  selection was said to run "under the inclusion rule's selection
  semantics", but the rule CONSUMES the snapshot's assessment facts —
  selection must precede the snapshot; and a hash binds rule bytes
  without resolving them ("the vocabulary travels with its hash" is
  false without a resolver). Resolution (facilitator's choice, per the
  F51 ratchet — public recomputation has no current consumer and would
  require new resolver machinery): the SIMPLER v1 — candidate-set
  equality is an ASSEMBLER invariant against the transactionally
  consistent selection; no public-recomputation claim. The stronger
  option (selection defined independently of rights inclusion +
  hash-resolvable selector/policy/rule bytes) is recorded as the
  upgrade path.
- **F88 (finish the authority cleanup — adopted)** — the spec claimed
  sole authority while F80 called future schemas+vectors "the frozen
  objects". Split defined: JSON Schemas govern STRUCTURE; the protocol
  governs SEMANTIC/STATE invariants; vectors demonstrate both;
  disagreement blocks freezing. This ledger's D16.1 body — hundreds of
  stale, exact-looking protocol lines including F82-removed governance
  fields, under a "standalone normative specification" heading — is
  replaced by a decision trail with spec links; Git history preserves
  the archaeology (−327 lines net).
- **F89 (report-field mismatch — mechanical)** — the verifier required
  the report's "snapshot/policy/rule ids and hashes" to match
  `admission`, but the report has no `policy_id`. Invariant corrected
  to the fields actually present.

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
authenticated freshness — never after an incident begins.** The same
trigger reintroduces a versioned signature envelope (the round-5
`snh-sig/1` design, deleted by F64 — raw signatures suffice while
exactly one pinned key exists per role). Until then, v1's posture is
F49/F55: halt on compromise; the chain freezes at the last independent
Zenodo checkpoint; archives carry the permanence promise.

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
validation; the FOUR pre-Slice-2 formats frozen (incl. the minimal
assessment-snapshot schema, F58) plus the F69 signature/key encoding
conformance vectors; the D20-as-amended AGENTS.md manifest-ownership
update (F56).
**Manifest assembly enters HERE (F52), on a FULLY ASSESSED FIXTURE
corpus** — not the unassessed full corpus: `snh-manifest/1` emission per
the frozen D16.1 spec, with real admission evidence
(snapshot/report) for every fixture work. Acceptance inherited from
Slice 1's former criterion 5: manifest round-trip — canonical bytes →
manifest_id stable across re-serialization; validates against the strict
schema. (intent_id no longer exists — F61.)
Chain mechanics on that manifest: prev-manifest hash chain, the D17
compare-and-append publication transaction, D18 GC roots + lifecycle
states. Build at a SECOND (newer) upstream revision; acceptance = the F5
three-set oracle: (a) source/selection delta, (b) stages
invalidated/executed, (c) artifact-byte/manifest delta, with invariants
(artifact change ⇔ byte change; unchanged bytes retain ids; every executed
stage explained by a changed declared input). Fixture tests: addition,
deletion, withdrawal (R8 path — must exercise the F26 chain invariants:
monotonic withdrawn map, disjoint slug sets, governing event ids
immutable except via valid O3(b) event-amendments — F47/F62),
output-preserving source edit, the R7 include-and-flag path with an
invalid work, an assessment-only delta (same upstream/toolchain, enlarged
assessment snapshot → changed admission evidence, distinct manifest_id,
scheduled-build comparison does NOT no-op — F24's scenario re-tested
under the F61 protocol), the F61 lost-ack test (unknown push result →
reachability check → exactly-once publication), and the governance
operations: a withdrawal with NO upstream change and — if O3(b) is
ratified — an event-amendment (amends chain verified; withdrawn set
unchanged); each yields a distinct manifest_id via changed content.

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
first pin), loaded as pinned verifier configuration** — the deposit
carries the actual genesis manifest bytes + signature (F63); full-corpus
assessment DATA migrated (the schema froze at Slice 2 — F58/round-12
cleanup: no schema freezes remain before this slice); signatures are raw
detached Ed25519 per F64; the F49/F55/F59 compromise runbook documented
(chain freezes at the last independent Zenodo checkpoint;
post-checkpoint signatures contested until an out-of-band cutoff notice;
artifact ids always identify exact bytes). No key-manifest, trust
branch, or recovery tooling in this slice (O5a — contingency appendix
only). Additional acceptance: a governance-withdrawal fixture executed
end-to-end (event signed by the governance key; F39/F62 transition
invariants verified by the published checker).
Publication = the D17.1 remote compare-and-append: one complete commit
(blobs + manifest + sig) fast-forward-pushed to the protected branch;
rejection → re-chain and retry per the F61 protocol (retained manifest,
reachability check on unknown results, scheduled-build no-op).
Forgejo auto-release polls upstream; admission is a fail-closed input
(policy hash in manifest). Serving tree (blobs/, releases/, history.json)
derives from the repo. Archival: SWH save-code-now per release,
non-blocking; **archive-verified is the F63 reproducible predicate (SWH
full visit + expected commit + all referenced blobs present + hashes
equal), stored as a disposable verifier report/Forgejo status — no
receipt artifact**. Acceptance: (1) two consecutive automated releases
from real upstream movement, chain verified end-to-end by the published
checker; (2) a forced concurrent-publish attempt loses the push race and
correctly re-chains, and a simulated lost push response resolves via the
F61 reachability check without double publication; (3) at least one
release has the archival predicate SUCCEED (F77 wording — no
"archive-verified" state is reached or stored) with all four F12
checks, with measured (not assumed) archival latency recorded in the
ledger.

### Slice 4 — citability layer
Quarterly Zenodo snapshot (concept DOI + first version DOI) — **each
deposit CONTAINS the then-current canonical manifest bytes + signature
(F63/F71 — never a record that merely names a head), doubling as an
independent authorship checkpoint (F59) under F70 credential separation
(deposit credentials unavailable to release CI)**; w3id.org/soranoha
registration; published
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
- Executable JSON Schemas (all four formats) + conformance vectors —
  dev, **BEFORE the freeze review (F80 reversed the round-12 order: the
  schemas and vectors ARE the objects the freeze approves; prose alone
  encodes interpretation for later)**. Before Slice 2.
- `snh-assessment-snapshot/1` MINIMAL content schema (per-contribution
  facts per the 2026-07-11 model) — dev with owner, **frozen before
  Slice 2 and exercised on the assessed fixture (F58)**. Its identity,
  retention, resolution, and verifier semantics are already normative in
  the protocol spec §5 (F30). Full-corpus assessment DATA migration remains a Slice-3
  prerequisite — schema and migration are separate obligations.
- O3 (governing-event mutability: immutable vs event-amendment, recast
  by F62) — owner, BEFORE the D16.1 freeze (F31). Facilitator and
  reviewer both recommend (b).
- Private evidence record's handling/retention policy — owner, before
  the first real withdrawal. (Per F82 the public `evidence_hash`
  commitment is removed from v1; the record is purely operational until
  a concrete audit consumer and encoding exist.)
- Pinned-key setup (O5a/F54): generate the two disjoint Ed25519 keys
  (release online for CI; governance offline), publish
  `keys/release.pub` + `keys/governance.pub`, AND publish the minimal
  trust anchor on Zenodo (F59/F63: the deposit carries key bytes +
  fingerprints + the ACTUAL genesis manifest bytes and signature — never
  a record that merely names a head — making it both the first pin
  and the first independent authorship checkpoint) BEFORE the first
  signed release — owner, before Slice 3. **F70: the Zenodo credential
  must be UNAVAILABLE to release CI — deposits are a manual owner action
  or a separately controlled MFA workflow; only then is the checkpoint
  operationally independent.** Quarterly Zenodo deposits thereafter
  double as the subsequent independent checkpoints; the fuller
  promise/paper restates the anchor at Slice 4.
  (Replaces the round-7/8 root-ceremony and fingerprint-venue items; no
  root key or key-manifest exists in v1.)
- **F75 pre-release discovery channel** — owner names a concrete,
  already-trusted channel carrying the Zenodo concept DOI + both
  fingerprints BEFORE the first signed release (candidates: Osaka
  University researcher page, ORCID-linked record, manually distributed
  verifier config); the Slice-4 promise document restates it. The
  Slice-4 paper CANNOT be the first pin (it postdates the first
  release).
- ~~`snh-archive-receipt/1` field schema~~ DELETED round 12 (F63):
  archive-verified is a reproducible predicate with a disposable report;
  no receipt format exists.
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
