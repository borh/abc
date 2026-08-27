# Publication Rearchitecture — Design Ledger

Current state (F103/F124/F129: evergreen — history, dates, and round
numbers live in the findings sections and Git history, not here):
- **`docs/design/snh-protocol-v1.md` is the SOLE NORMATIVE source.**
  This ledger is the decision/rationale record and dev handoff;
  nothing here overrides the spec. At the pre-Slice-2 freeze review
  the FROZEN objects are the executable JSON Schemas + conformance
  vectors (F80); schemas govern structure, the spec governs
  semantic/state invariants, vectors demonstrate both; any
  disagreement blocks the freeze (F88).
- Normative v1 = FOUR canonical JSON schemas (`snh-manifest/1`,
  `snh-assessment-snapshot/1`, `snh-admission-report/1`,
  `snh-governance-event/1`) + fixed raw signature/key/`releases/HEAD`
  encodings. Naming is DATELESS (F83, owner-ratified 2026-08-25):
  citable identity = the full typed manifest id.
- Storage/trust axes (round 18): kura — disposable private build
  state (Slice-1 CAS append-only, NO GC, F99) / Forgejo — publication
  repository + operational current-tip authority / resolver + mirrors
  (Radicle optional) — replaceable byte distribution / SWH —
  preservation / Zenodo — independent authorship checkpoint.
- **Slices 0–1: DONE (accepted 2026-08-25)** — see
  `soranoha/docs/slice-1-acceptance.md`. Golden reference regenerated
  and F4-verified (two builds, 0 artifact diffs, 17,602 works); kernel
  reproduces 17,602/17,602 works byte-equal through the trace store;
  double build = identical output with 0 executed stages (**R4
  CLOSED**); verifier clean (0 determinism violations, 159,538 blobs
  fixity-checked); all F7 bounds met at the pinned config (-Xmx4g,
  concurrency 16): cold 465.5 s, peak RSS 6.22 GiB, warm no-op 6.1 s.
  The kernel is policy-blind (F52/F60).
- **Slice 2: DONE (APPROVED 2026-08-26 at 5d9f16b7).** D16.1 protocol
  freeze at 22551310 (four schemas + conformance vectors, spec
  §11); §8 verifier / §9 transaction approved at d7e80a22; manifest
  assembly + F5 delta oracle approved at 379951dc; production F5
  qualification at a second real revision (a1da0f5a00 → the pinned
  0e9ea3e586, oracle ok = true, 0 unexplained executions) through the
  hermetic `soranoha-kernel` wrapper. Implementation-review rounds
  1–9 (F157–F183) are recorded in the sections below; suites 59
  tests / 312 assertions, locally and hermetically.
- Slice 3: LOCAL IMPLEMENTATION COMPLETE (2026-08-27 at 26cc95df) —
  release driver (fail-closed rights authority, pre-execution totality,
  preflight, scheduled-runner exit contract), governance CLI
  (offline-signed events through the §9 transaction), serving-tree
  export (verified-chain-only, staged atomic install, work-facing
  symlink layer), archival-observation CLI (§10 total report over a
  sole archived view), and the F98/F104 service-withdrawal acceptance
  over the checked-in Caddy configuration; suites 71 tests / 452
  assertions, locally and hermetically. Remaining Slice-3 work splits
  two ways. OWNER INPUTS: assessment evidence as versioned data (total
  accounting over the population, explicit not-evaluated facts
  included — not completed legal assessments for every work; only
  eligible completed assessments enter the first release) PLUS the
  authorized policy change from :blocked-pending-assessment-migration
  to :assessment-required (data alone does not lift the block);
  adoption of the cited statutory answers (Q1–Q5 + old-law
  transition — DRAFTED 2026-08-27 as
  abc/docs/evidence/external/jp-term-statute-citations.md, verbatim
  statute + 文化庁 guidance with retrieval dates; adoption is the
  owner's act); key custody for BOTH keys (the offline governance
  ceremony AND generating/provisioning the online RELEASE key to CI);
  the evidence-retention policy before the first real withdrawal.
  ENGINEERING TRACKS (independent; each names only its activation
  gate): the Forgejo scheduled release job — D5's driver and exit
  contract are ready, and the fixture-scale mechanics check EXECUTED
  successfully 2026-08-27 on the instance runner (dispatch-only
  workflow release-mechanics-check.yml: hermetic kernel on the
  runner, secret-channel seed, deploy-key push to the protected
  fixture chain, first dispatch published / re-runs exactly
  already-applied, independently verified from a second host);
  UNATTENDED production activation gates on the owner inputs above,
  the real runner/secrets contract (the publisher must be a
  DEDICATED, REPOSITORY-SCOPED runner — Forgejo host runners have no
  meaningful isolation and broader pools could receive the job; the
  runner half is AUTHORED 2026-08-27 in the infra repo as
  soranoha-release-runner.nix — registration token scoped to
  bor/soranoha, static isolated user with its own state root and
  umask 077, the single label soranoha-release:host, capacity 1,
  job-facing nix = the host daemon's package — pinned by the
  soranoha-release-runner-contracts check and awaiting operator
  activation; the secrets half, release-key provisioning and the
  production workflow, remains) —
  the year-one no-op PERFORMANCE gate alone is retired (unchanged
  invocation measured at chain length 365, 2026-08-27 — F12
  results); the service unit realizing the
  serving premises + TLS — AUTHORED 2026-08-28 in the infra repo as
  soranoha-serve.nix and REWORKED same day per external review
  (blocker: serving and signing must never share a principal): the
  backend runs the CHECKED-IN Caddyfile verbatim as its OWN
  soranoha-serve user — read access granted narrowly through the
  soranoha-serve group the exporter belongs to, the runner's
  umask-077 state unreadable to the resolver — with the Caddyfile
  pinned into the Nix store by revision + content hash (serving
  POLICY is immutable system state updated by deliberate
  deployment, never by a data release), condition-gated inert on
  serve/current, creating nothing under the publisher root; TLS =
  the host Caddy terminating with a Tailscale-provisioned
  certificate on soranoha.hyakutake-barbel.ts.net, transport-only
  vhost. Export-layout contract the production workflow's export
  step must fulfil under /var/lib/soranoha-runner: serve/ (setgid
  2750, group soranoha-serve), serve/trees/<id>/ (dirs 2750, files
  0640, umask 027), and serve/current — the atomic pointer,
  ALWAYS INSTALLED LAST, the readiness token the unit watches.
  Runner status: ACTIVE on speely 2026-08-28 and its registration
  scope VERIFIED server-side (runner id 5, bound to repo
  bor/soranoha alone — repo_id 22, absent from the instance-level
  runner list). The disposable live export EXECUTED 2026-08-28 as
  the real principals on speely, then disposed (service inactive,
  path unit re-armed, serve/ removed): the exporter installed
  trees per the layout contract and the path unit started serving
  the moment `current` landed; an atomic re-point (ln -s + mv -T)
  flipped content with the same MainPID, zero restarts, visible
  through the public HTTPS path; the checked-in Caddyfile's
  semantics held through TLS (immutable header + content on an
  existing digest path, max-age=60 pointer, 404 WITHOUT immutable
  on a missing digest path); and as soranoha-serve, reading the
  runner's registration, instance dir, and workspace were each
  Permission denied while the exported tree read fine. One defect
  found and fixed: through the vhost, digest paths first answered
  an EMPTY 200 — the checked-in Caddyfile host-matches its own
  listen address, so the preserved public Host fell through to the
  backend's default site; the transport vhost now rewrites Host to
  the upstream (header_up, contract-tested), and the earlier
  probe's "200, not a defect" reading is corrected. Release key
  PROVISIONED 2026-08-28 by the owner (born where it lives): pub
  00d915b90bcb2c9bd375a9e332eff9957af55add1fdce23e83af2d312a957b27
  (pinned at soranoha/config/keys/release.pub); the seed exists
  solely as the bor/soranoha Actions secret SORANOHA_RELEASE_SEED —
  the ONLY workflow secret, single copy per the owner decision, so
  seed loss ends the chain's ability to publish (recoverable only
  via full Forgejo restore). Production chain origin
  bor/soranoha-chain CREATED with protected main (force-push and
  deletion blocked, ff pushes allowed — the fixture-raced shape);
  the runner's ssh write identity generated in its state root with
  pinned known_hosts, its deploy key registered on the chain repo,
  authentication probed. The production workflow
  scheduled-release.yml is AUTHORED dispatch-only against the
  one-pointer serving contract (owner-inputs guard fails closed by
  name; exit contract 0/3 honored; export + atomic re-point only
  after a successful outcome; seed file removed even on failure);
  the cron trigger is added only at unattended activation. First
  dispatch EXECUTED 2026-08-28 (run 2724): the job was picked up by
  the dedicated runner (uuid 25346496…, the server-side-verified
  registration), the SHA-pinned checkout landed on 1828485d, and
  the owner-inputs guard stopped the job fail-closed naming exactly
  the two absent inputs (assessment-snapshot.json,
  governance.pub) — before any secret was written.
  Remaining before the first production publish: the offline
  governance ceremony's public key (pinned as
  soranoha/config/keys/governance.pub), the owner inputs (total
  assessment snapshot at abc/data/assessment-snapshot.json and the
  unblocked publication policy), and the one-time aozora mirror
  provisioning under the publisher root;
  production acceptances (1)–(3) — acceptance (3)'s SWH observation
  gates on public exposure. The tailnet-testable F12 probes EXECUTED
  2026-08-27 (results at the end of this ledger): capacity,
  transport, and concurrency checks pass; the origin-host maintenance
  measurements (M6/M7 and origin-side loose growth) are a pending
  one-shot during the speely deployment.
  Before deployment / the first public signed release: the deployment
  prerequisites below; the F75 ORCID work (the anchor's version DOI)
  published.
- Protocol decisions: all ratified (2026-08-25); deployment choices
  follow — O1 (assessment-based
  admission with the round-4 wording), O2a (Forgejo authoritative
  origin architecture), O3(b) (amendable-but-permanent governing
  events), O5a (fixed pinned key sets per chain; governance = ONE
  software Ed25519 key held as two separately controlled custody
  copies — owner medium amendment + round-27 F143 collapse —
  degraded/halt semantics per the F144 custody inventory), F83
  (dateless naming), F75 channel = an ORCID work naming the first
  anchor deposit's Zenodo VERSION DOI (F145). Extended 2026-08-27
  (rationale in the owner decision record at the end of this ledger):
  O2b provisional = the tailnet Forgejo as PRIVATE TEST ORIGIN (public
  exposure deferred); TLS = Caddy terminating with tailnet
  certificates via the speely nix configuration; key staging =
  governance ceremony now, release key at CI provisioning; rights
  assessment authority = Soranoha, singly (Aozora flags corroborate,
  never assess; no permission-based admission lane); F12 split =
  tailnet-testable origin/storage probes now, SWH observation
  public-only. Remaining owner item: private evidence-record retention
  policy, before the first real withdrawal.
- Deployment prerequisites (operational tasks, NOT owner decisions —
  F108): O2b named (the private test origin; public exposure a later
  step); the F12 storage/origin probes against it EXECUTED except the
  origin-host maintenance one-shot (SWH probing awaits public
  exposure); the software-key ceremony (offline generation of
  the ONE governance key; two authorized custody copies on separately
  controlled encrypted media, the copy operation recorded in the
  F144 inventory) + the F54/F70 credential-separated Zenodo
  trust-anchor deposit BEFORE the first signed release.
Owner: Bor Hodošček
Process: hammock-driven-design decision interview, 2026-08-24/25

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
| D13 | Identifiers: artifact id = typed content hash `snh:1:<type>:<sha256>` (prefix per the 2026-07-03 naming spec); release identity is DATELESS — the full typed manifest id is the citable name; dates are presentation/citation metadata (F83, owner-ratified 2026-08-25; supersedes the original dated release name — history in F76/F83). Snapshot DOIs quarterly (Zenodo concept + version DOIs). | owner 2026-08-24; F83 ratified 2026-08-25 | frozen at first release |
| D14 | Slug scheme kept: `作品ID_人物ID_carddir_zipstem` as stable human-facing work name; qualifier, never identity. | owner ratified rec, 2026-08-24 | frozen after first release |
| D15 | Naming ratified per 2026-07-03 Model B spec: new top-level `soranoha/` component; namespaces `soranoha.core` (shared types/config/the one canonicalizer), `soranoha.yomi` (source-acquirer+selector), `soranoha.kura` (trace store+CAS+verifier), `soranoha.ori` (stages/renderers/validation), `soranoha.za` (release assembly+publishing). Vocabulary reused, NOT the old spec's scope (no LOD/IIIF/XTDB in v1). `snh:` id prefix per D13. | owner, 2026-08-24 | dir rename trivial pre-first-release |
| D16 | Wire formats: normative v1 = the protocol spec's FOUR canonical JSON schemas (manifest, assessment-snapshot, admission-report, governance-event) + fixed raw signature/key/`releases/HEAD` encodings; canonicalizer `rfc8785-safe-integer-json-string-v1`; closed type registry. FREEZE at the pre-Slice-2 review, whose approved objects are the executable JSON Schemas + conformance vectors (F80/F88). Full semantics: `snh-protocol-v1.md` (sole normative source). Amendment history: F3→F13 (D16.1) and rounds 3–19 (F16–F104) in the findings sections. | review rounds 2–19 | **FROZEN 2026-08-25 at commit 22551310 (D16.1 approved; freeze-review F147–F156)** — changes only via decision-log entry |
| D17 | Publication = the spec §9 transaction: single-parent commit fast-forward-pushed to the protected branch (the remote ref is the CAS, D17.1/F10); rejection/unknown result → CURRENT-STATE reconciliation (discard the assembled manifest, refetch, recompute; F86); scheduled-build no-op on the F67 projection; nondeterminism halts. Amendment history: F1→F10→F61→F66/F67→F73/F79→F86 in the findings sections. | review rounds 2–16 | internal protocol, revisable |
| D18 | Retention/archival: all published manifests + referenced blobs are permanent GC roots; the publication repo carries the published artifact bytes (bytes-in-git so SWH archives real bytes; accepted subject to the F12 growth probe, F93); stored lifecycle state = `published` only; archival status = a successful `archive_verification(view, C, keys)` observation report (spec §10, F97/F101/F113); citation eligibility requires a successful observation satisfying the current citation policy; independent authorship checkpoints = credential-separated Zenodo deposits (F70). Amendment history: F2→F11/F12→F63/F71→F93/F97/F101. | owner-endorsed + review rounds 2–19 | promise text frozen at first public release |
| D19 | Trust: v1 = two disjoint ROLES with FIXED directly pinned Ed25519 key sets — RELEASE: one online CI key (manifests); GOVERNANCE: ONE offline owner-held SOFTWARE Ed25519 key, generated offline, held as TWO authorized custody copies on separately controlled encrypted offline media under the ceremony-declared complete persistent-copy inventory (withdrawal/amendment) — **O5a-as-amended, owner-RATIFIED 2026-08-25; key medium amended by owner 2026-08-25 (software, after the 5.4.3 firmware finding); composition collapsed per round-27 F143, owner-CONFIRMED 2026-08-25 (two non-threshold software keys add nothing over a second inventoried copy)**. For a given publication chain the pinned sets are established at genesis and never change; changing them ends that chain; successor continuity is outside v1 (F112/F120). The anchor authenticates the role assignment, never a flat key list (F126). Raw detached signatures over domain-separated messages; fingerprints over decoded raw key bytes; key bytes/fingerprints live in the independent anchor + pinned verifier config only (F116). One inventoried medium verifiably destroyed → continue on the remaining copy; an unexplained copy, lost custody of any medium, or possible disclosure → suspected compromise → HALT (F115/F144); compromise of any member → HALT; chain freezes at the last credential-separated Zenodo checkpoint (F55/F59/F70); freshness deferred. Key-manifest/recovery designs stay in the non-frozen contingency appendix; activation trigger: BEFORE any set change, continuity promise, or freshness consumer — never mid-incident. Amendment history: F8→F22–F47→O5a/F48–F55→F112–F117. | review-adopted; O5a staging round 9; owner-directed set-of-2 + round-22 corrections | contingency activates per O5a trigger |
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
- R4 **CLOSED (confirmed 2026-08-25)** — Slice-1 double-build produced an
  identical works map with 0 executed stages; verifier found 0 determinism
  violations across 159,538 blobs. Original probe: grep of
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

R7/R8 decided by owner 2026-08-24. R4 CLOSED 2026-08-25 (slice-1 double-build
identical with 0 executed stages). R5 and R9-GC were superseded by the external review
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
  small files well; ~2.4 GB raw/generation is in-budget **[F93, round 17:
  that figure is RAW corpus size, not measured packed-repository growth —
  bytes-in-git is accepted only after the F12 growth probe measures packed
  size across incremental releases, clone/fetch sizes, and repack cost;
  blob paths are sharded per spec §1]**), and SWH ingestion
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
  O3(b) event-amendment owner-RATIFIED 2026-08-25.
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
  channel (owner named the ORCID-linked record, 2026-08-25); F76
  dated name superseded by F83 dateless canonical id (owner-ratified).

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

### Owner decisions (ALL RATIFIED as of 2026-08-25; O1/O2 gated Slice 3, O3 gated the D16.1 freeze per F31)
- **O1 — admission rule.** Round-3 review VETOED the two-flag (なし/なし)
  rule: the in-repo rights-remediation design (abc/docs/superpowers/specs/
  2026-07-11-rights-assessment-remediation-design.md) states the catalog
  Boolean is a source assertion that must not be promoted to a legal
  assessment, work and person assessments are independent facts, and
  Aozora itself warns translations can retain independent rights.
  **Adopted rule (reviewer text; owner ratification recorded below):** public
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
  **RATIFIED by owner 2026-08-25 with the required round-4 wording:
  `public-release-allowed` is the INCLUSION RULE'S decision, not a new
  assessment_status value; the assessment facts remain {public-domain,
  in-copyright, undetermined, not-evaluated}. Consequence accepted:
  the first public release's scope is the set of works with completed
  assessments — the assessment-data migration is on the Slice-3
  critical path.**
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
  **Candidate shape recorded 2026-08-25; CORRECTED by round 17
  (F90):** the owner is considering Radicle for development, with
  Forgejo/GitHub mirrors. Radicle CANNOT currently be the D17.1
  serialization authority: pushes are local, authority derives from
  delegate-signed refs, and the remote helper's fast-forward check was
  removed (Radicle 1.4.0 release notes) — a sole delegate plus a local
  lock does NOT provide the cross-host conditional update that F78/F85
  assume (the facilitator's earlier claim here that it did is
  RETRACTED). **Recommendation: Forgejo is the authoritative protected
  HTTPS origin; Radicle is an optional downstream distribution
  mirror** (F100, round 18: NO RID pin in the F75 trust channel — a
  mirror has no current-state authority and hashes already
  authenticate its bytes, so a pinned RID would be permanent trusted
  metadata without a trust function or required consumer; mirror
  locations are ordinary discovery metadata, and a pinned RID returns
  only with an actual Radicle-based consumer requiring stable
  repository identity; every seeder is a byte replica). Making
  Radicle authoritative would reopen D17/O2 and require an external
  single-writer/CAS mechanism. F12 on the named origin must MEASURE
  (F93/F94): packed repository size after many incremental releases,
  clone/fetch sizes, repack time and peak RSS, loose-object growth
  before maintenance; SWH save-code-now acceptance of the ingestion
  URL (moderation possible for unfamiliar origins; git loader's
  default pack-size threshold ≈ 4 GiB). `radicle-artifact`
  (iroh-blobs/BLAKE3 release COBs) is NOT the v1 storage of record —
  parallel identity scheme, no SWH path, transport-key trust,
  explicitly early-stage (fails the F51 ratchet); revisit as an
  optional additional distribution location only.
  **Round-18 SPLIT proposal (owner may ratify): O2a — the
  ARCHITECTURAL requirement: an owner-controlled protected HTTPS Git
  origin with conditional (fast-forward-only) ref updates, implemented
  by Forgejo; other forges downstream mirrors — ratifiable NOW. O2b —
  the concrete hostname is Slice-3 DEPLOYMENT CONFIGURATION subject to
  the F12 probes, not a design decision.** **Reviewer (round 19): O2a
  is READY TO RATIFY — Forgejo provides the authoritative protected
  HTTPS ref and conditional-update boundary; mirrors remain ordinary
  byte distribution; O2b correctly reduced to deployment config + F12
  evidence. Owner ratification pending.** **O2a RATIFIED by owner
  2026-08-25: an owner-controlled protected HTTPS Git origin with
  conditional (fast-forward-only) ref updates, implemented by FORGEJO,
  is the authoritative publication origin; every other forge/location
  is downstream byte distribution. O2b (hostname/configuration + F12
  evidence) proceeds as a Slice-3 deployment prerequisite (F108) — no
  owner decision remains in O2.**
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
    needed.
  **(b) RATIFIED by owner 2026-08-25. The `event-amendment` kind
  exists in wire v1 (spec §4); the freeze's decision gate is closed —
  the freeze review now waits only on the executable schemas +
  conformance vectors and the minimal assessment-snapshot content
  schema.**
- **O5a — v1 trust boundary (REWRITTEN per F111, round 21 — the prior
  entry mixed the owner's trust-policy choice with D16 format-freeze
  mechanics, implementation obligations, and deleted design history;
  formats belong to D16, implementation to the dev handoff, history to
  findings F48–F55 and F61–F72). The RATIFIABLE PROPOSITION:**
  **"v1 uses a FIXED, directly pinned SET of Ed25519 keys per role —
  RELEASE: one online CI key, signs manifests; GOVERNANCE: two
  offline owner-held hardware keys, each generated on its own token,
  stored separately, signing withdrawal/amendment events (a signature
  verifies against any set member). For a given publication chain,
  the pinned sets are established at genesis and NEVER change;
  changing them ENDS that chain; successor continuity is outside v1
  (F120). It provides no authenticated freshness and no in-band
  rotation or recovery. A governance device KNOWN destroyed or failed leaves
  governance in DEGRADED one-key operation; an UNACCOUNTED-FOR token
  is suspected compromise and HALTS; COMPROMISE of any member HALTS
  the affected role. UPON SUSPECTED OR CONFIRMED KEY COMPROMISE,
  releases after the last independent checkpoint are CONTESTED until
  an out-of-band notice names the accepted cutoff (F125 — without the
  incident condition, every ordinary release between quarterly
  checkpoints would read as contested)."**
  (Amended by owner direction 2026-08-25 — governance-set size 2 —
  and corrected by F112/F115 round 22, F120/F125 rounds 23–24.)
  **RATIFIED by owner 2026-08-25 on the exact corrected text.
  Architecture review closed (rounds 24–25).**
  **KEY-MEDIUM AMENDMENT by owner 2026-08-25 (after the firmware
  finding — existing YubiKeys are 5.4.3, below the PIV-Ed25519 5.7.0
  floor; procurement declined): the governance keys are SOFTWARE
  Ed25519, not hardware tokens.**
  **COMPOSITION COLLAPSE per round-27 review F143/F144: the
  governance role is ONE software key held as TWO authorized custody
  copies on separately controlled encrypted offline media. Two
  non-threshold software keys — either signs, either's compromise
  halts — provide no property beyond a second inventoried copy, while
  costing a pinned identity, second-member verification, an anchor
  entry, and a both-keys ceremony. F115 accountability attaches to
  the ceremony-declared COMPLETE AUTHORIZED PERSISTENT-COPY INVENTORY
  (the deliberate copy operation itself recorded): operation
  continues only while every surviving medium is accounted for and
  controlled; one medium verifiably destroyed → continue on the
  remaining copy; an unexplained copy, lost custody, or possible
  disclosure → suspected compromise → halt (a copyable file has no
  "lost but unread" state). Encryption-secret custody is operational
  ceremony matter, outside the wire protocol. The owner's original
  "not just one" redundancy intent is preserved as two separately
  stored physical media. All other O5a clauses unchanged.
  OWNER-CONFIRMED 2026-08-25 ("the one-key collapse is fine") — the
  collapse is now owner-decided, not merely review-adopted.**

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
applied/recorded. **Owner RATIFIED F83 on 2026-08-25.**

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

## External design review round 17 (2026-08-25) — findings F90–F95 (storage)

Reviewer verdicts: F83 sound and materially simplifying; the
four-layer storage model close, but three of the facilitator's claims
overreached (Radicle transaction compatibility; carrier
interchangeability; withdrawal's effect on the blob resolver). Slices
0–1 unaffected — the new blockers attach to O2, public withdrawal
WORDING, and the archival promise, not the kernel.

- **F90 (Radicle authority conflicts with the publication transaction
  — Blocker to O2)** — O2 requires an authoritative HTTPS origin with
  protected fast-forward-only semantics; Radicle pushes are LOCAL,
  authority derives from delegate-signed refs, and the remote helper's
  fast-forward check was removed (Radicle 1.4.0 notes). A sole
  delegate + local lock does not provide the cross-host conditional
  update F78/F85 assume — the facilitator's prior-day claim is
  retracted in the O2 entry. Resolution: **Forgejo authoritative;
  Radicle an optional downstream distribution mirror.** Radicle-
  authoritative would reopen D17/O2 with an external single-writer/CAS
  mechanism.
- **F91 ("no storage location is trusted" conflates byte integrity
  with state authority — Strong, adopted)** — given a KNOWN artifact
  id any carrier authenticates bytes; no carrier but the authoritative
  origin attests the latest head, ordering, completeness, observed
  withdrawals, or current-vs-stale-valid-prefix. Spec §9 now states
  this; O2 is low-stakes for byte authenticity, HIGH-stakes for
  ordering, protection, availability, progress. Terminology adopted:
  **build store / authoritative publication repository / serving
  replica / preservation archive / independent checkpoint.**
- **F92 (withdrawal cannot reliably stop a global content-addressed
  URL — Blocker before promising withdrawal semantics)** — `withdrawn`
  acts on slugs; `/blobs/sha256/<hex>` identifies bytes globally;
  blobs may be shared by other admitted works; old commits, clones,
  archives remain regardless. The honest minimal promise (now spec
  §10): withdrawal removes the work from current official manifests,
  discovery, and work-facing serving routes; immutable bytes may
  remain retrievable by hash. Hash-level suppression, if ever legally
  required, is an explicit denylist + shared-blob policy outside the
  protocol.
- **F93 ("zero new storage" / 2.4 GB not yet Git measurements —
  Strong, adopted)** — dedup covers blob contents only; each release
  adds manifest, signature, commit, and NEW TREE objects; delta
  compression is an implementation outcome, not "free". The 2.4 GB
  figure is raw corpus size. Blob layout SHARDED
  (`blobs/sha256/<hex[0:2]>/<hex>`, spec §1); F12 must measure packed
  growth over many incremental releases, clone/fetch sizes, repack
  time/peak RSS, loose-object growth.
- **F94 (SWH acceptance and archive resolution unproven — Blocker
  before the archival promise)** — Save Code Now is described for
  public code repositories; unfamiliar origins may be moderated; the
  git loader has a ~4 GiB default pack-size threshold — the F12 probe
  is substantive. Locator gap closed by a DOCUMENTED RESOLUTION RECIPE
  (spec §10, Slice-4 doc obligation, no wire format): manifest id +
  artifact id → archived publication commit + sharded in-repo path →
  SWHID.
- **F95 (private-CAS GC writer/collector concurrency — Follow-up)** —
  blob-before-trace leaves a window where a new blob is unrooted;
  concurrent mark-and-sweep could collect it before the trace commits.
  Missing-blob-as-cache-miss makes it recoverable, so it does NOT
  block Slice 1; initial GC must either serialize with writers or use
  an age grace period + recheck. Dev note added to unknowns.

Recommended simplified storage decision (recorded; O2 host naming
remains the owner's): keep F83 and the four layers with NARROWER
claims — Forgejo authoritative, Radicle optional mirror; sharded
paths, bytes-in-git accepted only after the real F12 growth probe;
hashes make locations interchangeable as byte carriers, not as sources
of current state; withdrawal = removal from the current official
corpus and work-facing service, not erasure; SWH is the intended
archive SUBJECT TO successful ingestion + the documented recipe.

## External design review round 18 (2026-08-25) — findings F96–F100

Reviewer verdicts: round 17 fixes the reported defects; F83 remains
sound; two boundary problems remained — §9 overstated what the origin
proves, and §10 put service behavior inside the wire protocol. Slices
0–1 unaffected.

- **F96 (the origin does not "attest" — Blocker before freeze)** — the
  Forgejo ref is UNSIGNED and authenticated freshness is deferred: the
  origin operationally DESIGNATES the tip and serializes updates but
  cannot prove non-staleness/non-equivocation; a carrier cannot prove
  its chain is a prefix of the authoritative one without an
  independently obtained head/checkpoint. Spec §9 now carries the
  narrower decomposition (signature → issuance; links → ordering;
  hashes → bytes; ref → designated tip + CAS; checkpoint → historical
  cutoff); completeness is a verifier result.
- **F97 (reuse the verifier for archival — Blocker before advertising
  archival completeness)** — the bespoke predicate wording omitted
  manifest/governance signatures, `releases/HEAD`, and
  chain-transition evidence, and would drift from §7–§8. New
  definition: archive-verified ⇔ expected publication commit present
  AND the ordinary repository verifier succeeds using only the
  archived snapshot + independently pinned keys. One verification
  closure; Slice-3 wording updated.
- **F98 (protocol withdrawal ≠ service withdrawal — Strong, adopted)**
  — the protocol guarantees exactly: slug absent from current `works`,
  present in `withdrawn`, transition authorized by its governance
  event. "Removed from discovery and work-facing serving routes" is a
  soranoha.za SERVICE obligation — moved to the promise/service
  contract with a new Slice-3 acceptance test (4); the non-erasure
  statement is retained in BOTH places as essential product policy.
- **F99 (omit GC from Slice 1 — Strong simplification, adopted)** —
  F95 documented two possible solutions to a problem the kernel need
  not have: Slice-1 CAS is APPEND-ONLY, no GC; activation only when
  measured disk growth crosses a recorded threshold, then with the
  simpler writer/collector serialization. Supersedes F95's
  either/or.
- **F100 (no Radicle RID pin in F75 — Strong simplification,
  adopted)** — a mirror has no current-state authority and hashes
  already authenticate its bytes; a pinned RID would be permanent
  trusted metadata without a trust function or required consumer.
  Mirror locations are ordinary discovery metadata; a pinned RID
  returns only with an actual Radicle-based consumer.

Simplified decision state (recorded): storage and trust are TWO AXES,
not five layers —

| Place | Role |
|---|---|
| kura | disposable private build state |
| Forgejo | publication repository + operational current-tip authority |
| resolver / Radicle | replaceable byte distribution |
| SWH | preservation |
| Zenodo | independent authorship checkpoint |

O2 split proposed (recorded in the O2 entry): **O2a** ratify the
architectural requirement now (protected HTTPS Git, conditional ref
updates, implemented by Forgejo; other forges mirrors); **O2b** the
hostname is Slice-3 deployment configuration under F12.

## External design review round 19 (2026-08-25) — findings F101–F104

Reviewer verdicts: round 18 improves the design and deletes real
complexity; O2a technically ratifiable (validated: Forgejo provides
the authoritative protected HTTPS ref + conditional-update boundary;
O2b correctly reduced to deployment config + F12 evidence); F98–F100
hold; Slices 0–1 ready.

- **F101 (archive verification not bound to the expected commit —
  Blocker)** — "expected commit present AND ordinary verifier
  succeeds" joined two facts by co-presence only: a later snapshot can
  contain commit C while its branch head points to M₂; the verifier
  may validate M₂ while C's release is invalid. Fix (spec §10): one
  operation `verify_repository_at(C, pinned_keys)` — C:`releases/HEAD`
  is the target head, the chain and §7–§8 invariants verify from THAT
  head, and C must be the unique transition that advanced its parent's
  manifest head to that value; `archive_verified(C)` := C present AND
  the operation succeeds on archived data only. Negative fixture added
  to spec §11 (valid latest head + invalid expected commit must FAIL).
- **F102 ("completeness" overloaded — Blocker before freeze)** — §9
  called completeness a verifier result while §8 made
  candidate-selection totality assembler-only. Named apart (spec §9):
  REPOSITORY-CLOSURE completeness (all verification material present;
  verifier result), ADMISSION-PARTITION completeness (snapshot/report
  partition checks internally; verifier result), CANDIDATE-SELECTION
  totality (assembler-only in v1). Archive verification claims only
  repository-closure + the public §7–§8 invariants.
- **F103 (live slice text instructed superseded behavior — Blocker to
  the dev handoff)** — REPLACED rather than annotated: Slice 2's "D18
  GC roots + lifecycle states" → append-only CAS, `published` the only
  stored state; Slice 3's "re-chain and retry (retained manifest)" and
  the "correctly re-chains" acceptance → spec §9 current-state
  reconciliation (discard + recompute); the status header rewritten as
  CURRENT STATE ONLY (−84 lines) — the per-round fix inventories,
  "origin attests", five-layer wording, and F83-pending markers all
  removed from the top; history lives in the findings sections and
  Git.
- **F104 (bounded negative service test — Strong, adopted)** — "no
  work-facing routes" was unbounded and would wrongly suppress the
  public withdrawal history. The za service contract distinguishes
  current-corpus surfaces (must exclude the work), historical/
  governance surfaces (withdrawal statement + release history remain
  accessible), and the hash resolver (remains accessible under
  non-erasure); Slice-3 acceptance (4) is one table-driven integration
  test over the DECLARED route inventory. [Superseded mechanism: the
  declared-inventory/router-table formulation was implemented as a
  test-only Clojure registry and deleted (2026-08-27); the current
  boundary is the Slice-3 acceptance (4) entry — materialized static
  paths + the actual server configuration.]

## External design review round 20 (2026-08-25) — findings F105–F108

Reviewer verdicts: round 19 closes F101–F104 correctly; O2a ready to
ratify; F101–F104 hold; Slices 0–1 ready; the route-inventory test is
appropriately bounded (implementation note adopted: exercise the real
router table, never a parallel route registry). **O2a RATIFIED by
owner 2026-08-25.**

- **F105 (repository data source implicit — Blocker before verifier
  implementation)** — `verify_repository_at(C, pinned_keys)` did not
  name its data source, so an implementation could fetch a missing
  signature/blob from the live resolver and declare an incomplete
  archive complete. Fix (spec §8): the sole verifier primitive is
  `verify_repository_at(repository_view, C, pinned_keys)`; every read
  goes through the single non-fallback view; live/archive/mirror
  verification differ ONLY in the supplied view; the archived SWH
  snapshot is the sole view for `archive_verified`. View-isolation
  negative fixture added (§11): an archived view missing material the
  live origin has must FAIL.
- **F106 (define "publication commit" — Strong simplification,
  adopted)** — "C is the unique transition" was operationally vague
  (unique across which population?). Local definition (spec §8): one
  parent P; P:HEAD = H; C:HEAD = M ≠ H; M.prev_manifest = H; C
  contains M + its verification closure. Uniqueness is a CONSEQUENCE
  of the linear-history invariants, not a search.
- **F107 (decision table still archaeological — Blocker before
  treating the ledger as reconciled)** — D13 called ratified F83
  "proposed"; D17 instructed "re-chain"/"retained-manifest retry";
  D16 was 6,164 chars including deleted formats and obsolete gates;
  D19 was 3,236 chars of demoted trust protocol. All five rows (D13,
  D16–D19) REPLACED with the current decision + a pointer to the
  numbered findings: 12,298 → 2,990 chars. Git preserves every
  intermediate formulation.
- **F108 (O2b is not an owner decision — Strong domain cleanup,
  adopted)** — O2b's output is a configured hostname + passing F12
  evidence, an operational task. The status header now lists owner
  decisions (O2a, O1, O3, O5a, F75) separately from DEPLOYMENT
  PREREQUISITES (O2b hostname/config + F12 probes; keys + the
  credential-separated Zenodo deposit).

## External design review round 21 (2026-08-25) — findings F109–F111

Reviewer verdicts: round 20 closes F105–F108; O2a soundly ratified; O2
closed correctly; O3(b) remains the sensible choice; Slices 0–1
unblocked.

- **F109 (archive_verified(C) hid its observation inputs — Blocker
  before archival implementation)** — the result depends on the SWH
  view, the pinned keys, and the verifier version, not on C alone (the
  same C fails before SWH completes ingestion and passes after). Fix
  (spec §10): the explicit observation
  `archive_verification(archived_view, C, pinned_keys) → report`; the
  disposable report records the SWH snapshot identifier, C, the key
  fingerprints, and the verifier version + result. No signed receipt,
  no frozen schema — the reproducible observation is now actually
  reproducible.
- **F110 (view isolation ≠ target-tree reachability — Blocker)** — a
  required blob/signature could exist elsewhere in the SAME archived
  object graph (another branch, a later commit, a dangling object)
  while absent from C's tree — the co-presence error one level lower.
  Fix (spec §8): the view contract is COMMIT-SCOPED
  (`read_at(C, required_path)`, `parent_of(C)`); required material
  must be reachable at its prescribed path from C's tree; global
  object-store presence is insufficient. Tree-reachability negative
  fixture added (§11). Pre-genesis base case stated: the initial
  commit with the zero `releases/HEAD` is a valid empty state; every
  later valid target is a publication commit.
- **F111 (O5a ratification text was amendment archaeology — Blocker to
  O5a ratification)** — the entry mixed the owner's trust-policy
  choice with D16 format mechanics, implementation obligations, and
  deleted design history. REPLACED with the one-paragraph ratifiable
  proposition (one pinned key per role; no freshness, rotation, or
  recovery; halt on compromise; post-checkpoint releases contested;
  successor protocol designed and exercised BEFORE any expansion
  trigger). D16 owns formats; the handoff owns implementation; the
  findings own history. O5a is now a meaningful owner yes/no.

## Owner direction (2026-08-25, post round 21): governance-key redundancy + signer hardware

Owner raised two points against the F111 O5a proposition before
ratifying: (1) will YubiKey resident keys satisfy the Ed25519
requirement; (2) "I would rather not just have one."

Hardware facts (verified; sources: Yubico yubico-piv-tool release
notes and key-generation docs):
- The owner's existing keys are `sk-ssh-ed25519@openssh.com` (FIDO2).
  FIDO2/CTAP2 assertions sign authenticator data (RP-ID hash, flags,
  SIGNATURE COUNTER) + client-data hash — never the raw message — so
  they CANNOT verify under the §6 raw-Ed25519 wire contract, and
  bending the contract would resurrect the F64-deleted envelope plus
  a stateful counter. Those keys remain fine for SSH/forge
  authentication (transport, not protocol signing).
- YubiKey PIV Ed25519 (firmware ≥ 5.7.0) DOES produce plain Ed25519
  over the exact message — on-device generation, PIN + touch policy.
  FIDO2 and PIV are independent applets: the SAME physical YubiKey
  can keep its resident SSH keys AND hold a governance signing key.
  Owner action: check `ykman info` for firmware ≥ 5.7.0; pre-5.7
  devices need replacement for the governance role.

Decision (owner: "2 seems fine" — pinned set of two): the GOVERNANCE
role becomes a fixed pinned SET of two keys, each generated on its
own token, stored separately; a signature verifies against any
member; compromise of any member halts governance; loss of one device
does not, while the other remains. RELEASE stays a single CI key.
Spec §7/§10/§11 and the O5a proposition amended accordingly; the F54
setup task now publishes `keys/governance-1.pub` +
`keys/governance-2.pub` and fingerprints of every member.

Key-set lifecycle (answering "how will adding/removing keys work?" —
operational, NON-NORMATIVE; the spec deliberately defines no in-band
mechanism):
- BEFORE the first signed release: composition is free — nothing is
  pinned until the trust-anchor deposit + discovery-channel
  publication.
- AFTER the first signed release: the pinned sets NEVER change within
  v1 (F112, round 22 — the round-21 "planned re-pin epoch continuing
  the chain" is RETRACTED: the verifier checks the FULL chain under
  one `pinned_keys` argument, so removing a member fails historical
  signatures, retaining it authorizes future events, and v1 has no
  epoch/window machinery to distinguish the cases). Any change is
  the END of that chain (F120 scoping: per-chain, established at
  genesis; successor continuity — whether a new chain under new pins,
  or the contingency key protocol — is designed OUTSIDE v1, before it
  is needed).
- Distrusted key: not "removal" but COMPROMISE — halt, freeze at the
  last checkpoint, out-of-band notice, successor epoch designed
  deliberately (F49/F55).
- Custody (software-key amendment + round-27 F143/F144: ONE
  governance key, TWO authorized custody copies; accountability
  attaches to the ceremony-declared complete persistent-copy
  inventory): one inventoried medium verifiably destroyed or
  failed → governance continues on the remaining copy; the pinned
  set does not change (F115). An UNEXPLAINED COPY, LOST CUSTODY of
  any medium, or POSSIBLE DISCLOSURE is SUSPECTED COMPROMISE → halt
  ("loss without compromise" is not normally observable; a copyable
  file has no "lost but unread" state; "losing one requires no
  action" was unsafe wording and is retracted).
- "Old key signs new key" is the key-manifest protocol (rollback
  protection, validity windows, history root — F27–F46), parked
  NON-FROZEN in the contingency appendix. Wanting routine add/remove
  IS the O5a activation trigger: design and exercise the successor
  protocol before promising it, never mid-incident.

Owner-cadence facts (answering "just once per quarter?"): the
governance key is used ONLY to sign withdrawal/amendment events —
event-driven and possibly never in a given year; nothing scheduled.
The QUARTERLY owner action is the credential-separated Zenodo
checkpoint deposit (an upload of already-signed manifest bytes — no
governance signature involved). ONE-TIME: the key ceremony + trust
anchor before the first signed release. Daily releases involve only
the CI release key.

## External design review round 22 (2026-08-25) — findings F112–F117

Reviewer verdicts: F110/F111 sound; F109's model right but partially
applied; O5a NOT yet ratifiable as then worded; the two-key fixed set
itself reasonable; Slices 0–1 unaffected and ready.

- **F112 (post-release re-pinning cannot verify the existing chain —
  Blocker)** — the verifier checks the FULL chain under one
  `pinned_keys` argument: removing an old key fails historical
  signatures; retaining it authorizes future events; no epoch/window
  machinery distinguishes the cases. The facilitator's round-21
  "planned re-pin epoch continuing the chain" was incompatible with
  v1, not merely weaker — RETRACTED. v1 rule (spec §7): the pinned
  sets NEVER change after genesis; any change is a new separately
  verified trust epoch, or successor-protocol activation BEFORE the
  change.
- **F113 (F109 left the defective API in place — Blocker)** — the
  predicate-shaped `archive_verified(C)` is RETIRED everywhere (spec
  §10/§11, D18, Slice 3): archive verification is ONE operation,
  `archive_verification(archived_view, C, pinned_keys) → report`.
  "Citation eligibility from the latest report" was undefined
  (disposable reports have no ordering contract; a later failure need
  not invalidate an earlier success) — eligibility now requires a
  SUCCESSFUL observation satisfying the CURRENT citation policy.
- **F114 (pending decision already normative — High)** — spec §7 is
  marked [O5a-PENDING], the spec header lists O5a among open items,
  and D19 is reconciled to the two-role fixed-set text with the
  pending marker.
- **F115 ("lost without compromise" not observable — High)** — a
  device KNOWN destroyed/failed → DEGRADED one-key governance (set
  unchanged); an UNACCOUNTED-FOR token → suspected compromise → HALT.
  "Losing one requires no action" retracted as unsafe wording.
- **F116 (remove `keys/*.pub` from the normative protocol —
  Simplification, adopted)** — verification receives independently
  pinned keys and cannot trust repo-hosted copies; key bytes +
  fingerprints live in the anchor + pinned verifier config ONLY; repo
  copies (if kept for humans) are non-normative and MUST be ignored
  by verifiers.
- **F117 (exercise the hardware boundary — adopted)** — the key
  ceremony EXPLICITLY configures PIN + touch policy (YubiKey touch
  defaults can be Never) and has BOTH governance devices sign a fixed
  protocol conformance vector before deployment.

## External design review round 23 (2026-08-25) — findings F118–F124

Reviewer verdicts: the corrected O5a is sound in substance ("I would
ratify its intent"); a reconciliation pass, no new architecture; after
these edits O5a can close without another architectural review;
historical findings deliberately NOT swept for old terminology; Slices
0–1 ready.

- **F118 (§10 contradicted F115 — Blocker)** — "loss of one does not
  halt" wrongly included an unaccounted-for token. §10 now carries the
  exact trichotomy: known destroyed/failed → degraded; unaccounted-for
  → suspected compromise → halt; member compromise → halt.
- **F119 (Slice-3 handoff implemented the deleted key layout —
  Blocker)** — it still generated two total keys and required
  `keys/release.pub` + `keys/governance.pub`. Replaced: one release +
  two governance hardware keys, no repository key copies, all three
  key-byte sets + fingerprints in the anchor/pinned config, ceremony
  smoke evidence disposable.
- **F120 ("fixed forever" scoped to one chain — adopted)** — spec §7
  and the O5a proposition now read: for a given publication chain the
  pinned sets are established at genesis and never change; changing
  them ends that chain; successor continuity is outside v1. The
  speculative "new epoch vs successor protocol" choice is deleted from
  the normative spec.
- **F121 (total result contract — adopted)** — `report.result` =
  SUCCESS iff present + publication commit + repository verification
  succeeds; otherwise a FAILED report with the reason. No
  report-vs-exception implementation choice.
- **F122 (finish deleting repo key copies — adopted)** — v1 publishes
  NO repository key copies (the "optional human copy" allowance
  removed); the contingency appendix no longer auto-reintroduces a
  signature envelope at the trigger (the two-key verifier disproves
  "raw signatures require one key per role") — an envelope returns
  only with a real consumer for signer identification or key-lookup
  efficiency.
- **F123 (ceremony outside frozen vectors — adopted)** — §11 vectors
  use FIXTURE keys; the F117 hardware smoke signing is pre-release
  disposable evidence, never a frozen fixture or schema.
- **F124 (evergreen header — adopted)** — the status header is renamed
  "Current state — <date>"; review rounds no longer create header
  maintenance.

## External design review round 24 (2026-08-25) — findings F125–F129

Reviewer verdicts: round 23 nearly complete, no new machinery; after
these corrections, ratify O5a and STOP reviewing its architecture; no
additional format, report schema, key lifecycle, or ceremony artifact
warranted; O3(b) not reopened; Slices 0–1 ready.

- **F125 (ordinary releases accidentally contested — Blocker to
  exact-text ratification)** — the proposition's contested clause
  lacked an incident condition, making every release between quarterly
  checkpoints "contested". Corrected: UPON SUSPECTED OR CONFIRMED KEY
  COMPROMISE, releases after the last independent checkpoint are
  contested until the out-of-band cutoff notice.
- **F126 (anchor binds keys to ROLES — High)** — three keys but TWO
  role sets; a flat three-key anchor could authorize the online
  release key for governance. The anchor authenticates the role
  assignment (RELEASE = {K_release}; GOVERNANCE = {K_g1, K_g2});
  `pinned_keys` preserves the partition; overlapping/un-roled
  configurations are invalid. §11 gains table-driven cross-role
  vectors (release key on a governance event FAILS; governance key on
  a manifest FAILS; overlap REJECTED). No schema needed.
- **F127 (D19 missed F120 — adopted)** — the row now says changing
  pins ends the chain, successor continuity outside v1; the
  speculative epoch-vs-successor alternatives removed from the
  current-state table.
- **F128 (bound the totality claim — adopted)** — acquiring the view
  may fail OPERATIONALLY (not an observation); given a readable view,
  always a success/failed report; verifier rejection never confused
  with failure to perform the observation.
- **F129 (dateless evergreen header — adopted).**

## Code-architecture: the v1 choice (REDUCED per rounds 25–26)

The owner's Malli/Rust preference is honored OUTSIDE the wire/verifier
boundary (internal kura/za data shapes; the post-JADH2026 vibrato-pipe
tokenizer lane stays Rust). At the boundary, v1 states ONLY the v1
choice (F141 — no speculative migration policy for consumers that do
not exist; a real JVM-free consumer triggers a FRESH replacement
decision then):

- **ONE verifier, in Clojure**, sharing soranoha.core's single
  canonicalizer (D15), published through the existing Nix entry point
  as Slice 3's "published checker".
- **Hand-authored JSON Schemas** as the single structural source
  (F80/F133), validated with the repository's existing JSON Schema
  machinery.
- **ONE reusable BOUNDARY DECODE for all four wire formats
  (F132/F137, spec §1):** duplicate keys rejected; no coercion; exact
  schema validation; canonicalization; stored bytes == canonical
  bytes; id recomputed. Coercion is confined to local CLI/config
  input and never touches signed or hashed data.

## External design review round 25 (2026-08-25) — findings F130–F136

Reviewer verdicts: F125–F129 correctly applied; O5a's exact text
ratifiable, architecture review STOPPED; the round-24 probe/Malli/Rust
section was rebuilding complexity (three schema representations, two
verifiers, two canonicalizers) and is reduced above. Minimum path:
ratify O5a; run only the hardware smoke now; make F12 target-driven;
freeze the four direct JSON Schemas + table-driven vectors; implement
ONE verifier; defer property/state simulation, Rust independence, and
SWH pre-testing until each has a concrete consumer or observed gap.

- **F130 (Rust plan contradicted D15's one-canonicalizer and
  duplicated the verifier — Blocker)** — reduced: one Clojure
  verifier; Rust only on a real JVM-free consumer, and then as the
  sole verifier.
- **F131 (probes 3–4 were maintained test systems — Blocker)** —
  deleted; table-driven conformance corpus is the pre-freeze object;
  the facilitator's "nearly for free" and "disagreement = spec
  defect" claims corrected.
- **F132 (no coercion of signed wire values — High)** — exact
  construction + validation; coercion confined to CLI/config.
- **F133 (JSON Schema the single structural source — adopted)** —
  hand-authored schemas; no Malli authoring layer.
- **F134 (growth probe lacked decision thresholds — High)** — budget
  first (release horizon, packed/clone/repack/RSS budgets, sampled
  real deltas), then the smallest deciding run.
- **F135 (SWH pre-probe deleted — adopted)** — changes no decision.
- **F136 (hardware smoke kept — adopted)** — with an explicitly
  unused PIV slot so the test cannot overwrite existing credentials.

## External design review round 26 (2026-08-25) — findings F137–F141

Reviewer verdicts: round 25 corrects the architectural duplication;
O5a ratifiable (owner RATIFIED 2026-08-25 — all pending markers
removed); after these surgical corrections, STOP architectural review:
resolve O3, implement Slices 0–1, let the executable schemas and
vectors drive the freeze review; integrate surviving checks into
existing slice acceptance rather than creating probe projects.

- **F137 (exact wire decoding underspecified — Blocker)** — F132
  covered only manifest construction, while governance events are
  signed and admission objects hashed, and the verifier recomputed
  hashes without requiring canonical stored bytes. Fix (spec §1, one
  reusable BOUNDARY DECODE for all four formats): reject duplicate
  keys; parse without coercion; validate against the frozen schema;
  canonicalize; stored bytes == canonical bytes; recompute the id.
  Equivalent-but-noncanonical negative vector added (§11); §8 hash
  checks now require the decode.
- **F138 (stale key-copy allowance — High)** — the remaining-unknowns
  F54 entry still permitted optional repo `keys/` copies,
  contradicting the protocol and Slice 3. Deleted.
- **F139 (standalone hardware smoke deleted — adopted)** — it
  duplicated the F117 ceremony and mutated PIV slots for a throwaway
  key. Early check reduced to READ-ONLY `ykman info`; signing happens
  once, at the real ceremony.
- **F140 (no synthetic F12 harness — adopted)** — git packing depends
  on actual byte similarity, not sampled change rates; the probe runs
  on real adjacent-revision artifacts from Slices 1/2, smallest
  representative sequence against predetermined budgets, as the
  existing Slice-3 gate.
- **F141 (speculative future implementation policy deleted —
  adopted)** — "Rust becomes the sole verifier" and "Malli enters
  after repeated errors" decided migrations for nonexistent
  consumers. v1 states only the v1 choice; a real JVM-free consumer
  triggers a fresh decision.

## External design review round 27 (2026-08-25) — findings F142–F146

Context: after round 26 the owner ratified O1, O3(b), and the F75
channel, then — on learning the existing YubiKeys are firmware 5.4.3,
below the PIV-Ed25519 5.7.0 floor — declined procurement and amended
the O5a governance-key medium to software Ed25519. Reviewer verdict:
round 26 correctly applied; the software-key amendment reopened the
simplicity question; after these findings, STOP architectural review.
Slices 0–1 unaffected.

- **F142 (boundary-decode scope/test mismatch — Blocker, adopted)** —
  §1 limited the decode to the four wire formats while §8 said "every
  JSON-format artifact," which would sweep in schemaless
  `tei-validation` JSON. Both now say "each of the four protocol JSON
  objects"; all other artifacts are exact published bytes checked by
  hash alone. A DUPLICATE-KEY negative vector added to §11:
  reordered-key/whitespace vectors test canonicality but not the
  parser behavior that must reject duplicates BEFORE schema
  validation.
- **F143 (one governance key, two custody copies — Strong, adopted)**
  — the two governance keys were never threshold keys: either signs,
  and compromise of either halts the role. Once keys are exportable
  software, a second key identity provides nothing beyond a second
  inventoried copy, while costing a pinned identity, second-member
  verification, an anchor entry, a both-keys ceremony, and repeated
  count wording. v1: ONE governance key held as TWO authorized
  custody copies on separately controlled encrypted media; the
  deliberate ceremony copy operation is recorded in the custody
  inventory. The owner's "not just one" redundancy intent survives as
  two separately stored physical media.
- **F144 (auditable custody promise — High, adopted)** — "no other
  copies ever made" was unprovable for software key material, and a
  failed medium is not necessarily destroyed. Replaced with the
  COMPLETE AUTHORIZED PERSISTENT-COPY INVENTORY: operation continues
  only while every surviving medium is accounted for and controlled;
  unexplained copies, lost custody, or possible disclosure trigger
  compromise. Encryption-secret custody stays in the operational
  ceremony, outside the wire protocol.
- **F145 (F75 form corrected — High, adopted)** — "ORCID record
  carrying both governance-key fingerprints" omitted the release key
  and hard-coded a count. Now: the role-bound complete anchor lives
  in the first immutable Zenodo deposit; the ORCID record adds that
  deposit's specific VERSION DOI (files fixed, unlike the concept
  DOI) as a public work — one pointer, no duplicated fingerprints,
  no count drift.
- **F146 (pre-implementation checks section deleted — adopted)** —
  it held one completed observation (the 5.4.3 firmware result, now
  recorded in the O5a amendment history) and a duplicate of the
  Slice-3 F12 gate (real-bytes/predetermined-budgets detail folded
  into the Slice-3 preconditions).

## Freeze review round 1 (2026-08-25) — findings F147–F154; NOT APPROVED at 1c9dcfd2; all findings applied

Verdict: package close; four contract defects block D16.1. All eight
findings applied same day; suite green locally and hermetically
(28 tests, 149 assertions).

- **F147 (signature not bound to artifact — Blocker, applied)** — the
  public verify operation accepted a caller-supplied message, so a
  signature over one domain could be presented under another label.
  Fix: `verify-artifact-signature?` takes (type, artifact hex) and
  constructs the domain-separated message internally; arbitrary-message
  verification is private. Vector table extended with wrong-domain and
  wrong-subject rejection rows (10 cases total).
- **F148 (v1 key cardinality unenforced — Blocker, applied)** — role
  sets accepted any non-empty size, permitting unsupported in-chain key
  addition. Fix: pinned-keys configuration is scalar
  `{:release hex :governance hex}` — invalid cardinality is
  unrepresentable; set-shaped values rejected.
- **F149 (snapshot cannot express O1 — Blocker, applied)** — the
  ratified rule assesses the exact work/edition AND every contribution;
  the schema had only contribution facts. Fix: required candidate-level
  `work_assessment` (same fact shape, no contribution_id); spec §5 and
  fixtures updated; missing-work-assessment negative vector added.
- **F150 (valid vector contradicted "no dates in event bytes" —
  Blocker, applied)** — rule clarified to the enforceable structural
  form: no dedicated date/timestamp field; substantive dates may appear
  in the free-form statement.
- **F151 (schema-validity test vacuous — applied)** — replaced the
  map?/string? assertions with `ported-schema/schema-valid!` over all
  four schemas.
- **F152 (suite outside the Nix/check path — applied, D12 fulfilled
  rather than amended)** — `soranoha/flake.nix` (clj-nix deps cache,
  `checks.<system>.clj-nix-tests` runs the full kaocha suite
  hermetically with the shared canonicalization fixture); wired into
  root `check-no-build` (flake check --no-build) and `evidence-gate`
  (actual run), plus a focused `just soranoha-tests` recipe.
- **F153 (unused schema-file hash — applied)** — deleted; git + $id +
  schemas + vectors establish freeze identity.
- **F154 (semantic boundary rules undefined — applied)** — spec §5/§8
  now require real proleptic-Gregorian `effective_date` values and an
  absolute-URI-with-host `upstream_origin`, enforced in
  `soranoha.snh.semantic` (never JSON Schema `format`); tested
  including 2026-99-99 / 2027-02-29 rejections.
- Cleanup: trailing whitespace in generated vector JSON eliminated;
  "frozen" wording replaced with freeze-candidate in AGENTS.md.
  Owner rule adopted same session: code comments and names stand on
  their own — no plan/finding references (ADR links only) and no
  all-caps emphasis; soranoha sources swept accordingly.

## Freeze review round 2 (2026-08-25) — findings F155–F156; both applied; **D16.1 FREEZE APPROVED at 22551310**

Approval note: owner/reviewer confirmed the evidence independently
(local 27/149 green; hermetic Nix green; both semantic negatives fail
through decode with the specified reasons; meta-schema control fails,
all four schemas pass; whitespace/format gates pass; valid-vector ids
unchanged; one authoritative validation boundary). The §8 verifier,
§9 transaction, and §11 item 6's state/transaction fixtures require
their own review — not implicitly approved by this format freeze.

Verdict: F147–F154 substantively resolved; two narrow blockers, no
further architectural review needed.

- **F155 (schema-validity test still ineffective — applied)** — the
  round-1 fix delegated to a helper whose validating schema was a bare
  `{"$schema": …}` (declares a dialect, constrains nothing); the
  reviewer's control instance passed. Fix: the conformance test
  validates each schema against
  `{"$ref": "https://json-schema.org/draft/2020-12/schema"}` (the
  meta-schema) with an invalid-schema control proving the check can
  fail; the byte-faithful ported helper is untouched. Reviewer
  independently confirmed all four candidates pass the correct
  meta-schema.
- **F156 (semantic rules were optional helpers — applied)** — decode
  never called them, so encode accepted `upstream_origin` "not-a-uri"
  and `effective_date` "2026-99-99", leaving duplicate
  remember-to-call obligations. Fix: boundary decode applies the
  type-dispatched single-object semantic checks after structural
  validation (spec §1/§5/§8 updated — F154 rules now live inside the
  decode step; cross-object/transition invariants remain §8); encode
  inherits via its round trip; helper-only rejection assertions
  replaced with two full-boundary reject vectors
  (`release-manifest-invalid-relative-origin`,
  `assessment-snapshot-invalid-impossible-date`). Vector ids of valid
  fixtures unchanged.

Suites after both fixes: local 27 tests / 149 assertions green;
hermetic Nix check green.

## Slice-2 implementation review round 1 (2026-08-25) — findings F157–F163; NOT APPROVED at 25830364; all applied

Verdict on the §8/§9 implementation: architecture clean; correctness
gaps at the publication boundary. All seven findings and the
streaming simplification applied same day.

- **F157 (candidates pushed before verification — applied)** — a
  zeroed release signature reached the origin. Fix: both transaction
  paths verify the candidate commit with the §8 primitive before the
  CAS push (spec §9 step 5); test asserts a bad signature leaves the
  origin ref unchanged.
- **F158 (no-op/determinism decision only ran after losing a race —
  applied)** — identical state published twice; uncontended
  same-projection divergence published instead of halting. Fix: the
  projection/derived-state decision runs before any commit is created
  (spec §9 step 3) and is shared verbatim with reconciliation; tests
  cover the uncontended no-op and the uncontended determinism halt.
- **F159 (git view honored replacement refs / lazy fetch — applied)**
  — a replacement ref substituted a valid commit for an invalid one.
  Fix: every view invocation runs with --no-replace-objects and
  --no-lazy-fetch plus cleared alternates environment; alternate
  object directories are rejected at view construction; spec §8 view
  contract now says NON-SUBSTITUTING; replacement-ref negative
  fixture added.
- **F160 (manufactured commit timestamps — applied)** — the
  process-local counter was not cross-process monotonic and stamped
  production commits in 2023. Deleted; real commit time restored;
  identical-commit collision is documented as state convergence and
  the affected race test accepts both convergent outcomes.
- **F161 (single-object semantics split across callers — applied)** —
  reverse-sorted governance entries published and verified. Fix: all
  single-object rules (list sortedness/uniqueness/disjointness for
  all four types) moved into the boundary-decode semantic dispatch
  (spec §1/§8 updated); the chain verifier keeps only cross-object
  and transition rules and inherits the rest through decode.
- **F162 (validation_summary not re-derived — applied)** — a forged
  invalid_slugs claim verified. Fix: the verifier consumes the fixed
  projection {status, validated_artifact} of each tei-validation
  record, requires validated_artifact to name that work's TEI bytes,
  and requires invalid_slugs to equal exactly the sorted failed slugs
  (spec §8; matches the kernel's existing record fields). Fixture
  validation blobs are representative JSON.
- **F163 (invariant fixtures not isolating — applied)** — the genesis
  negative accepted any of three reasons. Fix: table-driven mutation
  suites with one exact expected reason per row (build-chain table,
  governance-chain table, genesis table, plus dedicated multi-work
  withdrawal-works and replacement-ref cases). The
  superseded-event-single-entry rule is documented as unreachable in
  a valid chain (subsumed by decode's entry uniqueness) and kept as
  defense in depth with a positive fixture.
- **Simplification (applied)** — verification streams: each manifest
  decoded exactly once, only the head manifest retained; the result
  carries head, ordered manifest ids, executed governance-event ids,
  and chain length — all the transaction consumes.

Suites after the round: 48 tests / 241 assertions green locally and
hermetically. Awaiting re-review.

## Slice-2 implementation review round 2 (2026-08-26) — findings F164–F167; NOT APPROVED at 7f6718fa; all applied

Verdict: the round-1 fixes are real; targeted probes found three
boundary blockers and one safe deletion. All applied same day, one
regression test per reproduced failure.

- **F164 (build no-op trusted an unverified head — applied)** — a
  permitted fast-forward commit that left `releases/HEAD` unchanged
  made the next identical build return :already-published while
  direct verification rejected that head. Fix: `publish-build!` fully
  verifies the fetched commit with the §8 primitive and takes head +
  decoded head manifest from its result, before the no-op decision
  and again in reconciliation (spec §9 step 2); `head-manifest-at`
  deleted, no duplicate decoding remains. Test: the crafted
  fast-forward tip now fails with :head-not-advanced instead of
  converging.
- **F165 (validation-record contract bypassable — applied)** — an
  unknown status string, a suffix-forged validated_artifact, and a
  duplicate-key record all verified. Fix: the consumed projection is
  a minimal contract — strict JSON parse with duplicate-key
  rejection, status exactly passed | warning | failed
  (:validation-status-unknown otherwise), validated_artifact exactly
  `sha256:<tei-hex>` (spec §8 F162 bullet amended). Three exact-reason
  mutation rows reproduce the probes.
- **F166 (git view inherited foreign redirects — applied)** — an
  external GIT_DIR or GIT_OBJECT_DIRECTORY let a view over an empty
  repository read another repository's commits. Fix: view discovery
  and every read run under a sanitized environment (all GIT_-prefixed
  variables stripped, environment replaced wholesale) and are bound
  to the git directory resolved at construction via --git-dir (spec
  §8 view contract). Probes cover both inherited variables through
  the construction seam.
- **F167 (unreachable superseded-entry check — applied)** — the check,
  its event-fetch callback, and the §8 sentence are deleted; decode's
  entry-uniqueness rule makes a duplicate-slug predecessor event
  unrepresentable in a valid chain (noted in §8). The subsumption
  test is gone; a compact positive fixture keeps pinning that
  amending one entry of a multi-entry event is legal.

Suites after the round: 50 tests / 249 assertions green locally and
hermetically. Awaiting re-review of this boundary.

## Slice-2 implementation review round 3 (2026-08-26) — finding F168; applied; §8/§9 APPROVED at d7e80a22

Verdict on round 2: F164–F167 correctly resolved; one remaining F166
edge blocked approval.

- **F168 (linked worktrees bypass alternates rejection — applied)** —
  the alternates check ran beneath --absolute-git-dir, which for a
  linked worktree is the per-worktree administration directory; the
  object store and alternates file live under the separate common
  directory, so a view over a linked worktree of a shared clone read
  a commit supplied only by the foreign alternate. Fix: construction
  resolves both directories (--path-format=absolute --git-dir
  --git-common-dir) and rejects common-dir ≠ git-dir outright —
  linked worktrees have no v1 consumer (spec §8 view contract). Test:
  a shared clone plus linked worktree; the clone is rejected for its
  alternates file, the worktree for the hidden common directory.
- Reviewer confirmation recorded: keep
  amending-one-entry-of-a-multi-entry-event-is-legal — it protects
  distinct supported behavior.

Suites after the round: 51 tests / 251 assertions green locally and
hermetically.

Reviewer verdict (2026-08-26): APPROVED — no remaining findings in the
§8 verifier / §9 transaction boundary at d7e80a22. Confirmed: shared
clones and linked worktrees rejected at construction; ordinary
worktrees and bare origins usable; F168 adds no speculative mechanism
(v1 narrowed to repository forms with present consumers); the retained
partial-amendment fixture is justified. Slice 2 proceeds to manifest
assembly against the real trace store and the F5 second-revision
oracle.

## Slice-2 manifest assembly + F5 delta oracle (2026-08-26; APPROVED at 379951dc via rounds 4–8)

The remaining Slice-2 body, implemented against the real kernel:

- `soranoha.za.assemble` — the first za namespace (admission is a
  release-assembler concern per D21/F60; the D20 AGENTS.md transfer was
  recorded before this work). `assemble-release` bridges kernel outputs
  to the §9 transaction's input: the assessment snapshot commits the
  candidate facts (sorted, contribution-sorted), the inclusion rule
  `za-public-domain-unanimous-v1` (id + hash over its canonical rule
  bytes) derives the total admitted/excluded/quarantined partition,
  both evidence artifacts encode through the boundary decode and
  publish with the release, works = admitted − withdrawn with every
  artifact byte read from the kura CAS (fixed [plaintext, tei,
  tei-validation] artifact order, source_content_hash from the
  extract stage's bundle identity), and the validation summary is
  derived from the same per-work records the verifier re-derives it
  from (include-and-flag: a failed validation is a summary entry,
  never an exclusion). `release-assembler` adapts to the transaction
  contract; the F87 totality gate compares the snapshot's slugs
  against the kernel's selected slug set passed independently.
- Fixture corpus harness (`soranoha.za.corpus`, test tree): a
  miniature aozorabunko checkout under git — cards/ work zips, the
  official catalog zip, the real provenance gate, real catalog read,
  real selection join, the real extract stage, and the real
  engine/trace/CAS. Deterministic in-process stages stand in for the
  subprocess parser/converter and the schema validator with matching
  wiring, output names, and record shapes, so `main/run-work!`
  executes unchanged and invalidation semantics are the engine's own.
- F5 three-set oracle (`soranoha.za.oracle`, test tree): (a)
  source/selection delta from per-slug zip content, (b) executed
  stages from the engine's cache decisions, (c) works-entry delta
  between decoded manifests, plus the explanation invariant — every
  stage executed in run 2 for a pre-existing work must have a changed
  declared input (modeled exactly on run-work!'s wiring).
- Acceptance (`soranoha.za.assemble-test`), all through publish +
  full §8 verification against a fixture origin:
  - kernel-backed genesis: works = admitted slugs; the R7 invalid
    work publishes and is flagged in invalid_slugs; artifact ids
    equal the kernel's CAS hashes; the published admission report
    carries the exact excluded/quarantined partition; manifest
    round-trip re-serializes to the same manifest_id (twice); the
    identical scheduled build is :already-published.
  - second revision (addition + deletion + content edit + catalog
    fan-out + output-preserving rezip): oracle sets exactly as
    predicted — the rezip re-executes extract only and its manifest
    entry is retained byte-identical; the catalog edit fans metadata
    re-execution across every work without touching artifacts; the
    content edit invalidates the full chain and changes only its own
    entry; zero unexplained executions; distinct manifest ids.
  - assessment-only delta (F24 under the F61 protocol): same store,
    revision, and works; enlarged assessment snapshot → changed
    admission evidence, distinct manifest_id, published (never
    no-op'd), works byte-identical.
  - totality: a selected-but-unassessed work blocks emission.
- Slice-2 fixture-test coverage mapping: withdrawal (R8 path, F26
  chain invariants), event-amendment, and the F61 lost-ack test are
  covered by the §8/§9 suites approved at d7e80a22
  (verify-test/transact-test); addition, deletion, output-preserving
  edit, R7 include-and-flag, and the assessment-only delta land here.

Suites after this work: 55 tests / 285 assertions green locally and
hermetically. The F5 second-upstream-revision acceptance runs on the
fixture corpus; the production run at a second real aozorabunko
revision remains a Slice-3-adjacent operation on the same machinery.

## Slice-2 implementation review round 4 (2026-08-26) — findings F169–F173; NOT APPROVED at 8abef9c7; all applied

Verdict on the assembly increment: transaction/verifier integration
strong; five boundary issues, three of them duplicated semantics the
redesign exists to eliminate. All applied same day.

- **F169 (inclusion_rule_hash did not bind the executable rule —
  applied)** — the hash covered an English description while the
  partition logic lived independently in code. Fix: the rule is now a
  small executable data value ({admit_when_all, exclude_when_any,
  exclude_reason, quarantine_reason}) consumed by the evaluator;
  changing any decision-bearing value changes the hash; no resolver or
  schema added. Test binds the published report's inclusion_rule_hash
  to the canonical hash of that value.
- **F170 (fixture catalog result unrepresentative — applied)** — the
  production metadata stage embedded the catalog file hash in the
  renderer-facing persons value via source_csv_provenance, so a real
  catalog edit re-rendered every work; the harness's "metadata only"
  expectation was true only of the substitute stage. Fix per the
  preferred option: the production stage now strips
  source_csv_provenance (which render ignores) from the persons
  output, restoring early cutoff and making the oracle's expected
  result true of the production graph; metadata stage-version bumped
  to 2 (output bytes change for identical inputs). The harness
  docstring now states exactly which stages are substituted
  (metadata, parse, convert, render, validate; extract and all
  orchestration real).
- **F171 (validation-record interpretation split again — applied)** —
  the assembler parsed records permissively while the verifier held
  the strict contract. Fix: one pure shared checker,
  verify/consumed-validation-record (strict duplicate-key parse,
  status domain, string validated_artifact), called by both the
  assembler's summary derivation and the verifier's re-derivation; a
  noncontractual kernel record now fails assembly, not just pre-push
  verification. Pure-contract regression test added.
- **F172 (toolchain provenance not fail-closed — applied)** — the
  production default clj-dev-0 let dependency/runtime changes retain
  stale derivations and an unchanged release projection. Fix: default
  removed; build! fails closed without a wrapper-supplied
  --clj-toolchain-id. Reconciliation recorded (spec §3 toolchain row):
  nix_closure_hash carries the stage's toolchain identity exactly as
  the derivation keys carry it — the Nix closure hash for
  nix-provisioned stages, the hashed binary/profile identity for
  subprocess stages; constant placeholders prohibited.
- **F173 (source_content_hash semantics disagreed with the frozen
  prose — applied)** — the spec said "hex of upstream source bytes";
  the implementation records the canonical source-bundle identity
  hash (member paths + member content hashes), which is the desirable
  semantics (identity survives archive-level repackaging). Post-freeze
  semantic clarification recorded in spec §3; no wire change.
- **Handoff correction (applied)** — build!'s disposable run report
  now carries everything the delta oracle consumes: per-work
  source_zip, source_relpath, source_content_hash, per-stage cache
  decisions, and the stage coordinate table; run-work! returns
  zip-hex and source facts. Still a schema-less, retention-free
  export. Slice 2 is not declared complete until the production F5
  run at a second real revision has been driven from these exports.

Suites after the round: 56 tests / 291 assertions green locally and
hermetically.

## Slice-2 implementation review round 5 (2026-08-26) — findings F174–F176; NOT APPROVED at 4a6585d8; all applied

Verdict on round 4: F169–F171 cleanly resolved and the F170
production simplification sound; two blockers and one exactness
correction remained.

- **F174 (report could not drive the explanation oracle — applied)**
  — the report carried final artifacts, source facts, cache
  decisions, and stage coordinates, but not the intermediate input
  hashes the invariant compares. Fix per the simplest option: the
  engine's already-computed derivation key is exported per work/stage
  (run-work! returns :trace-keys; the report carries "trace_keys").
  With equal stage-coordinate tables across two runs, an executed
  stage must carry a changed trace key; a same-key execution
  correctly surfaces missing-blob recovery work. The test oracle's
  hand-modeled stage-input map is deleted — unexplained-executions
  now compares the same trace keys the production export carries, so
  there is exactly one oracle representation.
- **F175 (F172 only partially implemented — applied; F172 REMAINS
  OPEN)** — two gaps: no wrapper yet supplies the promised
  Nix-derived identity (the CLI merely fails without one, and a
  caller could still pass a permanent placeholder), and
  validate-tei-stage — in-process Clojure — hashed only the TEI
  profile trio, so JVM validation-dependency changes could reuse its
  traces. The stage's toolchain identity now binds clj-toolchain-id
  alongside the three profile hashes. F172 stays explicitly open
  until the production F5 driver lands the content-derived wrapper
  identity; no placeholder wrapper is added in the interim.
- **F176 (identity abbreviated incorrectly — applied)** — the spec §3
  works row now states the exact hashed object: sha256 over the
  canonical bytes of the abc-source-bundle-v1 identity object
  {construction, members: [{path, member_hash}...],
  primary_text_member}. The round-4 ledger entries' "spec §2"
  references corrected to §3.

Per the reviewer, no additional harness: the already-required
production F5 run proves the real metadata-stage cutoff and the
wrapper wiring.

Suites after the round: 56 tests / 291 assertions green locally and
hermetically.

## Slice-2 implementation review round 6 (2026-08-26) — finding F177; NOT APPROVED at 0049cfc6; applied

Verdict on round 5: F175/F176 correct, F172 honestly open; one
blocker.

- **F177 (oracle precondition unenforced — applied)** — the trace-key
  invariant assumed equal stage coordinates but neither captured nor
  checked them, so a stage-version or toolchain change would be
  misreported as a changed declared input (the reviewer's probe with
  no coordinate data returned an empty violation list). Fix: each
  harness run captures its actual {stage-id, stage-version,
  toolchain-id} table (run-corpus! also takes an explicit stage set);
  unexplained-executions fails loudly with :runs-incomparable when
  either table is absent or they differ. Negative test: a
  toolchain-only bump (which re-executes exactly the re-keyed stage),
  a stage-version-only bump, and an absent coordinate table are all
  refused; a genuine same-coordinate cached rerun remains comparable
  with zero violations. Trace keys stay the sole representation; the
  production driver normalizes its existing "stages" report field
  under the still-open F172 work.

Suites after the round: 57 tests / 297 assertions green locally and
hermetically.

## Slice-2 implementation review round 7 (2026-08-26) — finding F178; NOT APPROVED at 829eee04; applied

Verdict on round 6: the F177 fix held for complete fixture-generated
runs; one fail-closed hole remained, plus a consolidation.

- **F178 (incomplete evidence still passed — applied)** — the
  precondition checked only that the coordinate tables existed and
  were equal, not that they covered every analyzed stage or that both
  runs carried a trace key for every compared execution; equal empty
  tables with an execution, and complete tables with the earlier
  run's key missing, both returned []. Fix: unexplained-executions
  now fails closed with :runs-incomparable when a compared
  execution's stage is not covered by the coordinate table or when
  either run lacks a string trace key for it — directly relevant to
  the F172 JSON normalization, where omission is possible. Both probe
  cases join the negative table, alongside a positive showing a
  same-key execution under complete evidence is a reported violation.
- **Consolidation (applied)** — one coordinate projection,
  trace/stage-coordinates, owned beside the derivation-key logic
  (logical stage key -> {stage-id, stage-version, toolchain-id} —
  exactly the non-input part of each derivation key); the production
  report's "stages" field and the fixture harness both consume it,
  deleting the duplicate projection and making coverage checkable.
  The run-corpus! stage-set arity and the two extra full-corpus runs
  are deleted — the oracle test mutates captured coordinate evidence
  directly, and trace invalidation under coordinate changes is
  already proven by the engine's derivation-key tests.

Suites after the round: 57 tests / 299 assertions green locally and
hermetically.

## Slice-2 implementation review round 8 (2026-08-26) — finding F179; NOT APPROVED at a5565d09; applied

Verdict on round 7: F178 refusal cases correct, harness-arity
deletion good; one consolidation blocker — otherwise ready.

- **F179 (the "one projection" still had three encodings — applied)**
  — derivation-key built its own string-keyed coordinate object,
  stage-coordinates built a keyword-keyed equivalent, and the report
  converted back to string keys, leaving the synchronization trap the
  consolidation was meant to remove: a future derivation coordinate
  could be hashed but omitted from the comparison table. Fix, in the
  net-deleting shape: trace/stage-coordinate is the single
  constructor of {"stage_id","stage_version","toolchain_id"};
  derivation-key adds "inputs" to that value and hashes it;
  stage-coordinates maps logical stage keys to that same value; the
  report converts only the outer logical keys to strings and passes
  coordinate values through unchanged. A coordinate added to the
  constructor is automatically both hashed and compared. No new test
  or abstraction — the derivation-key and oracle suites already cover
  the behavior.
- Recorded under the open F172: malformed-but-present coordinate
  values and non-hex trace-key strings still pass the oracle; the
  future external-report boundary decoder rejects them before
  invoking the oracle — no additional validation layer here, since no
  current consumer parses external reports.

Suites after the round: 57 tests / 299 assertions green locally and
hermetically.

Reviewer verdict (2026-08-26): no blocking findings at 379951dc — the
manifest-assembly / F5-oracle increment (rounds 4–8, F169–F179) is
approved. Open before Slice 2 is declared complete: the production F5
run at a second real aozorabunko revision, driven from the disposable
run reports, which also lands the F172 work (content-derived wrapper
toolchain identity; external-report boundary decode).

## Slice-2 implementation review round 9 (2026-08-26) — findings F180–F183; NOT APPROVED at 64b6ad4a; all applied

Reviewer verdict on the F172-closure increment: production evidence
credible and internally consistent, but three implementation contracts
fell short of the claimed clean oracle result. All fixed at cd9ffa69;
the production F5 pair was then rerun through the final wrapper (see
below).

- **F180 (blocker) — incomplete execution evidence was accepted.**
  decode-run required each work's cached/trace_keys stages only to be a
  *subset* of the declared coordinate table; deleting the same stage
  from both maps passed decoding (reproduced by the reviewer against
  the real revision-B report: a five-stage work against a six-stage
  table), silently erasing an execution from the oracle. Fix: both key
  sets must equal the declared stage set exactly — which also subsumes
  the separate cached/trace-keys divergence check (deleted, with its
  :cached-and-trace-keys-diverge reason). Tests: omitting a declared
  stage from cached or from trace_keys each refuse decoding.
- **F181 (blocker + simplification) — unexplained_artifact_changes
  deleted.** The delta CLI treated *any* executed stage as sufficient
  explanation for *any* artifact change (reviewer flipped the real
  052211 report to metadata-only execution with changed TEI bytes; the
  CLI still said ok), and conversely a valid warm-cache run can obtain
  changed artifacts entirely from cache while executing nothing. The
  rule was both too weak and too strong; F5's sound checks already
  exist (content hashes establish byte changes; trace-key comparison
  explains executions). Fix: the artifact delta stays descriptive and
  `ok` depends on unexplained executions only. No
  artifact-to-producing-stage model added.
- **F182 (blocker) — wrapper identity did not authenticate its runtime
  environment.** The wrapper hashed a Clojure derivation label and the
  lockfile but invoked clojure against the caller's mutable HOME,
  Clojure configuration, and Maven cache. Fix: reuse the existing
  hermetic machinery from soranoha/flake.nix — the clj-nix offline
  dependency cache with HOME, JAVA_TOOL_OPTIONS(-Duser.home),
  CLJ_CONFIG, GITLIBS bound to store paths and CLJ_CACHE /
  XDG_CONFIG_HOME on a per-invocation scratch dir — and derive the
  identity from the actual Clojure closure store path, the
  dependency-cache closure store path, and deps.edn, retaining the
  full sha256 (clj-nix-<64 hex>, no truncation). PATH is now bound
  wholesale rather than prefixed onto the caller's.
- **F183 (correction) — deterministic failure output.** The CLI
  serialized unexplained-execution rows from unordered maps/sets
  without sorting; rows are now sorted by slug then stage, making the
  deterministic-JSON claim true on failing runs too.

What checked out per the reviewer: both suites at 59/311 (now 59/312
with the F180 coverage assertions), Nix evaluation/format/hygiene
gates, matching coordinate tables and complete evidence in the two
production reports as generated, source delta = git delta, and the
reproducible execution/artifact counts including the catalog-only
052211 change.

### Production F5 rerun through the final wrapper (F182 identity)

Same corpus revisions (A = a1da0f5a00, 17,592 selected; B = the pinned
0e9ea3e586, 17,602 selected), same store /db/soranoha/kernel-full,
both builds through the hermetic `nix run .#soranoha-kernel`
(identity clj-nix-6b66028fa5941a0e2d389d7eff295e131aee7d9ae3bdc9bde
fb4f3f164884147, visible in the reports' stage-coordinate tables).
Run A re-keyed every pure-Clojure stage under the new identity
(70,347 executions); run B executed only the delta. Reports
run-1787730983384.json / run-1787731321127.json; logs + verdict under
/db/soranoha/publication-rearchitecture/slice2/logs/
(f5-run-a-hermetic.log, f5-run-b-hermetic.log,
f5-delta-hermetic.json).

Verdict **ok = true** under the corrected gate (unexplained
executions only): 0 unexplained executions; the selected
cards/*/files/*.zip delta matched Git exactly: 10 added, 1 modified,
0 removed (the full Git delta is broader — 30 additions and 223
modifications, including six changed index ZIPs outside the selected
work-zip population);
executed: extract 11, render/validate 12, metadata 17,595 (parse and
convert do not bind the clj identity, so their earlier traces remain
valid); artifact delta descriptive: 10 added, 2 changed (the
source-explained 004820 and the catalog-driven 052211 — source zip,
parser-IR, and plaintext byte-identical, only TEI + validation record
changed), 17,590 retained byte-identical. The delta JSON is
byte-identical across CLI reruns. Checkout restored to the pinned
master. Suites: 59 tests / 312 assertions green locally and
hermetically.

## Slice-2 completion (2026-08-26) — **APPROVED at 5d9f16b7**

Slice 2 closed after nine implementation-review rounds (F157–F183):
§8/§9 boundary approved at d7e80a22, assembly/F5 fixture increment at
379951dc, F172 closure + production F5 evidence + wrapper hermeticity
at 5d9f16b7 (a1da0f5a00 → the pinned 0e9ea3e586, oracle ok = true, 0
unexplained executions; reports and verdict under
/db/soranoha/publication-rearchitecture/slice2/logs/). Suites: 59
tests / 312 assertions green locally and hermetically. This is the
ledger's implementation-transcript cutoff; subsequent implementation
evidence lives in commits and PR review.

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
trigger (O5a): BEFORE any pinned-set change, promising
cryptographic continuity, or serving an external consumer that requires
authenticated freshness — never after an incident begins.** (F122,
round 23: the trigger does NOT automatically reintroduce a signature
envelope — the two-key governance verifier disproves the premise that
raw signatures require one key per role; an envelope returns only when
a real consumer needs signer identification or key-lookup efficiency.)
Until then, v1's posture is
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
append-only history ledger; **the Slice-1 CAS is APPEND-ONLY — NO GC
exists in the initial kernel (F99, round 18: missing blobs are already
recoverable cache misses; GC activates only when measured disk growth
crosses a recorded threshold, and then with the simpler
writer/collector serialization — the concurrency problem is deleted,
not documented)**), `yomi` (clone management + catalog/selector
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
validation (BOTH SATISFIED — owner-ratified 2026-08-25); the FOUR
pre-Slice-2 formats frozen (incl. the minimal
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
Chain mechanics on that manifest: prev-manifest hash chain and the
spec §9 publication transaction exercised against a local fixture
origin. Storage stays the append-only CAS (F99 — no GC); `published`
is the ONLY stored lifecycle state (F71). Build at a SECOND (newer) upstream revision; acceptance = the F5
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
operations: a withdrawal with NO upstream change and an
event-amendment (O3(b) ratified; amends chain verified; withdrawn set
unchanged); each yields a distinct manifest_id via changed content.

### Slice 3 — za publishing + CI (rewritten per D17.1/D18.1/D21)
Production-deployment prerequisites (local implementation is complete;
these gate deployment and the first public signed release, never the
fixture/local work): O1 RATIFIED (owner, 2026-08-25, with the round-4
wording: public-release-allowed is the inclusion rule's decision) —
the assessment evidence committed as versioned data (F24 snapshot
source) remains outstanding; O2 host named and
F12-probed; rights admission consumed-from-abc or transferred (F14c);
F12 repo-growth probe run against the chosen origin ON REAL
adjacent-revision artifacts from Slices 1/2 — smallest representative
sequence against PREDETERMINED budgets (packed size, clone/fetch
bytes, repack time, peak RSS); no synthetic harness (F140/F146); O5a
signing in place (F119 reconciliation; key medium amended by owner
2026-08-25; composition per round-27 F143/F144): ONE online CI
RELEASE key + ONE offline GOVERNANCE software Ed25519 key generated
offline, held as TWO authorized custody copies on separately
controlled encrypted media (the copy operation recorded in the
custody inventory); NO repository key copies (F122); **the F54
minimal trust anchor published BEFORE the first signed release: the
ROLE-BOUND assignment — RELEASE = {release key}, GOVERNANCE =
{governance key} — as key bytes + fingerprints in the FIRST immutable
Zenodo deposit (F126: the anchor authenticates roles, never a flat
key list; repo-hosted keys cannot authenticate themselves; the
Slice-4 promise document arrives too late to be the first pin),
loaded as pinned verifier configuration; smoke signing by the
governance key recorded as disposable ceremony evidence (F123)** — the deposit
carries the actual genesis manifest bytes + signature (F63); full-corpus
assessment DATA migrated (the schema froze at Slice 2 — F58/round-12
cleanup: no schema freezes remain before this slice; "full-corpus"
means TOTAL ACCOUNTING — explicit not-evaluated facts included, not
completed legal assessments for every work; only eligible completed
assessments enter the first release — and after migration an
AUTHORIZED policy change from :blocked-pending-assessment-migration to
:assessment-required is also required: data alone does not lift the
block); signatures are raw
detached Ed25519 per F64; the F49/F55/F59 compromise runbook documented
(chain freezes at the last independent Zenodo checkpoint;
post-checkpoint signatures contested until an out-of-band cutoff notice;
artifact ids always identify exact bytes). No key-manifest, trust
branch, or recovery tooling in this slice (O5a — contingency appendix
only). Additional acceptance: a governance-withdrawal fixture executed
end-to-end (event signed by the governance key; F39/F62 transition
invariants verified by the published checker) — IMPLEMENTED
(2026-08-27, in the governance-CLI increment).
Publication = the D17.1 remote compare-and-append: one complete commit
(blobs + manifest + sig) fast-forward-pushed to the protected branch;
rejection → CURRENT-STATE reconciliation per spec §9 (F86: DISCARD the
assembled manifest; fully verify the new head; recompute the desired
projection and expected content against it → success when the desired
state is already published / determinism-halt / requeue); an unknown
push result converges on the same reconciliation via the reachability
check; scheduled-build no-op per the F67 projection.
Forgejo auto-release polls upstream — PENDING deployment integration:
the invokable driver and its scheduled-runner exit contract are
implemented, and the unattended job (a production consumer per D5)
awaits the real runner/secrets contract; admission is a fail-closed
input (policy hash in manifest). Serving tree (blobs/, releases/, governance/,
releases/HEAD, plus the work-facing symlink layer works/<slug>/<type>,
withdrawn/<slug>.json, releases/latest — relative names into chain
content, no derived documents and no runtime resolution) derives from
the verified chain, staged and atomically installed into a previously
absent destination. The static server is Caddy (owner-selected
2026-08-27); the checked-in soranoha/config/caddy/Caddyfile is the
whole serving configuration (immutable cache headers only on existing
digest-addressed files, short-cache pointer layer, listen/root from
the environment). Before public activation the TLS owner must be
named: an external terminator fronting Caddy, or Caddy's production
configuration terminating TLS. Deployment
preconditions: the destination parent is exporter-owned (one
cooperating exporter; nothing else creates the destination — the rename
gives atomic namespace visibility, not no-clobber or crash durability;
the serving tree is regenerable, the publication repository is the
durable record), and — AMENDED 2026-08-28 per external review — the
exporter and the resolver run as SEPARATE OS principals: the earlier
same-principal wording would have collapsed the network-facing
resolver and the release authority into one user, letting a
compromised static server reach signing material. The exporter
installs the serving tree readable to a narrow serving group the
resolver belongs to (dirs 2750/files 0640); everything else under
the publisher root stays owner-only to the exporter. Chain
history reads from the manifests themselves. Archival: SWH
save-code-now per release,
non-blocking; **archival status per F113 (one operation, one name):
`archive_verification(archived_view, C, pinned_keys) → report` — C
present + a publication commit (F106) + the §8 primitive succeeding
with the SWH snapshot as the sole repository view (no fallback
reads); the disposable report records the SWH snapshot id, C, key
fingerprints, and verifier version + result — no receipt artifact;
citation eligibility = a successful observation under the current
citation policy**. Acceptance: (1) two consecutive automated releases
from real upstream movement, chain verified end-to-end by the published
checker; (2) a forced concurrent-publish attempt loses the push race and
reconciles per spec §9 current-state rules — discards its manifest,
recomputes against the new head, and publishes NOTHING when the
desired state is already published — and a simulated lost push
response resolves via the reachability check plus the same
reconciliation without double publication; (3) at least one
release has the archival predicate SUCCEED (F77 wording — no
"archive-verified" state is reached or stored) with all four F12
checks, with measured (not assumed) archival latency recorded in the
ledger; (4) **F98/F104 service-withdrawal acceptance — against the
REAL static-serving boundary, per Q3 ("static files, no resolver
service")**: the acceptance exercises the concrete paths the
export/deployment step materializes plus the actual checked-in
static-server configuration — no executable resolver and no test-only
route registry (a Clojure inventory consumed only by its own test was
built and DELETED as a parallel-registry seam; do not recreate it).
The bounded surface partition holds: current-corpus surfaces (catalog,
slug content, downloads) EXCLUDE the withdrawn work;
historical/governance surfaces (the withdrawal statement, release
history) REMAIN accessible; the hash resolver REMAINS accessible under
the explicit non-erasure policy; search enters the acceptance only
after a real search surface exists. Materialization + configuration
are not blocked on the production host: once the server/configuration
is selected, this acceptance runs locally against the generated tree;
only real-origin behavior and the SWH observation need external
deployment. The protocol-level absence from `works` is already checked
by the §8 verifier. IMPLEMENTED (2026-08-27, approved at 26cc95df):
static_serve_test drives the checked-in Caddyfile over HTTP against a
generated tree with a withdrawn work.

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
- ~~O3~~ RATIFIED (b) by owner 2026-08-25 — the event-amendment kind
  exists in wire v1; the freeze's decision gate is closed.
- Private evidence record's handling/retention policy — owner, before
  the first real withdrawal. (Per F82 the public `evidence_hash`
  commitment is removed from v1; the record is purely operational until
  a concrete audit consumer and encoding exist.)
- Pinned-key setup (O5a/F54; governance-set amendment 2026-08-25;
  key medium AMENDED to software by owner 2026-08-25 — the existing
  YubiKeys are firmware 5.4.3, below the 5.7.0 PIV-Ed25519 floor; no
  5.4.3 applet produces the spec-§6 raw detached Ed25519, and
  procurement was declined; the 5.4.3 devices keep their SSH/FIDO2
  uses and play no role in v1 signing; composition collapsed per
  round-27 F143/F144): generate the RELEASE key (online, CI) and ONE
  GOVERNANCE software Ed25519 keypair — generated OFFLINE, written as
  TWO authorized custody copies to separately controlled encrypted
  offline media (the deliberate copy operation recorded in the
  ceremony-declared complete persistent-copy inventory; F115/F144
  accountability attaches to it); governance signing only ever on an
  offline machine; encryption-secret custody is ceremony-operational,
  outside the wire protocol. The ceremony has the governance key sign
  a fixed protocol conformance vector as disposable evidence (F117
  analog). Key bytes + fingerprints go in the independent anchor +
  pinned verifier configuration ONLY; v1 publishes NO repository key
  copies (F122/F138). Publish the minimal
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
- **F75 pre-release discovery channel — NAMED by owner 2026-08-25:
  the ORCID record; form corrected by round-27 F145.** Remaining
  obligation (owner, before the first signed release): deposit the
  role-bound COMPLETE anchor in the first immutable Zenodo record,
  then add that record's specific VERSION DOI (not merely the concept
  DOI — a version DOI's files are fixed) as a public ORCID work. ONE
  pointer to the one anchor: no fingerprints duplicated outside the
  anchor, no key counts to drift. The Slice-4 promise document
  restates it; the Slice-4 paper CANNOT be the first pin (it
  postdates the first release).
- ~~`snh-archive-receipt/1` field schema~~ DELETED round 12 (F63):
  archive-verified is a reproducible predicate with a disposable report;
  no receipt format exists.
- **F94 archive resolution recipe** — documented mapping manifest id +
  artifact id → archived publication commit + sharded in-repo path →
  SWHID (spec §10; no wire format) — dev/owner, Slice 4, before the
  archival promise is advertised.
- **F95→F99 private-CAS GC** — SUPERSEDED (round 18): the Slice-1 CAS
  is APPEND-ONLY; no GC is implemented or specified in the initial
  kernel (missing blobs are already recoverable cache misses). GC
  activates only when measured disk growth crosses a recorded
  threshold; at that trigger, implement writer/collector
  SERIALIZATION. Dev; the threshold is recorded in the ledger when
  set.
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

- **BDASL (dasl.ing/bdasl.html; assessed 2026-08-25): NOT relevant to
  v1.** BLAKE3-CID streaming verification + trustless HTTP range
  requests for LARGE payloads — the same BLAKE3-verified-streaming
  family as radicle-artifact's iroh-blobs, already declined (round 17).
  Fails the F51 ratchet three ways: no consumer needs range/streaming
  verification of tens-of-KB text artifacts (whole-file sha256 after
  fetch is trivially cheap); an editors' draft dated 2026-08-20; and
  the spec itself recommends against its CIDs in open environments —
  disqualifying for citation identity. Would also be a second,
  parallel identity scheme (the F62/F63 anti-pattern). Contingent
  relevance only: if large full-corpus bundles are ever distributed
  over untrusted mirrors, BLAKE3/bao-style verified streaming is the
  transport-verification prior art — a transport detail, never the
  identity of record.
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

## Owner decision record — 2026-08-27 (decision interview, corrected per external review)

- **O2b provisional: private test origin.** The existing tailnet
  Forgejo (code.hyakutake-barbel.ts.net) is the origin for the test
  deployment. Tailscale Serve is tailnet-only, so this origin is
  PRIVATE — it exercises HTTPS, Forgejo compare-and-append behavior,
  repository growth, cloning, and repacking, but not anonymous
  verification of the current tip and not SWH ingestion. True public
  exposure is a later, separate step.
- **TLS owner.** Caddy terminates TLS with Tailscale-provisioned
  certificates; the serving deployment lives in the speely nix
  configuration.
- **Key ceremony staging.** The offline governance ceremony proceeds
  now (media, custody inventory, smoke-signing evidence per F123); the
  online RELEASE key is generated at CI provisioning time, so it is
  born where it lives.
- **Rights assessment authority: Soranoha, singly.** One authority per
  assessment (the source contract's rule; statutory dates cannot be
  inserted into an Aozora assertion): Soranoha performs the cited
  public-domain assessment under statutory answers the owner adopts;
  Aozora's flags remain corroborating source facts — never the
  assessor, and no permission-based admission lane is created. The
  executable admission rule stands unchanged: a work admits only when
  the work and every contribution are public-domain. Statutory
  groundwork = Q1–Q5 of the aozora-rights-source contract PLUS an
  old-law transition question (works whose protection is affected by
  old-law/current-law transitional provisions; Agency for Cultural
  Affairs transition guidance), each answered as a citation for owner
  adoption.
- **Coverage expectations, bounded.** 16,558 — the census partition
  row "`なし`, every contributor has a parseable CE year ≤ 1967, no
  `翻訳者` and no `原題`" — is a LEXICAL UPPER BOUND on first-tranche
  candidates — not an approved tranche, coverage figure, or recovery
  promise; the なし ∩ text-URL intersection is uncomputed, and absence
  of 翻訳者/原題 does not establish absence of foreign-term issues
  (nationality is not a catalog field). Works outside the first
  tranche are DEFERRED FOR ASSESSMENT; some may remain protected after
  Q4/Q5 analysis. F3's 1967 death-year frontier corroborates the
  non-revival transition hypothesis but does not establish who
  assessed Aozora's flags or under which rule — Q6 stays open. The
  owner's principle (works Aozora releases without qualification
  should be releasable) is recorded as a coverage aspiration to test
  against completed assessments, not as an authority route.
- **F12 split.** The tailnet-testable storage/origin probes (packed
  size, clone/fetch bytes, repack time, peak RSS, loose-object growth
  before maintenance — budgets ratified before the run), plus the
  existing F12 pass/fail origin checks (object-size limits, force-push
  prevention, branch-deletion protection, rejected-concurrent-push
  recovery), proceed against the private test origin; SWH
  save-code-now probing and the archival observation are public-only
  and deferred to public exposure.
- **F12 budgets RATIFIED (2026-08-27), recorded before the run.**
  Evidence base (measured from the production store and the two
  qualified revisions, stated in artifact-hash terms): unique
  published-artifact hashes 52,753 (revision A) / 52,783 (revision B)
  / 52,788 (union) — the incremental revision adds 35 unique blobs,
  2,668,760 raw bytes; corpus volume 2.570 GB raw (tei 1.826 GB, mean
  104 KB, max 12.45 MB; plaintext 722 MB, mean 41 KB; tei-validation
  21.5 MB); manifest size is an ESTIMATE (~7–10 MB) until the probe
  measures a real one. Probe sequence: genesis full release +
  incremental release from the two qualified revisions, then up to
  three further real upstream revisions qualified via the F5 oracle;
  stop and report if qualification fails. Budgets: capacity limits M1
  genesis packed size after repack ≤ 2.6 GiB, M4 fresh-full-clone
  received-pack bytes ≤ 2.65 GiB at probe end; owner-selected service
  limits M6 repack wall time ≤ 15 min, M7 repack peak RSS ≤ 4 GiB
  (the run records origin hardware, repack command/configuration,
  thread count, available memory, maintenance window); proportional
  thresholds for ORDINARY upstream releases M2 packed growth and M5
  fetch bytes ≤ new unique raw artifact bytes + 10 MiB overhead
  (full-generation/toolchain rebuilds are genesis-class events
  recorded separately — M2/M5 do not apply); alert (not prohibition)
  M3 > 1,000 loose objects or > 50 MiB per ordinary release before
  maintenance. Definition: clone/fetch bytes = packfile bytes received
  on disk (.git/objects/pack growth) over the real network protocol.
  Origin checks: force-push and deletion protection exercised only on
  a disposable identically-configured branch/repository; object-size
  acceptance = pushing and cloning the real 12.45 MB maximum artifact
  (no synthetic margin); rejected-concurrent-push recovery per §9.
  365 × 10 MiB ≈ 3.6 GiB is a scenario ceiling for ordinary
  upstream-only cadence, not a general annual bound.
- State: main pushed to origin at 48369cea; subsequent documentation
  commits stay local until the next push instruction.

## F12 growth-probe results — 2026-08-27 (tailnet-testable portion)

Executed against the private test origin (Forgejo 16.0.3 at
code.hyakutake-barbel.ts.net) on the disposable repositories
bor/soranoha-f12-probe (growth) and bor/soranoha-f12-protections
(destructive negative checks), with the ratified budgets recorded
beforehand. Real artifacts only: publication chain pre-genesis
31d70bb6 → genesis manifest 2651c070… (revision a1da0f5a00, 17,592
works) → incremental manifest 6533e11e… (revision 0e9ea3e586, 17,602
works), driven by the release CLI end-to-end with a probe rights
policy, probe assessment snapshot (fixture facts, no rights
authority), and the RFC 8032 fixture keys.

Origin checks — ALL PASS: ordinary and fast-forward pushes accepted;
force-push of rewritten history rejected (pre-receive declined) with
apply_to_admins=true, so the owner account cannot bypass; protected-
branch deletion rejected; plain non-fast-forward push refused; the
largest published blob (12,454,646 B tei) pushed, verified at head,
and cloned. Configuration gap found and fixed during setup: Forgejo's
protection default leaves apply_to_admins=false — the production
repository must set it true.

Rejected-concurrent-push reconciliation (§9) — PASS, exercised on the
disposable identically-protected repository bor/soranoha-f12-race
with a fixture-scale chain (the transaction code is size-blind, so
the full corpus was not rerun): two independent clones raced a
governance append; the competitor's complete publication landed
between the racer's fetch and its first push; the racer's
compare-and-swap push returned :rejected; the reconciliation loop
refetched, revalidated the unchanged signed event against the
advanced head, and republished on attempt 2. Independent verification
of the final accepted chain: 3 manifests, both governance events
applied, works and withdrawn sets exactly as expected.

Transport — both named transports exercised against the origin: ssh
(port 63333, all release pushes and the M4 clone) and HTTPS
(smart-HTTP terminated by the front Caddy): fresh full clone over
HTTPS 629,666,943 B received pack in 13 s; genesis-state clone 14 s;
the M5 follower fetch below ran over HTTPS.

Measurements vs budgets — capacity checks PASS; M6/M7 are
publisher-host observations with the origin-host measurement pending:
| # | measured | budget | result |
|---|---|---|---|
| M1 genesis packed (repack -adf) | 630,590,254 B (601 MiB; 24% of the 2.57 GB raw) | ≤ 2.6 GiB | PASS |
| M2 incremental packed growth | 988,658 B (below even the 2,668,760 B raw new bytes) | ≤ raw+10 MiB = 13,154,520 B | PASS |
| M3 loose before maintenance | genesis-class: 53,023 objects (publisher clone); ordinary release: 80 objects follower-side | alert > 1,000 (ordinary) | no alert follower-side; origin-side pending |
| M4 fresh full clone | 629,666,943 B received packfile, 24 s (ssh); byte-identical packfile in 13 s over HTTPS | ≤ 2.65 GiB | PASS |
| M5 follower fetch after one release | 4,576,279 B retained packfile (4,581,083 B pack-dir growth incl. index), fetched over HTTPS with fetch.unpackLimit=1 | ≤ 13,154,520 B | PASS |
| M6 full repack wall | 16.35 s | ≤ 15 min | publisher observation; origin pending |
| M7 repack peak RSS | 1,160,516 KB (1.11 GiB) | ≤ 4 GiB | publisher observation; origin pending |

M5 was first recorded as 7,035,971 B of loose-object-store growth;
that measured Git's post-unpack state, not the ratified quantity
(received packfile bytes on disk), and is superseded by the
retained-pack measurement above. M4 was first recorded as
631,367,363 B; that was total object-directory size (pack plus
locally generated index), superseded by the packfile value above. M6/M7 (and origin-side loose
growth) were measured on the publisher clone at farspark, which
establishes client-side feasibility, not Forgejo-server maintenance
capacity; the origin-host measurement is a pending ONE-SHOT during
the speely deployment — no monitoring framework.

Operational record: repack = `git repack -adf --threads=16`, git
2.54.0, host farspark (Ryzen 9 7950X3D, 32 threads, 94 GB RAM, 70 GB
available), no maintenance window needed at these sizes. The probe
API credential lives outside the workspace under owner-only
permissions (no in-repo secret file, no ignore-entry convention). Real
manifest size measured: 9,095,537 / 9,100,699 B (the earlier 7–10 MB
estimate is retired). Probe sequence stopped at two releases per the
stop rule: no further real upstream revisions are qualifiable from
this host (github.com unreachable; the full local aozorabunko clone
carries no newer revision).

Publication wall time has an algorithmic growth problem — "workable
at daily cadence" is NOT established. Measured 2,918 s (genesis) /
4,642 s (release 2) despite release 2 adding only 35 blobs: the repo
writer launches subprocesses per file (hash-object and update-index
each) and every invocation re-verifies the full chain by walking
every historical manifest with one cat-file per read (~53k
subprocesses per verification pass; on the order of 159k subprocesses
for the genesis invocation and 264k for release 2), so cost grows
with releases × works. No-op/unchanged invocation wall-time bound:
≤ 30 min, measured 1,319 s (22 min) for one unchanged invocation at
the recorded workload of 17,602 works, chain length 2 (build fully
cached, outcome already-published, no push). The trigger has
effectively fired: at releases × works growth, linear extrapolation
puts the next few releases around or beyond the 30-minute bound —
this is a present limit, not a speculative future breach. Correcting
it is a PREREQUISITE for unattended production activation of the
scheduled job; fixture-scale scheduler integration can continue
meanwhile. Correction shape: profile first, then deepen the existing
repository/view boundary with the smallest measured
batching/work-elimination change; any content reuse must still prove
path reachability at each commit. No second verifier, queue, cache
protocol, monitoring, or new service; nothing implemented in this
probe. Public-only remainder (SWH save-code-now acceptance, archival
observation) stays deferred to public exposure.

No-op bound correction implemented 2026-08-27, profile first. The
measured decomposition on the real chain attributed the pass almost
entirely to per-read subprocess spawns: 11.8 ms per read × 52,806
artifact reads per commit × 2 commits ≈ 1,248 s of the 1,370 s
measured full verification pass; hashing is 1.75 ms/MB (≈ 4.5 s per
commit); the writer does not run in a no-op. Two changes, both
inside the existing view/verifier boundary: (1) within one
verification pass, reads are served by a single persistent
`git cat-file --batch` subprocess (view/with-batch) started under
the same --git-dir binding, hardening flags, and sanitized
environment as spawned reads, with the subprocess owned end-to-end
(stderr inherited, stdin closed and clean termination required on
success, destroy on failure, payload framing asserted); (2)
within-pass cross-commit work elimination — a work artifact's
content is verified once per pass, and an older commit reuses the
younger commit's verification only when the younger, already-
verified commit carried the same hex at the same path AND a tree
comparison proves this commit's entry identical, so every commit
still proves each artifact's path reachability in its own tree;
declared lengths are checked on both routes, the reused facts live
only for the one pass, and a regression test pins that a valid head
above a historical commit storing same-length wrong bytes at one
artifact path still fails with blob-hash-mismatch. No cross-pass
state, no second verifier, no service.

Measured after, on the real chain: full verification pass 15.4 s
(26.1 s with batching alone; 1,370 s before), historical-commit
marginal ≈ 2.4 s. End-to-end unchanged invocation through the REAL
nix wrapper: 30 s steady-state (40 s with first nix evaluation),
outcome already-published — the earlier direct-CLI qualification
rested on a false premise: the toolchain identity hashes the Clojure
runtime, dependency cache, and deps.edn, NOT kernel source, so the
rebuilt wrapper still supplies clj-nix-6b66028f… and the hermetic
unchanged invocation is measurable now. Year-one chain-length cost
MEASURED 2026-08-27, retiring the projection. Construction: the
disposable probe chain (bor/soranoha-f12-probe, branch main) was
extended from head commit
b265c1505b1f9a055ea047c3821485200af09a8c to
4f06199716fadebd17426788e81aebdc7171d035 — 366 first-parent commits
(zero state + 365 release commits), one real withdrawal of a head
work plus 362 linear event-amendments, every manifest a real
full-corpus manifest (17,601 works after the withdrawal; 365
manifest files, 3,321,645,792 B logical), every successor built by
the transaction's own successor construction and signed with the
fixture keys; head manifest
78c04aac9438426b515fc58b01bc0b940cc6f10058070bca872ea31756ad3190.
This is a GOVERNANCE-HEAVY, full-manifest chain-LENGTH measurement,
not 365 independent releases: by construction the amendments
inherit the same assessment snapshot and admission report, so this
chain's per-commit distinct content is the manifest and event;
per-release distinct-content cost is evidenced separately by the
two REAL releases' measured historical marginal (≈ 2.4 s/commit),
which this chain's observed marginal matches
((806.8 − 13) / 364 ≈ 2.2 s/commit). Measurements, in run order on
the same persistent clone: (1) full verification pass at chain
length 365 immediately after crafting, timed in-process
(System/nanoTime around verify-repository-at): 806.8 s, with the
365 crafted manifests still LOOSE objects; (2) the script's
end-of-run fetch then auto-packed them — object store after: base
pack 628,680,029 B + new pack 41,555,798 B, zero loose; (3)
end-to-end unchanged invocation via the same recorded release-CLI
command (concurrency 16) through the REAL nix wrapper, GNU time
wall clock, fresh JVM, page cache warm from the preceding verify,
kernel derivation rebuilt by nix during the run: 758.3 s, outcome
already-published against the head manifest above, peak RSS
4.7 GiB. The 758 s figure is NOT conservative: it is faster than
the standalone 806.8 s verification, i.e. the packed store and warm
cache outweighed the rebuild. Both observations sit under the
≤ 30 min bound with over 2.2× headroom — the bound is met at the
year-one workload. It is still projected to be reached around chain
length ≈ 700; the response then is REMEASURE, then choose cadence
or a verifier improvement — v1's append-only chain back to genesis
with permanent manifest/blob roots means pruning history cannot
reduce verification cost, and a chain rollover would be a separate
protocol/trust decision, not retention. No monitoring added. 72
tests / 453 assertions pass.
