# Publication Rearchitecture — Design Ledger

Status: **Direction ratified; Slice 0 ready; release-architecture protocol
decisions closed by external-review amendments D16–D20 (2026-08-24, pending
owner veto).** Design is NOT declared complete until the slice-1 double-build
confirms R4 and the manifest-v1 freeze (D16) is exercised against a real
build. Authoritative record: decision log + review findings + external
review F1–F9 + dev handoff below.
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
| D3 | Identifier shape: per-artifact content id (machine layer) + human release tags; releases possibly multiple/day. Proposed: cheap tags per release + periodic DOI snapshots for citation — **tag grammar & DOI cadence still open (Q2)**. | owner "recommendation OK probably" | open detail |
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
| D16 | **Manifest v1 frozen before slice 1** (external review F3): canonicalizer = `rfc8785-safe-integer-json-string-v1` (legacy c14n-v0 excluded from kernel); strict schema `snh-manifest/1`; detached signature; derived release name; type registry {tei,txt,val,manifest}; full spec in F3 section. | review-adopted 2026-08-24, pending owner veto | schema rev possible pre-first-release; frozen after |
| D17 | **Publication = atomic compare-and-append transaction** (F1): flock + head re-check + complete-write + atomic HEAD advance; idempotent by manifest_id; CI concurrency is optimization only. | review-adopted 2026-08-24 | internal protocol, revisable |
| D18 | **Retention/lifecycle** (F2): all published manifests are permanent GC roots; states built→published→archived→citable; public git repo carries published artifact bytes (SWH archives real bytes); archival receipts in subsequent manifests; indefinite promise advertised only at ≥ archived. | review-adopted 2026-08-24, citable-lag is product intent — owner may veto | promise text frozen at first public release |
| D19 | **Trust** (F8): offline root key → key-manifest → operational signing keys; revocation procedure; fork/equivocation consumer rule anchored in SWH-archived checkpoint history. | review-adopted 2026-08-24 | upgradeable to Tessera log |
| D20 | **Ownership** (F6): TEI profile schemas consumed as explicit flake input from abc during migration (no schema copies); AGENTS.md ownership transfer recorded before slice-1 implementation. | review-adopted 2026-08-24 | owner action: AGENTS.md edit |

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
- Q6 Where TEI-profile/schema versions sit in the derivation key (a profile
  change invalidates all TEI artifacts — acceptable? presumably yes, few min).
- Q7 On-demand regeneration guarantees: how long are old toolchains kept
  runnable (Nix closure rot), and is that promise ever made publicly?

### Identifier side (researched; sources in agent report)

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
- R5 **Mitigated (rule)** — concurrent CI runs could fork the manifest hash
  chain. Rule: release assembly+publish serialized (Forgejo concurrency
  group); build stages may run concurrently (SQLite WAL + atomic blob
  writes are safe under concurrency).
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
- R9 **Mitigated** — upstream malformed revision → fail closed, no release,
  CI alert; SWH/Zenodo pushes non-blocking (retry next run; release
  validity never depends on them); disk/fixity health via verifier sweep.
  Observability = CI logs + release chain + verifier reports.
- R10 **Accepted** — single-operator bus factor; structurally mitigated by
  public git manifests, mirrors, SWH (corpus survives the operator).
- R11 **Mitigated** — signing keys in Forgejo secrets; fingerprint published;
  rotation = new key signed by old, recorded in manifest chain.

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
via the published paper/docs fingerprint). Equivocation: consumer rule —
a valid chain must match the checkpoint history in the SWH-archived public
repo; conflicting signed heads = integrity incident, published as such.
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
- Transparency: research.swtch.com/tlog; c2sp.org/tlog-tiles; github.com/transparency-dev/tessera
- Zenodo versioning & limits: zenodo.org/help/versioning; support.zenodo.org (50 GB/100-file caps)
- Identifier guidance: datatracker.ietf.org/doc/html/draft-kunze-ark-42; w3.org/Provider/Style/URI; w3.org/2001/tag/doc/metaDataInURI-31.html; datacite.org/blog/cool-dois/; McMurry et al. 10.1371/journal.pbio.2001414; RDA dynamic data 10.15497/RDA00016
- Forgejo concurrency best-effort: forgejo.org/docs/v15.0/user/actions/reference/
- w3id redirect-only: github.com/perma-id/w3id.org

## Dev Handoff (2026-08-24; slices 0–2 amended by F4/F5/F7, D16–D18)

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

The manifest emitted in this slice is **manifest v1 per D16** (frozen before
implementation; not a slice-2 invention). TEI profile artifacts consumed as
flake input from abc per D20.

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
deletion, withdrawal (R8 path), output-preserving source edit, and the R7
include-and-flag path with an invalid work.

### Slice 3 — za publishing + CI
Static tree (blobs/, releases/, per-work history.json), signed tags,
Forgejo auto-release (serialized publish per R5; poll upstream), SWH
save-code-now (non-blocking per R9). Acceptance: two consecutive automated
releases from real upstream movement, chain verifies end-to-end with the
published checker script.

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
- Manifest v1 JSON Schema file authored from the D16/F3 freeze — dev,
  before slice 1 (mechanical transcription, no open semantics).
- Tokenizer lane (vibrato-pipe) identity design — owner, post-JADH2026 (D4).
- Zenodo record metadata + first snapshot timing — owner, slice 4.
- w3id.org PR — owner, slice 4.
- Forgejo runner sizing/secrets — owner, before slice 3.
- Q7 (public on-demand-regeneration promise): resolved by D2 — archive is
  the contract; regeneration stays best-effort and unadvertised.

## Prior art findings (2026-08-24)

### Build-system side (researched; sources in agent report)

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

### Archival storage & verification side (researched; sources in agent report)

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
