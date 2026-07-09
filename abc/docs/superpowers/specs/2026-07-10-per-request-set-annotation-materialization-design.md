# Per-Request-Set Annotation Materialization + Corpus-Scale Ruby Join Statistics — Design

Date: 2026-07-10
Status: Draft (hammock pass done; slice A settled, slice B carries two named
provisional items to be settled by bounded probes during implementation)
Depends on: ADR 0028 (first slice + D6 widening + input-view coverage
machine-check), ADR 0026/0027 (identity discipline), ADR 0024/0025
(span/render discipline)

## Problem

Two gaps, one project:

1. **A request set can declare the annotation input view but nothing
   materializes annotation artifacts per request set.** `demo-annotation-ja`
   resolves with a `parser-ir-body-annotations-v1` view, and the input-view
   coverage machine-check explicitly names "the annotation materializer
   consumes annotation views directly" as the reason the view is legitimate —
   yet `abc.tools.materialize-annotations/materialize-annotations!` is
   reachable only from the fixture-driven design-bundle gate
   (`validate_design_bundle.clj:1028`). The batch loop
   (`soranoha/materialize-entry!`, `soranoha.clj:455`) wires parser-IR,
   publication, and analysis materializers but not annotations. The
   annotation release guardrails (`manifest_index.clj:302-357`) likewise run
   only in the design-bundle gate.
2. **ADR 0028's deferred questions D7 (span_preservation) and D9
   (exploratory provenance floor) are waiting on real-pipeline corpus data.**
   The 2026-07-09 probe's statistics (67.8% whole-token alignment, 32.2%
   straddle, 92.6% of straddles stem-ruby) were computed with throwaway
   Aozora-markup stripping and MeCab — explicitly *not* the real
   adapter → AAT → parser-IR → renderer pipeline, and not the pinned
   Vibrato/Sudachi runners the flake now exposes
   (`docs/handoffs/ruby-annotation-probe-2026-07-09.md`, its own caveat).

## Evidence (what exists, verified 2026-07-10)

- Batch loop: `soranoha reproduce!` → `materialize-snapshot-root!`
  (`soranoha.clj:483-530`) fans out per work via snapshot plans; committed
  plan `data/snapshot-plans/demo-basic-ja.json` drives the 2-work fixture
  chain (`examples/v0/example-work/parser-ir.json` for both works);
  source-snapshot-driven request sets get `generated-snapshot-plan`
  (`soranoha.clj:159`). Deterministic `generated_at` comes from the plan.
  The loop is stateless: `delete-tree!` + full rebuild, no staleness state.
- Annotation materializer: pure in-memory fn taking
  `{producer-manifest parser-ir annotation-policy input-plaintext-policy-hash
  output-dir generated-at}` and writing `body-annotations.json` +
  `annotation.manifest.json` (`materialize_annotations.clj:85`). Spans come
  from `parser-ir-plaintext/render-with-annotations` (unicode-scalar,
  ruby + gaiji kinds).
- Policy registry: `data/annotation-policies/ruby-gaiji-v1.json`;
  `analysis-identity/annotation-policy-hash` = JCS SHA-256; the request-set
  annotation view carries `{input_view_kind, policy_hash}` only (D6 two-key
  shape) — no policy id in identity.
- The policy self-describes its alignment: `"aligns_to":
  "parser-ir-plaintext-body-v1"`, and the annotation output schema requires
  `input_plaintext_policy_hash`.
- Corpus-scale inputs for statistics: full-corpus AAT dumps exist on `/db`
  (`/db/ab-validator/aat-corpus/aozora-full-*`); `ab-aat-to-parser-ir` is a
  nix-pinned CLI (`ab-validator/flake.nix:1470`); pinned tokenizer runners
  `vibrato-tokenize`/`sudachi` with UniDic-2512 dictionaries (incl.
  `unidic-novel`) are flake apps; 57-work × 5-adapter parser-IR rows exist at
  `/db/ab-validator/parser-ir/publication-bundle-full-current-20260706/rows/`
  (real but selection-biased; kept as a cross-check, not the sample).
- The join primitive (`annotation_join.clj`) is deliberately "a generated
  view; never a canonical artifact" (ADR 0028).

## Non-goals

- No annotation-consuming analysis recipes and no
  `analysis-recipe.schema.json` enum widening (deferred with the first such
  recipe; see ADR 0028 status).
- No reading-agreement statistics in v1 (needs kana-normalization machinery;
  D7/D9 need span/classification data, not reading agreement).
- No skip-recompute identity wiring for the statistics run (one-shot view
  over pinned inputs; record a follow-up only if reruns become routine).
- No manifest-index validator unification for analysis/tokenized kinds in
  the batch path (recorded as a parity gap, out of scope).
- No changes to manifest schema, request-set schema, or any identity object
  shape. The ONE schema that rotates in slice A is
  `snapshot-index.schema.json` (see A5) — nothing else.

## Slice A — per-request-set annotation materialization

**A1. Trigger.** `materialize-entry!` materializes annotation artifacts
when the resolved request set's `request_set_identity_object.input_views`
contain an input view of kind `parser-ir-body-annotations-v1`. Zero such
views → step skipped (all existing request sets except
`demo-annotation-ja`). Exactly one is supported in v1. More than one
(distinct policy hashes are representable, and ADR 0028 D1 would give them
distinct `artifact_id`s) → fail closed with a named error: the per-work
*file layout* and snapshot-reference semantics for multiple annotation
views are deliberately undecided until a real request set needs them —
inventing a layout now would be speculation.

**A2. Policy resolution — by content hash, once per run, fail closed.** The
identity view carries only `policy_hash`. Resolution scans the
`data/annotation-policies/*.json` registry, computes
`annotation-policy-hash` for each file, and selects the unique match.
Zero matches → throw (`"Unknown annotation policy hash"` with the hash and
registry dir); more than one *file* matching → throw (duplicate registry
entries signal registry corruption even though the values are identical).
Resolution happens ONCE per run in `materialize-snapshot-root!` (policy
resolution is a request-set-level concern), and the resolved
`{view, policy}` pairs are passed down to `materialize-entry!` as plain
values — never re-scanned per work. Rationale: the D6 two-key identity is
already canonical; the registry is the natural resolution surface; this
mirrors recipe resolution while respecting that annotation views are
hash-keyed, and it costs no schema rotation.
*Alternative rejected for now:* adding `annotation_policy_ids` +
`resolved_annotation_policy_labels` to definitions/resolved request sets
(request-set schema v0.1.5 rotation + six-fixture churn) — revisit when an
audit-label need actually appears.

**A3. Aligned-view requirement — enforced at materialization time, fail
closed on ambiguity.** The policy's `aligns_to` names the view kind whose
policy hash becomes `input_plaintext_policy_hash`. The per-run resolution
step looks up the input views of that kind in the same request set:
exactly one → take its `policy_hash`; zero → throw (annotation view without
its aligned view); more than one (distinct policy hashes are representable
in identity) → throw (ambiguous alignment). This is deliberately *not* an
identity/resolve-time rule: the requirement is a property of the specific
policy (a future annotation family could align to something else), so the
resolver stays policy-agnostic and the coverage machine-check stays as-is.
*Verification point (do not assume):* the design-bundle gate already passes
an `input-plaintext-policy-hash` to `materialize-annotations!`
(`validate_design_bundle.clj:1028`); the implementer must confirm the batch
path's value (the plaintext view's `policy_hash`) is semantically the same
quantity, so the fixture gate and the batch path cannot disagree silently.

**A4. Producer and layout.** The producer is the per-work parser-IR artifact
written earlier in the same `materialize-entry!` call (its manifest file and
parser-IR value are already in scope). Outputs land at
`<root>/artifacts/works/<slug>/annotations/body-annotations.json` +
`annotation.manifest.json`, parallel to `analysis/`. The annotation manifest
file joins the loose manifest-reference list that feeds
`snapshot-index.json`.

**A5. Layout-policy/kind plumbing — the one schema rotation.**
`snapshot-index.schema.json` hard-enums artifact kinds in two places:
`layoutPolicy.loose_artifact_kinds` items (`["tei","plaintext"]`) and
`artifactReference.artifact_kind`. Both get `"annotation"`; schema version
0.1.0 → 0.1.1. `snapshot_index.clj` itself passes kinds through (no code
enum). Ripple (the schema-contracts drill, per project memory): regenerate
`schemas/schema-contracts.json` via the abc wrapper `--write`; byte-copy
the schema into ab-validator's `nix-schemas/` mirror; regenerate
`ab-validator/data/abc-schemas/schema-contracts.json` via its own
`--write`; verify BOTH drift checks by actually building them
(`nix build .#checks.x86_64-linux.schema-contract-drift --no-link` in each
flake — `--no-build` is blind to this class of breakage). The committed
example `examples/v0/snapshot/snapshot-index.json` stays untouched: it is
gated on *internal* hash consistency only (verified: no test compares its
embedded `snapshot_index_schema_hash` to the live schema file), and
regenerating it has no producing tool committed. `"annotation"` is then
added to the demo plan's `loose_artifact_kinds`; the generated-plan layout
policy gains it too. The end-to-end test (A7) proves the plumbing is
complete — any missed enum fails there.

**A6. Release guardrails move into the batch path for annotations.** After
the per-work loop, `materialize-snapshot-root!` runs a distinct, named
validation step (e.g. `validate-annotation-manifests!`) that indexes the
produced manifest files (`manifest-index/index-manifest-files`) and applies
`validate-annotation-release-guardrail!` +
`validate-annotation-copied-fields!`. It is a separate step alongside the
existing reference validation — not braided into `materialize-entry!` — so
it stays cheap to widen to other artifact kinds later. Rationale: this is
the first artifact kind whose *batch* entry postdates the guardrails;
wiring them costs one call and prevents releasing annotation manifests that
the design-bundle gate would reject. The analysis/tokenized parity gap is
recorded (Non-goals), not expanded here.

**A7. Test vehicle.** A committed snapshot plan
`data/snapshot-plans/demo-annotation-ja.json` mirroring `demo-basic-ja`'s
(same fixture parser-IR for both works, deterministic `generated_at`).
End-to-end test drives `materialize-snapshot-root!` for
`demo-annotation-ja` into a temp root and asserts: annotation manifests
exist per work and validate against `manifest.schema.json`; sidecars
validate against `annotation-output.schema.json`;
`annotation_policy_hash` equals the ruby-gaiji-v1 hash;
`input_plaintext_policy_hash` equals the plaintext view's `policy_hash`;
copied parser-IR fields match the producer; snapshot-index references
include the annotation manifests; both guardrail validators pass; and
`demo-basic-ja` (no annotation view) produces no `annotations/` dir.
Unit tests cover A2's fail-closed cases and A3's missing-aligned-view case.

**A8. State/time/identity.** No new state (full-rebuild loop unchanged); no
new time source (`generated_at` from the plan); no identity changes (the
annotation identity object is ADR 0028 D1 as built; `request_set_id` is
untouched — the annotation view was already in identity).

## Slice B — corpus-scale ruby join statistics (renderer-exact)

**B1. Sample.** A deterministic stride sample of ruby-bearing works from the
pinned Aozora corpus, mirroring the probe's methodology so numbers are
comparable, but N = 200 (probe used 80). Pipeline per work: AAT from an
existing full dump on `/db` (or `ab-check` if the dump lacks the work) →
`ab-aat-to-parser-ir` (nix-pinned) → parser-IR. *Why not the 57-work
publication bundle:* selection-biased (publication-validation works) and
5-adapter redundant; it serves as a cross-check only.

**B2. Extraction.** `render-with-annotations` on each parser-IR — the real
renderer, the real plaintext view, unicode-scalar spans. This is the
fidelity upgrade over the probe.

**B3. Tokenization.** Pinned `vibrato-tokenize` with `unidic-novel` (probe's
primary dictionary, ADR 0027 candidate profile). Token spans are
reconstructed by walking surface forms over the rendered plaintext
(scalar-count based); reconstruction failure for a work is recorded, not
papered over. Reconstructed spans are sorted and non-overlapping *by
construction* (contiguous segmentation walked left-to-right), which is
exactly `annotation-join/join`'s documented precondition.
*Token handoff contract (abc owns it):* one file per work
(`<work-id>.tokens.jsonl`), one JSON object per token per line, in text
order; required field `"surface"` (string); extra fields permitted and
ignored by v1. The wrapper script (thin, tokenizer-side) produces this; the
join/stats tool consumes only this — the tool never invokes tokenizers or
any ab-validator binary itself (no cross-project process coupling inside
the stats tool).
*Provisional (probe during implementation):* exact `vibrato-tokenize`
output format/flags; whether whitespace/newline handling needs
normalization before span walking.

**B4. Statistics v1.** Per-work and aggregate: annotation counts by kind
(ruby/gaiji); join classification counts and rates (aligned-single,
aligned-multi, stem-prefix, conflict); degenerate/failed-span counts;
works with reconstruction failures. One comparability table against the
probe's numbers. Reading agreement is v2 (Non-goals).

**B5. Tooling and outputs.** A new `soranoha annotation-join-stats` command
(or `clojure -M:abc/annotation-join-stats`) taking
`--parser-ir-dir/--work-list`, `--tokens-dir`, `--out-dir`; it writes
per-work JSONL + `aggregate.json` + `report.md` to the operator-supplied
out-dir (convention: under `/db`, sibling of other corpus reports — large
data never in-repo). Sample selection and AAT→parser-IR conversion are
*operator-side* steps (nix-pinned `ab-aat-to-parser-ir` invocations,
recorded verbatim in the handoff) — deliberately outside the abc tool's
boundary. A committed handoff
`abc/docs/handoffs/annotation-join-stats-<date>.md` records run inputs
(dump id, store paths, N, dictionary), the exact operator-side commands,
headline numbers, and the proposed D7/D9 dispositions. The join output
remains a view: no manifests, no identity objects (ADR 0028).

**B6. What this settles.** The handoff presents evidence for: D7
(span_preservation flag — is renderer-exact span survival good enough that
a preservation assertion is checkable?) and D9 (provenance floor for
exploratory joins). The ADR update itself is a separate, human-ratified
step; this slice produces the evidence, not the ruling.

## Execution order

Slice A first (self-contained, TDD, in-repo). Slice B tooling second (the
join/stats CLI + wrapper are in-repo and testable on fixtures); the actual
corpus run + handoff last, executed once the tooling merges, with its two
provisional items settled by bounded probes (a single-work vibrato run; a
single-work AAT→parser-IR conversion) before the full N=200 run.

## Acceptance Criteria

- `materialize-snapshot-root!` on `demo-annotation-ja` produces per-work
  annotation artifacts that pass both annotation guardrail validators:
  `test/abc/tools/soranoha_annotation_test.clj` (new).
- Policy-resolution and aligned-view failures fail closed:
  same test ns, unit cases.
- Existing request sets are unaffected (no `annotations/` dir for
  `demo-basic-ja`; full suite + design bundle green).
- Join/stats tool computes classification statistics on the committed
  fixture chain: `test/abc/tools/annotation_join_stats_test.clj` (new).
- Corpus run handoff committed with headline numbers and D7/D9 evidence:
  `docs/handoffs/` (post-merge step).
