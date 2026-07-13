# Comment Standards

Conventions for source-comment content in `ab-validator/` (Rust) and
`abc/src/` (Clojure). These are coding standards, not architecture
decisions — hence a convention doc, not an ADR. Enforced by
`scripts/comment-hygiene-check.sh`.

## Allowed external references

- Code symbols (functions, types, fields) documented for the reader.
- Specific ADRs (e.g., `ADR 0037`).
- Upstream/external specifications: JIS, Unicode, TEI, IIIF, RDF, SHACL,
  JSON-LD, aozora.gr.jp, and `U+XXXX` codepoint notation.

## Not allowed

- The project's own issue-tracker numbers (`#NNN`). One-off references
  are rewritten as self-contained prose.
- Handoff documents, dated specs/reports (`docs/handoffs/…`,
  `docs/superpowers/{specs,reports}/…`).
- Project-management labels: `Task N`, `Plan G.4`, `plan amendment`,
  `Plan Blocker`, transient `Phase` labels (`Phase-C`, `Phase-3 Lane B`,
  `Phase 1.2`, `Phase-0 responsibility`, …), transient `Tier` labels.
- `TODO` markers — convert to a factual limitation statement instead.
- Speculative/forward-looking filler ("revisit", "eventually export",
  "a later task", "someday").

## Retained named invariants

Six issue numbers name durable, cross-file semantic invariants and are
kept as stable identifiers (they behave like the "Tier-A canary"
invariant name). They are defined in [`glossary.md`](glossary.md):
`#78`, `#228`, `#331`, `#333`, `#384`, `#435`. The "Tier-A canary"
invariant name is likewise retained.

## Retained algorithm-stage markers

Comments of the form `// --- Phase N: <description> ---` (and the
`Phase 0: …` marker in `ab-aozora-pipeline`'s `pipeline.rs`) describe a
function's own internal processing stages, not project phases. They stay.
The canonical example is `ab-aat-to-parser-ir/src/sentences.rs`.

## Program output is out of scope

String literals emitted as program output (e.g. the ablation report
headers in `ab-ortho-detect-ml/src/main.rs`) are not comments. The
cleanup does not rewrite them, and the hygiene check excludes them.

## UNSTABLE / stability posture

Per-item `UNSTABLE` markers are not used. A single crate- or
module-level `# Stability` section states the pre-1.0 semver posture once.
