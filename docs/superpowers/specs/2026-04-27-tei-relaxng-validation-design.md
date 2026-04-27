# TEI P5 RelaxNG Validation — Design

Status: Approved
Date: 2026-04-27
Source milestone: `docs/next-steps.md` candidate "TEI profile + validation pipeline" (now scoped to validation-only; ODD promotion deferred).

## Goal

Validate every TEI document carried or produced by the v0 contract harness against the upstream TEI P5 RelaxNG schema, using Jing in-process. Pin the schema via Nix.

## Why Jing rather than xmllint

The architecture note (`docs/high-level-architecture-note.md` §"TEI and XML Profile") establishes Jing as the compatibility baseline for TEI validation. Jing is the reference RelaxNG implementation, has full coverage of RelaxNG patterns used by `tei_all.rng`, and produces structured locator-rich error reports. `xmllint`'s libxml2 RelaxNG implementation has known gaps on uncommon patterns and emits less actionable diagnostics. Since the harness already runs on the JVM (Clojure), an in-process Jing validator costs nothing in extra runtime overhead, mirrors the `abc.tools.shacl` design (Jena in-process, structured violations), and avoids subprocess-output parsing.

## Non-Goals

- No ODD-driven schema customization in this milestone. `schemas/tei-profile.odd` stays a stub; future work can use `roma`/`teiroma` to generate a project-specific subset.
- No Schematron rules. Jing supports embedded Schematron, but `tei_all.rng` doesn't ship them in the v0 use case; revisit when an ODD with `<constraintSpec>` exists.
- No replacement of the existing `xmllint --noout` well-formedness check on the ODD stub. That's a sanity check for the ODD's own XML well-formedness, separate from validating TEI documents.
- No metadata/`<teiHeader>` content modeling. Sub-project B handles that; A only validates whatever TEI is present.
- No additional TEI fixtures beyond the existing `examples/v0/example-work/tei.xml`. Materialized TEI artifacts will be added when downstream pipeline stages exist.

## Architecture

Three small units; the pattern follows `abc.tools.shacl`.

### `abc.tools.tei` (new)

Thin Jing wrapper. No project knowledge beyond loading a RelaxNG file and validating an XML file.

API:

- `load-schema [path]` — reads the RelaxNG schema at `path` and returns a Jing `com.thaiopensource.validate.Schema`. Loaded once per harness run.
- `validate! {:keys [schema xml-path label]}` — runs Jing against the XML file. Returns `:ok` on conformance; on non-conformance throws `ex-info` with `:errors` set to a vector of structured violation maps and `:label` echoed back.

Violation map shape:

```clojure
{:label   "examples/v0/example-work/tei.xml"
 :severity "error"   ; or "fatal" / "warning" depending on Jing's classification
 :line     18
 :column   17
 :message  "element charName: not allowed here ..."}
```

All violations from one validation are aggregated before throwing — no fail-fast. Errors are collected via a Jing `ErrorHandler` (we implement one that pushes to a Clojure atom).

### Schema source via Nix flake

Add to `flake.nix`:

```nix
tei-schema = pkgs.fetchurl {
  url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng";
  hash = "sha256-7MSfAMN/SQtd9xa2cuPbpjXrHknM2I+5aYWUmN9CwIQ=";
};
```

Hash verified at design time against TEI P5 Release 4.11.0 (published 2026-02-18). The URL serves the current release; the pinned hash is the durable identity.

### Wrapper-injected env var

Extend the `validate-design-bundle` app's shell-script wrapper in `flake.nix` to export `TEI_SCHEMA_PATH=${tei-schema}` alongside the existing `PATH` exports. The Clojure side reads `TEI_SCHEMA_PATH` and fails loudly when unset, with a clear "set TEI_SCHEMA_PATH or run via `nix run .#validate-design-bundle`" message — this gives local-dev users (without Nix) actionable feedback instead of silent skipping.

### `abc.tools.validate-design-bundle` (wiring)

Add a new step between the existing `==> Checking XML fixtures` (well-formedness on the ODD) and `==> Checking git-cliff configuration`:

```
==> Validating TEI against P5 RelaxNG
tei rng validation ok
```

Process:

1. Read `TEI_SCHEMA_PATH` env var; fail loudly if missing.
2. Load schema once via `tei/load-schema`.
3. Validate each TEI document. v0 set: `examples/v0/example-work/tei.xml`. The validator function takes a seq of paths so future TEI artifacts slot in cleanly.
4. Aggregate violations and surface via the existing `check-errors!` printer (which already handles `:errors` vectors of strings — we render here, mirroring the SHACL pass).

### Fixture fix

The current `examples/v0/example-work/tei.xml` contains `<charName>` inside `<char>`. TEI 4.11.0 does not define `<charName>` (likely intended `<localName>`, but the existing `<desc>` already covers the human-readable description). Drop the `<charName>` line. Verify the rest of the document — `<ruby>/<rb>/<rt>`, `<g ref="..."/>`, etc. — passes Jing.

## Data Flow

```
TEI XML path
  → jing/load-schema (cached per run)
  → tei/validate! schema xml-path
    ↳ Jing parses and validates with our ErrorHandler
    ↳ ErrorHandler collects violations into an atom
  → :ok | throw ex-info {:errors [violation-maps] :label ...}
                                            → rendered by validate-design-bundle
```

## Test Plan

### `test/abc/tools/tei_test.clj` (new)

Positive:

- `validate!` returns `:ok` for `examples/v0/example-work/tei.xml` against `tei_all.rng` (test reads `TEI_SCHEMA_PATH`; if unset, the test skips with a clear message — matching the harness behavior).

Negative (constructed in-test, no temp files needed if Jing accepts a string source; if it requires a file, write a temp file):

- A TEI document missing the required `<teiHeader>` triggers a violation; `:errors` non-empty; at least one entry's `:message` references `teiHeader`.
- A TEI document with an undefined element (`<charName>`-style) triggers a violation that mentions the offending element name.

### `test/abc/tools/validate_design_bundle_test.clj` (extension)

A new smoke test analogous to `validate-shacl-smoke-test`: loads `TEI_SCHEMA_PATH` (skip if unset), runs `validate!` on the example fixture, asserts `:ok`. Mirrors the SHACL smoke test in style.

## Dependencies

- Add `org.relaxng/jing {:mvn/version "20241231"}` to `deps.edn` and `nix/clj-nix-deps.edn`. Verified current at design time on Maven Central; if a newer release exists at implementation time, prefer it.
- Regenerate `deps-lock.json` via `bin/update-clj-nix-lock`.
- The `pkgs.fetchurl` derivation lands `tei_all.rng` in the Nix store at a deterministic path; no project-relative file change for the schema bytes.

## Error Handling

- Jing emits warnings, errors, and fatal errors via `ErrorHandler` callbacks. We collect all three into the violation vector with severity tagged so the renderer can prefix accordingly.
- Per-document validation is binary: either `:ok` or throw with all violations aggregated.
- The harness aggregates violations across all TEI documents in one pass before failing the run.

## Acceptance Criteria

- `clojure -M:test` passes including the new `tei-test` and the extended `validate_design_bundle_test`.
- `nix run .#validate-design-bundle` prints the new `==> Validating TEI against P5 RelaxNG` step and exits 0; `examples/v0/example-work/tei.xml` validates clean.
- `nix flake check` evaluates and the focused-test check passes with the new dependency.
- `clojure -M:abc/validate-design-bundle` outside Nix without `TEI_SCHEMA_PATH` exits 1 with a message naming the missing env var, not a silent skip.
- Restoring the original `<charName>` line in the example fixture causes the new TEI step to fail with a violation referencing the offending element. (Proves the new check is enforced, not skipped.)

## Sequencing

1. Add `org.relaxng/jing` dep, regenerate clj-nix lock.
2. Add `tei-schema` `pkgs.fetchurl` derivation in `flake.nix` and wire `TEI_SCHEMA_PATH` into the `validate-design-bundle` wrapper script.
3. TDD: positive test for `tei/load-schema` → minimal namespace.
4. TDD: positive then negative tests for `tei/validate!` → wrapper around Jing's `ValidationDriver` with custom `ErrorHandler`.
5. Fix `examples/v0/example-work/tei.xml` (remove `<charName>`) and confirm Jing passes.
6. Wire new step into `validate-design-bundle`. Add smoke test in `validate_design_bundle_test`.
7. Update `flake.nix` `contract-surface` check + focused-test alias to include the new files.
8. End-to-end verification: `nix run .#validate-design-bundle`, `nix flake check`.

## References

- Apache/W3C RelaxNG; Jing reference impl — `https://relaxng.org/jclark/jing.html`
- TEI P5 4.11.0 release — `https://github.com/TEIC/TEI/releases/tag/P5_Release_4.11.0`
- Upstream RelaxNG schema — `https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng`
- `docs/high-level-architecture-note.md` §"TEI and XML Profile"
- `src/abc/tools/shacl.clj` — pattern reference for in-process JVM validator with structured violations
