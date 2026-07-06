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

- `load-schema [path]` — reads the RelaxNG schema at `path` and returns a Jing `com.thaiopensource.validate.Schema`. **Pure function**; the caller holds the returned value and reuses it. The "load once per harness run" claim lives in the harness wiring (where the binding holds the schema for the loop), not in this namespace.
- `validate! {:keys [schema xml-path label]}` — instantiates a fresh `ValidationDriver` from the schema, runs it on `xml-path`, and returns a map `{:label ..., :violations [...]}`. **Does not throw.** Severity classification is preserved on each violation; the caller decides whether warnings should fail the run.

Violation map shape:

```clojure
{:severity :error    ; one of :warning, :error, :fatal
 :line     18
 :column   17
 :message  "element charName: not allowed here ..."}
```

Aggregation is per-file: all warnings, errors, and fatals from one document end up in `:violations` in the order Jing emits them. No fail-fast.

**Concurrency.** Jing's `ValidationDriver` is not thread-safe; we instantiate one per call to `validate!` and the `ErrorHandler` writes to a fresh `volatile!` (not an atom — single-thread access during one validate call) used only inside that call. The harness validates files **sequentially**; if parallel validation is wanted later, each thread builds its own driver from the shared `Schema`.

**Memory.** `tei_all.rng` is ~1 MB on disk; the parsed `Schema` object will be larger but constant per harness run. The harness already keeps Jena/Aristotle graphs in heap. Real cost is real but small at v0 scale; no lazy-load strategy is needed yet. Re-evaluate if/when materialized TEI artifacts grow into the hundreds.

### Schema source via Nix flake

Add to `flake.nix`:

```nix
tei-schema = pkgs.fetchurl {
  url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng";
  hash = "sha256-7MSfAMN/SQtd9xa2cuPbpjXrHknM2I+5aYWUmN9CwIQ=";
};
```

Hash verified at design time against TEI P5 Release 4.11.0 (published 2026-02-18). The URL is unversioned (serves "current"); the pinned `hash` is the durable identity. If TEI ships a new release upstream, `pkgs.fetchurl` fails with a hash mismatch — which is the standard Nix forcing function for a deliberate version bump, not a fault. We treat that as a feature: drift is impossible without an explicit code change.

**Schema refresh policy.** TEI publishes roughly quarterly. A bump is a deliberate workflow:

1. Update the URL hash (and any URL change) in `flake.nix`.
2. Re-run `nix run .#validate-design-bundle` and `nix flake check`.
3. If the example fixture starts failing, fix the fixture and document the spec/release we moved to in the commit message.

This v0 milestone does not automate refresh; an item in `docs/next-steps.md` can track it once it becomes painful.

### Wrapper-injected env var

Extend the `validate-design-bundle` app's shell-script wrapper in `flake.nix` to export `TEI_SCHEMA_PATH=${tei-schema}` alongside the existing `PATH` exports. The Clojure side reads `TEI_SCHEMA_PATH` and fails loudly when unset, with a clear "set TEI_SCHEMA_PATH or run via `nix run .#validate-design-bundle`" message — this gives local-dev users (without Nix) actionable feedback instead of silent skipping.

### `abc.tools.validate-design-bundle` (wiring)

Add a new step between the existing `==> Checking XML fixtures` (well-formedness on the ODD) and `==> Checking git-cliff configuration`:

```
==> Validating TEI against P5 RelaxNG
tei rng validation ok
```

Process:

1. Read `TEI_SCHEMA_PATH` env var. If missing, throw `ex-info` with a clear message naming the variable and the canonical `nix run` invocation. The harness's existing `-main` printer surfaces this and exits non-zero.
2. Load the schema **once** here — `(let [schema (tei/load-schema (System/getenv "TEI_SCHEMA_PATH"))] ...)` — and reuse the binding across all paths in this step.
3. Iterate the TEI document set sequentially: v0 is `["examples/v0/example-work/tei.xml"]`. The validator function takes a seq so future fixtures slot in cleanly.
4. **Severity policy.** For each document, partition `:violations` by `:severity`:
   - `:error` and `:fatal` are conformance failures and contribute to the harness step's failure set.
   - `:warning` is logged via Telemere (`tel/log! :warn ...`) but does not fail the step. Jing emits warnings for non-conformance-affecting issues like dangling IDREFs in standalone documents; treating those as failures is wrong.
5. **Cross-file aggregation.** The harness step accumulates failure-tier violations from every document, then renders each violation to a single line (severity, label, line:col, message) and throws once via `check-errors!` if the accumulated vector is non-empty. `check-errors!` already takes a vector of strings; the rendering happens at the harness boundary, mirroring how the SHACL pass converts violation maps to strings.

The schema binding is the only "cache" — it's a local `let` in this step. The `abc.tools.tei` namespace itself is stateless.

### Fixture fix

The current `examples/v0/example-work/tei.xml` contains a `<charName>` element inside `<char>`. **Verified against TEI P5 4.11.0** (`tei_all.rng` `<define name="char">`): the `<char>` content model is `(unicodeProp | unihanProp | localProp | mapping | figure | model.graphicLike | model.noteLike | model.descLike)*`. `<charName>` is not defined — it is invalid markup, not a typo for an existing element.

The TEI-idiomatic way to attach a project-specific name to a character is `<localProp name="charName" value="..."/>`: a "local property" carries non-Unicode-standard project metadata. Replace the existing line:

```xml
<charName>Example unresolved Aozora gaiji fixture</charName>
```

with:

```xml
<localProp name="charName" value="Example unresolved Aozora gaiji fixture"/>
```

This preserves the original semantic intent (a name attached to a character entry) within the schema, rather than silently dropping it. The existing `<desc>` line stays.

Other elements in the fixture — `<ruby>/<rb>/<rt>`, `<g ref="..."/>`, `<head>`, `<p>`, `<div>` — are valid in TEI 4.11.0 (TEI added native `<ruby>` in 4.2.0). The Jing pass over the fixed fixture is the authoritative check.

## Data Flow

```
TEI XML path(s) + loaded schema (held in harness binding)
  → tei/validate! {:schema, :xml-path, :label}
    ↳ fresh ValidationDriver per call
    ↳ ErrorHandler writes to a per-call volatile! (single-thread)
    ↳ returns {:label, :violations [{:severity, :line, :column, :message}, ...]}
  → harness partitions :violations by severity
    ↳ :warning → tel/log! :warn (does not fail step)
    ↳ :error / :fatal → accumulate into failure vec
  → after all files: render failure vec to strings, throw via check-errors!
                                            → rendered by validate-design-bundle's -main
```

## Test Plan

### `test/abc/tools/tei_test.clj` (new)

A test fixture (`use-fixtures :once`) reads `TEI_SCHEMA_PATH`; if unset, **all** tests in this namespace fail with a clear message rather than silently skipping. Skipping would let CI green-light a run where the harness contract isn't being exercised. The harness has the same loud-failure behavior; tests must mirror it, not diverge.

Positive:

- `validate!` returns a map with empty `:violations` for `examples/v0/example-work/tei.xml` against the schema at `TEI_SCHEMA_PATH` (after the fixture fix from this spec).

Negative (constructed in-test by writing temp files; Jing's source API is file/InputSource based):

- A TEI document missing `<teiHeader>` produces a non-empty `:violations` vector containing an `:error`-severity entry whose `:message` references `teiHeader`.
- A TEI document with an undefined element (`<bogusElement>`) produces a violation entry that mentions the offending element name.
- A TEI document with a Jing-warning-tier issue (e.g., a dangling `IDREF`) produces a violation with `:severity :warning` and **does not** end up in the harness's failure set when run through the harness step. (This second assertion is in the harness extension below; the unit test here just checks severity preservation.)

### `test/abc/tools/validate_design_bundle_test.clj` (extension)

Two new tests, mirroring `validate-shacl-smoke-test` plus the loud-fail contract:

1. **Smoke positive.** With `TEI_SCHEMA_PATH` set (otherwise fail loudly per the namespace fixture above), the harness's TEI step returns nil for the example fixture set.
2. **Loud-fail when env unset.** Temporarily unset `TEI_SCHEMA_PATH` (via `with-redefs` on `System/getenv` or by calling the step's helper with explicit `nil`) and assert the helper throws `ex-info` with a message naming the variable. This catches the divergence between "test skipped" (silent) and "harness fails" (loud) that would otherwise hide regressions in the loud-fail path.

## Dependencies

- Add `org.relaxng/jing {:mvn/version "20241231"}` to `deps.edn` and `nix/clj-nix-deps.edn`. Verified current at design time on Maven Central; if a newer release exists at implementation time, prefer it.
- Regenerate `deps-lock.json` via `bin/update-clj-nix-lock`.
- The `pkgs.fetchurl` derivation lands `tei_all.rng` in the Nix store at a deterministic path; no project-relative file change for the schema bytes.

## Error Handling

- Jing emits warnings, errors, and fatal errors via `ErrorHandler` callbacks. We collect all three into the violation vector with severity tagged so the renderer can prefix accordingly.
- Per-document validation is binary: either `:ok` or throw with all violations aggregated.
- The harness aggregates violations across all TEI documents in one pass before failing the run.

## Acceptance Criteria

- `clojure -M:test` passes including the new `tei-test` and the extended `validate_design_bundle_test` (including the loud-fail-when-env-unset assertion).
- `nix run .#validate-design-bundle` prints the new `==> Validating TEI against P5 RelaxNG` step and exits 0; `examples/v0/example-work/tei.xml` validates clean.
- `nix flake check` evaluates and the focused-test check passes with the new dependency.
- `clojure -M:abc/validate-design-bundle` outside Nix without `TEI_SCHEMA_PATH` exits 1 with a message naming the missing env var. The matching test (loud-fail extension above) covers the same code path so test and harness contracts agree.
- Restoring the original `<charName>` line in the example fixture causes the new TEI step to fail with a violation referencing the offending element. (Proves the new check is enforced, not skipped.)
- Injecting a Jing-warning-tier issue (e.g., a dangling `IDREF`) into the example fixture surfaces a `tel/log! :warn` line but does **not** fail the harness step. (Proves warnings are not silently elevated to failures.)

## Sequencing

1. Add `org.relaxng/jing` dep, regenerate clj-nix lock.
2. Add `tei-schema` `pkgs.fetchurl` derivation in `flake.nix` and wire `TEI_SCHEMA_PATH` into the `validate-design-bundle` wrapper script.
3. TDD: positive test for `tei/load-schema` → minimal namespace.
4. TDD: positive then negative tests for `tei/validate!` (including the warning-severity preservation case) → wrapper around Jing's `ValidationDriver` with custom `ErrorHandler`.
5. **TDD-driven fixture fix.** Write a positive test in `tei_test.clj` asserting that `examples/v0/example-work/tei.xml` validates clean. Run it: it fails on the existing `<charName>` element. Replace `<charName>` with `<localProp name="charName" value="..."/>` (per Fixture Fix above). Re-run: passes. The fixture fix is then driven by a failing test, not by manual confirmation.
6. Wire new step into `validate-design-bundle` with the warning vs error/fatal severity policy. Add the smoke positive + loud-fail tests in `validate_design_bundle_test`.
7. Update `flake.nix` `contract-surface` check + focused-test alias to include the new files.
8. End-to-end verification: `nix run .#validate-design-bundle`, `nix flake check`.

## References

- Apache/W3C RelaxNG; Jing reference impl — `https://relaxng.org/jclark/jing.html`
- TEI P5 4.11.0 release — `https://github.com/TEIC/TEI/releases/tag/P5_Release_4.11.0`
- Upstream RelaxNG schema — `https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng`
- `docs/high-level-architecture-note.md` §"TEI and XML Profile"
- `src/abc/tools/shacl.clj` — pattern reference for in-process JVM validator with structured violations
