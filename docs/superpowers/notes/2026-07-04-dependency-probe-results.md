# Dependency Probe Results

Captured during Tasks 5/6 of
`2026-07-04-dependency-and-code-quality-hardening`.

## Jena / SHACL

Runtime facts:

- Local `java -version`: OpenJDK `21.0.10`.
- `nix develop -c java -version`: OpenJDK `21.0.12`.
- `nix develop -c clojure -e '(System/getProperty "java.version")'`: `21.0.12`.
- `.github/workflows/validation.yml` runs `nix run .#validate-design-bundle`;
  it does not use the runner JVM directly for ABC Clojure execution.

Current behavior gates on the existing Jena line:

```bash
bin/kaocha --focus abc.tools.shacl-test
bin/kaocha --focus abc.tools.manifest-to-rdf-test
```

Both pass. The Java 21 precondition for a future Jena 6.1.0 probe is satisfied
by the Nix runtime path. No Jena version bump was merged in this slice; the next
Jena action is a dedicated dependency probe commit that updates only
`org.apache.jena/jena-shacl`, regenerates locks, and reruns the SHACL/RDF gates
plus `nix run .#validate-design-bundle` and `nix flake check --print-build-logs`.

## Titanium JSON-LD

Current behavior gate:

```bash
bin/kaocha --focus abc.tools.linked-art-test
```

Passes, including the ADR 0013 external-context refusal test. The
`2.0.0-M2` update remains probe-only because it is a milestone release and may
change publication-view bytes. A future probe must compare committed Linked Art
fixture bytes/digests and keep the external-context refusal invariant green.

## Saxon

`deps.edn`'s Maven Saxon dependency is live. Evidence:

- `src/abc/tools/schematron.clj` imports
  `net.sf.saxon.s9api.Processor` and `XdmNode`.
- `bin/kaocha --focus abc.tools.schematron-test` passes with the current
  Saxon dependency.
- Direct `bin/kaocha --focus abc.tools.tei-test` requires `TEI_SCHEMA_PATH`;
  the established sandbox form `ABC_TEI_SCHEMA_SKIP=1 bin/kaocha --focus
  abc.tools.tei-test` exits green with the tests skipped.
- End-to-end TEI behavior remains covered by `nix run .#validate-design-bundle`.

Conclusion: the Maven Saxon dependency is not dead weight. The next Saxon action
is a dedicated SaxonJ `12.9` probe before any jump to `13.0`.

## Legacy Runtime Dependencies

XTDB:

- `abc.xtdb` / `abc.load` own the XTDB runtime dependency.
- Task 4 removed startup-on-require for `abc.xtdb`.
- `timeout 10s clojure -M:test -e "(require 'abc.xtdb) (println :xtdb-required)"`
  exits `0`.
- `abc.load` still does not make a standalone child process exit promptly, but
  the narrower probes show that the remaining process hold comes from
  `abc.annotation`, not XTDB.

Tawny/OWL:

- `uk.org.russet/tawny-owl` is owned by `src/abc/owl.clj` and
  `test/abc/owl_test.clj`.
- It is not part of the active v0 `abc.tools.*` publication path.
- The `3.0.0` update remains blocked on legacy namespace disposition.

GraalJS:

- No project source imports GraalJS directly; it is present for the JVM schema /
  `m3` stack.
- Because `m3` validation is active in `abc.tools.schema`, a GraalJS update
  should be tested through schema and design-bundle gates, not by dependency
  evaluation alone.

## Disposition

Recommended current disposition remains: status quo plus explicit boundary.
Do not make the full legacy tree a blocking lint or dependency-upgrade surface
until `../ab-validator` produces parser-IR that drives TEI/plaintext publication
here and the old parser-era namespaces can be quarantined or retired.
