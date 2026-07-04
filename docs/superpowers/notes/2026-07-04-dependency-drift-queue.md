# Dependency Drift Queue

Captured during Task 1 of `2026-07-04-dependency-and-code-quality-hardening`.
`clojure -M:update` exited non-zero as expected after the conservative
baseline dependency refresh. The remaining updates are deferred to probe lanes
or legacy-disposition work rather than mixed into the baseline commit.

| File | Dependency | Current | Latest | Disposition |
| --- | --- | --- | --- | --- |
| `.github/workflows/validation.yml` | `actions/checkout` | `v4` | `v7` | Handled by Task 2 CI-maintenance slice. |
| `deps.edn` | `com.apicatalog/titanium-json-ld` | `1.7.0` | `2.0.0-M2` | Probe only with ADR 0013 JSON-LD fixture bytes and external-context refusal. |
| `deps.edn` | `com.xtdb/xtdb-core` | ~~`1.24.5`~~ | — | **Resolved 2026-07-04.** XTDB removed entirely (retired, not upgraded to v2). See plan `docs/superpowers/plans/2026-07-04-remove-xtdb.md`. |
| `deps.edn` | `net.sf.saxon/Saxon-HE` | `9.6.0-4` | `13.0` | First prove the Maven Saxon dependency is live; remove/quarantine if not, otherwise probe SaxonJ 12.9 before 13. |
| `deps.edn` | `org.apache.jena/jena-shacl` | `5.3.0` | `6.1.0` | Probe with Java 21 local/Nix/CI policy and LOD/SHACL gates. |
| `deps.edn` | `org.graalvm.js/js-language` | `24.2.2` | `25.1.3` | Defer unless a current v0 command proves this runtime is active. |
| `deps.edn` | `uk.org.russet/tawny-owl` | `2.3.3` | `3.0.0` | Defer until `abc.owl` legacy disposition. |

Available upstream references printed by Antq:

- `actions/checkout`: <https://github.com/actions/checkout/blob/v7/CHANGELOG.md>
- `xtdb`: <https://github.com/xtdb/xtdb/compare/1.24.5...v2.1.0>
- `graaljs`: <https://github.com/graalvm/graaljs/blob/graal-25.1.3/CHANGELOG.md>
- `tawny-owl`: <https://github.com/phillord/tawny-owl/compare/2.3.3...3.0.0>
