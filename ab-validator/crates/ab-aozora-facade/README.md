# Aozora parser facade

`ab-aozora-facade` parses Aozora Bunko notation and exposes HTML rendering,
canonical source serialization, exact source replay, diagnostics, and incremental
editing. It is an independent fork of
[P4suta/aozora](https://github.com/P4suta/aozora) at revision
`1a4f864603970983719655aa4af4525958ac2d38`.

```rust
use ab_aozora_facade::Document;

let document = Document::new("｜青梅《おうめ》".to_owned());
let tree = document.parse();

// A kanji base at the start of a line needs no explicit ruby-base marker.
assert_eq!(tree.to_source(), "青梅《おうめ》");
assert_eq!(tree.sanitized(), "｜青梅《おうめ》");
assert!(tree.to_html().contains("<ruby>"));
```

`Document` owns the source buffer. A parsed `Tree` borrows that buffer and owns
its flat node store. The store interns repeated strings and addresses content
and segment pools through integer handles.

The default feature set is empty. `entries` exposes typed wire projections
without JSON serialization. `json` adds JSON projections and enables
`serde_json/preserve_order`; `schema` adds schema introspection. Publication
uses `entries` to avoid changing workspace JSON map ordering.

Run checks through the monorepo's Nix flake. See the repository root `AGENTS.md`
for development commands.

Dual-licensed under [Apache-2.0](LICENSE-APACHE) or [MIT](LICENSE-MIT).
[NOTICE](NOTICE) retains upstream attribution.
