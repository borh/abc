# Parser-IR Synthetic Evidence Additions

**Date:** 2026-07-07
**Parser-IR schema:** `0.6.0`
**Parser-IR schema hash:** `sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340`
**Mapping version:** `0.2.5`
**Mapping hash:** `sha256:20a3b9a7079b727918ccc5ef20924bc0cef5e0359a9a9647535c1a173c8781f4`

The following fields are produced by conversion policy, not by AAT source
fields and not by the generated-probe divergence taxonomy:

| Parser-IR Field | Source | Reason |
|---|---|---|
| `sentence_segmentation` | converter policy | declares splitter identity and coordinate system |
| `sentences[]` | visible body text + splitter | sentence evidence for TEI rendering |
| `sentences[].tags` | overlap with `orthographic_annotations.annotations` | renderer-facing orthographic sentence classification |
| `orthographic_annotations` | optional sidecar | detector provenance, not AAT markup |

These fields are intentionally absent from
`transform_rule_descriptions` until the mapping schema has a dedicated
synthetic-evidence section.
