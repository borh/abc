# JADH 2026 paper demo artifacts (frozen)

Frozen evidence for the accepted JADH 2026 abstract "Sustaining Aozora Bunko
as Versioned Corpus Infrastructure" (`docs/presentations/jadh-2026-wip-slides.md`
is the presentation counterpart). These bytes back specific numbers in the
abstract and must not be regenerated in place; a new run gets a new directory.

- `demo-rashomon-real/` — 羅生門 (000879) processed through the aozora2html
  adapter lane: parser-IR, divergence record (including the abstract's three
  nested-ruby LOSS occurrences, rule L-45), TEI, plaintext, and manifests.
- `demo-melos-real/` — 走れメロス (000035), same layout; the TEI-familiar
  comparison target.
- `demo-trace.md` — the walkthrough narrative connecting source inventory,
  one-work divergence, and tokenizer comparison.
- `demo-source-corpus-snapshot.json` — the shared source snapshot both demo
  works were drawn from.
- `tokenizer-comparison-view.md` — the frozen 求むる / 竹馬の友 tokenizer
  comparison regions (Sudachi A/B/C vs Vibrato with UniDic CWJ/CSJ).

Provenance locators inside these files (e.g. `/db/...` AAT corpus paths) are
historical: they describe the producing run and are not expected to resolve.
The aozora2html comparison lane is retired; these artifacts are the surviving
record of its output for the paper's demo works.
