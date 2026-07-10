# 走れメロス Soranoha TEI WIP Evidence Snapshot

This record captures a verified output from the 2026-07-10 hermetic
full-corpus `build-publication` run. It makes the presentation excerpt and its
validation evidence inspectable without treating a temporary output directory
as a repository dependency.

## Inputs

- Soranoha revision `606937f5`
- Aozora Bunko revision `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`
- Work slug `001567_000035_1567_ruby_4948`
- Parser profile `aozora2html`
- Publication profile `tei-profile-v0`
- Snapshot date `2026-07-10`
- Flake-provided parser, adapter, converter, and validation binaries; all
  ambient `AB_*` variables unset

The run used an ad-hoc full-corpus configuration because the committed
`abc/config/publication-basic-ja.json` still selects the unsupported transitional
profile `parser-ir-publication-basic-ja-v1`. There is not yet a stable bounded
single-work command suitable for a live presentation.

## Verified outputs

| Artifact | SHA-256 |
| --- | --- |
| `tei.xml` | `c5fa74b94b853878857f390e900ebe04bbf7479493042ef580faa9c9ce1d3481` |
| `plain.txt` | `b4147bcc5424b725bbc1f6f3a95355060f9dcf36bf27b95860afa6352be13894` |
| `tei.manifest.json` | `cad1ffba1cf80067dd13b6981172b30c93d0e5af72d2f99c39c36ee42a3a8631` |
| `tei-validation-result.json` | `d4a373cf59e6909352a57e95fa53e571558107fd5ace3a8398f22c19b11b2c61` |

The TEI manifest records ArtifactID
`sha256:c65d60679502f267cf54d1614d457e93d1852261245335bd901138a1c052d7da`
and validation status `passed`. Relax NG, Schematron, and XML well-formedness
all passed with zero findings.

The manifest references `preservation.json`, which was not included in the
temporary four-file presentation copy. Its parser build, parser configuration,
and AAT–Parser-IR mapping coordinates are null. This snapshot therefore supports
the displayed XML and validation claims, but not a claim that every identity
coordinate is populated.

## Displayed excerpt

```xml
<p>
  <s xml:id="s000000">　メロスは激怒した。</s>
  <s xml:id="s000001">必ず、かの
    <ruby type="furigana" rend="right">
      <rb>邪智暴虐</rb><rt>じゃちぼうぎゃく</rt>
    </ruby>の王を除かなければならぬと決意した。</s>
  <s xml:id="s000002">メロスには政治がわからぬ。</s>
  <s xml:id="s000003">メロスは、村の牧人である。</s>
</p>
```

## Provisional full-corpus recipe

The verified run resolved `.#ab-validator-aozorabunko-corpus`, then invoked
`nix run .#soranoha -- build-publication` from `abc/` with an ad-hoc JSON config
selecting `aozora2html`, `tei-profile-v0`, fail-soft operation, and
`materialization_scope` `full-corpus`. This is provenance for the snapshot, not
the final live-demo recipe: it runs the full corpus and takes approximately two
hours.
