# aozora2html visible_text_body_order Characterization

Date: 2026-07-03
Source run: `aozora2html-full-20260703T020301Z` (668 per-report failures, ~662 unique works)

## Method

For each failing work, ran `cargo run -p ab-check --example vtbo_locate` against
the persisted AAT (resolved by glob, since filenames are suffixed) and the
source bytes (work_id joined to `index.json.works[].txt_path`, extracted from
the zip entry). The locator mirrors the exact `visible_text_body_order`
property: `decode_source_bytes` -> `body_text` -> `comparison_lossy_body` ->
`normalize_visible` (NFKC + whitespace-collapse) on the source side, and
`comparison_visible_text_projection` -> `normalize_visible` on the AAT side,
then finds the first projection char that is not a subsequence of the source.

## Sample findings (first 20)

Run with `AB_AOZORA2HTML_VTBO_SAMPLE=20`; `characterized 21 works` (the loop
counts one extra row beyond 20 because the sample bound is checked after the
increment). 20 per-work `.txt` outputs were written.

divergence indices observed (work_id, index, divergence char, one-line
context from the projection window):

| work_id | pi | divchar | projection context around divergence |
| --- | --- | --- | --- |
| 000081_4418 | 0 | `※` | `※ 赤い手の長い蜘蛛と、銀いろのなめくぢと…` |
| 000081_4421 | 925 | `※` | `…帰って行きました。 ※ あとでカン蛙は腕を組んで考へました。…` |
| 000081_4422 | 345 | `※` | `…ひを揚げただけでした。 ※ 黄色な方の一本が、こゝ…` |
| 000081_4427 | 0 | `※` | `※ 旧暦の六月二十四日の晩…` |
| 000081_4441 | 1559 | `※` | `…になってしまひました。 ※ ところがその話はだんだ…` |
| 000081_4442 | 3507 | `※` | `…豆を抜きはじめました。 ※ 火は赤く燃えてゐます。…` |
| 000081_4468 | 7 | `※` | `本郷区菊坂町 ※ 九時過ぎたので、床屋の…` |
| 000933_47815 | 54 | `ノ` | `…「石田ノ局」を出し物とし、ほかに…` |
| 000933_47823 | 4 | `ノ` | `…「女殺油ノ地獄」の芝居を、見て戻つ…` |
| 000933_49177 | 5463 | `ノ` | `…皇后の磐ノ姫といふ方は大変嫉妬の激…` |
| 000081_455 | 0 | `[` | `[表記について] ●底本に…` |
| 000236_4300 | 5396 | `[` | `…クルミ[「クルミ」は底本では「 ルミ」と誤植]さん…` |
| 000124_1315 | 26639 | `<` | `…(小熊生) 広瀬操吉<br/> 今野紫藻<br/>…` |
| 000042_42768 | 6152 | `ώ` | `…「氷」に通じ χιών(雪)にも通じる。露語…` |
| 000064_4527 | 1283 | `ト` | `…こなしとゝめるも云ふも一ト筋道横町の方に植木は多し…` |

recurring patterns (named from the real data):

- **Pattern A — standalone `※` scene/section separator retained by AAT,
  stripped by lossy source (dominant, 8/20 inspected works).** The Aozora
  source uses a standalone `※` (e.g. `　　　　　　　　　　※`) as a decorative
  scene-break mark — distinct from the gaiji marker `※［＃…］`. The AAT
  preserves this `※` in `comparison_visible_text_projection`, but the source
  lossy projection `comparison_lossy_body` strips every `※` (it cannot tell a
  scene-break `※` from a gaiji-description `※`). Confirmed for 000081_4418
  (`※` count in raw source = 2 standalone, 0 gaiji-form).
- **Pattern B — editorial annotation brackets retained by AAT, stripped by
  lossy source.** The AAT keeps bracketed editorial annotations such as
  `[表記について]` (000081_455, pi=0) or the底本誤植 correction note
  `[「クルミ」は底本では「 ルミ」と誤植]` (000236_4300, pi=5396) as visible
  text; `comparison_lossy_body` collapses them on the source side, so the
  subsequence stalls at the `[`.
- **Pattern C — old-kana `ノ` in proper nouns (subsequence exhaustion).**
  Three works (000933_47815 / 000933_47823 / 000933_49177) stall on a fullwidth
  katakana `ノ` inside old-style proper-noun spellings (`「石田ノ局」`,
  `「女殺油ノ地獄」`, `磐ノ姫`). The source cursor is exhausted, i.e. the lossy
  source emits fewer `ノ` than the AAT projection requires at that point —
  needs per-case inspection to decide whether the AAT duplicates a `ノ` or the
  lossy source collapses one.
- **Pattern D — minority adapter-bug candidates.** 000124_1315 stalls on `<`
  (`<br/>` HTML literal leaking into the AAT visible-text projection — the
  adapter is emitting raw markup as if it were text). 000042_42768 stalls on
  `ώ` inside `χιών(雪)` (Greek diacritic retained by one side and renormalized
  away by the other after NFKC). Both look like adapter output bugs, not
  property lossiness.

## Next-step routing

- **acceptable-abstraction shaped (Patterns A, B):** the lossy source
  projection (`comparison_lossy_body`) deliberately strips `※` scene markers
  and editorial `[……]` annotations, while the AAT legitimately preserves them.
  These are NOT adapter bugs. Routing: widen the lossy source projection
  (preserve standalone `※` and pass-through editorial brackets) OR relax the
  VTBO property to ignore these token classes before the subsequence check.
  Document the intent either way.
- **investigate-then-route (Pattern C):** the `ノ`-in-proper-noun stalls need
  a side-by-side trace (run the locator with `--context 200`) to decide
  adapter-duplication vs lossy-source-collapse before filing.
- **adapter-bug shaped (Pattern D):** `<br/>` literal in visible-text output
  and Greek `ώ` retention are adapter bugs — file against the adapter so it
  does not emit markup / improperly-normalized glyphs into the visible-text
  projection.

Full 668-work run is a follow-up; this report establishes the classification
method and the dominant sample patterns. The largest bucket by far is Pattern A
(standalone `※`, ~40% of the inspected sample), so the property/lossy-source
handling of `※` is the highest-leverage fix.
