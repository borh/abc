# TEI-EAJ Aozora Melos Comparison Report

Status: Generated comparison probe
Date: 2026-07-04

## Inputs

- ABC TEI: `paper/demo-melos-real/tei.xml`
- TEI-EAJ source root: `/nix/store/5m0yq00kf3nxlmc3x4g2jx42gvkwrbgs-source`
- TEI-EAJ source revision: `77a675fc2771936f9544505d922d4cd45075338c`
- TEI-EAJ XML files scanned: 62
- TEI-EAJ Melos files found: 2
- Complete Melos files: 2
- Draft Melos files: 0

No draft Melos TEI files were found in the pinned TEI-EAJ checkout.
Draft coverage below is therefore corpus-wide structural context, not
a literal draft Melos text comparison.

## Level/State Coverage

| State | Level | Files |
| --- | --- | --- |
| complete | Level 3 | 39 |
| complete | Level 4 | 6 |
| draft | Level 2 | 3 |
| draft | Level 3 | 5 |
| draft | Level 4 | 7 |
| etc |  | 2 |

## Melos File Comparisons

| TEI-EAJ file | State | Level | Base text equal | ABC chars | TEI-EAJ chars | ABC p | TEI-EAJ p | ABC note | TEI-EAJ note | TEI-EAJ persName | TEI-EAJ said |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `data/complete/tei_lib_lv4/1567_header_updated.xml` | complete | Level 4 | no | 9806 | 9790 | 1 | 22 | 1 | 3 | 104 | 59 |
| `data/complete/tei_lib_lv4/1567_tei.xml` | complete | Level 4 | no | 9806 | 9790 | 1 | 19 | 1 | 0 | 229 | 59 |

### First Text Differences

- `data/complete/tei_lib_lv4/1567_header_updated.xml`:
  first difference at normalized base-text offset 9790.
  ABC window: `を、皆に見られるのが、たまらなく口惜しいのだ。」勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）`
  TEI-EAJ window: `を、皆に見られるのが、たまらなく口惜しいのだ。」勇者は、ひどく赤面した。`
- `data/complete/tei_lib_lv4/1567_tei.xml`:
  first difference at normalized base-text offset 9790.
  ABC window: `を、皆に見られるのが、たまらなく口惜しいのだ。」勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）`
  TEI-EAJ window: `を、皆に見られるのが、たまらなく口惜しいのだ。」勇者は、ひどく赤面した。`

## Feature Prevalence Across All TEI-EAJ Files

| Feature | Files With Feature | Max Count In One File | ABC Count |
| --- | --- | --- | --- |
| p | 62 | 498 | 1 |
| note | 57 | 22 | 1 |
| div | 20 | 115 | 0 |
| head | 12 | 9 | 0 |
| ruby | 5 | 88 | 88 |
| rp | 1 | 176 | 0 |
| persName | 45 | 2190 | 3 |
| rs | 4 | 182 | 0 |
| placeName | 29 | 96 | 0 |
| roleName | 4 | 16 | 0 |
| said | 4 | 59 | 0 |
| listPerson | 14 | 2 | 0 |
| person | 14 | 45 | 0 |
| revisionDesc | 11 | 1 | 0 |
| editorialDecl | 4 | 1 | 0 |
| sourceDesc | 62 | 1 | 1 |
| front | 12 | 1 | 0 |
| body | 60 | 1 | 1 |
| back | 8 | 1 | 0 |

## All TEI-EAJ XML Files

| Path | State | Level | Title | p | ruby | note | persName | placeName | roleName | said | listPerson |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `data/complete/tei_lib_lv3/1126_tei.xml` | complete | Level 3 | 三つの宝 | 172 | 0 | 1 | 11 | 0 | 0 | 0 | 1 |
| `data/complete/tei_lib_lv3/15099_tei.xml` | complete | Level 3 | 長崎小品 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/15938_tei.xml` | complete | Level 3 | 元禄時代小説第一巻「本朝二十不孝」ぬきほ（言文一致訳） | 7 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/236_tei.xml` | complete | Level 3 | ア、秋 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/2509_tei.xml` | complete | Level 3 | 天災と国防 | 29 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/42320_tei.xml` | complete | Level 3 | 西洋にはない | 5 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/43077_tei.xml` | complete | Level 3 | 地震雑感 | 31 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/43563_tei.xml` | complete | Level 3 | デンマルク国の話: 信仰と樹木とをもって国を救いし話 | 27 | 0 | 2 | 2 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/4411_tei.xml` | complete | Level 3 | 岩波茂雄宛書簡 | 4 | 0 | 1 | 6 | 0 | 0 | 0 | 1 |
| `data/complete/tei_lib_lv3/4464_tei.xml` | complete | Level 3 | 秋田街道 | 18 | 0 | 2 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/45093_tei.xml` | complete | Level 3 | 乳母車 | 6 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/4872_tei.xml` | complete | Level 3 | 愛読書の印象 | 6 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/50502_tei.xml` | complete | Level 3 | 北海道に就いての印象 | 9 | 0 | 4 | 2 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/51307_tei.xml` | complete | Level 3 | みだれ髪 | 8 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/51520_tei.xml` | complete | Level 3 | かたい大きな手 | 78 | 0 | 1 | 4 | 0 | 0 | 55 | 1 |
| `data/complete/tei_lib_lv3/53386_tei.xml` | complete | Level 3 | 〔月光の鉛のなかに〕 | 2 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/53617_tei.xml` | complete | Level 3 | 巡禮紀行 | 3 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/55783_tei.xml` | complete | Level 3 | 夢 | 129 | 0 | 1 | 3 | 0 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/56996_tei.xml` | complete | Level 3 | 書簡 山田邦子宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/56998_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/56999_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 9 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57001_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57002_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57003_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57004_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57005_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57006_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57037_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 1 | 3 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57038_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57039_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57040_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57041_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57042_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57043_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57044_tei.xml` | complete | Level 3 | 書簡 大杉栄宛 | 3 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57046_tei.xml` | complete | Level 3 | 書簡 木村荘太宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57047_tei.xml` | complete | Level 3 | 書簡 武部ツタ宛 | 2 | 0 | 1 | 2 | 2 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/57048_tei.xml` | complete | Level 3 | 書簡 武部ツタ宛 | 2 | 0 | 1 | 2 | 3 | 0 | 0 | 0 |
| `data/complete/tei_lib_lv3/86_tei.xml` | complete | Level 3 | 二人小町 | 139 | 0 | 1 | 4 | 0 | 0 | 0 | 1 |
| `data/complete/tei_lib_lv4/104_15099.xml` | complete | Level 4 | 長崎小品 | 60 | 0 | 1 | 13 | 0 | 0 | 0 | 1 |
| `data/complete/tei_lib_lv4/1567_header_updated.xml` | complete | Level 4 | 走れメロス | 22 | 88 | 3 | 104 | 24 | 13 | 59 | 1 |
| `data/complete/tei_lib_lv4/1567_tei.xml` | complete | Level 4 | 走れメロス | 19 | 88 | 0 | 229 | 51 | 16 | 59 | 1 |
| `data/complete/tei_lib_lv4/45245_tei.xml` | complete | Level 4 | 高瀬舟 | 5 | 0 | 7 | 7 | 5 | 0 | 11 | 1 |
| `data/complete/tei_lib_lv4/50362_tei.xml` | complete | Level 4 | 黒船来航 | 13 | 0 | 1 | 20 | 13 | 1 | 0 | 0 |
| `data/complete/tei_lib_lv4/7928_tei.xml` | complete | Level 4 | 旅人（一幕） | 112 | 0 | 1 | 66 | 0 | 0 | 0 | 1 |
| `data/draft/tei_lib_lv2/01.xml` | draft | Level 2 | 源氏物語 第1冊 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv2/02.xml` | draft | Level 2 | 源氏物語 第2冊 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml` | draft | Level 2 | Title | 4 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv3/1126_tei.xml` | draft | Level 3 | 三つの宝 | 156 | 0 | 1 | 11 | 0 | 0 | 0 | 1 |
| `data/draft/tei_lib_lv3/1576_tei.xml` | draft | Level 3 | 新ハムレット | 498 | 0 | 1 | 2190 | 96 | 0 | 0 | 1 |
| `data/draft/tei_lib_lv3/43563_tei.xml` | draft | Level 3 | デンマルク国の話: 信仰と樹木とをもって国を救いし話 | 27 | 0 | 2 | 2 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv3/52208_tei.xml` | draft | Level 3 | 帝大聖書研究会終講の辞 | 19 | 0 | 2 | 2 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv3/86_tei.xml` | draft | Level 3 | 二人小町 | 125 | 0 | 1 | 6 | 0 | 0 | 0 | 1 |
| `data/draft/tei_lib_lv4/1805_tei.xml` | draft | Level 4 | 安重根 | 34 | 0 | 2 | 45 | 5 | 0 | 0 | 2 |
| `data/draft/tei_lib_lv4/2571_tei.xml` | draft | Level 4 | 鈴木三重吉宛書簡―明治三十九年 | 4 | 0 | 1 | 3 | 4 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv4/4244-1_tei.xml` | draft | Level 4 | 獄中への手紙 | 25 | 10 | 22 | 2 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv4/4244-3_tei.xml` | draft | Level 4 | 獄中への手紙 | 14 | 6 | 3 | 2 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv4/4244-4_tei.xml` | draft | Level 4 | 獄中への手紙 | 9 | 3 | 2 | 2 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv4/46453_tei.xml` | draft | Level 4 | 春 | 93 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| `data/draft/tei_lib_lv4/54457_tei.xml` | draft | Level 4 | 秋の暮 | 16 | 0 | 1 | 2 | 3 | 0 | 0 | 0 |
| `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml` | etc |  | 勢理客村湧川親雲上勤職書 | 7 | 0 | 0 | 2 | 7 | 1 | 0 | 1 |
| `data/etc/校異源氏物語_header更新版.xml` | etc |  | 校異源氏物語・きりつぼ | 7 | 0 | 2 | 0 | 0 | 0 | 0 | 0 |

## Interpretation

- The literal Melos comparison covers every Melos TEI file in the pinned TEI-EAJ checkout.
- Finished and draft TEI-EAJ files are both included in the corpus-wide structural profile.
- Paragraph and source-note/source-attribution structure remain the immediate Level 3 gap for ABC.
- TEI-EAJ Level 4 entity, role, place, and speech markup should remain an enrichment comparison track unless ABC declares an editorial enrichment layer.
